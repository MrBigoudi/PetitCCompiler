(** Main program for the petitC Compiler *)

open Format
open Syntax
open Lexing
open Typer

(** Print the compiler usage *)
let usage = "usage: petitCCompiler [options] file.c"

(** A compiler option *)
let parse_only = ref false
let type_only = ref false

(** The option list*)
let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, " stop after typing";
  ]

(** Check if the file given as argument seems correct *)
let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1


let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  Ocolor_format.eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc


(** The main function for the compiler *)
let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
    try
      (* parsing *)
      let parsed_ast = Parser.file Lexer.token lb in
      begin
        close_in c;
        if !parse_only then exit 0 (* parse only *)
          else 
            let typed_ast = Typer.type_ast parsed_ast in
              if !type_only then exit 0 (* type only *)
                else failwith "todo production de code" 
      end
    with
      | Lexer.Lexing_error(s,token) ->
          report (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb);
          if(token<>"")
            then 
              Ocolor_format.eprintf "\t@{<red>Lexical error @}: %s -> @{<blue>%s @}@." s token
            else
              Ocolor_format.eprintf "\t@{<red>Lexical error @}: %s@." s;
          exit 1
      | Parser.Error ->
          report (Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb);
          Ocolor_format.eprintf "\t@{<red>Syntax error @}@.";
          exit 1
      | Typer.Typing_error(s, loc_err) ->
          report loc_err;
          Ocolor_format.eprintf "\t@{<red>Typing error @}: %s@." s;
          exit 1
      | e ->
          Ocolor_format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
          exit 2
