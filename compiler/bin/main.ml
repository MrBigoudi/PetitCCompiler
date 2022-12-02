(** Main program for the petitC Compiler *)

open Format
open Syntax
open Lexing

(** Print the compiler usage *)
let usage = "usage: petitCCompiler [options] file.c"

(** A compiler option *)
let parse_only = ref true

(** The option list*)
let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
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
      begin
        ignore(Parser.file Lexer.token lb);
        close_in c;
        if !parse_only 
          then exit 0
          else failwith "Typage"
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
        Ocolor_format.eprintf "\tsyntax error @.";
        exit 1
      | e ->
        Ocolor_format.eprintf "Anomaly: %s\n@." (Printexc.to_string e);
        exit 2