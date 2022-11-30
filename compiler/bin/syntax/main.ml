open Format
open Parser
open Lexer

let usage = "usage: petitCCompiler [options] file.py"

let parse_only = ref true

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1


let () =
  let c = open_in file in
  try
    let lb = Lexing.from_channel c in
      let cpt = ref 0 in
      begin
        while true do 
          begin
            Parser.file Lexer.token lb;
            incr cpt;
            print_int !cpt;
          end
        done;
        close_in c;
        if !parse_only then exit 0;
          (* Interp.file f *)
          failwith "Typage"
      end
  with
    | Lexer.Lexing_error s ->
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	eprintf "syntax error@.";
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2



