open Syntax
open X86_64
open Format

(** Compile a typed ast and put the resulting assembler in the output file
    val compile_program : tfileInclude string -> void *)
let compile_program (p:Ast_typed.tfileInclude) ofile =
  let p = 
    failwith "TODO code production"
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f