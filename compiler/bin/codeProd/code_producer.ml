open Syntax
open X86_64
open Format

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

(** Compile a typed expression
    val compile_expr : texpression -> text *)
let rec compile_expr (texp: Ast_typed.texpression) =
  let tdesc, typ = texp.tdesc, texp.typ in
  match tdesc with 
  | TEconst cst -> compile_const cst
  | _ -> failwith "TODO"



(** Compile a constant 
    val compile_const : const -> text *)
and compile_const cst =
  match cst with
  | Int i -> pushq (imm i)
  | True -> pushq (imm 1)
  | False -> pushq (imm 0)
  | Null -> failwith "TODO"



(** Compile a typed ast and put the resulting assembler in the output file
    val compile_program : tfileInclude -> string -> program *)
let compile_program (p:Ast_typed.tfileInclude) ofile =
  let p = 
    failwith "TODO code production"
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f