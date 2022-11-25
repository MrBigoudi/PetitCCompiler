(* petitC Abstract Syntax Tree *)
type ident = string


and decl_fct =
  | DFct of ctype * ident * param list * block


and cfile =
  | CFinclude of decl_fct list


and ctype =
  | Tvoid
  | Tint
  | Tbool
  | Tptr of ctype


and param =
  | PIdnt of ctype * ident


and const =
  | Cint of int
  | Cnull


and unop =
  | Uamp   (* & *)
  | Ustar  (* * *)
  | Unot   (* ! *)


and expr =
  { desc: desc ;
    loc : Lexing.position * Lexing.position }

and desc =
  | Econst of const (*constant*)
  | Eident of ident
  (* | Eptr of *)
  | Eassign of expr * expr
  | Ecall of ident * expr list
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr 
  (* | Esize of  *)


and binop =
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * / % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* && || *)


and instr =
  | Iempt
  | Iexpr of expr
  | Iif of expr * instr * instr
  | Iwhile of expr * instr
  | Ifor of decl_var option * expr option * expr list * instr
  | Iblock of block
  | Iret of expr option
  | Ibrk
  | Icontinue



and decl_instr =
  | DIvar of decl_var
  | DInstr of instr
  (* option Dfct of decl_fct *)


and block =
  | Blck of decl_instr list


and decl_var =
  | DVar of ctype * ident * expr option
