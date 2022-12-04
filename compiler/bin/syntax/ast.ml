(** Abstract Syntax Tree for the petitC Compiler *)

(** Identifiers *)
type ident = string

(** Localisation in file *)
type loc = Lexing.position * Lexing.position 

(** Constants *)
type const = 
  | Int of int
  | True
  | False
  | Null

(** Possible types *)
type typ = 
  | Tint 
  | Tbool
  | Tvoid
  | Tptr of typ
  (* type de retour * list de type pour les arguments *)
  | Tfct of typ * typ list

(* *NULL is possible but warning from gcc*)
(** Unary operators *)
type unop = Unot | Ustar | Uamp | Uincr_l | Udecr_l | Uincr_r | Udecr_r | Uplus | Uminus

(* comparaison between a ptr and an int is possible, but warning from gcc *)
(** Arithmetical operations *)
type arith_binop = Badd | Bsub | Bmul | Bdiv | Bmod
(** Logical operations *)
type logic_binop = Beq | Bneq | Blt | Ble | Bgt | Bge
(** And and Or operations *)
type andor_binop = Band | Bor

(** Binary operations *)
type binop = Arith of arith_binop | Logic of logic_binop | AndOr of andor_binop

(** Function parameters *)
type param = Param of typ * ident

(** Expressions *)
type expression = {
  desc: desc;
  loc: loc
}

(** Description of expressions *)
and desc =
  | Econst of const 
  | Evar of ident
  | Eunop of unop * expression
  | Ebinop of binop * expression * expression
  | Eassign of expression * expression
  | Ecall of ident * expression list
  | Esizeof of typ (* maybe adding primitives *)

(** Variable declarations *)
type dvar = Dvar of typ * ident * expression option

(** Instruction declarations *)
and dinstr = {
  descdi: descdi;
  locdi: loc
}
and descdi = 
  | DinstrFct of dfct
  | DinstrVar of dvar
  | Dinstr of instr

(** Function declarations *)
and dfct = Dfct of typ * ident * param list * block

(** Instruction blocks *)
and block = Block of dinstr list

(** Instructions *)
and instr =
  | Iempt 
  | Iexpr of expression
  | Iif of expression * instr * instr
  | Iwhile of expression * instr
  | Ifor of dvar option * expression option * expression list * instr
  | Iblock of block
  | Iret of expression option
  | Ibreak 
  | Icontinue

(** File inclusions *)
type fileInclude = FileInclude of dfct list
