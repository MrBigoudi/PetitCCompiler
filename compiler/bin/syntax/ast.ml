type ident = string

type const = 
  | Int of int
  | True
  | False
  | Null

type typ = 
  | Tint 
  | Tbool
  | Tvoid
  | Tptr of typ

(* *NULL is possible but warning from gcc*)
type unop = Unot | Ustar | Uamp | Uincr_l | Udecr_l | Uincr_r | Udecr_r | Uplus | Uminus

(* comparaison between a ptr and an int is possible, but warning from gcc *)
type arith_binop = Badd | Bsub | Bmul | Bdiv | Bmod
type logic_binop = Beq | Bneq | Blt | Ble | Bgt | Bge
type andor_binop = Band | Bor

type binop = Arith of arith_binop | Logic of logic_binop | AndOr of andor_binop

type param = Param of typ * ident

type expression = {
  desc: desc;
  loc: Lexing.position * Lexing.position
}

and desc =
  | Econst of const 
  | Evar of ident
  | Eunop of unop * expression
  | Ebinop of binop * expression * expression
  | Eassign of expression * expression
  | Ecall of ident * expression list
  | Esizeof of expression (* maybe adding primitives *)

type dvar = Dvar of typ * ident * expression option

and dinstr = 
  | DinstrVar of dvar
  | Dinstr of instr

and dfct = Dfct of typ * ident * param list * block

and block = Block of dinstr list

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

type fileInclude = FileInclude of dfct list
