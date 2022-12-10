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
  (* return type * type list of the arguments *)
  | Tfct of typ * typ list

(** val typ_to_string : typ -> string *)
let typ_to_string typ =
  let rec ty_list acc l =
    match l with 
    | [] -> acc
    | ty::[] -> ty_list (acc^" "^(aux "" ty)) []
    | ty::cdr -> ty_list (acc^" "^(aux "" ty)^",") cdr
  and aux acc typ =
    match typ with 
    | Tint -> acc^("Tint")
    | Tbool -> acc^("Tbool")
    | Tvoid -> acc^("Tvoid")
    | Tptr ty -> (aux acc ty)^"*"
    | Tfct(ty, ty_l) -> acc^("( "^(aux "" ty)^", ["^(ty_list "" ty_l)^" ] )")
  in (aux "" typ)

(* NULL is possible but warning from gcc *)
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

(* for errors explanation *)
(** val op_to_string : binop -> string *)
let op_to_string op = 
  let sop =
    match op with
     | Arith(Badd) -> "+"
     | Arith(Bsub) -> "-"
     | Arith(Bmul) -> "*"
     | Arith(Bdiv) -> "/"
     | Arith(Bmod) -> "%"
     | Logic(Beq)  -> "="
     | Logic(Bneq)  -> "!="
     | Logic(Blt)  -> "<"
     | Logic(Ble)  -> "<="
     | Logic(Bgt)  -> ">"
     | Logic(Bge)  -> ">="
     | AndOr(Band) -> "&&"
     | AndOr(Bor) -> "||"
  in sop

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
and dfct = {
  descdfct: descdfct;
  locdfct: loc
}
and descdfct = Dfct of typ * ident * param list * block

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
