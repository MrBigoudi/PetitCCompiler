type ident = string

type const = 
  | Int of int
  | True
  | False
  | Null

(* *NULL is possible but warning from gcc*)
type unop = Unot | Ustar

(* comparaison between a ptr and an int is possible, but warning from gcc *)
type binop = Badd | Bsub | Bmul | Bdiv | Bmod

(*type expression = {
  desc: desc;
  loc: Lexing.position * Lexing.position
}*)

type expression =
  | Econst of const 
  | Eunop of unop * expression
  | Ebinop of binop * expression * expression

let a = Econst(Int(1))
let b = Econst(True)
let c = Econst(Null)
let d = Eunop(Unot, a)
let e = Ebinop(Badd, b, a)
let f = Ebinop(Bsub, c, a)
