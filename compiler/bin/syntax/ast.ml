type ident = string

type const = 
  | Int of int
  | True
  | False
  | Null

type unop = Unot

type binop = Badd | Bsub

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
