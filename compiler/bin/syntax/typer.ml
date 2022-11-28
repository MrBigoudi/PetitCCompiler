open Ast

type typ = 
  | Tint 
  | Tbool
  | Tvoid
  | Tptr of typ

module Smap = Map.Make(String)
type env = typ Smap.t

(* lvalue : variable ou *e1 *)

let rec type_expr (*env*) =  function
  | Econst Null -> Tptr(Tvoid)
  | Econst _ -> Tint (* dunno if its more subtle but lets do this way *)
  | Eunop (Unot, e) -> (* NULL is of type void* and !NULL of type int *)
      if type_expr e = Tvoid then failwith "erreur : invalid use of void expression"
      else Tint
  | Eunop (Ustar, e) -> 
      if type_expr e = Tvoid then failwith "erreur : error: void value not ignored as it ought to be"
      else (if type_expr e = Tptr(ty) then ty else failwith "erreur : invalid type of unary `*`"
  | Ebinop (_, _, _) -> Tint (* addition of pointers dont work as expected, be more careful ! *)

let a = type_expr (Econst(Int(1)))
let a'= Econst(Int(1))
let b = type_expr (Econst(True))
let b'= Econst(True)
let c = type_expr (Econst(Null))
let c'= Econst(Null)
let e = type_expr (Ebinop(Badd, b', a'))
let f = type_expr (Ebinop(Bsub, c', a'))
