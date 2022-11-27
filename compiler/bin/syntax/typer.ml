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
  | Eunop (Unot, c) -> (* NULL is of type void* and !NULL of type int *)
      if type_expr c = Tvoid then failwith "erreur : invalid use of void expression"
      else Tint
  | Ebinop (_, _, _) -> Tint (* addition of pointers dont work as expected, be more careful ! *)

let a = type_expr (Econst(Int(1)))
let a'= Econst(Int(1))
let b = type_expr (Econst(True))
let b'= Econst(True)
let c = type_expr (Econst(Null))
let c'= Econst(Null)
let e = type_expr (Ebinop(Badd, b', a'))
let f = type_expr (Ebinop(Bsub, c', a'))
