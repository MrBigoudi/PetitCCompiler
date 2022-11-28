open Ast

module Smap = Map.Make(String)
type env = typ Smap.t

(* lvalue : variable ou *e1 *)

let warning = function 
  | 1 -> print_string "TODO better warnings int ptr lol\n"
  | 2 -> print_string "TODO better warnings cast ¯\\_(ツ)_/¯\n"
  | _ -> print_string "WTF is this warning ????\n"

(* TODO : faire les tests *)
let rec equ_type ty1 ty2 = match ty1, ty2 with
  | t1, t2 when t1 = t2 -> true
  | Tint, Tbool -> true
  | Tvoid, Tint -> false
  | Tvoid, Tbool -> false
  | Tvoid, Tptr(_) -> false
  | Tptr(Tvoid), Tptr(t) -> true
  | Tptr(Tint), Tptr(Tbool) -> warning 2 ; true (* /!\ call warning to do *)
  | Tptr(Tbool), Tptr(Tint) -> warning 2 ; true 
  | Tptr(Tvoid), _ -> warning 5 ; true (* /!\ call warning to do *)
  | Tptr(t), Tint -> warning 1 ; true 
  | Tptr(t), Tptr(t') -> equ_type t t'
  | _, _ -> equ_type ty2 ty1

let rec type_expr (*env*) =  function
  | Econst const -> type_const const (* dunno if its more subtle but lets do this way *)
  | Eunop (Unot, e) -> (* NULL is of type void* and !NULL of type int *)
      if type_expr e = Tvoid then failwith "erreur : invalid use of void expression"
      else Tint
  | Eunop (Ustar, e) -> 
      if type_expr e = Tvoid then failwith "erreur : error: void value not ignored as it ought to be"
      else (match type_expr e with Tptr(ty) -> ty | _ -> failwith "erreur : invalid type of unary `*`")
  | Ebinop (op, e1, e2) -> type_binop op e1 e2 (* addition of pointers dont work as expected, be more careful ! *)

and type_const const = match const with
  | Int _ -> Tint
  | True | False -> Tbool
  | Null -> Tptr(Tvoid)
  | _ -> failwith "erreur compilo 2 TODO"
      
and type_binop op e1 e2 = let t1 = type_expr e1 in let t2 = type_expr e2 in match op with
  | Logic(_) -> begin if not (equ_type Tvoid t1) && equ_type t1 t2 then Tint 
  else failwith "erreur : addition ptr pas faite encore"
  end
  (* TODO + - with pointers *)
  | Arith(_) | AndOr(_) -> begin if equ_type Tint t1 && equ_type t1 t2 then Tint 
  else failwith "erreur : TODO"
  | _ -> Tint

let a = type_expr (Econst(Int(1)))
let a'= Econst(Int(1))
let b = type_expr (Econst(True))
let b'= Econst(True)
let c = type_expr (Econst(Null))
let c'= Econst(Null)
(*let f = type_expr (Ebinop(Logic(Beq), c', a'))*)
let f = type_expr (Ebinop(Logic(Beq), c', c'))

(*let _ = if equ_type (Tptr(Tbool)) (Tptr(Tint)) then print_string "égal" else print_string "pas égaux"*)
