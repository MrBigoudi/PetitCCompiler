open Ast

module Smap = Map.Make(String)
type env = typ Smap.t

(* lvalue : variable ou *e1 *)

(* TODO : faire les tests *)
let equ_type ty1 ty2 = match ty1, ty2 with
  | t1, t2 when t1 = t2 -> true
  | Tint, Tbool -> true
  | Tbool, Tint -> true
  | Tptr(_), Tptr(Tvoid) -> true
  | Tptr(Tvoid), Tptr(_) -> true
  | _, _ -> false

let is_lvalue e = match e.desc with
  | Evar _ -> true
  | Eunop(Ustar, _) -> true
  | _ -> false

(* might want to return the type of the thing it points to ? *)
let is_ptr t = match t with
  | Tptr(_) -> true
  | _       -> false

let rec type_expr env e = match e.desc with 
  | Econst const -> type_const const (* dunno if its more subtle but lets do this way *)
  | Evar var -> Smap.find var env
  | Eunop (op, e) -> type_unop env op e
  | Ebinop (op, e1, e2) -> type_binop env op e1 e2 (* addition of pointers dont work as expected, be more careful ! *)
  | _ -> failwith "Oops not now"

and type_const const = match const with
  | Int _ -> Tint
  | True | False -> Tbool
  | Null -> Tptr(Tvoid)

and type_unop env op e = let t = type_expr env e in match op with
  | Unot -> if t = Tvoid then failwith "erreur : invalid use of void expression" else Tint
  | Ustar -> if t = Tvoid then failwith "erreur : error: void value not ignored as it ought to be"
      else (match t with Tptr(ty) -> ty | _ -> failwith "erreur : invalid type of unary `*`")
  | Uamp -> if is_lvalue e then Tptr(t) else failwith "erreur : lvalue required as unary ‘&’ operand"
  | Uincr_l -> if is_lvalue e then t else failwith "erreur : lvalue required as increment operand"
  | Uincr_r -> if is_lvalue e then t else failwith "erreur : lvalue required as increment operand"
  | Udecr_l -> if is_lvalue e then t else failwith "erreur : lvalue required as decrement operand"
  | Udecr_r -> if is_lvalue e then t else failwith "erreur : lvalue required as decrement operand"
  | Uplus -> if equ_type t Tint then Tint else failwith "erreur : wrong type argument to unary plus"
  | Uminus -> if equ_type t Tint then Tint else failwith "erreur : wrong type argument to unary minus"
      
and type_binop env op e1 e2 = let t1 = type_expr env e1 in let t2 = type_expr env e2 in match op with
  | Logic(_) -> begin if not (equ_type Tvoid t1) && equ_type t1 t2 then Tint 
    else failwith "erreur : addition ptr pas faite encore"
  end
  (* TODO + - with pointers *)
  | Arith(Badd) -> 
    begin
      match t1 with 
        | t1 when equ_type t1 t2 -> Tint
        | t1 when equ_type t1 Tint && is_ptr t2 -> t2
        | Tptr(_) when equ_type t2 Tint -> t1
        | _ -> failwith "erreur : invalid operands to binary +"
    end
  | Arith(Bsub) -> 
      begin
        match t1 with 
        | t1 when equ_type t1 t2 -> Tint
        | t1 when equ_type t1 Tint && is_ptr t2 -> t2
        | Tptr(_) when equ_type t2 Tint -> t1
        | Tptr(_) when t1 = t2 -> Tint
        | _ -> failwith "erreur : invalid operands to binary -"
      end
  | Arith(_) -> if equ_type t1 t2 then Tint else failwith "erreur : invalid operands to binary"
  | AndOr(_) -> begin if equ_type Tint t1 && equ_type t1 t2 then Tint 
    else failwith "erreur : TODO"
  end
