open Ast
open Ast_typed

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

(** val type_expr : typ Smap.t -> expression -> texpression *)
let rec type_expr env e = 
  let d, ty = compute_type_expr env e in 
  { tdesc = d ; typ = ty }

(** val compute_type_expr : typ Smap.t -> expression -> tdesc * typ *)
and compute_type_expr env e = 
  match e.desc with 
  | Econst const -> type_const const (* dunno if its more subtle but lets do this way *)
  | Evar var -> Smap.find var env
  | Eunop (op, e) -> type_unop env op e
  | Ebinop (op, e1, e2) -> type_binop env op e1 e2 (* addition of pointers dont work as expected, be more careful ! *)
  | _ -> failwith "Oops not now"

and type_const const = match const with
  | Int n -> TEconst(Int(n)), Tint
  | True -> TEconst(True), Tbool
  | False -> TEconst(False), Tbool
  | Null -> TEconst(Null), Tptr(Tvoid)

and type_unop env op e = let {_,t} = type_expr env e in match op with
  | Unot as op -> if t = Tvoid then failwith "erreur : invalid use of void expression" else TEunop(op, type_expr env e), Tint
  | Ustar as op -> if t = Tvoid then failwith "erreur : error: void value not ignored as it ought to be"
      else (match t with Tptr(ty) -> TEunop(op, type_expr env e), ty | _ -> failwith "erreur : invalid type of unary `*`")
  | Uamp as op -> if is_lvalue e then TEunop(op, type_expr env e), Tptr(t) else failwith "erreur : lvalue required as unary ‘&’ operand"
  | Uincr_l as op -> if is_lvalue e then TEunop(op, type_expr env e), t else failwith "erreur : lvalue required as increment operand"
  | Uincr_r as op -> if is_lvalue e then TEunop(op, type_expr env e), t else failwith "erreur : lvalue required as increment operand"
  | Udecr_l as op -> if is_lvalue e then TEunop(op, type_expr env e), t else failwith "erreur : lvalue required as decrement operand"
  | Udecr_r as op -> if is_lvalue e then TEunop(op, type_expr env e), t else failwith "erreur : lvalue required as decrement operand"
  | Uplus as op-> if equ_type t Tint then TEunop(op, type_expr env e), Tint else failwith "erreur : wrong type argument to unary plus"
  | Uminus as op -> if equ_type t Tint then TEunop(op, type_expr env e), Tint else failwith "erreur : wrong type argument to unary minus"
      
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

(** val type_expr : typ Smap.t -> instr -> typ -> tinstr *)
let rec type_instr env ist t0 = 
  let d, new_env = compute_type_instr env ist t0 in 
  { tdesci = d ; env = new_env }

(** val compute_type_instr : typ Smap.t -> instr -> typ -> tdesci * env *)
and compute_type_instr env ist t0 = match ist with
  | Iempt -> TIempt, env
  | Ibreak -> TIbreak, env
  | Icontinue -> TIcontinue, env
  | Iexpr e -> TIexpr(type_expr env e), env
  | Iret None -> if tO = Tvoid then TIret(None), env else failwith "erreur : Non-void function 'fctName' should return a value"
  | Iret Some(e) -> if equ_type t0 ((type_expr env e).typ) then TIret(Some(e)), env else failwith "erreur : Erreur bizarre"
  | Iif(e, i1, i2) -> compute_type_if env e i1 i2 t0
  | Iwhile(e, i) -> compute_type_while env e i t0
  | Ifor(dvar, e, elist, i) -> compute_type_for env dvar e elist i t0
  | Iblock(Block dinstr_list) -> compute_type_block env dinstr_list t0

(** val compute_type_if : typ Smap.t -> expression -> instr -> instr -> typ -> tdesci * env *)
and compute_type_if env e i1 i2 t0 =
  let te = (type_expr env e) in
    if (equ_type Tvoid (te.typ)) 
      then failwith "erreur : Expected expression"
    else 
      let ti1 = (type_instr env i1 t0) in
      let ti2 = (type_instr env i2 t0) in
        TIif(te, ti1, ti2), env

(** val compute_type_while : typ Smap.t -> expression -> instr -> typ -> tdesci * env *)
and compute_type_while env e i t0 =
  let te = (type_expr env e) in
      if(equ_type Tvoid (te.typ))
        then failwith "erreur : Expected expression"
    else 
      let ti = (type_instr env i t0) in TIwhile(te, ti), env


(** val compute_type_for : typ Smap.t -> dvar -> instr -> instr -> typ -> tdesci * env *)
and compute_type_for env dvar e elist i t0 =
  (** val createTExprList : expression list -> texpression list -> texpression list *)
  let rec createTExprList exprList acc = 
    match exprList with
    | [] -> acc
    | e::cdr -> let texpr = (type_expr env e) in
                  (createTExprList cdr acc@[texpr])
  in
  match dvar with 
  | None -> (* d; for(;e;l) *)
    begin
      match e with 
      | None -> (* for(d;;l) -> for(d;true;l) *)
        let te = {tdesc=TEconst(True), typ=Tbool} in 
          let te_list = (createTExprList elist []) in
            let s = (type_instr env i t0) in
              TIfor(None, te, te_list, s), env
      | Some e ->
        let te = (type_expr env e) in
        if(equ_type Tvoid (te.typ))
          then failwith "erreur : Statement requires expression of scalar type ('void' invalid)"
          else
            let te_list = (createTExprList elist []) in
            let s = (type_instr env i t0) in
              TIfor(None, te, te_list, s), env
    end
  | _ -> (* for(d;e;l) *) failwith "TODO for with dvar (maybe change in Parser ?)"

(** val compute_type_block : typ Smap.t -> dinstr list -> typ -> tdesci * env *)
and compute_type_block env di_list t0 =
  (** val compute_type_block_instr : dinstr list -> typ Smap.t -> tdinstr list -> tdesci * env *)
  let rec compute_type_block_instr di_list new_env tdi_list =
    match di_list with 
    | [] -> TIblock(TBlock(tdi_list)), env
    | cur_di::cdr ->
      let cur_tdi = 
    

(** val compute_type_dinstr : typ Smap.t -> dinstr -> typ -> tdesci * env *)
and compute_type_dinstr env di t0 =
  match di with 
  | Dinstr(i) -> (compute_type_instr env i t0)