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
  | Evar var -> TEvar var, (Smap.find var env)
  | Eunop (op, e) -> type_unop env op e
  | Ebinop (op, e1, e2) -> type_binop env op e1 e2 (* addition of pointers dont work as expected, be more careful ! *)
  | _ -> failwith "Oops not now"

and type_const const = match const with
  | Int n -> TEconst(Int(n)), Tint
  | True -> TEconst(True), Tbool
  | False -> TEconst(False), Tbool
  | Null -> TEconst(Null), Tptr(Tvoid)

and type_unop env op e = let te = type_expr env e in let t = te.typ in
  match op with
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
      
and type_binop env op e1 e2 = 
  let t1 = (type_expr env e1) in
  let t2 = (type_expr env e2) in
  let t1_type = t1.typ in 
  let t2_type = t2.typ in 
  match op with
  | Logic _ as op -> begin if not (equ_type Tvoid t1_type) && equ_type t1_type t2_type then TEbinop(op, t1, t2), Tint 
    else failwith "erreur : addition ptr pas faite encore"
  end
  (* TODO + - with pointers *)
  | Arith(Badd)  as op -> 
    begin
      match t1_type with 
        | t1_type when equ_type t1_type t2_type -> TEbinop(op, t1, t2), Tint 
        | t1_type when equ_type t1_type Tint && is_ptr t2_type -> TEbinop(op, t1, t2), t2_type
        | Tptr(_) when equ_type t2_type Tint -> TEbinop(op, t1, t2), t1_type
        | _ -> failwith "erreur : invalid operands to binary +"
    end
  | Arith(Bsub) as op -> 
      begin
        match t1_type with 
        | t1_type when equ_type t1_type t2_type -> TEbinop(op, t1, t2), Tint
        | t1_type when equ_type t1_type Tint && is_ptr t2_type -> TEbinop(op, t1, t2), t2_type
        | Tptr(_) when equ_type t2_type Tint -> TEbinop(op, t1, t2), t1_type
        | Tptr(_) when t1_type = t2_type -> TEbinop(op, t1, t2), Tint
        | _ -> failwith "erreur : invalid operands to binary -"
      end
  | Arith(_) as op -> if equ_type t1_type t2_type then TEbinop(op, t1, t2), Tint else failwith "erreur : invalid operands to binary"
  | AndOr(_) as op -> begin if (equ_type Tint t1_type && equ_type t1_type t2_type) then TEbinop(op, t1, t2), Tint 
    else failwith "erreur : TODO"
  end

(** val type_instr : typ Smap.t -> instr -> typ -> tinstr *)
let rec type_instr env ist t0 = 
  let d, new_env = compute_type_instr env ist t0 in 
  { tdesci = d ; env = new_env }

(** val compute_type_instr : typ Smap.t -> instr -> typ -> tdesci * env *)
and compute_type_instr env ist t0 = match ist with
  | Iempt -> TIempt, env
  | Ibreak -> TIbreak, env
  | Icontinue -> TIcontinue, env
  | Iexpr e -> TIexpr (type_expr env e), env
  | Iret None -> if t0 = Tvoid 
                    then TIret(None), env 
                    else failwith "erreur : Non-void function 'fctName' should return a value"
  | Iret Some(e) -> let texp = (type_expr env e) in
                    if (equ_type t0 (texp.typ))
                      then TIret(Some(texp)), env 
                      else failwith "erreur : Erreur bizarre"
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
  (* val createTExprList : expression list -> texpression list -> texpression list *)
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
        let te = {tdesc = TEconst(True); typ = Tbool} in 
          let te_list = (createTExprList elist []) in
            let s = (type_instr env i t0) in
              TIfor(None, Some(te), te_list, s), env
      | Some e ->
        let te = (type_expr env e) in
        if(equ_type Tvoid (te.typ))
          then failwith "erreur : Statement requires expression of scalar type ('void' invalid)"
          else
            let te_list = (createTExprList elist []) in
            let s = (type_instr env i t0) in
              TIfor(None, Some(te), te_list, s), env
    end
  | _ -> failwith "TODO for with dvar (maybe change in Parser ?)" (* for(d;e;l) *)


(** val compute_type_block : typ Smap.t -> dinstr list -> typ -> tdesci * env *)
and compute_type_block env di_list t0 =
  (* val compute_type_block_instr : dinstr list -> typ Smap.t -> tdinstr list -> tdesci * env *)
  let rec compute_type_block_instr di_list new_env tdi_list =
    match di_list with 
    | [] -> TIblock(TBlock(tdi_list)), env (* restore the previous env *)
    | cur_di::cdr ->
      let (cur_tdi, new_env) = compute_type_dinstr new_env cur_di t0 in
        (compute_type_block_instr cdr new_env (tdi_list@[cur_tdi])) (* update the block env *)
  in (compute_type_block_instr di_list env [])
    

(** val compute_type_dinstr : typ Smap.t -> dinstr -> typ -> tdinstr * env *)
and compute_type_dinstr env di t0 =
  match di with 
  | DinstrVar v -> (compute_type_dinstr_var env v t0)
  | DinstrFct fct -> (compute_type_dinstr_fct env fct t0)
  | Dinstr i -> let (tdesci, env) = (compute_type_instr env i t0) in (TDinstr({tdesci=tdesci; env=env}), env)


(** val compute_type_dinstr_var : typ Smap.t -> dvar -> typ -> tdinstr * env *)
and compute_type_dinstr_var env v t0 = 
  match v with Dvar(typ, ident, exp) ->
    (* check if variable is of type void *)
    if (equ_type typ Tvoid) 
      then failwith ("erreur : Variable '"^ident^"' has incomplete type 'void' ") 
      else
      (* check if name already used *)
      if (Smap.mem ident env) 
        then failwith ("erreur : Redefinition of '"^ident^"'")
        (* adding variable to new env *)
        else 
          match exp with 
          | None -> TDinstrVar(TDvar(typ, ident, None)), (Smap.add ident typ env)
          | Some(e) -> 
            let (e_tdesc, e_typ) = (compute_type_expr env e) in
              (* typ x = e; -> test if 'typ' equivalent to type of 'e'*)
              if not (equ_type e_typ typ) 
                then failwith ("erreur : Incompatible conversion initializing 'typ' with an expression of type 'te.typ' ")
                else TDinstrVar(TDvar(typ, ident, Some({tdesc=e_tdesc; typ=e_typ}))), (Smap.add ident typ env)


(** val compute_type_dinstr_fct : typ Smap.t -> dfct -> typ -> tdinstr * env *)
and compute_type_dinstr_fct env fct t0 =  
  match fct with Dfct(typ, ident, param_list, block) ->
    (* check if name already used *)
    if (Smap.mem ident env) 
      then failwith ("erreur : Redefinition of '"^ident^"'")
      else failwith("TODO decl func")