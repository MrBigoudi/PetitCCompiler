open Ast
open Ast_typed

(** Exception for typing errors *)
exception Typing_error of string * loc

(** dummy location for temporary tests 
    val dummy_loc -> loc *)
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

(** handle a typing error 
    val handle_error : int -> loc -> unit *)
let handle_error err_num pos =
  let error = 
    match err_num with
    | 0 -> "Undefined reference to `main'"
    | 1 -> "Invalid use of void expression"
    | 2 -> "Void value not ignored as it ought to be"
    | 3 -> "Invalid type of unary '*'"
    | 4 -> "Lvalue required as unary '&' operand"
    | 5 -> "Lvalue required as increment operand"
    | 6 -> "Lvalue required as decrement operand"
    | 7 -> "Wrong type argument to unary plus"
    | 8 -> "Wrong type argument to unary minus"
    | 9 -> "Use of undeclared identifier"
    | 10 -> "Invalid operands to binary expression"
    | 11 -> "lvalue required as left operand of assignment"
    | 12 -> "Incompatible types for assignation"
    | 13 -> "The void shall have no size..."
    | 14 -> "Implicit declaration of function"
    | 15 -> "Too few arguments to function call"
    | 16 -> "Too many arguments to function call"
    | 17 -> "Passing expressions to parameter of incompatible type"
    | 18 -> "Non-void function should return a value"
    | 19 -> "This function does not return a value of the correct type" (* made up error message *)
    | 20 -> "Expected expression"
    | 21 -> "Statement requires expression of scalar type"
    | 22 -> "Variable has incomplete type"
    | 23 -> "Redefinition of identifier"
    | 24 -> "Redefinition of parameter"
    | 25 -> "Incompatible conversion"
    | 26 -> "Can't redefine stdandard function" (* made up error message *)
    | 27 -> "Main function should not take arguments" (* made up error message *)
    | 28 -> "Main function should return an int" (* made up error message *)
    (* TODO *)
    | _ -> "Unkown error"
  in raise (Typing_error(error, pos))


(** check if there is a main function in the file *)
let main_is_present = ref false

(** val check_main_in_env : dmap -> typ -> ident -> param list -> unit *)
let check_main_in_env env typ id param_list =
  (* print_string id; *)
  if not (String.equal id "main") then () 
  else
    (* print_string "tried"; *)
    try
      let _ = search_dmap id env
        in match typ with 
        | Tint -> 
          begin
            match param_list with 
              | [] -> (main_is_present := true)
              | _ -> (handle_error 27 dummy_loc)
          end
        | _ -> (handle_error 28 dummy_loc)
    with _ -> ()



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

(** val type_expr : dmap -> expression -> texpression *)
let rec type_expr env e = 
  let d, ty = compute_type_expr env e in 
  { tdesc = d ; typ = ty }

(** val compute_type_expr : dmap -> expression -> tdesc * typ *)
and compute_type_expr env e = 
  let loc = e.loc in
  match e.desc with 
  | Econst const -> type_const const
  | Evar var -> (type_var var env loc)
  | Eunop (op, e) -> (type_unop env op e loc)
  | Ebinop (op, e1, e2) -> (type_binop env op e1 e2 loc) (* addition of pointers dont work as expected, be more careful ! *)
  | Eassign (e1, e2) -> (type_assign env e1 e2 loc)
  | Ecall (id, e_list) -> (type_call env id e_list loc)
  | Esizeof ty -> if equ_type ty Tvoid then (handle_error 13 loc) else TEsizeof(ty), Tint


(** val type_const : const -> tdesc * typ *)
and type_const const = match const with
  | Int n -> TEconst(Int(n)), Tint
  | True -> TEconst(True), Tbool
  | False -> TEconst(False), Tbool
  | Null -> TEconst(Null), Tptr(Tvoid)


(** val type_var : ident -> dmap -> expression.loc -> tdesc * typ *)
and type_var var env loc = 
  try TEvar var, (search_dmap var env)
    with _ -> (handle_error 9 loc)


(** val type_unop : dmap -> unop -> expression -> expression.loc -> tdesc * typ *)
and type_unop env op e loc = 
  let te = type_expr env e in 
  let t = te.typ in
    match op with
    | Unot as op -> if t = Tvoid then (handle_error 1 loc) else TEunop(op, te), Tint
    | Ustar as op -> if t = Tvoid then (handle_error 2 loc)
        else (match t with Tptr(ty) -> TEunop(op, te), ty | _ -> (handle_error 3 loc))
    | Uamp as op -> if is_lvalue e then TEunop(op, te), Tptr(t) else (handle_error 4 loc)
    | Uincr_l as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 5 loc)
    | Uincr_r as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 5 loc)
    | Udecr_l as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 6 loc)
    | Udecr_r as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 6 loc)
    | Uplus as op-> if equ_type t Tint then TEunop(op, te), Tint else (handle_error 7 loc)
    | Uminus as op -> if equ_type t Tint then TEunop(op, te), Tint else (handle_error 8 loc)
        

(** val type_binop : dmap -> unop -> expression -> expression -> expression.loc -> tdesc * typ *)
and type_binop env op e1 e2 loc = 
  let t1 = (type_expr env e1) in
  let t2 = (type_expr env e2) in
  let t1_type = t1.typ in 
  let t2_type = t2.typ in 
  match op with
  | Logic _ as op -> 
    begin if not ((equ_type Tvoid t1_type) && equ_type t1_type t2_type)
       then TEbinop(op, t1, t2), Tint 
    else (handle_error 10 loc)
  end
  (* TODO + - with pointers *)
  | Arith(Badd)  as op -> 
    begin
      match t1_type with 
        | t1_type when equ_type t1_type t2_type -> TEbinop(op, t1, t2), Tint 
        | t1_type when equ_type t1_type Tint && is_ptr t2_type -> TEbinop(op, t1, t2), t2_type
        | Tptr(_) when equ_type t2_type Tint -> TEbinop(op, t1, t2), t1_type
        | _ -> (handle_error 10 loc)
    end
  | Arith(Bsub) as op -> 
    begin
      match t1_type with 
      | t1_type when equ_type t1_type t2_type -> TEbinop(op, t1, t2), Tint
      | t1_type when equ_type t1_type Tint && is_ptr t2_type -> TEbinop(op, t1, t2), t2_type
      | Tptr(_) when equ_type t2_type Tint -> TEbinop(op, t1, t2), t1_type
      | Tptr(_) when t1_type = t2_type -> TEbinop(op, t1, t2), Tint
      | _ -> (handle_error 10 loc)
    end
  | Arith(_) as op -> 
    if equ_type t1_type t2_type
      then TEbinop(op, t1, t2), Tint 
      else (handle_error 10 loc)
  | AndOr(_) as op -> 
    begin if (equ_type Tint t1_type && equ_type t1_type t2_type) 
      then TEbinop(op, t1, t2), Tint 
      else (handle_error 10 loc)
    end


(** val type_assign : dmap -> expression -> expression -> expression.loc -> tdesc * typ *)
and type_assign env e1 e2 loc =
  let te1 = type_expr env e1 in
  let te2 = type_expr env e2 in
  if (is_lvalue e1) && (equ_type te1.typ te2.typ) then TEassign(te1, te2), te1.typ else 
    (if (is_lvalue e1) then (handle_error 11 loc) else (handle_error 12 loc))


(** val type_call : dmap -> ident -> expression list -> expression.loc -> tdesc * typ *)
and type_call env id e_list loc = 
  (* test if function exists in env *)
  let fct_typ = begin
    try search_dmap id env with _ -> (handle_error 14 loc);
  end in
    match fct_typ with 
      | Tfct(ret_typ, param_typ) ->
        (* test if given parameters are correct *)
        let rec test_param_fun_call e_list param_typ_list te_list =
          match (e_list,param_typ_list) with
          | ([],[]) -> TEcall(id, te_list), ret_typ
          | ([],_) -> (handle_error 15 loc) (* not enough params *)
          | (_,[]) -> (handle_error 16 loc) (* too many params *)
          | (e::e_cdr), (p::p_cdr) ->
            let te = type_expr env e in
              (* test if compatible type *) 
              if not (equ_type te.typ p) 
                then (handle_error 17 e.loc) (* incompatible types *)
                (* recursive call *)
                else (test_param_fun_call e_cdr p_cdr (te_list@[te]))
          in (test_param_fun_call e_list param_typ [])
      | _ -> assert false





(** val type_instr : dmap -> instr -> typ -> tinstr *)
let rec type_instr env ist t0 = 
  let d, new_env = compute_type_instr env ist t0 in 
  { tdesci = d ; env = new_env }

(** val compute_type_instr : dmap -> instr -> typ -> tdesci * dmap *)
and compute_type_instr (env:dmap) ist t0 = match ist with
  | Iempt -> TIempt, env
  | Ibreak -> TIbreak, env
  | Icontinue -> TIcontinue, env
  | Iexpr e -> TIexpr (type_expr env e), env
  | Iret None -> if t0 = Tvoid 
                    then TIret(None), env 
                    else (handle_error 18 dummy_loc)
  | Iret Some(e) -> let texp = (type_expr env e) in
                    if (equ_type t0 (texp.typ))
                      then TIret(Some(texp)), env 
                      else (handle_error 19 dummy_loc)
  | Iif(e, i1, i2) -> compute_type_if env e i1 i2 t0
  | Iwhile(e, i) -> compute_type_while env e i t0
  | Ifor(dvar, e, elist, i) -> compute_type_for env dvar e elist i t0
  | Iblock(Block dinstr_list) -> compute_type_block env dinstr_list t0

(** val compute_type_if : dmap -> expression -> instr -> instr -> typ -> tdesci * dmap *)
and compute_type_if env e i1 i2 t0 =
  let te = (type_expr env e) in
    if (equ_type Tvoid (te.typ)) 
      then (handle_error 20 dummy_loc)
    else 
      let ti1 = (type_instr env i1 t0) in
      let ti2 = (type_instr env i2 t0) in
        TIif(te, ti1, ti2), env

(** val compute_type_while : dmap -> expression -> instr -> typ -> tdesci * dmap *)
and compute_type_while env e i t0 =
  let te = (type_expr env e) in
      if(equ_type Tvoid (te.typ))
        then (handle_error 20 dummy_loc)
    else 
      let ti = (type_instr env i t0) in TIwhile(te, ti), env


(** val compute_type_for : dmap -> dvar -> instr -> instr -> typ -> tdesci * dmap *)
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
          then (handle_error 21 dummy_loc)
          else
            let te_list = (createTExprList elist []) in
            let s = (type_instr env i t0) in
              TIfor(None, Some(te), te_list, s), env
    end
  | _ -> failwith "TODO for with dvar (maybe change in Parser ?)" (* for(d;e;l) *)


(** val compute_type_block : dmap -> dinstr list -> typ -> tdesci * dmap *)
and compute_type_block env di_list t0 =
  (* val compute_type_block_instr : dinstr list -> dmap -> tdinstr list -> tdesci * dmap *)
  let rec compute_type_block_instr di_list new_env tdi_list =
    match di_list with 
    | [] -> TIblock(TBlock(tdi_list)), env (* restore the previous env *)
    | cur_di::cdr ->
      let (cur_tdi, new_env) = compute_type_dinstr new_env cur_di t0 in
        (compute_type_block_instr cdr new_env (tdi_list@[cur_tdi])) (* update the block dmap *)
  in (compute_type_block_instr di_list (new_block_dmap env) [])
    

(** val compute_type_dinstr : dmap -> dinstr -> typ -> tdinstr * dmap *)
and compute_type_dinstr env di t0 =
  match di with 
  | DinstrVar v -> (compute_type_dinstr_var env v t0)
  | DinstrFct fct -> (compute_type_dinstr_fct env fct t0)
  | Dinstr i -> let (tdesci, env) = (compute_type_instr env i t0) in (TDinstr({tdesci=tdesci; env=env}), env)


(** val compute_type_dinstr_var : dmap -> dvar -> typ -> tdinstr * dmap *)
and compute_type_dinstr_var env v t0 = 
  match v with Dvar(typ, ident, exp) ->
    (* check if variable is of type void *)
    if (equ_type typ Tvoid) 
      then (handle_error 22 dummy_loc) 
      else
      (* check if name already used *)
      if (in_new_env_dmap ident env) 
        then (handle_error 23 dummy_loc) 
        (* adding variable to new env *)
        else 
          match exp with 
          | None -> TDinstrVar(TDvar(typ, ident, None)), (add_dmap ident typ env)
          | Some(e) -> 
            let (e_tdesc, e_typ) = (compute_type_expr env e) in
              (* typ x = e; -> test if 'typ' equivalent to type of 'e'*)
              if not (equ_type e_typ typ) 
                then (handle_error 25 dummy_loc)
                else TDinstrVar(TDvar(typ, ident, Some({tdesc=e_tdesc; typ=e_typ}))), (add_dmap ident typ env)


(** val compute_type_dinstr_fct : dmap -> dfct -> typ -> tdinstr * dmap *)
and compute_type_dinstr_fct env fct t0 = 
  let t_dfct, env = (compute_type_dfct env fct t0) in TDinstrFct t_dfct, env

(** val compute_type_dfct : dmap -> dfct -> typ -> tdfct * dmap *)
and compute_type_dfct env fct t0 = 
  match fct with
    Dfct (typ, ident, p_list, Block(dinstr_list)) ->
    (* check if standard function *)
    if (String.equal ident "malloc" || String.equal ident "putchar") then (handle_error 26 dummy_loc)
    (* check if function name already used *)
    else if(in_new_env_dmap ident env)
      then (handle_error 22 dummy_loc)
      else
        (* getting type of all parameters and the new env with these parameters *)
        (* val get_param_types : param list -> typ list -> ident list -> dmap -> typ list * dmap *)
        let rec get_param_types p_list types idents env = 
          match p_list with
          | [] -> (types, env)
          | p::cdr -> 
            let (typ,id) = match p with Param(typ,id) -> (typ,id)
              (* testing if two params have same name *)
              in 
                if List.mem id idents 
                  then (handle_error 24 dummy_loc)
                  else
                    let new_env = (add_dmap ident typ env) in
                      (* adding params to new env *)
                      (get_param_types cdr (types@[typ]) (idents@[ident]) new_env)
        in let (p_types, new_env) = (get_param_types p_list [] [] env)
          (* adding fun prototype to new env and adding all parameters to new env *)
          in let fun_typ = Tfct(typ,p_types)
            in let new_env =  (add_dmap ident fun_typ new_env)
            (* checking return type of the function *)
              in let (tdesci,_) = (compute_type_block new_env dinstr_list typ) (* t0 is now the fun return typ *)
                in match tdesci with 
                  | TIblock t_block -> TDfct(typ, ident, p_list, t_block), new_env
                  | _ -> assert false (* should not end up here *)

          
(** Type a parsed ast
    val type_ast : fileInclude -> tfileInclude *)
and type_ast parsed_ast =
	(* create new env *)
	let (env:dmap) = { old_env=Smap.empty; new_env=Smap.empty} in
  (* add void* malloc(int n) and int putchar(int c) in the env *)
  let env = add_dmap "malloc" (Tfct(Tptr(Tvoid), [Tint])) env in
  let env = add_dmap "putchar" (Tfct(Tint, [Tint]))  env in
    let dfct_list = match parsed_ast with FileInclude(l) -> l in
      let rec compute_type_dfct_list dfct_list new_env tdfct_list =
        match dfct_list with 
        | [] -> TFileInclude(tdfct_list)
        | cur_dfct::cdr -> 
          let (typ,ident,param_list) = match cur_dfct with Dfct(typ,ident,param_list,_) -> (typ,ident,param_list) in
            let (cur_tdfct, new_env) = compute_type_dfct new_env cur_dfct typ in
            (check_main_in_env new_env typ ident param_list);
            (compute_type_dfct_list cdr new_env (tdfct_list@[cur_tdfct])) (* update the global env *)
      in 
        let typed_ast = (compute_type_dfct_list dfct_list env []) in
          if not (!main_is_present) then (handle_error 0 dummy_loc)
          else typed_ast
