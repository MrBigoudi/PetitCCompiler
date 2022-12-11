open Ast
open Ast_typed

(** Exception for typing errors *)
exception Typing_error of string * loc * string option

(** dummy location for temporary tests 
    val not_found_loc -> loc *)
let not_found_loc = Lexing.dummy_pos, Lexing.dummy_pos

(** handle a typing error 
    val handle_error : int -> loc -> unit *)
let handle_error err_num pos stropt =
  let error = 
    match err_num with
    | 0  -> "Undefined reference to `main'"
    | 1  -> "Invalid use of void expression"
    | 2  -> "Void value not ignored as it ought to be"
    | 3  -> "Invalid type of unary"
    | 4  -> "lvalue required as operand"
    | 7  -> "Wrong type argument to unary"
    | 8  -> "Wrong type argument to unary"
    | 9  -> "Use of undeclared identifier"
    | 10 -> "Invalid operands to binary expression"
    | 11 -> "lvalue required as left operand of assignment"
    | 12 -> "Incompatible types for assignation"
    | 13 -> "The void shall have no size..."
    | 14 -> "Implicit declaration of function"
    | 15 -> "Too few arguments to function call"
    | 16 -> "Too many arguments to function call"
    | 17 -> "Passing expressions to parameter of incompatible type in"
    | 18 -> "Non-void function should return a value"
    | 19 -> "Function does not return a value of the correct type" 
    | 20 -> "Expected expression in block"
    | 21 -> "Statement requires expression of scalar type"
    | 22 -> "Variable has incomplete type"
    | 23 -> "Redefinition of identifier"
    | 24 -> "Redefinition of parameter" 
    | 25 -> "Incompatible conversion"
    | 26 -> "Can't redefine standard function"
    | 27 -> "Main function should not take arguments" 
    | 28 -> "Main function should return an int" 
    | 29 -> "Break statement not within a loop"
    | 30 -> "Continue statement not within a loop"
    | 31 -> "Called object which is not a function or function pointer"
    | _  -> "Unkown error"
  in raise (Typing_error(error, pos, stropt))


(** check if there is a main function in the file *)
let main_is_present = ref false

(** check if we can use break and continue *)
let in_loop = ref false

(** val check_main_in_env : dmap -> typ -> ident -> param list -> unit *)
let check_main_in_env env typ id param_list =
  if not (String.equal id "main") then () 
  else
    try
      let _ = search_dmap id env
        in match typ with 
        | Tint -> 
          begin
            match param_list with 
              | [] -> (main_is_present := true)
              | _ -> (handle_error 27 not_found_loc None)
          end
        | _ -> (handle_error 28 not_found_loc None)
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

(** val type_expr : dmap -> expression -> int -> texpression *)
let rec type_expr env e fpcur = 
  let d, ty, fpcur = compute_type_expr env e fpcur in 
  { tdesc = d ; typ = ty }, fpcur

(** val compute_type_expr : dmap -> expression -> int -> tdesc * typ * int *)
and compute_type_expr env e fpcur = 
  let loc = e.loc in
  match e.desc with 
  | Econst const -> let desc, typ = type_const const in desc, typ, fpcur
  | Evar var -> let desc, typ = (type_var var env loc fpcur) in desc, typ, fpcur
  | Eunop (op, e) -> let desc, typ = (type_unop env op e loc fpcur) in desc, typ, fpcur
  | Ebinop (op, e1, e2) -> (type_binop env op e1 e2 loc fpcur)
  | Eassign (e1, e2) -> (type_assign env e1 e2 loc fpcur)
  | Ecall (id, e_list) -> (type_call env id e_list loc fpcur)
  | Esizeof ty -> if equ_type ty Tvoid then (handle_error 13 loc None) else TEsizeof(ty), Tint, fpcur


(** val type_const : const -> tdesc * typ *)
and type_const const = 
  match const with
  | Int n -> TEconst(Int(n)), Tint
  | True -> TEconst(True), Tbool
  | False -> TEconst(False), Tbool
  | Null -> TEconst(Null), Tptr(Tvoid)


(** val type_var : ident -> dmap -> expression.loc -> int -> tdesc * typ *)
and type_var var env loc fpcur = 
  try TEvar {ident = var; offset = fpcur} , (search_dmap var env)
    with _ -> (handle_error 9 loc (Some(var)))


(** val type_unop : dmap -> unop -> expression -> expression.loc -> int -> tdesc * typ *)
and type_unop env op e loc fpcur = 
  let (te,_) = type_expr env e fpcur in 
  let t = te.typ in
    match op with
    | Unot as op -> if t = Tvoid then (handle_error 1 loc None) else TEunop(op, te), Tint
    | Ustar as op -> if t = Tvoid then (handle_error 2 loc None)
        else (match t with Tptr(ty) -> TEunop(op, te), ty | _ -> (handle_error 3 loc (Some("*"))))
    | Uamp as op -> if is_lvalue e then TEunop(op, te), Tptr(t) else (handle_error 4 loc (Some("&")))
    | Uincr_l as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 4 loc (Some("++")))
    | Uincr_r as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 4 loc (Some("++")))
    | Udecr_l as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 4 loc (Some("--")))
    | Udecr_r as op -> if is_lvalue e then TEunop(op, te), t else (handle_error 4 loc (Some("--")))
    | Uplus as op-> if equ_type t Tint then TEunop(op, te), Tint else (handle_error 7 loc (Some("+")))
    | Uminus as op -> if equ_type t Tint then TEunop(op, te), Tint else (handle_error 8 loc (Some("-")))
        

(** val type_binop : dmap -> unop -> expression -> expression -> expression.loc -> int -> tdesc * typ * int *)
and type_binop env op e1 e2 loc fpcur = 
  let (t1, fpmax1) = (type_expr env e1 fpcur) in
  let (t2, fpmax2) = (type_expr env e2 fpcur) in
  let fpmax = (min fpmax1 fpmax2) in
  let t1_type = t1.typ in 
  let t2_type = t2.typ in 
  match op with
  | Logic _ as op -> 
    begin if (not (equ_type Tvoid t1_type) && equ_type t1_type t2_type)
       then TEbinop(op, t1, t2), Tint, fpmax
    else (handle_error 10 loc (Some((op_to_string op))))
  end
  | Arith(Badd)  as op -> 
    begin
      match t1_type with 
        | t1_type when (not (is_ptr t1_type)) && (equ_type t1_type t2_type) -> TEbinop(op, t1, t2), Tint, fpmax
        | t1_type when equ_type t1_type Tint && is_ptr t2_type -> TEbinop(op, t1, t2), t2_type, fpmax
        | Tptr(_) when equ_type t2_type Tint -> TEbinop(op, t1, t2), t1_type, fpmax
        | _ -> (handle_error 10 loc (Some((op_to_string op))))
    end
  | Arith(Bsub) as op -> 
    begin
      match t1_type with 
      | t1_type when equ_type t1_type t2_type -> TEbinop(op, t1, t2), Tint, fpmax
      | Tptr(_) when t1_type = t2_type -> TEbinop(op, t1, t2), Tint, fpmax
      | Tptr(_) when equ_type t2_type Tint -> TEbinop(op, t1, t2), t1_type, fpmax
      | _ -> (handle_error 10 loc (Some((op_to_string op))))
    end
  | Arith(_) as op -> 
    if equ_type t1_type t2_type
      then TEbinop(op, t1, t2), Tint, fpmax
      else (handle_error 10 loc (Some((op_to_string op))))
  | AndOr(_) as op -> 
    begin if (equ_type Tint t1_type && equ_type t1_type t2_type) 
      then TEbinop(op, t1, t2), Tint, fpmax
      else (handle_error 10 loc (Some((op_to_string op))))
    end


(** val type_assign : dmap -> expression -> expression -> expression.loc -> int -> tdesc * typ *)
and type_assign env e1 e2 loc fpcur =
  let (te1, fpmax1) = type_expr env e1 fpcur in
  let (te2, fpmax2) = type_expr env e2 fpcur in
  let fpmax = (min fpmax1 fpmax2) in
  if (is_lvalue e1) && (equ_type te1.typ te2.typ) then TEassign(te1, te2), te1.typ, fpmax else 
    (if (is_lvalue e1) then (handle_error 12 loc None) else (handle_error 11 loc None))


(** val type_call : dmap -> ident -> expression list -> expression.loc -> int -> tdesc * typ *)
and type_call env id e_list loc fpcur = 
  (* test if function exists in env *)
  let fct_typ = 
    begin
      try search_dmap id env with _ -> (handle_error 14 loc (Some(id)));
    end 
  in
    match fct_typ with 
      | Tfct(ret_typ, param_typ) ->
        (* test if given parameters are correct *)
        let rec test_param_fun_call e_list param_typ_list te_list fpmax =
          match (e_list,param_typ_list) with
          | ([],[]) -> TEcall({ident = id; offset = fpmax}, te_list), ret_typ, fpmax
          | ([],_) -> (handle_error 15 loc (Some(id))) (* not enough params *)
          | (_,[]) -> (handle_error 16 loc (Some(id))) (* too many params *)
          | (e::e_cdr), (p::p_cdr) ->
            let (te, fptmp) = type_expr env e fpcur in
              (* test if compatible type *) 
              if not (equ_type te.typ p) 
                then (handle_error 17 e.loc (Some(id))) (* incompatible types *)
                (* recursive call *)
                else (test_param_fun_call e_cdr p_cdr (te_list@[te]) (min fpmax fptmp))
          in (test_param_fun_call e_list param_typ [] fpcur)
      | _ -> handle_error 31 loc (Some(id))





(** val type_instr : dmap -> instr -> typ -> dinstr.loc -> int -> tinstr * int *)
let rec type_instr env ist t0 locdi fpcur = 
  let d, new_env, fpcur = compute_type_instr env ist t0 locdi fpcur in 
  { tdesci = d ; env = new_env }, fpcur

(** val compute_type_instr : dmap -> instr -> typ -> dinstr.loc -> int -> tdesci * dmap * int *)
and compute_type_instr (env:dmap) ist t0 locdi fpcur = 
  match ist with
  | Iempt -> TIempt, env, fpcur
  | Ibreak -> if !in_loop then TIbreak, env, fpcur else (handle_error 29 locdi None)
  | Icontinue -> if !in_loop then TIcontinue, env, fpcur else (handle_error 30 locdi None)
  | Iexpr e -> let (t, fpcur) = (type_expr env e fpcur) in TIexpr t, env, fpcur
  | Iret None -> if t0 = Tvoid 
                    then TIret(None), env, fpcur
                    else (handle_error 18 locdi None)
  | Iret Some(e) -> let (texp, fpcur) = (type_expr env e fpcur) in
                    if (equ_type t0 (texp.typ))
                      then TIret(Some(texp)), env, fpcur 
                      else (handle_error 19 locdi None)
  | Iif(e, i1, i2) -> compute_type_if env e i1 i2 t0 locdi fpcur
  | Iwhile(e, i) -> compute_type_while env e i t0 locdi fpcur
  | Ifor(dvar, e, elist, i) -> compute_type_for env dvar e elist i t0 locdi fpcur
  | Iblock(Block dinstr_list) -> compute_type_block env dinstr_list t0 false fpcur

(** val compute_type_if : dmap -> expression -> instr -> instr -> typ -> dinstr.loc -> int -> tdesci * dmap * int *)
and compute_type_if env e i1 i2 t0 locdi fpcur =
  let (te, fpcur) = (type_expr env e fpcur) in
    if (equ_type Tvoid (te.typ)) 
      then (handle_error 20 locdi (Some("if")))
    else 
      let (ti1, fpmax1) = (type_instr env i1 t0 locdi fpcur) in
      let (ti2, fpmax2) = (type_instr env i2 t0 locdi fpcur) in
        TIif(te, ti1, ti2), env, (min fpmax1 fpmax2)

(** val compute_type_while : dmap -> expression -> instr -> typ -> dinstr.loc -> int -> tdesci * dmap * int *)
and compute_type_while env e i t0 locdi fpcur =
  in_loop := true;
  let (te, fpcur) = (type_expr env e fpcur) in
      if(equ_type Tvoid (te.typ))
        then (handle_error 20 locdi (Some("while")))
    else 
      let ti, fpcur = (type_instr env i t0 locdi fpcur) in (in_loop := false ; TIwhile(te, ti), env, fpcur)


(** val compute_type_for : dmap -> dvar -> instr -> instr -> typ -> dinstr.loc -> int -> tdesci * dmap * int *)
and compute_type_for env dvar e elist i t0 locdi fpcur =
  in_loop := true;
  (* val createTExprList : expression list -> texpression list -> dmap -> int -> texpression list * int *)
  let rec createTExprList exprList acc env fpmax = 
    match exprList with
    | [] -> acc, fpmax
    | e::cdr -> let (texpr, fptmp) = (type_expr env e fpcur) in
                  (createTExprList cdr (acc@[texpr]) env (min fpmax fptmp))
  in
  let tdvar, new_env =
    match dvar with 
    (* d; for(;e;l) *)
    | None -> None, env
    (* for(d;e;l) *)
    | Some(d) -> match compute_type_dinstr_var (new_block_dmap env) d locdi fpcur with (* creating new empty block env for the for loop *)
      | TDinstrVar(tdvar), new_env, _ -> Some(tdvar), new_env
      | _ -> handle_error 20 locdi (Some("for"))
  in 
    begin
      match e with 
      | None -> (* for(d;;l) -> for(d;true;l) *)
        let te = {tdesc = TEconst(True); typ = Tbool} in 
          let (te_list, new_fp) = (createTExprList elist [] new_env fpcur) in
            let s, new_fp = (type_instr new_env i t0 locdi new_fp) in
              in_loop := false; 
              TIfor(tdvar, Some(te), te_list, s), env, new_fp (* return previous env *)
      | Some e ->
        let te, fpcur = (type_expr new_env e fpcur) in
        if(equ_type Tvoid (te.typ))
          then (handle_error 21 locdi None)
          else
            let te_list, new_fp = (createTExprList elist [] new_env fpcur) in
            let s, new_fp = (type_instr new_env i t0 locdi new_fp) in
              in_loop := false; 
              TIfor(tdvar, Some(te), te_list, s), env, new_fp (* return previous env *)
    end


(** val compute_type_block : dmap -> dinstr list -> typ -> bool -> int -> tdesci * dmap * int *)
and compute_type_block env di_list t0 from_dfct fpcur =
  (* val compute_type_block_instr : dinstr list -> dmap -> tdinstr list -> int -> tdesci * dmap * int *)
  let rec compute_type_block_instr di_list new_env tdi_list fpmax =
    match di_list with 
    | [] -> TIblock(TBlock(tdi_list)), env, fpmax (* restore the previous env *)
    | cur_di::cdr ->
      let (cur_tdi, new_env, fptmp) = compute_type_dinstr new_env cur_di t0 fpcur in
        (compute_type_block_instr cdr new_env (tdi_list@[cur_tdi]) (min fpmax fptmp)) (* update the block dmap *)
  in 
    (* if new block from decl fun then do not create a new env *)
    if from_dfct then (compute_type_block_instr di_list env [] fpcur)
      else (compute_type_block_instr di_list (new_block_dmap env) [] (-8)) (* first local var at -8 *)
    

(** val compute_type_dinstr : dmap -> dinstr -> typ -> int -> tdinstr * dmap * int *)
and compute_type_dinstr env di t0 fpcur =
  let locdi = di.locdi in
  match di.descdi with 
  | DinstrVar v -> (compute_type_dinstr_var env v locdi fpcur)
  | DinstrFct dfct -> (compute_type_dinstr_fct env dfct fpcur)
  | Dinstr i -> let (tdesci, env, fpcur) = (compute_type_instr env i t0 locdi fpcur) in (TDinstr({tdesci=tdesci; env=env}), env, fpcur)


(** val compute_type_dinstr_var : dmap -> dvar -> dinstr.loc -> int -> tdinstr * dmap * int *)
and compute_type_dinstr_var env v locdi fpcur = 
  let new_fp = (fpcur - 8) in
  match v with Dvar(typ, ident, exp) ->
    (* check if variable is of type void *)
    if (equ_type typ Tvoid) 
      then (handle_error 22 locdi (Some(ident))) 
      else
      (* check if name already used *)
      if (in_new_env_dmap ident env) 
        then (handle_error 23 locdi (Some(ident))) 
        (* adding variable to new env *)
        else 
          match exp with 
          | None -> TDinstrVar(TDvar(typ, {ident = ident; offset = new_fp}, None)), (add_new_dmap ident typ env), new_fp
          | Some(e) -> 
            let (e_tdesc, e_typ, _) = (compute_type_expr env e new_fp) in
              (* typ x = e; -> test if 'typ' equivalent to type of 'e'*)
              if not (equ_type e_typ typ) 
                then (handle_error 25 locdi (Some(ident)))
                else TDinstrVar(TDvar(typ, {ident = ident; offset = new_fp}, Some({tdesc=e_tdesc; typ=e_typ}))), (add_new_dmap ident typ env), new_fp


(** val compute_type_dinstr_fct : dmap -> dfct -> int -> tdinstr * dmap * int *)
and compute_type_dinstr_fct env fct fpcur = 
  (* print_dmap env; *)
  let t_dfct, env, fpcur = (compute_type_dfct env fct false fpcur) (* false for non global function declaration *)
    in TDinstrFct t_dfct, env, fpcur

(** val compute_type_dfct : dmap -> dfct -> bool -> int -> tdfct * dmap * int *)
and compute_type_dfct env fct is_global fpcur = 
  let fct, locdi = fct.descdfct, fct.locdfct in
  match fct with
    Dfct (typ, ident, p_list, Block(dinstr_list)) ->
    (* check if standard function *)
    if (is_global && (String.equal ident "malloc" || String.equal ident "putchar")) then (handle_error 26 locdi (Some(ident)))
    (* check if function name already used *)
    else if(in_new_env_dmap ident env)
      then (handle_error 22 locdi (Some(ident)))
      else
        (* getting type of all parameters and the new env with these parameters *)
        (* val get_param_types : param list -> typ list -> dmap -> typ list * dmap *)
        let rec get_param_types p_list types env = 
          match p_list with
          | [] -> (types, env)
          | Param(typ,id)::cdr -> 
            let new_env = try (add_new_dmap id typ env) with _ -> (handle_error 24 locdi (Some(id))) 
              in
                (* adding params to new env *)
                (get_param_types cdr (types@[typ]) new_env)
        in
          (* getting correct offset for all parameters *)
          let rec param_list_to_tparam_list p_list acc fp_param =
            match p_list with
            | [] -> acc
            | Param(typ,id)::cdr -> 
              let new_id = {ident = id; offset = fp_param} in
                let new_param = TParam(typ, new_id) in
                  (param_list_to_tparam_list cdr (acc@[new_param]) (fp_param+8)) (* +8 for parameters *)
        in let (p_types, new_env) = (get_param_types p_list [] (new_block_dmap env))
          (* adding fun prototype to new env and adding all parameters to new env *)
          in let fun_typ = Tfct(typ,p_types) in
            let new_env = try (add_new_dmap ident fun_typ new_env) with _ -> (handle_error 23 locdi (Some(ident)))
            (* checking return type of the function *)
              in
                (* new environment with only the function declaration *)
                let new_env_without_params = try (add_new_dmap ident fun_typ env) with _ -> (handle_error 23 locdi (Some(ident)))
                in
                (* print_dmap new_env; *)
                let (tdesci,_,_) = (compute_type_block new_env dinstr_list typ true fpcur) (* t0 is now the fun return typ *)
                in let new_plist = (param_list_to_tparam_list p_list [] 16) (* first param at +16 *)
                in match tdesci with 
                  | TIblock t_block -> TDfct(typ, {ident = ident; offset = fpcur}, new_plist, t_block), new_env_without_params, fpcur
                  | _ -> assert false (* should not end up here *)

          
(** Type a parsed ast
    val type_ast : fileInclude -> tfileInclude *)
and type_ast parsed_ast =
	(* create new env *)
	let (env:dmap) = { old_env=Smap.empty; new_env=Smap.empty} in
  (* add void* malloc(int n) and int putchar(int c) in the env *)
  let env = add_new_dmap "malloc" (Tfct(Tptr(Tvoid), [Tint])) env in
  let env = add_new_dmap "putchar" (Tfct(Tint, [Tint]))  env in
  let env = new_block_dmap env in
    let dfct_list = match parsed_ast with FileInclude(l) -> l in
      let rec compute_type_dfct_list dfct_list new_env tdfct_list =
        match dfct_list with 
        | [] -> TFileInclude(tdfct_list)
        | cur_dfct::cdr -> 
          let (typ,ident,param_list,loc) = match cur_dfct with {descdfct=Dfct(typ,ident,param_list,_) ; locdfct=loc} -> (typ,ident,param_list,loc) in
            let (cur_tdfct, new_env, _) = compute_type_dfct new_env cur_dfct true 0 in (* true for global fct definition *)
            (check_main_in_env new_env typ ident param_list);
            (* add type of f in global env *)
            let rec get_fct_type param_list types_list =
              match param_list with
              | [] -> Tfct(typ,types_list)
              | Param(ptyp,_)::cdr -> (get_fct_type cdr (types_list@[ptyp]))
            in
            let new_env = 
                try (add_old_dmap ident (get_fct_type param_list []) new_env) 
              with _ -> (handle_error 23 loc (Some(ident)))
            in
            let new_env = {old_env = new_env.old_env ; new_env = Smap.empty} in
              (compute_type_dfct_list cdr new_env (tdfct_list@[cur_tdfct])) (* update the global env *)
      in 
        let typed_ast = (compute_type_dfct_list dfct_list env []) in
          if not (!main_is_present) then (handle_error 0 not_found_loc None)
          else typed_ast
