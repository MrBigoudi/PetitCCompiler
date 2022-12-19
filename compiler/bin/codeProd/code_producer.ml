open Syntax
open X86_64
open Format

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp


(** Compile a typed expression
    val compile_expr : Ast_typed.texpression -> text *)
let rec compile_expr (exp: Ast_typed.texpression) =
  let tdesc = exp.tdesc in
  match tdesc with 
  | TEconst cst              -> compile_const cst
  | TEvar tident             -> compile_var tident
  | TEunop (op, te)          -> compile_unop op te
  | TEbinop (op, te1, te2)   -> compile_binop op te1 te2
  | TEassign (te1, te2)      -> compile_assign te1 te2
  | TEcall (tident, te_list) -> compile_call tident te_list
  | TEsizeof typ             -> compile_sizeof typ

(** Compile a constant 
    val compile_const : Ast.const -> text *)
and compile_const cst =
  (* comment "compile const" ++ *)
  match cst with
  | Int i -> pushq (imm i)
  | True -> pushq (imm 0x1)
  | False -> pushq (imm 0x0)
  | Null -> pushq (imm 0x0)

(** Compile a variable
    val compile_var : Ast_typed.tident -> text *)
and compile_var tident =
  (* comment "compile var" ++ *)
  pushq (ind ~ofs:tident.offset rbp) (* return the variable adress *)

(** Compile a unary operation 
    val compile_unop : Ast.unop -> Ast_typed.texpression -> text *)
and compile_unop op te =
  let typ = te.typ in
  compile_expr te ++
  popq rax ++
  (match op with 
    | Unot    -> compile_unop_unot typ
    | Ustar   -> compile_unop_ustar typ
    | Uamp    -> compile_unop_uamp te
    | Uincr_l -> compile_unop_uincr typ
    | Udecr_l -> compile_unop_udecr typ
    | Uincr_r -> compile_unop_uincr typ (* diff with Uincr_l TODO ? *)
    | Udecr_r -> compile_unop_udecr typ (* diff with Uincr_l TODO ? *)
    | Uplus   -> compile_unop_uplus typ
    | Uminus  -> compile_unop_uminus typ
  ) ++
  pushq !%rax 

(** Compile a not expression
    val compile_unop_ustar : typ -> text *)
and compile_unop_unot typ =
  match typ with
  | Tbool -> xorq (imm 0x1) !%rax
  | Tfct _ -> assert false
  | _ -> cmpq (imm 0x0) !%rax ++ sete !%al ++ movsbq !%al rax

(** Compile a star expression
    val compile_unop_ustar : typ -> text *)
and compile_unop_ustar typ =
  match typ with 
  | Tptr _ -> movq (ind rax) !%rax (* address is in rax -> put the value in rax *)
  | _ -> assert false

(** Compile an ampersand expression
    val compile_unop_uamp : texpression -> -> text *)
and compile_unop_uamp te =
  match te.typ with 
  | Tptr _ -> nop (* do nothing, address already in rax *)
  | _ -> begin
          match te.tdesc with 
          | TEvar id -> leaq (ind ~ofs:id.offset rbp) rax
          | _ -> assert false
        end

(** Compile an incrementation expression
    val compile_unop_uincr : typ -> text *)
and compile_unop_uincr typ =
  match typ with 
  | Tint -> incq (!%rax) (* if int then incr *)
  | Tbool -> movq (imm 0x1) !%rax (* if bool then true *)
  | Tptr _ -> addq (imm 0x8) !%rax (* if pointer then add 0x8 = length of words to curr address *)
  | _ -> assert false

(** Compile a derementation expression
    val compile_unop_udecr : typ -> text *)
and compile_unop_udecr typ =
    match typ with 
    | Tint -> decq (!%rax)
    | Tbool -> xorq (imm 0x1) !%rax
    | Tptr _ -> subq (imm 0x8) !%rax
    | _ -> assert false

(** Compile a unary plus
    val compile_unop_uplus : typ -> text *)
and compile_unop_uplus typ =
  match typ with
  | Tint -> nop (* do nothing *)
  | Tbool -> nop (* do nothing *)
  | _ -> assert false

(** Compile a unary minus
    val compile_unop_uminus : typ -> text *)
and compile_unop_uminus typ =
  match typ with
  | Tint -> negq !%rax (* negate the value *)
  | Tbool -> negq !%rax (* do nothing *)
  | _ -> assert false

(** Compile binary operations
    val compile_binop : binop -> texpression -> texpression -> text *)
and compile_binop op te1 te2 = 
  let beg = (
    compile_expr te1 ++
    compile_expr te2 ++
    popq rbx ++ popq rax 
  )
  in
  (
  match op with 
    | Arith(Badd) -> beg ++ addq !%rbx !%rax
    | Arith(Bsub) -> beg ++ subq !%rbx !%rax
    | Arith(Bmul) -> beg ++ imulq !%rbx !%rax
    | Arith(Bdiv) -> beg ++ cqto ++ idivq !%rbx
    | Arith(Bmod) -> failwith "TODO modulo"
    | Logic(Beq)  -> beg ++ cmpq !%rbx !%rax ++ sete !%al ++ movsbq !%al rax
    | Logic(Bneq) -> beg ++ cmpq !%rbx !%rax ++ setne !%al ++ movsbq !%al rax
    | Logic(Blt)  -> beg ++ cmpq !%rbx !%rax ++ setl !%al ++ movsbq !%al rax
    | Logic(Ble)  -> beg ++ cmpq !%rbx !%rax ++ setle !%al ++ movsbq !%al rax
    | Logic(Bgt)  -> beg ++ cmpq !%rbx !%rax ++ setg !%al ++ movsbq !%al rax
    | Logic(Bge)  -> beg ++ cmpq !%rbx !%rax ++ setge !%al ++ movsbq !%al rax
    | AndOr op    -> compile_binop_andor te1 te2 op
  ) ++
  pushq !%rax

(** Compile an and/or operation
    val compile_binop_and : texpression -> texpression -> andor_binop -> text *)
and compile_binop_andor te1 te2 op=
  let imm1, imm2 = match op with 
    | Band -> 0x0, 0x1
    | Bor  -> 0x1, 0x0
  in
  compile_expr te1++
  popq rax ++
  (* test first expression then test second if one wasn't enough *)
  cmpq (imm imm1) !%rax ++
  je "1f" ++
  compile_expr te2 ++
  popq rax ++
  cmpq (imm imm1) !%rax ++
  je "1f" ++
  movq (imm imm2) !%rax ++
  jmp "2f" ++
  label "1" ++
  movq (imm imm1) !%rax ++
  label "2"


(** Compile an assignation 
    val compile_assign : texpression -> texpression -> text *)
and compile_assign te1 te2 =
  (* move value inside correct address *)
  let move_value = 
    match te1.tdesc with 
    | TEvar ti -> movq !%rbx (ind ~ofs:ti.offset rbp) (* te1 is a variable *)
    | TEunop (Ustar, te1_prim) -> (* te1 is a derefenced pointer *)
        (compile_expr te1_prim ++ (* te1_prim is an address *)
        popq rax ++
        movq !%rbx (ind rax))
    | _ -> (assert false) (* can't assign to something else *)
  in
  compile_expr te1 ++
  compile_expr te2 ++
  popq rbx ++ (* get assigned value *)
  popq rax ++ (* get address *)
  move_value ++
  pushq (imm 0x1) (* the expression value is true *)


(** Compile a call to a standard function
    val compile_call_std : ident -> texpression list -> text *)
and compile_call_std ident te_list =
  let te = match te_list with 
    | [te] -> te
    | _ -> assert false (* only malloc and putchar for now *)
  in
  compile_expr te ++
  comment "caller -> put args in rdi (cause standard function)" ++
  popq rdi++
  comment "caller -> call func" ++
  call ident ++
  comment "caller -> stack the result" ++
  pushq !%rax ++
  comment "caller -> end "
  


(** Compile a function call
    val compile_call : tident -> texpression list -> text *)
and compile_call f l =
  if (String.equal f.ident "putchar") || (String.equal f.ident "malloc") 
    then (compile_call_std f.ident l) 
  else
  (* put all arguments in the stack *)
  comment "caller -> put args in stack" ++
  List.fold_left (fun code e -> code ++ compile_expr e) nop l ++
  (* call the function *)
  comment "caller -> call func" ++
  call f.ident ++
  (* remove arguments from stack *)
  comment "caller -> unstack args" ++
  popn (8 * List.length l) ++ 
  (* return the function result *)
  comment "caller -> stack the result" ++
  pushq !%rax ++
  comment "caller -> end "


(** Compile a call to sizeof 
    val compile_sizeof : typ -> text *)
and compile_sizeof typ =
  let rec get_size (typ: Ast.typ) =
    match typ with
    | Tfct(t,l) -> (get_size t)+(sizeof_list l 0)
    | _ -> 0x8
  and sizeof_list types res =
    match types with
    | [] -> res
    | typ::cdr -> (sizeof_list cdr (res+(get_size typ)))
  in
    let size = (get_size typ)
      in pushq (imm size)



(** Compile an instruction 
    val compile_instr : text -> text -> tinstr -> text * text *)
and compile_instr (global_code: text) (cur_code: text) (instr: Ast_typed.tinstr) =
  let tdesci, env = instr.tdesci, instr.env in
  match tdesci with 
  | TIempt                        -> global_code, cur_code
  | TIexpr exp                    -> global_code, cur_code ++ compile_expr exp ++ popq rax
  | TIif (exp, i1, i2)            -> failwith "TODO"
  | TIwhile (exp, ins)            -> failwith "TODO"
  | TIfor (var, exp, exp_list, i) -> failwith "TODO"
  | TIblock block                 -> compile_block global_code cur_code block
  | TIret exp                     -> compile_instr_ret global_code cur_code exp
  | TIbreak                       -> failwith "TODO"
  | TIcontinue                    -> failwith "TODO"


(** Compile a return instruction
    val compile_instr_ret : text -> text -> texpression -> text * text *)
and compile_instr_ret global_code cur_code exp =
  let code = 
    match exp with
    | Some(exp) ->
      (
        compile_expr exp ++ 
        (* put result in rax *)
        comment "callee -> put result in rax" ++
        popq rax ++ 
        comment "callee -> unstack activation table" ++
        leave ++
        comment "callee -> end" ++
        ret
      )
    | None ->
      (
        (* put 0x0 in rax *)
        comment "callee -> put result in rax (0x0 cause void)" ++
        movq (imm 0x0) !%rax ++ 
        comment "callee -> unstack activation table" ++
        leave ++
        comment "callee -> end" ++
        ret
      )
  in global_code, cur_code ++ code


(** Compile a variable declaration 
    val compile_decl_var : text -> text -> tdvar -> text * text *)
and compile_decl_var (global_code: text) (cur_code: text) (dvar: Ast_typed.tdvar) =
  match dvar with TDvar(typ, tident, texp) ->
    let value =
      match texp with 
      | Some(exp) -> compile_expr exp
      | None -> pushq (imm 0x0) (* default value *)
    in
    let code =
      (* get the variable value in rax *)
      value ++
      popq rax ++
      (* stocke it at the correct position *)
      movq !%rax (ind ~ofs:tident.offset rbp)
    in global_code, cur_code ++ code


(** Compile an instruction declaration 
    val compile_decl_instr : text -> text -> tdinstr -> text * text *)
and compile_decl_instr (global_code: text) (cur_code: text) (dinstr: Ast_typed.tdinstr) =
  match dinstr with
  | TDinstrFct tdfct -> compile_decl_fun global_code cur_code tdfct
  | TDinstrVar tdvar -> compile_decl_var global_code cur_code tdvar
  | TDinstr tinstr -> compile_instr global_code cur_code tinstr


(** Compile an instruction block 
    val compile_block : text -> text -> tblock -> text * text *)
and compile_block (global_code: text) (cur_code: text) (blck: Ast_typed.tblock) =
  match blck with TBlock l ->
    let rec aux tdi_l gbl_c cur_c = 
      match tdi_l with
      | [] -> gbl_c, cur_c
      | tdi::cdr ->
        let gbl_c, cur_c = (compile_decl_instr gbl_c cur_c tdi)
          in (aux cdr gbl_c cur_c)
    in (aux l global_code cur_code)


(** Compile a function declaration
    val compile_decl_fun : text -> text -> tdfct -> text * text *)
and compile_decl_fun (global_code: text) (cur_code: text) (dfct: Ast_typed.tdfct) =
  match dfct with TDfct(typ, tident, tparam_l, tblock) ->
    let beg_code =
      label tident.ident ++
      (* save rbp *)
      comment "callee -> save rbp" ++
      pushq !%rbp ++
      movq  !%rsp !%rbp ++
      (* allocate activation table *)
      comment "callee -> allocate activation table" ++
      pushn (-tident.offset)
    in 
    let useless_end_code = 
      (
        comment "callee -> put result in rax (0x0 cause no return)" ++
        movq (imm 0x0) !%rax ++ 
        comment "callee -> unstack activation table" ++
        leave ++
        comment "callee -> end" ++
        ret
      )
    in
    let gbl_c, cur_c = compile_block global_code nop tblock
      in 
      (* if void function then add empty ret *)
      let end_code = match typ with 
        | Tfct(Tvoid,_) -> useless_end_code
        | _ -> nop
      in 
        (* if main function then add empty ret *) 
        let end_main = 
          if String.equal tident.ident "main" 
            then useless_end_code
          else nop
      in
        let code = 
          beg_code ++ 
          cur_c ++
          (* comment "cur_code -> end" ++ *)
          end_code ++
          end_main
        in gbl_c, cur_code ++ code


(** Compile a file include representing a program 
    val compile_file_include : text -> text -> tfileInclude -> text * text *)
and compile_file_include (global_code: text) (cur_code: text) (tprog: Ast_typed.tfileInclude) =
  (* hand made fold left over the tdfct list *)
  let rec aux_fold_left dfct_list global_code cur_code =
    match dfct_list with 
    | [] -> global_code, cur_code
    | dfct::cdr -> 
      let global_code, cur_code = compile_decl_fun global_code cur_code dfct
        in (aux_fold_left cdr global_code cur_code)
  in
  match tprog with 
    TFileInclude dfct_list -> (aux_fold_left dfct_list global_code cur_code)


(** Compile a typed ast and put the resulting assembler in the output file
    val compile_program : Ast_typed.tfileInclude -> string -> program *)
let compile_program (p:Ast_typed.tfileInclude) ofile =
  let global_code, cur_code = (compile_file_include (globl "main") nop p) in
  let p = 
    { text = 
        global_code ++
        cur_code;
        data = nop
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f