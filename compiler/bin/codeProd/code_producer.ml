open Syntax
open X86_64
open Format

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

let label_counter = ref 0

let tmp_parent_rbp =  0

let label_to_string (i:int) (letter:string option) =
  match letter with
  | Some(letter) ->  (Int.to_string i)^letter
  | None -> (Int.to_string i)


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
    | Uincr_l -> compile_unop_uincr te false (* false for left *)
    | Udecr_l -> compile_unop_udecr te false
    | Uincr_r -> compile_unop_uincr te true (* true for right *)
    | Udecr_r -> compile_unop_udecr te true
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
    val compile_unop_uincr : texpression -> bool -> text *)
and compile_unop_uincr te is_right =
  let assign = 
    match te.tdesc with
    | TEvar id -> 
      (
        comment "incr -> assign TEvar" ++
        movq !%rax (ind ~ofs:id.offset rbp)
      )
    | TEunop (Ustar,te_prim) -> 
      (
        comment "incr -> assign TEunop (Ustar, _)" ++
        compile_expr te_prim ++ (* te_prim is an address *)
        popq rbx ++
        movq (ind rbx) !%rcx ++ (* save value before assignment *)
        movq !%rax (ind rbx)
      )
    | _ -> (assert false) (* can't assign to something else *)
  in
  let typ = te.typ in
  let new_val = 
    match typ with 
    | Tint -> incq (!%rax) (* if int then incr *)
    | Tbool -> movq (imm 0x1) !%rax (* if bool then true *)
    | Tptr _ -> 
      (
        movq !%rax !%rcx ++ (* save value before assignment *)
        leaq (ind ~ofs:0x8 rax) rax (* if pointer then add 0x8 = length of words to curr address *)
      )
    | _ -> assert false
  in
  let expr_value =
    if is_right 
      then movq !%rcx !%rax (* putting old value in rax if it's a right incr *)
      else nop (* putting new value in rax if it's a left incr *)
  in
  (
    movq !%rax !%rcx ++ (* save value before assignment *)
    comment "incr -> new val" ++
    new_val ++
    comment "incr -> assign" ++
    assign ++
    comment "incr -> is right incr" ++
    expr_value ++
    comment "incr -> done"
  )

(** Compile a derementation expression
    val compile_unop_udecr : texpression -> bool -> text *)
and compile_unop_udecr te is_right =
  let assign = 
    match te.tdesc with
    | TEvar id -> 
      (
        comment "decr -> assign TEvar" ++
        movq !%rax (ind ~ofs:id.offset rbp)
      )
    | TEunop (Ustar,te_prim) -> 
      (
        comment "decr -> assign TEunop (Ustar, _)" ++
        compile_expr te_prim ++ (* te_prim is an address *)
        popq rbx ++
        movq (ind rbx) !%rcx ++ (* save value before assignment *)
        movq !%rax (ind rbx)
      )
    | _ -> (assert false) (* can't assign to something else *)
  in
  let typ = te.typ in
  let new_val = 
    match typ with 
    | Tint -> decq (!%rax) (* if int then decr *)
    | Tbool -> xorq (imm 0x1) !%rax (* if bool then xor *)
    | Tptr _ -> 
      (
        movq !%rax !%rcx ++ (* save value before assignment *)
        leaq (ind ~ofs:(-0x8) rax) rax (* if pointer then sub 0x8 = length of words to curr address *)
      )
    | _ -> assert false
  in
  let expr_value =
    if is_right 
      then movq !%rcx !%rax (* putting old value in rax if it's a right incr *)
      else nop (* putting new value in rax if it's a left incr *)
  in
  (
    movq !%rax !%rcx ++ (* save value before assignment *)
    comment "decr -> new val" ++
    new_val ++
    comment "decr -> assign" ++
    assign ++
    comment "decr -> is right incr" ++
    expr_value ++
    comment "decr -> done"
  )

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
    | Arith(Badd) -> beg ++ (compile_binop_add te1 te2)
    | Arith(Bsub) -> beg ++ (compile_binop_sub te1 te2)
    | Arith(Bmul) -> beg ++ imulq !%rbx !%rax
    | Arith(Bdiv) -> beg ++ cqto ++ idivq !%rbx
    | Arith(Bmod) -> beg ++ compile_binop_mod()
    | Logic(Beq)  -> beg ++ cmpq !%rbx !%rax ++ sete !%al ++ movsbq !%al rax
    | Logic(Bneq) -> beg ++ cmpq !%rbx !%rax ++ setne !%al ++ movsbq !%al rax
    | Logic(Blt)  -> beg ++ cmpq !%rbx !%rax ++ setl !%al ++ movsbq !%al rax
    | Logic(Ble)  -> beg ++ cmpq !%rbx !%rax ++ setle !%al ++ movsbq !%al rax
    | Logic(Bgt)  -> beg ++ cmpq !%rbx !%rax ++ setg !%al ++ movsbq !%al rax
    | Logic(Bge)  -> beg ++ cmpq !%rbx !%rax ++ setge !%al ++ movsbq !%al rax
    | AndOr op    -> compile_binop_andor te1 te2 op
  ) ++
  pushq !%rax

(** Compile a modulo operation
    val compile_binop_mod : unit -> text *)
and compile_binop_mod() =
  (
    comment "mod -> start" ++
    comment "mod -> save the numerator in rcx" ++
    movq !%rax !%rcx ++
    comment "mod -> get quotient in rax" ++
    cqto ++
    idivq !%rbx ++
    comment "mod -> multiply quotient and denominator" ++
    imulq !%rax !%rbx ++
    comment "mod -> substract numerator and product" ++
    subq !%rbx !%rcx ++
    comment "mod -> put result in rax" ++
    movq !%rcx !%rax ++
    comment "mod -> end"
  )



(** Compile an add operation
    val compile_binop_add : texpression -> texpression -> text *)
and compile_binop_add te1 te2 =
  match te1.typ with
  | Tptr _ -> 
    begin
      match te2.typ with
      | Tptr _ -> assert false (* cannot add two pointers *)
      | _ -> 
        (
          (* get new address *)
          comment "add -> ptr start" ++
          imulq (imm 0x8) !%rbx ++
          leaq (ind ~index:rbx rax) rax ++
          comment "add -> ptr done"
        )
      end
  | _ ->
    begin
      match te2.typ with
      | Tptr _ -> 
        (
          (* get new address *)
          comment "add -> ptr start" ++
          imulq (imm 0x8) !%rax ++
          leaq (ind ~index:rax rbx) rax ++
          comment "add -> ptr done"
        )
      | _ -> addq !%rbx !%rax
      end

(** Compile a sub operation
    val compile_binop_sub : texpression -> texpression -> text *)
and compile_binop_sub te1 te2 =
  match te1.typ with
  | Tptr _ -> 
    begin
      match te2.typ with
      | Tptr _ -> subq !%rbx !%rax (* sub of two pointers *)
      | _ -> 
        (
          (* get new address *)
          comment "sub -> ptr start" ++
          imulq (imm (-0x8)) !%rbx ++
          leaq (ind ~index:rbx rax) rax ++
          comment "sub -> ptr done"
        )
      end
  | _ ->
    begin
      match te2.typ with
      | Tptr _ -> assert false (* cannot substract a pointer to a non pointer *)
      | _ -> subq !%rbx !%rax
      end


(** Compile an or operation 
    val compile_andor_or : texpression -> texpression -> text *)
and compile_andor_or te1 te2 =
  let beg_label = !label_counter in
  begin
    label_counter := !label_counter + 2;
    (
      comment "binop -> or start" ++
      (* test first expression; if not false then true, else check second expression *)
      compile_expr te1 ++
      popq rax ++
      cmpq (imm 0x0) !%rax ++
      jne (label_to_string beg_label (Some("f"))) ++ (* go to label forward if not false *)
      compile_expr te2 ++
      popq rax ++
      cmpq (imm 0x0) !%rax ++
      jne (label_to_string beg_label (Some("f"))) ++ (* go to label forward if not false *)
      movq (imm 0x0) !%rax ++ (* if te1 and te2 are false then false *)
      jmp (label_to_string (beg_label+1) (Some("f"))) ++
      label (label_to_string beg_label None) ++
      movq (imm 0x1) !%rax ++ (* if te1 or te1 is true then true *)
      label (label_to_string (beg_label+1) None) ++
      comment "binop -> or end"
    )
  end

(** Compile an and operation 
    val compile_andor_and : texpression -> texpression -> text *)
and compile_andor_and te1 te2 =
  let beg_label = !label_counter in
  begin
    label_counter := !label_counter + 2;
    (
    comment "binop -> and start" ++
    (* test first expression; if false then false, else check second expression *)
    compile_expr te1 ++
    popq rax ++
    cmpq (imm 0x0) !%rax ++
    je (label_to_string beg_label (Some("f"))) ++ (* go to label 1 forward if false *)
    compile_expr te2 ++
    popq rax ++
    cmpq (imm 0x0) !%rax ++
    je (label_to_string beg_label (Some("f"))) ++ (* go to label 1 forward if false *)
    movq (imm 0x1) !%rax ++ (* if te1 and te2 are not false then true *)
    jmp (label_to_string (beg_label+1) (Some("f"))) ++
    label (label_to_string beg_label None) ++
    movq (imm 0x0) !%rax ++ (* if te1 or te1 is false then false *)
    label (label_to_string (beg_label+1) None) ++
    comment "binop -> and end"
    )
  end
    
(** Compile an and/or operation
    val compile_binop_and : texpression -> texpression -> andor_binop -> text *)
and compile_binop_andor te1 te2 op =
  let res =
    match op with 
    | Band -> (compile_andor_and te1 te2)
    | Bor  -> (compile_andor_or te1 te2)
  in
  begin
    label_counter := !label_counter - 2; (* restore old value for label counter *)
    res
  end


(** Compile an assignation 
    val compile_assign : texpression -> texpression -> text *)
and compile_assign te1 te2 =
  (* move value inside correct address *)
  let move_value = 
    match te1.tdesc with 
    | TEvar ti -> 
      (
        compile_expr te1 ++
        popq rax ++ (* get return address *)
        popq rbx ++ (* get assigned value *)
        movq !%rbx (ind ~ofs:ti.offset rbp) (* te1 is a variable *)
      )
    | TEunop (Ustar, te1_prim) -> (* te1 is a derefenced pointer *)
        (
          comment "assign -> dereferenced pointer start" ++
          compile_expr te1_prim ++ (* te1_prim is an address *)
          popq rax ++ (* get return address *)
          popq rbx ++ (* get assigned value *)
          movq !%rbx (ind rax) ++
          comment "assign -> dereferenced pointer end"
        )
    | _ -> (assert false) (* can't assign to something else *)
  in
  comment "assign -> start" ++
  comment "assign -> compile expr 2 start" ++
  compile_expr te2 ++
  popq rdx ++
  pushq !%rdx ++
  comment "assign -> compile expr 1 start" ++
  move_value ++
  pushq !%rdx ++ (* the expression value is the one of the assigned value *)
  comment "assign -> end"


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
  let rec put_args_in_stack te_list acc =
    match te_list with 
    | [] -> acc
    | te::cdr -> let acc = (compile_expr te) ++ acc
                  in (put_args_in_stack cdr acc)
  in
  if (f.depth == 0) && ((String.equal f.ident "putchar") || (String.equal f.ident "malloc")) (* if f global *)
    then (compile_call_std f.ident l) 
  else
  (* put all arguments in the stack *)
  comment "caller -> put args in stack" ++
  (put_args_in_stack l nop) ++
  (* put parent rbp in the stack *)
  comment "caller -> put parent rbp in stack" ++
  leaq (ind ~ofs:0 rbp) r9 ++ (* get rbp address in r9 *)
  pushq !%r9 ++ (* push rbp address *)
  (* call the function *)
  comment "caller -> call func" ++
  call f.ident ++
  (* remove arguments from stack *)
  comment "caller -> unstack parent rbp" ++
  popq r9 ++
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
    val compile_instr : text -> text -> tinstr -> int -> text * text * int *)
and compile_instr (global_code: text) (cur_code: text) (instr: Ast_typed.tinstr) (last_loop_label: int) =
  let tdesci, _ = instr.tdesci, instr.env in
  match tdesci with 
  | TIempt                        -> global_code, cur_code, last_loop_label
  | TIexpr exp                    -> global_code, cur_code ++ compile_expr exp ++ popq rax, last_loop_label
  | TIif (exp, i1, i2)            -> compile_instr_if global_code cur_code exp i1 i2 last_loop_label
  | TIwhile (exp, ins)            -> compile_instr_while global_code cur_code exp ins
  | TIfor (var, exp, exp_list, i) -> compile_instr_for global_code cur_code var exp exp_list i
  | TIblock block                 -> compile_block global_code cur_code block last_loop_label
  | TIret exp                     -> compile_instr_ret global_code cur_code exp last_loop_label
  | TIbreak                       -> compile_instr_break global_code cur_code last_loop_label
  | TIcontinue                    -> compile_instr_continue global_code cur_code last_loop_label


(** Compile an if instruction
    val compile_intr_if : text -> text -> texpression -> tinstr -> tinstr -> int -> text * text * int *)
and compile_instr_if global_code cur_code exp i1 i2 last_loop_label =
  let beg_label = !label_counter
  in
  begin
    label_counter := !label_counter + 3;
    let g1, c1, _ =
      compile_instr nop nop i1 last_loop_label
    in
    let g2, c2, _ =
      compile_instr nop nop i2 last_loop_label
    in
    let code =
      (
        comment "if -> compile expr start" ++
        compile_expr exp ++
        comment "if -> compile expr end" ++
        popq rax ++
        cmpq (imm 0x0) !%rax ++ (* return 0x0 if exp is false *)
        je (label_to_string (beg_label+1) (Some("f"))) ++ (* jump to else if false *)
        comment "if -> first instr start" ++
        label (label_to_string beg_label None) ++ (* if instr *)
        c1 ++
        jmp (label_to_string (beg_label+2) (Some("f"))) ++ (* skip else statement *)
        comment "if -> first instr end" ++
        comment "if -> second instr start" ++
        label (label_to_string (beg_label+1) None) ++ (* else instr *)
        c2 ++
        jmp (label_to_string (beg_label+2) (Some("f"))) ++
        comment "if -> second instr end" ++
        label (label_to_string (beg_label+2) None) (* begin next instruction *)
      )
    in 
      begin
        label_counter := !label_counter - 3; (* restore old value for label counter *)
        global_code ++ g1 ++ g2, cur_code ++ code, last_loop_label
      end
  end


(** Compile a while instruction
    val compile_instr_while : text -> text -> texpression -> tinstr -> text -> text * int *)
and compile_instr_while global_code cur_code exp ins =
  let beg_label = !label_counter
  in
  begin
    label_counter := !label_counter + 3;
    let g1, c1, _ =
      compile_instr nop nop ins beg_label
    in
    let code = 
      (
        comment "while -> start" ++
        label (label_to_string (beg_label+2) None) ++
        comment "while -> compile expr start" ++
        compile_expr exp ++
        comment "while -> compile expr end" ++
        popq rax ++
        comment "while -> condition check start" ++
        cmpq (imm 0x0) !%rax ++ (* return 0x0 if exp is false *)
        je (label_to_string (beg_label+1) (Some("f"))) ++ (* leave while loop *) 
        comment "while -> condition check end" ++
        comment "while -> instr start" ++
        c1 ++
        comment "while -> instr end" ++
        label (label_to_string beg_label None) ++ (* label for continue *)
        jmp (label_to_string (beg_label+2) (Some("b"))) ++ (* check the expression again *)
        label (label_to_string (beg_label+1) None) ++ (* begin next instruction *)
        comment "while -> end"
      )
    in 
      begin
        label_counter := !label_counter - 3; (* restore old value for label counter *)
        global_code ++ g1, cur_code ++ code, beg_label
      end
  end


(** Compile a return instruction
    val compile_instr_ret : text -> text -> texpression -> int -> text * text * int *)
and compile_instr_ret global_code cur_code exp last_loop_label =
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
  in global_code, cur_code ++ code, last_loop_label


(** Compile a break instruction 
    compile_instr_break : text -> text -> int -> text * text * int *)
and compile_instr_break global_code cur_code last_loop_label =
  let code =
    (
      comment "break -> start" ++
      jmp (label_to_string (last_loop_label + 1) (Some("f"))) ++ (* the first label after the loop *)
      comment "break -> end"
    )
  in
  global_code, cur_code ++ code, last_loop_label


(** Compile a continue instruction 
    compile_instr_continue : text -> text -> int -> text * text * int *)
and compile_instr_continue global_code cur_code last_loop_label =
  let code =
    (
      comment "continue -> start" ++
      jmp (label_to_string last_loop_label (Some("f"))) ++ (* the first label before the loop *)
      comment "continue -> end"
    )
  in
  global_code, cur_code ++ code, last_loop_label


(** Compile a for instruction
    compile_instr_for : text -> text -> tdvar option -> texpression option -> texpression list -> tinstr -> text * text * int *)
and compile_instr_for global_code cur_code var exp exp_list i =
  let beg_label = !label_counter
  in
  begin
    label_counter := !label_counter + 3;
    let rec compile_expression_list exp_list acc =
      match exp_list with
      | [] -> 
        (
          comment "for -> step expressions start" ++
          acc ++
          comment "for -> step expressions end"
        )
      | exp::cdr ->
        let code = acc ++ (compile_expr exp) ++ popq rax
          in (compile_expression_list cdr code)
    in
    let code_decl_var =
      match var with 
      | None -> 
        (
          cur_code ++
          comment "for -> start" ++
          comment "for -> no function declaration"
        )
      | Some(d) -> 
        let _, c, _ = compile_decl_var global_code cur_code d beg_label
          in 
        (
          c ++
          comment "for -> start"
        )
    in
    let code_comparison =
      let comp_exp_condition =
        match exp with
        | None -> 
          (
            comment "for -> empty check expression" ++
            movq (imm 0x1) !%rax
          )

        | Some(e) ->
          (
            comment "for -> compile expr start" ++
            compile_expr e ++
            comment "for -> compile expr end" ++
            popq rax
          )
        in       
      (
        comp_exp_condition ++
        comment "for -> condition check start" ++
        cmpq (imm 0x0) !%rax ++ (* return 0x0 if exp is false *)
        je (label_to_string (beg_label+1) (Some("f"))) ++ (* leave for loop *) 
        comment "for -> condition check end"
      )
    in
    let global_code_instr, code_instr, _ = compile_instr nop nop i beg_label
    in 
    let code =
      (
        code_decl_var ++
        label (label_to_string (beg_label+2) None) ++ (* 1b for beginning of for loop *)
        code_comparison ++
        code_instr ++
        label (label_to_string beg_label None) ++ (* label for continue *)
        compile_expression_list exp_list nop ++
        jmp (label_to_string (beg_label+2) (Some("b"))) ++ (* go back to beginning of the loop *)
        label (label_to_string (beg_label+1) None) ++ (* 2f for end of for loop *)
        comment "for -> end"
      ) 
    in
      begin
        label_counter := !label_counter - 3; (* restore old value for label counter *)
        global_code ++ global_code_instr, code, beg_label
      end
  end


(** Compile a variable declaration 
    val compile_decl_var : text -> text -> tdvar -> int -> text * text * int *)
and compile_decl_var (global_code: text) (cur_code: text) (dvar: Ast_typed.tdvar) last_loop_label =
  match dvar with TDvar(_, tident, texp) ->
    let value =
      match texp with 
      | Some(exp) -> compile_expr exp
      | None -> pushq (imm 0x0) (* default value *)
    in
    let code =
      (* get the variable value in rax *)
      value ++
      popq rax ++
      (* store it at the correct position *)
      movq !%rax (ind ~ofs:tident.offset rbp)
    in global_code, cur_code ++ code, last_loop_label


(** Compile an instruction declaration 
    val compile_decl_instr : text -> text -> tdinstr -> int -> text * text * int *)
and compile_decl_instr (global_code: text) (cur_code: text) (dinstr: Ast_typed.tdinstr) last_loop_label =
  match dinstr with
  | TDinstrFct tdfct -> compile_decl_fun global_code cur_code tdfct last_loop_label
  | TDinstrVar tdvar -> compile_decl_var global_code cur_code tdvar last_loop_label
  | TDinstr tinstr -> compile_instr global_code cur_code tinstr last_loop_label


(** Compile an instruction block 
    val compile_block : text -> text -> tblock -> int -> text * text * int *)
and compile_block (global_code: text) (cur_code: text) (blck: Ast_typed.tblock) last_loop_label =
  match blck with TBlock l ->
    let rec aux tdi_l gbl_c cur_c last_loop_label = 
      match tdi_l with
      | [] -> gbl_c, cur_c, last_loop_label
      | tdi::cdr ->
        let gbl_c, cur_c, last_loop_label = (compile_decl_instr gbl_c cur_c tdi last_loop_label)
          in (aux cdr gbl_c cur_c last_loop_label)
    in (aux l global_code cur_code last_loop_label)


(** Compile a function declaration
    val compile_decl_fun : text -> text -> tdfct -> int -> text * text * int *)
and compile_decl_fun (global_code: text) (cur_code: text) (dfct: Ast_typed.tdfct) last_loop_label =
  match dfct with TDfct(typ, tident, _, tblock) ->
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
    let gbl_c, cur_c, last_loop_label = compile_block global_code nop tblock last_loop_label
      in 
      (* if void function then add empty ret *)
      let end_code = match typ with 
        | Tfct(ty,_) when Typer.equ_type ty Tvoid -> 
          (
            comment "useless ret -> start" ++
            useless_end_code ++
            comment "useless ret -> end"
          )
        | _ -> 
          begin
            (* print_string (Ast.typ_to_string typ); *)
            nop
          end
      in 
        (* if main function then add empty ret *) 
        let end_main = 
          if String.equal tident.ident "main" 
            then 
              (
              comment "useless ret -> start" ++
              useless_end_code ++
              comment "useless ret -> end"
              )
          else nop
      in
        let code = 
          beg_code ++ 
          cur_c ++
          (* comment "cur_code -> end" ++ *)
          end_code ++
          end_main
        in gbl_c, cur_code ++ code, last_loop_label


(** Compile a file include representing a program 
    val compile_file_include : text -> text -> tfileInclude -> int -> text * text * int *)
and compile_file_include (global_code: text) (cur_code: text) (tprog: Ast_typed.tfileInclude) last_loop_label =
  (* hand made fold left over the tdfct list *)
  let rec aux_fold_left dfct_list global_code cur_code last_loop_label =
    match dfct_list with 
    | [] -> global_code, cur_code, last_loop_label
    | dfct::cdr -> 
      let global_code, cur_code, last_loop_label = compile_decl_fun global_code cur_code dfct last_loop_label
        in (aux_fold_left cdr global_code cur_code last_loop_label)
  in
  match tprog with 
    TFileInclude dfct_list -> (aux_fold_left dfct_list global_code cur_code last_loop_label)


(** Compile a typed ast and put the resulting assembler in the output file
    val compile_program : Ast_typed.tfileInclude -> string -> program *)
let compile_program (p:Ast_typed.tfileInclude) ofile =
  let global_code, cur_code, _ = (compile_file_include (globl "main") nop p (!label_counter)) in
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