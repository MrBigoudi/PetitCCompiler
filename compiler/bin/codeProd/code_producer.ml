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
  | TEassign (te1, te2)      -> failwith "TODO"
  | TEcall (tident, te_list) -> failwith "TODO"
  | TEsizeof typ             -> failwith "TODO"

(** Compile a constant 
    val compile_const : Ast.const -> text *)
and compile_const cst =
  match cst with
  | Int i -> pushq (imm i)
  | True -> pushq (imm 0x1)
  | False -> pushq (imm 0x0)
  | Null -> pushq (imm 0x0)

(** Compile a variable
    val compile_var : Ast_typed.tident -> text *)
and compile_var tident =
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
    | Uamp    -> compile_unop_uamp typ
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
    val compile_unop_uamp : tdesc -> text *)
and compile_unop_uamp tdesc =
  match tdesc with 
  | Tptr _ -> nop (* do nothing, address already in rax *)
  | _ -> assert false  

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
  compile_expr te1 ++
  compile_expr te2 ++
  popq rbx ++ popq rax ++
  (
  match op with 
    | Arith(Badd) -> addq !%rbx !%rax
    | Arith(Bsub) -> subq !%rbx !%rax
    | Arith(Bmul) -> imulq !%rbx !%rax
    | Arith(Bdiv) -> cqto ++ idivq !%rbx
    | Arith(Bmod) -> compile_binop_mod()
    | Logic(Beq)  -> cmpq !%rbx !%rax ++ sete !%al ++ movsbq !%al rax
    | Logic(Bneq) -> cmpq !%rbx !%rax ++ setne !%al ++ movsbq !%al rax
    | Logic(Blt)  -> cmpq !%rbx !%rax ++ setl !%al ++ movsbq !%al rax
    | Logic(Ble)  -> cmpq !%rbx !%rax ++ setle !%al ++ movsbq !%al rax
    | Logic(Bgt)  -> cmpq !%rbx !%rax ++ setg !%al ++ movsbq !%al rax
    | Logic(Bge)  -> cmpq !%rbx !%rax ++ setge !%al ++ movsbq !%al rax
    | AndOr(Band) -> failwith "TODO"
    | AndOr(Bor)  -> failwith "TODO"
  ) ++
  pushq !%rax


(** Compile a modulo operation
    val compile_binop_mod : text *)
and compile_binop_mod() =
  (* mov    -0x8(%rbp),%eax from gcc *)
  (* cltd                   from gcc *)
  (* idivl  -0xc(%rbp)      from gcc *)
  (* mov    %edx,%eax       from gcc *)
  failwith "TODO modulo"



(** Compile an instruction 
    val compile_instr : text -> text -> tinstr -> text * text *)
and compile_instr (global_code: text) (cur_code: text) (instr: Ast_typed.tinstr) =
  let tdesci, env = instr.tdesci, instr.env in
  match tdesci with 
  | TIempt                        -> failwith "TODO"
  | TIexpr exp                    -> failwith "TODO"
  | TIif (exp, i1, i2)            -> failwith "TODO"
  | TIwhile (exp, ins)            -> failwith "TODO"
  | TIfor (var, exp, exp_list, i) -> failwith "TODO"
  | TIblock block                 -> failwith "TODO"
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
        popq rax ++ 
        ret
      )
    | None -> ret 
  in code ++ cur_code, global_code


(** Compile a variable declaration 
    val compile_decl_var : text -> text -> tdvar -> text * text *)
and compile_decl_var (global_code: text) (cur_code: text) (dvar: Ast_typed.tdvar) =
  failwith "TODO compile decl var"


(** Compile an instruction declaration 
    val compile_decl_instr : text -> text -> tdinstr -> text * text *)
and compile_decl_instr (global_code: text) (cur_code: text) (dinstr: Ast_typed.tdinstr) =
  failwith "TODO compile decl instr"


(** Compile an instruction block 
    val compile_block : text -> text -> tblock -> text * text *)
and compile_block (global_code: text) (cur_code: text) (blck: Ast_typed.tblock) =
  failwith "TODO compile block"


(** Compile a function declarations 
    val compile_decl_fun : text -> text -> tdfct -> text * text *)
and compile_decl_fun (global_code: text) (cur_code: text) (dfct: Ast_typed.tdfct) =
  failwith "TODO compile decl fun"


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
  let global_code, cur_code = (compile_file_include nop nop p) in
  let p = 
    { text = 
        (* main function *)
        globl "main" ++ label "main" ++
        movq !%rsp !%rbp ++
        global_code ++
        movq (imm 0) !%rax ++ (* exit *)
        ret ++
        (* TODO putchar function *)
        (* TODO malloc function *)
        (* TODO sizeof function *)
        cur_code;
      
      data = nop (* empty I guess ? *)
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f