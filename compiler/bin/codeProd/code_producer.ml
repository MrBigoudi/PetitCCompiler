open Syntax
open X86_64
open Format

let popn n = addq (imm n) !%rsp
let pushn n = subq (imm n) !%rsp

(** Compile a typed expression
    val compile_expr : Ast_typed.texpression -> text *)
let rec compile_expr (exp: Ast_typed.texpression) =
  let tdesc, typ = exp.tdesc, exp.typ in
  match tdesc with 
  | TEconst cst              -> compile_const cst
  | TEvar tident             -> compile_var tident
  | TEunop (op, te)          -> compile_unop op te typ
  | TEbinop (op, te1, te2)   -> failwith "TODO"
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
  pushq (ind ~ofs:tident.offset rbp)


(** Compile a unary operation 
    val compile_unop : Ast.unop -> Ast_typed.tdesc -> Ast.typ -> text *)
and compile_unop op te typ =
  compile_expr te ++
  popq rax ++
  (match op with 
    | Unot    -> notq !%rax
    | Ustar   -> compile_unop_ustar typ
    | Uamp    -> failwith "TODO"
    | Uincr_l -> failwith "TODO"
    | Udecr_l -> failwith "TODO"
    | Uincr_r -> failwith "TODO"
    | Udecr_r -> failwith "TODO"
    | Uplus   -> failwith "TODO"
    | Uminus  -> failwith "TODO") ++
  pushq !%rax 

(** Compile a star expression
    val compile_unop_ustar : typ -> text *)
and compile_unop_ustar typ =
  match typ with 
  | Tint -> incq (!%rax) (* if int then incr *)
  | Tbool -> movq (imm 0x1) !%rax (* if bool then true *)
  | Tptr _ -> addq (imm 0x8) !%rax (* if pointer then add 0x8 = length of words *)
  | _ -> assert false



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