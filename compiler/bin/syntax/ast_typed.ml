(** Abstract Syntax Tree for the petitC Compiler *)

open Ast

module Smap = Map.Make(String)
type env = typ Smap.t

exception = Environnement_error of string

(** double maps : using old and new environnements for blocks *)
type dmap = { old_env : env ; new_env : env}

(* val search_dmap : ident -> dmap -> bool -> typ *)
let search_dmap id doub_map two_envs = 
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  match Smap.find_opt id nenv, Smap.find_opt id oenv with
    | None, None -> raise Environnement_error "no id found"
    | None, Some(t) -> if two_envs then t else raise Environnement_error "no id found"
    | Some(t), None -> t
    | Some(t1), Some(t2) -> t1

(* val in_new_env : ident -> dmap -> bool *)
let in_new_env id doub_map =
  let nenv = doub_map.new_env in 
  Smap.mem id nenv

(* val add_dmap : ident -> typ -> dmap -> dmap ->  *)
let add_dmap id ty doub_map =
  let nenv = doub_map.new_env in
  if Smap.mem id nenv then raise Environnement_error "id already existing" else { old_env = dmap.old_env ; new_env = (Smap.add id typ nenv) }

(* val union_dmap : dmap -> env *)
let union_dmap doub_map =
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  let union_fun = fun (_, old_id, new_id) -> Some(new_id)
  Smap.union union_fun oenv nenv

(** Expressions *)
type texpression = {
  tdesc: tdesc;
  typ: typ;
}

(** Description of texpressions *)
and tdesc =
  | TEconst of const 
  | TEvar of ident
  | TEunop of unop * texpression
  | TEbinop of binop * texpression * texpression
  | TEassign of texpression * texpression
  | TEcall of ident * texpression list
  | TEsizeof of typ (* maybe adding primitives *)

(** Variable declarations *)
type tdvar = TDvar of typ * ident * texpression option

(** Instruction declarations *)
and tdinstr = 
  | TDinstrFct of tdfct
  | TDinstrVar of tdvar
  | TDinstr of tinstr

(** Function declarations *)
and tdfct = TDfct of typ * ident * param list * tblock

(** Instruction blocks *)
and tblock = TBlock of tdinstr list

(** Instructions *)
and tinstr = {
  tdesci: tdesci;
  env: env
}

(** Description of tinstr *)
and tdesci = 
  | TIempt 
  | TIexpr of texpression
  | TIif of texpression * tinstr * tinstr
  | TIwhile of texpression * tinstr
  | TIfor of tdvar option * texpression option * texpression list * tinstr
  | TIblock of tblock
  | TIret of texpression option
  | TIbreak 
  | TIcontinue

(** File inclusions *)
type tfileInclude = TFileInclude of tdfct list
