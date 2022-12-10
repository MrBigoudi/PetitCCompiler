(** Abstract Syntax Tree for the petitC Compiler *)

open Ast

module Smap = Map.Make(String)
type env = typ Smap.t

exception Environnement_error of string

(** double maps : using old and new environnements for blocks *)
type dmap = { old_env : env ; new_env : env}

(** val search_dmap : ident -> dmap -> typ *)
let search_dmap id doub_map = 
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  match Smap.find_opt id oenv, Smap.find_opt id nenv with
    | None, None -> raise (Environnement_error("no id found"))
    | None, Some(t) -> t
    | Some(t), None -> t
    | _, Some(t2) -> t2

(** val in_new_env : ident -> dmap -> bool *)
let in_new_env_dmap id doub_map =
  let nenv = doub_map.new_env in 
  Smap.mem id nenv

(** val add_new_dmap : ident -> typ -> dmap -> dmap ->  *)
let add_new_dmap id ty doub_map =
  let nenv = doub_map.new_env in
  if Smap.mem id nenv 
    then raise (Environnement_error("id already existing")) 
    else { old_env = doub_map.old_env ; new_env = (Smap.add id ty nenv) }

(** val add_old_dmap : ident -> typ -> dmap -> dmap *)
let add_old_dmap id ty doub_map =
  let oldv = doub_map.old_env in
  if Smap.mem id oldv
    then raise (Environnement_error("id already existing")) 
    else { old_env = (Smap.add id ty oldv) ; new_env = doub_map.new_env }

(** val union_dmap : dmap -> env *)
let union_dmap doub_map =
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  let union_fun _ _ new_val = Some(new_val) in
    Smap.union union_fun oenv nenv

(** val new_block_dmap : dmap -> dmap *)
let new_block_dmap doub_map =
  let oenv = union_dmap doub_map in
  let nenv = Smap.empty in 
    { old_env = oenv; new_env = nenv }

(* for debugging *)
(** val print_dmap : dmap -> unit *)
let print_dmap doub_map =
  let f key typ =
    print_string ("key: "^key^", typ: "^typ_to_string typ^"\n")
  in
  print_string "\nold env:\n";
  Smap.iter f doub_map.old_env;
  print_string "\nnew env:\n";
  Smap.iter f doub_map.new_env;
  print_string "\n";

(* -------------------------------------------------- *)

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
  | TEsizeof of typ (* maybe adding primitives for potential future extensions *)

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
  env: dmap
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
