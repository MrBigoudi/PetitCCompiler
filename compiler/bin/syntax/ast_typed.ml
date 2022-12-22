(** Abstract Syntax Tree for the petitC Compiler *)

open Ast

module Smap = Map.Make(String)
type env = typ Smap.t
type offset_env = (int*int) Smap.t

exception Environnement_error of string

(** add offset to idents *)
type tident = {ident: ident; offset: int; depth: int}

(** double maps : using old and new environnements for blocks *)
type dmap = { old_env : env ; new_env : env}
type dmap_offset = {old_env : offset_env ; new_env : offset_env}

(** val search_dmap : ident -> dmap_offset -> int * int *)
let search_dmap id doub_map = 
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  match Smap.find_opt id oenv, Smap.find_opt id nenv with
    | None, None -> raise (Environnement_error("no id found"))
    | None, Some(t) -> t
    | Some(t), None -> t
    | _, Some(t2) -> t2

(** val in_new_env_dmap : ident -> dmap_offset -> bool *)
let in_new_env_dmap id doub_map =
  let nenv = doub_map.new_env in 
  Smap.mem id nenv

(** val add_new_dmap : ident -> int -> int -> dmap_offset -> dmap_offset *)
let add_new_dmap id offset depth doub_map =
  let nenv = doub_map.new_env in
  { old_env = doub_map.old_env ; new_env = (Smap.add id (offset,depth) nenv) }

(** val add_old_dmap : ident -> int -> int -> dmap_offset -> dmap_offset *)
let add_old_dmap id offset depth doub_map =
  let oldv = doub_map.old_env in
  { old_env = (Smap.add id (offset, depth) oldv) ; new_env = doub_map.new_env }

(** val union_dmap : dmap_offset -> env *)
let union_dmap doub_map =
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  let union_fun _ _ new_val = Some(new_val) in
    Smap.union union_fun oenv nenv


(** val new_block_dmap : dmap_offset -> dmap_offset *)
let new_block_dmap doub_map =
  let oenv = union_dmap doub_map in
  let nenv = Smap.empty in 
    { old_env = oenv; new_env = nenv }

(** val search_dmap : ident -> dmap -> typ *)
let search_dmap_typ id (doub_map: dmap) = 
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  match Smap.find_opt id oenv, Smap.find_opt id nenv with
    | None, None -> raise (Environnement_error("no id found"))
    | None, Some(t) -> t
    | Some(t), None -> t
    | _, Some(t2) -> t2

(** val in_new_env_dmap : ident -> dmap -> bool *)
let in_new_env_dmap_typ id (doub_map: dmap) =
  let nenv = doub_map.new_env in 
  Smap.mem id nenv

(** val add_new_dmap : ident -> typ -> dmap -> dmap  *)
let add_new_dmap_typ id ty (doub_map: dmap) =
  let nenv = doub_map.new_env in
  if Smap.mem id nenv 
    then raise (Environnement_error("id already existing")) 
    else ({ old_env = doub_map.old_env ; new_env = (Smap.add id ty nenv) }:dmap)

(** val add_old_dmap : ident -> typ -> dmap -> dmap *)
let add_old_dmap_typ id ty (doub_map: dmap) =
  let oldv = doub_map.old_env in
  if Smap.mem id oldv
    then raise (Environnement_error("id already existing")) 
    else ({ old_env = (Smap.add id ty oldv) ; new_env = doub_map.new_env }:dmap)

(** val union_dmap : dmap -> env *)
let union_dmap_typ (doub_map: dmap) =
  let oenv = doub_map.old_env in
  let nenv = doub_map.new_env in
  let union_fun _ _ new_val = Some(new_val) in
    Smap.union union_fun oenv nenv


(** val new_block_dmap : dmap -> dmap *)
let new_block_dmap_typ (doub_map: dmap) =
  let oenv = union_dmap_typ doub_map in
  let nenv = Smap.empty in 
    ({ old_env = oenv; new_env = nenv }:dmap)


(** val print_dmap_typ : dmap -> unit *)
let print_dmap_typ (doub_map: dmap) =
  let f key typ =
    print_string ("key: "^key^", typ: "^typ_to_string typ^"\n")
  in
  begin
    print_string "\nold env:\n";
    Smap.iter f doub_map.old_env;
    print_string "\nnew env:\n";
    Smap.iter f doub_map.new_env;
    print_string "\n"
  end

(** val print_dmap : dmap_offset -> unit *)
let print_dmap (doub_map: dmap_offset) =
  let f key value =
    let offset, depth = value in
      print_string ("key: "^key^", offset: "^(Int.to_string offset)^", depth: "^(Int.to_string depth)^"\n")
  in
  begin
    print_string "\nold env:\n";
    Smap.iter f doub_map.old_env;
    print_string "\nnew env:\n";
    Smap.iter f doub_map.new_env;
    print_string "\n"
  end



(** Expressions *)
type texpression = {
  tdesc: tdesc;
  typ: typ;
}

(** Description of texpressions *)
and tdesc =
  | TEconst of const 
  | TEvar of tident
  | TEunop of unop * texpression
  | TEbinop of binop * texpression * texpression
  | TEassign of texpression * texpression
  | TEcall of tident * texpression list
  | TEsizeof of typ (* maybe adding primitives *)

(** Variable declarations *)
type tdvar = TDvar of typ * tident * texpression option

(** Instruction declarations *)
and tdinstr = 
  | TDinstrFct of tdfct
  | TDinstrVar of tdvar
  | TDinstr of tinstr

(** Function parameters *)
and tparam = TParam of typ * tident

(** Function declarations *)
and tdfct = TDfct of typ * tident * tparam list * tblock

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
