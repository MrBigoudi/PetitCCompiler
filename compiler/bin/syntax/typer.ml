(* petitC Typer *)

open Ast

type typ = 
  | Tvoid
  | Tint
  | Tbool
  | Tptr of typ

module Smap = Map.Make(String)

type env = typ Smap.t

let rec type_expr env = function
  | Econst Cint _ -> Tint
  | Econst Cnull  -> Tptr(Tvoid)
  (* true and false does not exist !! *)
