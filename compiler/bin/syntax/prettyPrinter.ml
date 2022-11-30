(** Pretty printer *)

open Format
open Ast

(* puis plus joliment, en utilisant les boîtes de Format *)
let rec print fmt e = match e.desc with
  | Econst const  -> print_const const fmt
  | Eunop (op, e) -> print_unop op fmt e
  | Ebinop (op,e1,e2)   -> print_binop op fmt e1 e2
  | _ -> failwith "Oops not now"

and print_const const fmt = match const with
  | Int n -> fprintf fmt "%d" n
  | True  -> fprintf fmt "true"
  | False -> fprintf fmt "false"
  | Null  -> fprintf fmt "NULL"

and print_binop op fmt e1 e2 = match op with
  | Arith(op) -> arith op fmt e1 e2
  | Logic(op) -> logic op fmt e1 e2
  | AndOr(op) -> andor op fmt e1 e2

and arith op fmt e1 e2 = match op with
  | Badd -> fprintf fmt "(@[%a +@ %a@])" print e1 print e2
  | Bsub -> fprintf fmt "(@[%a -@ %a@])" print e1 print e2
  | Bmul -> fprintf fmt "(@[%a *@ %a@])" print e1 print e2
  | Bdiv -> fprintf fmt "(@[%a /@ %a@])" print e1 print e2
  | Bmod -> fprintf fmt "(@[%a %%@ %a@])" print e1 print e2

and logic op fmt e1 e2 = match op with
  | _ -> failwith "Oops not now"

and andor op fmt e1 e2 = match op with
  | _ -> failwith "Oops not now"

and print_unop op fmt e = match op with
  | Unot -> fprintf fmt "(@[!%a@])" print e 
  | Ustar -> fprintf fmt "(@[*%a@])" print e
  | _ -> failwith "Oops not now"
