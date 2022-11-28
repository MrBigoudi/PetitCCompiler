(** Pretty printer *)

open Format
open Ast

(* puis plus joliment, en utilisant les boîtes de Format *)
let rec print fmt = function
  | Econst const  -> print_const const fmt
  | Eunop (op, e) -> print_unop op fmt e
  | Ebinop (op,e1,e2)   -> print_binop op fmt e1 e2

and print_const const fmt = match const with
  | Int n -> fprintf fmt "%d" n
  | True  -> fprintf fmt "true"
  | False -> fprintf fmt "false"
  | Null  -> fprintf fmt "NULL"

and print_binop op fmt e1 e2 = match op with
  | Badd -> fprintf fmt "(@[%a +@ %a@])" print e1 print e2
  | Bsub -> fprintf fmt "(@[%a -@ %a@])" print e1 print e2
  | Bmul -> fprintf fmt "(@[%a *@ %a@])" print e1 print e2
  | Bdiv -> fprintf fmt "(@[%a /@ %a@])" print e1 print e2
  | Bmod -> fprintf fmt "(@[%a %%@ %a@])" print e1 print e2
  (*| Bmul -> fprintf fmt "(@[%a *@ %a@])" print e1 print e2*)

and print_unop op fmt e = match op with
  | Unot -> fprintf fmt "(@[!%a@])" print e 
  | Ustar -> fprintf fmt "(@[*%a@])" print e

let a = Econst(Int(1))
let b = Econst(True)
let c = Econst(Null)
let d = Eunop(Unot, a)
let i = Eunop(Ustar, c)
let e = Ebinop(Badd, b, a)
let f = Ebinop(Bmul, b, a)
let g = Ebinop(Bmod, b, a)
let h = Ebinop(Bsub, c, a)

let () = print std_formatter i
