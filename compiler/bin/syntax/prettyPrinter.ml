(** Pretty printer *)

open Format
open Ast

(* puis plus joliment, en utilisant les boîtes de Format *)
let rec print fmt = function
  | Econst n      -> fprintf fmt "%d" n
  | Ebinop (op,e1,e2)   -> print_binop op fmt e1 e2
  | _ -> fprintf fmt ""

and print_binop op fmt e1 e2 = match op with
  | Badd -> fprintf fmt "(@[%a +@ %a@])" print e1 print e2
  | Bmul -> fprintf fmt "(@[%a *@ %a@])" print e1 print e2
  | _ -> fprintf fmt ""
;;