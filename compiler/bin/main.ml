(** Pretty printer *)

open Format
open Lexing
open Parser

(* print one token *)
let print_token fmt = function
  | ASSIGN -> fprintf fmt "="
  | CST n  -> fprintf fmt "%d" n
  | PLUS   -> fprintf fmt "+"
  | TIMES  -> fprintf fmt "*"
;;
  

(* print all the tokens from a given string *)
let rec print_tokens e fmt = 
  let token = Lexer.token (Lexing.from_string e) in
    match token with 
    | EOF -> fprintf fmt "\n"
    | token -> ((print_token fmt token); (print_tokens e fmt));
;;

(* tests *)
let lb = from_channel stdin
let t = ref EOF                  (* le prochain lexème à examiner *)
let next () = t := token lb
let () = next ()

let e = "2 + 8*5";;
let () = printf "e = @[%a@]@." print_tokens e;;