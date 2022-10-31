(** Pretty printer *)

open Format
open Syntax.Ast
open Syntax.PrettyPrinter

let e = Ebinop((Badd), (Econst 2), (Ebinop((Bmul), (Econst 8), (Econst 5))));;

let test e res =
  let str = (asprintf "e = @[%a@]@." print e) in
    begin
      print_string "****** TEST ******\n";
      print_string "Expected result :\n";
      print_string res;
      print_string "Actual result :\n";
      print_string str;
      (assert (str=res))
    end


let () = 
  (test e "e = (2 + (8 * 5))\n")