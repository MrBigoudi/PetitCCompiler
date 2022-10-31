# 3 "lexer.mll"
 
    open Lexing
    (* open Parser  *)

    (* exception for lexing errors *)
    exception Lexing_error of string

    (* note Prof : penser à appeler la fonction Lexing.new_line
        à chaque retour chariot (caractère '\n') *)

    type token = 
        | BOOL
        | BREAK
        | CONTINUE
        | ELSE
        | FALSE
        | FOR
        | IF
        | INT
        | NULL
        | RETURN
        | SIZEOF
        | TRUE
        | VOID
        | WHILE
        | ASSIGN
        | AMP
        | NOT
        | OR
        | AND
        | EQUAL
        | NOT_EQUAL
        | LESS_THAN
        | LESS_EQUAL
        | GREATER_THAN
        | GREATER_EQUAL
        | PLUS
        | MINUS
        | TIMES
        | DIV
        | MOD
        | INCR
        | DECR
        | BEG
        | END
        | LPAR
        | RPAR
        | LBRA
        | RBRA
        | COMMA
        | SEMI_COLON
        | CST of int
        | EOF
        | IDENT of string


    (* table mapping keywords to tokens *)
    let kwd_table =
    [
        "bool", BOOL;
        "break", BREAK;
        "continue", CONTINUE;
        "else", ELSE;
        "false", FALSE;
        "for", FOR;
        "if", IF;
        "int", INT;
        "NULL", NULL;
        "return", RETURN;
        "sizeof", SIZEOF;
        "true", TRUE;
        "void", VOID;
        "while", WHILE;
    ]

    (* initiate the hash table + getter *)
    let manage_kw =
        (* initiate the hash map *)
        let h = Hashtbl.create 32 in
        List.iter (fun (key, token) -> Hashtbl.add h key token) kwd_table;
        (* get the value corresponding to key *)
        fun key ->
            let key = String.lowercase_ascii key in 
            (* if not a keyword return an ident *)
            try Hashtbl.find h key with _ -> IDENT key


# 90 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\224\255\117\000\083\000\225\255\196\000\227\255\228\255\
    \015\001\090\001\231\255\232\255\233\255\234\255\237\255\239\255\
    \001\000\015\000\002\000\003\000\004\000\068\000\103\000\081\000\
    \002\000\254\255\101\000\255\255\002\000\247\255\248\255\246\255\
    \249\255\244\255\242\255\236\255\235\255\105\000\213\000\253\255\
    \254\255\098\000\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\030\000\255\255\029\000\255\255\255\255\
    \026\000\025\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \015\000\014\000\012\000\010\000\255\255\005\000\004\000\003\000\
    \002\000\255\255\017\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\001\000\255\255";
  Lexing.lex_default =
   "\255\255\000\000\255\255\255\255\000\000\255\255\000\000\000\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\000\000\028\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\255\255\040\000\000\000\
    \000\000\255\255\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\024\000\025\000\024\000\027\000\024\000\000\000\024\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \024\000\021\000\024\000\000\000\000\000\014\000\022\000\002\000\
    \011\000\010\000\015\000\017\000\007\000\016\000\036\000\026\000\
    \004\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\035\000\006\000\019\000\023\000\018\000\034\000\
    \033\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\009\000\005\000\008\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\013\000\020\000\012\000\037\000\037\000\
    \032\000\031\000\037\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\030\000\029\000\027\000\
    \004\000\042\000\000\000\000\000\028\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
    \037\000\037\000\037\000\037\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\000\000\041\000\
    \001\000\000\000\027\000\000\000\000\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\039\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\024\000\028\000\000\000\255\255\024\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\024\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\017\000\000\000\000\000\000\000\000\000\018\000\
    \019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\002\000\002\000\
    \020\000\021\000\002\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\022\000\023\000\026\000\
    \037\000\041\000\255\255\255\255\026\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
    \002\000\002\000\002\000\002\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\255\255\038\000\
    \000\000\255\255\028\000\255\255\255\255\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
    \009\000\009\000\009\000\009\000\009\000\038\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 117 "lexer.mll"
             ( comment lexbuf )
# 288 "lexer.ml"

  | 1 ->
# 120 "lexer.mll"
             ( new_line lexbuf; token lexbuf )
# 293 "lexer.ml"

  | 2 ->
# 121 "lexer.mll"
             ( token lexbuf )
# 298 "lexer.ml"

  | 3 ->
# 124 "lexer.mll"
             ( ASSIGN )
# 303 "lexer.ml"

  | 4 ->
# 125 "lexer.mll"
             ( AMP )
# 308 "lexer.ml"

  | 5 ->
# 127 "lexer.mll"
             ( NOT )
# 313 "lexer.ml"

  | 6 ->
# 128 "lexer.mll"
             ( OR )
# 318 "lexer.ml"

  | 7 ->
# 129 "lexer.mll"
             ( AND )
# 323 "lexer.ml"

  | 8 ->
# 130 "lexer.mll"
             ( EQUAL )
# 328 "lexer.ml"

  | 9 ->
# 131 "lexer.mll"
             ( NOT_EQUAL )
# 333 "lexer.ml"

  | 10 ->
# 132 "lexer.mll"
             ( LESS_THAN )
# 338 "lexer.ml"

  | 11 ->
# 133 "lexer.mll"
             ( LESS_EQUAL )
# 343 "lexer.ml"

  | 12 ->
# 134 "lexer.mll"
             ( GREATER_THAN )
# 348 "lexer.ml"

  | 13 ->
# 135 "lexer.mll"
             ( GREATER_EQUAL )
# 353 "lexer.ml"

  | 14 ->
# 137 "lexer.mll"
             ( PLUS )
# 358 "lexer.ml"

  | 15 ->
# 138 "lexer.mll"
             ( MINUS )
# 363 "lexer.ml"

  | 16 ->
# 139 "lexer.mll"
             ( TIMES )
# 368 "lexer.ml"

  | 17 ->
# 140 "lexer.mll"
             ( DIV )
# 373 "lexer.ml"

  | 18 ->
# 141 "lexer.mll"
             ( MOD )
# 378 "lexer.ml"

  | 19 ->
# 142 "lexer.mll"
             ( INCR )
# 383 "lexer.ml"

  | 20 ->
# 143 "lexer.mll"
             ( DECR )
# 388 "lexer.ml"

  | 21 ->
# 145 "lexer.mll"
             ( BEG )
# 393 "lexer.ml"

  | 22 ->
# 146 "lexer.mll"
             ( END )
# 398 "lexer.ml"

  | 23 ->
# 147 "lexer.mll"
             ( LPAR )
# 403 "lexer.ml"

  | 24 ->
# 148 "lexer.mll"
             ( RPAR )
# 408 "lexer.ml"

  | 25 ->
# 149 "lexer.mll"
             ( LBRA )
# 413 "lexer.ml"

  | 26 ->
# 150 "lexer.mll"
             ( RBRA )
# 418 "lexer.ml"

  | 27 ->
# 151 "lexer.mll"
             ( COMMA )
# 423 "lexer.ml"

  | 28 ->
# 152 "lexer.mll"
             ( SEMI_COLON )
# 428 "lexer.ml"

  | 29 ->
let
# 156 "lexer.mll"
               id
# 434 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 156 "lexer.mll"
                   ( manage_kw id )
# 438 "lexer.ml"

  | 30 ->
let
# 157 "lexer.mll"
                 s
# 444 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 157 "lexer.mll"
                   ( CST (int_of_string s) )
# 448 "lexer.ml"

  | 31 ->
# 158 "lexer.mll"
                   ( EOF )
# 453 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 38
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 163 "lexer.mll"
           (token lexbuf)
# 465 "lexer.ml"

  | 1 ->
# 164 "lexer.mll"
           (comment lexbuf)
# 470 "lexer.ml"

  | 2 ->
# 165 "lexer.mll"
           (failwith "Unfinished comment")
# 475 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;

