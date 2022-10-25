(* Analyseur lexical pour petitC *)

{
    open Lexing
    open Parser

    (* exception à lever pour signaler une erreur lexicale *)
    exception Lexing_error of string

    (* note : penser à appeler la fonction Lexing.new_line
        à chaque retour chariot (caractère '\n') *)

    (* table mapping keywords to tokens *)
    let kwd_table =
    [
        "bool"     = BOOL
        "break"    = BREAK
        "continue" = CONTINUE
        "else"     = ELSE
        "false"    = FALSE
        "for"      = FOR
        "if"       = IF
        "int"      = INT
        "NULL"     = NULL
        "return"   = RETURN
        "sizeof"   = SIZEOF
        "true"     = TRUE
        "void"     = VOID
        "while"    = WHILE
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

}

(* white spaces *)
let space = [' ' '\t' '\r']

(* letters *)
let alpha = ['a'-'A' 'z'-'Z']

(* digits *)
let digit = [ '0'-'9']

(* identifiers *)
let ident = ([alpha '_'])([alpha digit '_'])*


(* identifies tokens *)
rule token = parse
    (* manage comments *)
    | "//" [^ '\n']* '\n'
    | "//" [^ '\n']* eof
    | "/*"   { comment lexbuf }
    
    (* manage white spaces *)
    | '\n'   { new_line lexbuf; token lexbuf }
    | space+ { token lexbuf }
    
    (* manages operations *)
    | "="    { ASSIGN }
    | "||"   { OR }
    | "&&"   { AND }
    | "=="   { EQUAL }
    | "=!"   { NOT_EQUAL }
    | "<"    { LESS_THAN }
    | "<="   { LESS_EQUAL }
    | ">"    { GREATER_THAN }
    | ">="   { GREATER_EQUAL }
    | "+"    { PLUS }
    | "-"    { MINUS }
    | "*"    { TIMES }
    | "/"    { DIV }
    | "%"    { MOD }
    | "++"   { INCR }
    | "--"   { DECR }

    (* manages syntax tokens *)
    | "{"    { BEG }
    | "}"    { END }
    | "("    { LPAR }
    | ")"    { RPAR }
    | ","    { COMMA }
    | ";"    { SEMI_COLON }
    
    | ident as id { manage_kw id }
    | integer as s { CST (int_of_string s) }
    | eof    { EOF }


(* deals with comments *)
and comments = parse
    | "*/" {token lexbuf}
    | _    {comment lexbuf}
    | eof  {failwith "Unfinished comment"}
