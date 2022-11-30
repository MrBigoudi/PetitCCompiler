(* Lexer for petitC *)

{
    open Lexing
    open Parser

    (* exception for lexing errors *)
    exception Lexing_error of string

    (* note Prof : penser à appeler la fonction Lexing.new_line
        à chaque retour chariot (caractère '\n') *)

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

}

(* white spaces *)
let space = [' ' '\t' '\r']

(* letters *)
let alpha = ['a'-'A' 'z'-'Z']

(* digits *)
let digit = [ '0'-'9']

(* identifiers *)
let ident = (alpha | '_')(alpha | digit | '_')*

(* chars *)
let character = [' '-'&'  '('-'['  ']'-'~'  '\\'  '\''  '\n'  '\t'  '\r'] (* without ' and \ *)

(* integers *)
let integer = '0' | (['1'-'9']digit*) | ('\''(character)'\'')

(* preprocessing *)
let include = "#include"space+'<'(character # '>')*">\n"

(* identifies tokens *)
rule token = parse
    (* manage comments *)
    | "//" [^ '\n']* { new_line lexbuf; token lexbuf }
    | "//" [^ '\n']* eof { EOF }
    | "/*"   { comment lexbuf }
    
    (* manage white spaces *)
    | '\n'   { new_line lexbuf; token lexbuf }
    | space+ { token lexbuf }
    
    (* manages operations *)
    | "="    { ASSIGN }
    | "&"    { AMP }
    (* logical operations *)
    | "!"    { NOT }
    | "||"   { OR }
    | "&&"   { AND }
    | "=="   { EQUAL }
    | "!="   { NOT_EQUAL }
    | "<"    { LESS_THAN }
    | "<="   { LESS_EQUAL }
    | ">"    { GREATER_THAN }
    | ">="   { GREATER_EQUAL }
    (* arithmetical operations *)
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
    | "["    { LBRA }
    | "]"    { RBRA }
    | ","    { COMMA }
    | ";"    { SEMI_COLON }

    (* manages includes *)
    | include { new_line lexbuf; INCLUDE }
    | ident as id  { manage_kw id }
    | integer as s { CST (int_of_string s) }
    | eof          { EOF }


(* deals with comments *)
and comment = parse
    | "*/" {token lexbuf}
    | '\n' { new_line lexbuf; comment lexbuf }
    | _    {comment lexbuf}
    | eof  {failwith "Unfinished comment"}
