(** Lexer for the petitC Compiler *)

{
    open Lexing
    open Parser
    (* open Format *)

    (** Exception for lexing errors *)
    exception Lexing_error of string * string

    (** Table mapping keywords to tokens *)
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

    (** Initiate the hash table and the getters *)
    let manage_kw =
        (* initiate the hash map *)
        let h = Hashtbl.create 32 in
        List.iter (fun (key, token) -> Hashtbl.add h key token) kwd_table;
        (* get the value corresponding to key *)
        fun key ->
            let key = String.lowercase_ascii key in 
            (* if not a keyword return an ident *)
            try Hashtbl.find h key with _ -> IDENT key

    (** Handle the error message
        val error_handler : string -> string -> unit *)
    let error_handler error_msg token =
        raise( Lexing_error (error_msg, token))


    (** Raise an error if an integer is not valid
        val check_interger : string -> unit *)
    let check_integer integer =
        try ignore(Int32.of_string integer) 
            with _ -> (error_handler "not a valid integer" integer)

}

(** White spaces *)
let space = [' ' '\t' '\r']

(** Letters *)
let alpha = ['a'-'z' 'A'-'Z']

(** Digits *)
let digit = [ '0'-'9']

(** Identifiers *)
let ident = (alpha | '_')(alpha | digit | '_')*

(** Chars *)
let character = [' '-'&'  '('-'['  ']'-'~'  '\n'  '\t'  '\r'] (* without ' and \ *)

(** Integers *)
let integer = '0' | (['1'-'9']digit*) | '\''(character)'\''

(** Preprocessing *)
let include = "#include"space+'<'(character # '>')*">\n"

(** Identifies tokens *)
rule token = parse
    (* manage comments *)
    | "//" [^ '\n']* { new_line lexbuf; token lexbuf }
    | "//" [^ '\n']* eof { EOF }
    | "/*" { comment lexbuf }
    
    (* manage white spaces *)
    | '\n'   { new_line lexbuf; token lexbuf }
    | space+ { token lexbuf }
    
    (* manages operations *)
    | "["    { LBRA }
    | "]"    { RBRA }

    | "!"    { NOT }
    | "++"   { INCR }
    | "--"   { DECR }
    | "&"    { AMP }

    | "*"    { TIMES }
    | "/"    { DIV }
    | "%"    { MOD }
    | "-"    { MINUS }
    | "+"    { PLUS }

    | "<"    { LESS_THAN }
    | "<="   { LESS_EQUAL }
    | ">"    { GREATER_THAN }
    | ">="   { GREATER_EQUAL }

    | "!="   { NOT_EQUAL }
    | "=="   { EQUAL }

    | "||"   { OR }
    | "&&"   { AND }

    | "="    { ASSIGN }

    (* manages syntax tokens *)
    | "{"    { BEG }
    | "}"    { END }
    | "("    { LPAR }
    | ")"    { RPAR }
    | ","    { COMMA }
    | ";"    { SEMI_COLON }

    (* manages includes *)
    | include { new_line lexbuf; INCLUDE }

    (* manages chars *)
    | "\'\\t\'"  { CST (Char.code '\t') }
    | "\'\\\'\'" { CST (Char.code '\'') }
    | "\'\\\\\'" { CST (Char.code '\\') }
    | "\'\\n\'"  { CST (Char.code '\n') }
    | '\''(character)'\'' as c { CST (Char.code (String.get c 1)) }
    | integer as s { begin check_integer s; CST (int_of_string s); end }

    | ident as id  { manage_kw id }
    | eof  { EOF }
    | _ as c { error_handler "illegal character" (String.make 1 c) }


(** Deals with comments *)
and comment = parse
    | "*/" {token lexbuf}
    | '\n' { new_line lexbuf; comment lexbuf }
    | _    {comment lexbuf}
    | eof  { error_handler "unfinished comment" "" }