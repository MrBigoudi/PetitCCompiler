(* Analyseur syntaxique pour petitC *)


%{
  open Ast

%}

%token INCLUDE
/* types */
%token BOOL INT VOID 
/* values */
%token TRUE FALSE NULL
%token <int> CST
%token <string> IDENT
/* manages syntax */
%token BEG END LPAR RPAR LBRA RBRA COMMA SEMI_COLON
/* operators (times : ptr assignation ?) */
%token ASSIGN AMP PLUS MINUS TIMES DIV MOD INCR DECRR
%token OR AND EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%token SIZEOF
/* intstruction */
%token IF ELSE WHILE FOR RETURN BREAK CONTINUE
%token EOF

/* Les priorités et associativités des tokens */

%right ASSIGN
%left OR
%left AND
%left EQUAL NOT_EQUAL
%left LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
%left MINUS PLUS
%left TIMES DIV MOD
%right NOT AMP INCR DECR ustar uplus uminus 
%nonassoc IF
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> prog

%%

prog:
  incl = includ*
  main = decl_fct*
  EOF {}
;

decl_fct:
  ty = typ 
  id = IDENT
  LPAR p = param* RPAR
  bloc {}
;

typ: 
  /* TODO */ {}
; 

param:
  ty = typ
  id = IDENT 
  {}
;



stmt:
| id = IDENT LPAREN actuals = separated_list(COMMA, expr) RPAREN
    { Scall (id, actuals) }
| IF e = expr s = stmt
    { Sif (e, s, Sblock []) }
| IF e = expr s1 = stmt ELSE s2 = stmt
    { Sif (e, s1, s2) }
| REPEAT e = expr b = stmt
    { Srepeat (e, b) }
| BEGIN is = stmt* END
    { Sblock is }
;

expr:
| c = CST {}
| id = IDENT {}
| TIMES e = expr %prec ustar  {}
| e1 = expr LBRA e2 = expr RBRA {}
| e1 = expr ASSIGN e2 = expr  {}
| id = IDENT LPAR el = separated_list(COMMA, expr) RPAR {}
/* might be shorter with using a group */ 
| INCR e = expr {}
| DECR e = expr {}
| e = expr INCR {}
| e  = expr DECR {}
| AMP e = expr  {}
| NOT e = expr  {}
| PLUS e = expr %prec uplus {}
| MINUS e = expr %prec uminus {}
| e1 = expr o = bin_op e2 = expr {}
| SIZEOF LPAREN e = expr RPAREN {}
| LPAREN e = expr RPAREN {}
;

%inline bin_op:
| EQUAL {}
| NOT_EQUAL {}
| LESS_THAN {}
| LESS_EQUAL {}
| GREATER_THAN {}
| GREATER_EQUAL  {}
| PLUS {}
| MINUS {}
| TIMES {}
| DIV {}
| MOD {}
| AND {}
| OR {}
;
