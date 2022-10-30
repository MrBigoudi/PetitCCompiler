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
%right AMP INCR DECR ustar uplus uminus 
%nonassoc IF
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> prog

%%

prog:
  defs = def*
  main = stmt*
  EOF
    { { defs = defs;
        main = Sblock main; } }
;

def:
| DEF f = IDENT
  LPAREN formals = separated_list(COMMA, IDENT) RPAREN body = stmt
    { { name = f;
        formals = formals;
        body = body } }
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
| c = CST
| id = IDENT
| 
| e1 = expr o = op e2 = expr     { Ebinop (o, e1, e2) }
| MINUS e = expr %prec uminus    { neg e }
| LPAREN e = expr RPAREN         { e }
;

%inline op:
| PLUS  { Add }
| MINUS { Sub }
| TIMES { Mul }
| DIV   { Div }
;

