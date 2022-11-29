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
%token ASSIGN AMP PLUS MINUS TIMES DIV MOD INCR DECR
%token OR AND NOT EQUAL NOT_EQUAL LESS_THAN LESS_EQUAL GREATER_THAN GREATER_EQUAL
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
%nonassoc LBRA
%nonassoc endif
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start file

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.cfile> file

%%

file:
includ*
fctl = decl_fct*
EOF { CFinclude fctl }
;

decl_fct:
  ty = typ 
  id = IDENT
  LPAR pl = param* RPAR
  bl = block { DFct(ty, id, pl, bl) }
;

typ: 
|VOID { Tvoid }
|INT { Tint }
|BOOL { Tbool }
|ty = typ TIMES { Tptr(ty) } 
; 

param:
  ty = typ id = IDENT { PIdnt(ty, id) }
;

includ:
  INCLUDE {}
;

expr:
| d = desc { { desc = d ; loc = $startpos, $endpos } }

desc:
| c = CST { Econst (Int c) }
| c = TRUE { Econst (True) }
| c = FALSE { Econst (False) }
| c = NULL { Econst (Null) }
| id = IDENT { Evar id }
| TIMES e = expr %prec ustar  { Eunop(Ustar(e)) }
| e1 = expr LBRA e2 = expr RBRA { Eunop(Ustar(Binop(Badd, e1, e2))) }
| e1 = expr ASSIGN e2 = expr  { Eassign(e1, e2) }
| id = IDENT LPAR el = separated_list(COMMA, expr) RPAR { Ecall(id, el) }
/* might be shorter with using a group */ 
| INCR e = expr { Eunop(Uincr_l, e) }
| DECR e = expr { Eunop(Udecr_l, e) }
| e = expr INCR { Eunop(Uincr_r, e) }
| e = expr DECR { Eunop(Udecr_r, e) }
| AMP e = expr  { Eunop(Uamp, e) }
| NOT e = expr  { Eunop(Unot, e) }
| PLUS e = expr %prec uplus { Ebinop(Badd, (Econst 0), e) }
| MINUS e = expr %prec uminus { Ebinop(Bsub, (Econst 0), e) }
| e1 = expr op = bin_op e2 = expr { Ebinop(op, e1, e2) }
| SIZEOF LPAR e = expr RPAR { Esizeof(e) }
| LPAR e = expr RPAR { e }
;

instr:
| SEMI_COLON { Iempt }
| e = expr SEMI_COLON { Iexpr e }
| IF LPAR e = expr RPAR ist = instr %prec endif { Iif(e, ist, Iempt) }
| IF LPAR e = expr RPAR ist1 = instr ELSE ist2 = instr { Iif(e, ist1, ist2) }
| WHILE LPAR e = expr RPAR ist = instr { Iwhile(e, ist) }
| FOR LPAR dv = decl_var? SEMI_COLON e1 = expr? SEMI_COLON el = separated_list(COMMA, expr) RPAR ist = instr { Ifor(dv, e1, el, ist) }
| bl = block { Iblock bl }
| RETURN e = expr? SEMI_COLON { Iret e }
| BREAK SEMI_COLON { Ibrk }
| CONTINUE SEMI_COLON { Icontinue }
;

block:
| BEG di = decl_instr* END { Blck di }
;

decl_instr:
| dv = decl_var SEMI_COLON { DIvar dv }
| ist = instr { DInstr ist }
;

ass_var:
EQUAL e = expr { e }

decl_var:
ty = typ id = IDENT e = ass_var? { DVar(ty, id, e) }

%inline bin_op:
| EQUAL { Beq }
| NOT_EQUAL { Bneq }
| LESS_THAN { Blt }
| LESS_EQUAL { Ble }
| GREATER_THAN { Bgt }
| GREATER_EQUAL  { Bge }
| PLUS { Badd }
| MINUS { Bsub }
| TIMES { Bmul }
| DIV { Bdiv }
| MOD { Bmod }
| AND { Band }
| OR { Bor }
;
