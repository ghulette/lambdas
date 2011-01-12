%{
open Term
%}

%token <string> ID
%token <int> NUM
%token BEGIN
%token END
%token TRUE
%token FALSE
%token LAMBDA, DOT
%token LET, IN
%token PLUS, MINUS, MULT
%token EQUAL
%token IF
%token THEN
%token ELSE
%token EOF
%start main
%type <(Term.t option)> main

%left LET IN
%left LAMBDA DOT
%left IF THEN ELSE
%nonassoc EQUAL
%left PLUS MINUS
%left MULT
%left APP
%left BEGIN END
%nonassoc ID NUM TRUE FALSE

%%

main:
  | expr EOF { Some $1 }
  |          { None }
;

expr:
  | expr %prec APP expr { App ($1,$2) }
  | BEGIN expr END { $2 }
  | ID { Var $1 }
  | LAMBDA ID DOT expr { Lambda ($2,$4) }
  | LET ID EQUAL expr IN expr { App (Lambda($2,$6),$4) }
  | NUM { Num $1 }
  | TRUE { True }
  | FALSE { False }
  | IF expr THEN expr ELSE expr { If ($2,$4,$6) }
  | expr PLUS expr { Binop (Plus,$1,$3) }
  | expr MINUS expr { Binop (Minus,$1,$3) }
  | expr MULT expr { Binop (Mult,$1,$3) }
  | expr EQUAL expr { Binop (Eq,$1,$3) }
;
