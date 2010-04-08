%{
open Term
%}

%token <string> ID
%token <int> NUM
%token BEGIN
%token END
%token TRUE
%token FALSE
%token LAMBDA
%token DOT
%token PLUS, MINUS
%token EQUAL
%token IF
%token THEN
%token ELSE
%token EOF
%start main
%type <(Term.t option)> main

%left ID NUM TRUE FALSE
%left BEGIN END
%left APP LAMBDA DOT
%left IF THEN ELSE
%left PLUS MINUS
%left EQUAL

%%

main:
  | expr EOF { Some $1 }
  |          { None }
;

expr:
  | expr expr %prec APP { App ($1,$2) }
  | BEGIN expr END { $2 }
  | ID { Var $1 }
  | LAMBDA ID DOT expr { Lambda ($2,$4) }
  | NUM { Num $1 }
  | TRUE { True }
  | FALSE { False }
  | IF expr THEN expr ELSE expr { If ($2,$4,$6) }
  | expr PLUS expr { Binop (Plus,$1,$3) }
  | expr MINUS expr { Binop (Minus,$1,$3) }
  | expr EQUAL expr { Binop (Eq,$1,$3) }
;
