%{
open Term
%}

%token <string> ID
%token <int> NUM
%token BEGIN
%token END
%token LAMBDA
%token DOT
%token PLUS
%token EQUAL
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token EOF
%start main
%type <(Term.t option)> main

%%

main:
  | expr EOF { Some $1 }
  |          { None }
;

expr:
  | BEGIN expr END { $2 }
  | ID { Var $1 }
  | LAMBDA ID DOT expr { Lambda ($2,$4) }
  | expr expr { App ($1,$2) }
  | NUM { Num $1 }
  | TRUE { True }
  | FALSE { False }
  
;
