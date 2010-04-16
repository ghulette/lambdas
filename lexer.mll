{
open Parser

let line_count = ref 1
exception Illegal_char of char
}

let id = ['a'-'z' 'A'-'Z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let nat = ['0'-'9']*

rule tokenizer = parse
  | "--"       { comment lexbuf }
  | '('        { BEGIN }
  | ')'        { END }
  | '\\'       { LAMBDA }
  | '.'        { DOT }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { MULT }
  | '='        { EQUAL }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "let"      { LET }
  | "in"       { IN }
  | id as word { ID word }
  | nat as num { NUM (int_of_string num) }
  | [' ' '\t'] { tokenizer lexbuf }
  | ['\n']     { incr line_count; tokenizer lexbuf }
  | _ as c     { raise (Illegal_char c) }
  | eof        { EOF }
and comment = parse
  | '\n'       { tokenizer lexbuf }
  | eof        { EOF }
  | _          { comment lexbuf}
