open Printf
open Term
open Interp

(* Application should bind left. *)
let precedence = "(\\x.\\y.x) 1 2";;

(* This will return 1 under static scoping, but 2 under dynamic. *)
let static_vs_dynamic_scope = "\\x.(\\f.((\\x.f 3) 2) (\\y.x)) 1";;

(* Fixed point combinator. *)
let fix = "\\f.((\\x.f (x x)) (\\x.f (x x)))";;

(* Fixed point combinator for call-by-value. *)
let fix2 = "\\f.((\\x.f (\\z.(x x) z)) (\\x.f (\\z.(x x) z)))";;

let ifthenelse = "(\\x.if 0 = x then 1 else 2) 1";;

(* Main *)
  
let run_test (msg,expr) =
  printf "Test: %s\n" msg;
  printf "Input: %s\n" expr;
  let parser = Parser.main Lexer.tokenizer in
  match parser (Lexing.from_string expr) with
    | None -> failwith "Parse error"
    | Some t -> 
      let ts = string_of_term t in
      printf "Parsed: %s\n" ts;
      let t' = interp t in
      let ts' = string_of_term t' in
      printf "Eval: %s\n" ts'

let main () = 
  let tests = [
    ("Precedence",precedence);
    ("Static vs. Dynamic Scope",static_vs_dynamic_scope);
    ("Fix-point",fix);
    ("Fix-point 2",fix2);
    ("If-then-else",ifthenelse);
  ] in
  List.iter run_test tests
;;

main ();
  
