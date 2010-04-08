open Printf
open Term
open Interp

(* Application should bind left. *)
let precedence = "(\\x.\\y.x) 1 2";;

(* This will return 1 under static scoping, but 2 under dynamic. *)
let static_vs_dynamic_scope = "\\x.(\\f.((\\x.f 3) 2) (\\y.x)) 1";;

(* Main *)
  
let run_test expr =
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
    precedence;
    static_vs_dynamic_scope
  ] in
  List.iter run_test tests
;;

main ();
  
