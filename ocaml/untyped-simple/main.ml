open Printf
open Term
open Interp

(* Main *)

let main () =
  let parser = Parser.main Lexer.tokenizer in
  match parser (Lexing.from_channel stdin) with
    | None -> failwith "Error: parse error"
    | Some t -> 
      let ts = string_of_term t in
      printf "%s => " ts;
      try 
        let t' = interp t in
        let ts' = string_of_term t' in
        printf "%s\n" ts'
      with Runtime_error ->
        printf "Error\n"
;;

main ();
