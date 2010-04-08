(* Terms *)

type t = 
  | Var of string
  | Lambda of string * t
  | App of t * t
  | Num of int
  | Eq of t * t
  | Plus of t * t
  | True
  | False
  | If of t * t * t
  
let rec string_of_term = function
  | Var x -> x
  | Lambda (x,t) -> 
    let s = string_of_term t in
    "(\\" ^ x ^ "." ^ s ^ ")"
  | App (t1,t2) -> 
    let s1 = string_of_term t1 in
    let s2 = string_of_term t2 in
    "(" ^ s1 ^ " " ^ s2 ^ ")"
  | Num n -> string_of_int n
  | Eq (t1,t2) -> 
    let s1 = string_of_term t1 in
    let s2 = string_of_term t2 in
    "(" ^ s1 ^ "=" ^ s2 ^ ")"
  | Plus (t1,t2) -> 
    let s1 = string_of_term t1 in
    let s2 = string_of_term t2 in
    "(" ^ s1 ^ "+" ^ s2 ^ ")"
  | True -> "true"
  | False -> "false"
  | If (t1,t2,t3) ->
    let s1 = string_of_term t1 in
    let s2 = string_of_term t2 in
    let s3 = string_of_term t3 in
    "(if " ^ s1 ^ " then " ^ s2 ^ " else " ^ s3 ^ ")"
