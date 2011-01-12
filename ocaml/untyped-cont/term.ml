(* Terms *)

type binop =
  | Plus
  | Minus
  | Mult
  | Eq

type t = 
  | Var of string
  | Lambda of string * t
  | Closure of string * t * (t Env.env)
  | App of t * t
  | Num of int
  | Binop of binop * t * t
  | True
  | False
  | If of t * t * t

let string_of_binop = function
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Eq -> "="
  
let rec string_of_term = function
  | Var x -> x
  | Lambda (x,t) -> 
    let s = string_of_term t in
    "(\\" ^ x ^ "." ^ s ^ ")"
  | Closure (x,t,e) -> string_of_term (Lambda (x,t))
  | App (t1,t2) -> 
    let s1 = string_of_term t1 in
    let s2 = string_of_term t2 in
    "(" ^ s1 ^ " " ^ s2 ^ ")"
  | Num n -> string_of_int n
  | Binop (op,t1,t2) -> 
    let sop = string_of_binop op in
    let s1 = string_of_term t1 in
    let s2 = string_of_term t2 in
    "(" ^ s1 ^ sop ^ s2 ^ ")"
  | True -> "true"
  | False -> "false"
  | If (t1,t2,t3) ->
    let s1 = string_of_term t1 in
    let s2 = string_of_term t2 in
    let s3 = string_of_term t3 in
    "(if " ^ s1 ^ " then " ^ s2 ^ " else " ^ s3 ^ ")"
