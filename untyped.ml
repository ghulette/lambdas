(* Environment *)

exception Not_found

let empty = []

let bind e x v = (x,v) :: e

let rec lookup e x = match e with
  | [] -> raise Not_found
  | (x',v) :: e' -> if x = x' then v else lookup e' x


(* Terms *)

exception Runtime_error

type term = 
  | Num of int
  | Var of string
  | Lambda of string * term
  | App of term * term
  
let rec string_of_term = function
  | Num n -> string_of_int n
  | Var x -> x
  | Lambda (x,t) -> "(\\" ^ x ^ "." ^ (string_of_term t) ^ ")"
  | App (t1,t2) -> "(" ^ (string_of_term t1) ^ " " ^ (string_of_term t2) ^ ")"

let rec eval e = function
  | Num n -> Num n
  | Var x -> eval e (lookup e x)
  | Lambda (x,t) -> Lambda (x,t)
  | App (t1,t2) -> 
    let v1 = eval e t1 in
    let v2 = eval e t2 in
    match v1 with
      | Lambda (x,t2') -> 
        let e' = bind e x v2 in
        eval e' t2'
      | otherwise -> raise Runtime_error

let test1 () = 
  let t = App(Lambda ("y",Var "y"),App(Lambda ("x",Var "x"),Num 5)) in
  let v = eval empty t in
  print_string (string_of_term v)

let test2 () = 
  let t1 = Lambda ("x",App(Var "x",Var "x")) in
  let t2 = App (t1,t1) in
  let v = eval empty t2 in
  print_string (string_of_term v)

;;
test1 ();
test2 ();
