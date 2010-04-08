open Env
open Term


(* Interpreter *)

exception Runtime_error
  
let rec eval e = function
  | Var x -> eval e (lookup e x)
  | Lambda (x,t) -> Closure (x,t,e)
  | Closure (x,t,e) -> Closure (x,t,e)
  | App (t1,t2) -> 
    let v1 = eval e t1 in
    let v2 = eval e t2 in
    begin match v1 with
      | Closure (x,t',e') ->
        let e'' = bind e' x v2 in
        eval e'' t'
      | otherwise -> raise Runtime_error
    end
  | Num n -> Num n
  | Eq (t1,t2) ->
    let v1 = eval e t1 in
    let v2 = eval e t2 in
    begin match (v1,v2) with
      | Num n1,Num n2 -> if n1 = n2 then True else False
      | otherwise -> raise Runtime_error
    end
  | Plus (t1,t2) ->
    let v1 = eval e t1 in
    let v2 = eval e t2 in
    begin match (v1,v2) with
      | Num n1,Num n2 -> Num (n1+n2)
      | otherwise -> raise Runtime_error
    end
  | True -> True
  | False -> False
  | If (t1,t2,t3) ->
    let v1 = eval e t1 in
    begin match v1 with
      | True -> eval e t2
      | False -> eval e t3
      | otherwise -> raise Runtime_error
    end
    
let interp t = eval Env.empty t
