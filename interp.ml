open Env
open Term


(* Interpreter *)

exception Runtime_error
  
let rec eval e = function
  | Var x -> eval e (lookup e x)
  | Lambda (x,t) -> (e,Lambda (x,t))
  | App (t1,t2) -> 
    let (e1,v1) = eval e t1 in
    let (_,v2) = eval e t2 in
    begin match v1 with
      | Lambda (x,p) ->
        let e' = bind e1 x v2 in
        eval e' p
      | otherwise -> raise Runtime_error
    end
  | Num n -> (e,Num n)
  | Eq (t1,t2) ->
    let (_,v1) = eval e t1 in
    let (_,v2) = eval e t2 in
    begin match (v1,v2) with
      | Num n1,Num n2 -> if n1 = n2 then (e,True) else (e,False)
      | otherwise -> raise Runtime_error
    end
  | Plus (t1,t2) ->
    let (_,v1) = eval e t1 in
    let (_,v2) = eval e t2 in
    begin match (v1,v2) with
      | Num n1,Num n2 -> (e,Num (n1+n2))
      | otherwise -> raise Runtime_error
    end
  | True -> (e,True)
  | False -> (e,False)
  | If (t1,t2,t3) ->
    let (_,v1) = eval e t1 in
    begin match v1 with
      | True -> eval e t2
      | False -> eval e t3
      | otherwise -> raise Runtime_error
    end
    
let interp t = 
  let (_,t') = eval Env.empty t in t'
