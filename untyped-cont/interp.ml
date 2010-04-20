open Env
open Term

(* Interpreter *)

exception Runtime_error

type cont =
  | EndCont
  | RatorCont of t * t env * cont
  | RandCont of t * cont
  | BinOp1Cont of binop * t * t env * cont
  | BinOp2Cont of binop * t * cont
  | IfTestCont of t * t * t env * cont

let rec apply_cont k v = match k with
  | EndCont -> 
    Printf.printf "%s (DONE)\n" (string_of_term v);
    v
  | RatorCont (argt,e,k) ->
    eval e (RandCont (v,k)) argt
  | RandCont (procv,k) -> 
    begin match procv with
      | Closure (x,body,saved_env) ->
        let saved_env' = bind saved_env x v in
        eval saved_env' k body
      | otherwise -> raise Runtime_error
    end
  | BinOp1Cont (op,t2,e,k) ->
    eval e (BinOp2Cont (op,v,k)) t2
  | BinOp2Cont (op,v1,k) ->
    let r = match op,v1,v with
      | Eq, Num n1, Num n2 -> if n1 = n2 then True else False
      | Plus, Num n1, Num n2 -> Num (n1+n2)
      | Minus, Num n1, Num n2 -> Num (n1-n2)
      | Mult, Num n1, Num n2 -> Num (n1*n2)
      | otherwise -> raise Runtime_error
    in
    apply_cont k r
  | IfTestCont (t2,t3,e,k) ->
    begin match v with 
      | True -> eval e k t2
      | False -> eval e k t3
      | otherwise -> raise Runtime_error
    end

and eval e k = function
  | Var x -> apply_cont k (lookup e x)
  | Lambda (x,t) -> apply_cont k (Closure (x,t,e))
  | Closure (x,t,e) -> apply_cont k (Closure (x,t,e))
  | App (t1,t2) -> eval e (RatorCont (t2,e,k)) t1
  | Binop (op,t1,t2) -> eval e (BinOp1Cont (op,t2,e,k)) t1
  | Num n -> apply_cont k (Num n)
  | True -> apply_cont k True
  | False -> apply_cont k False
  | If (t1,t2,t3) -> eval e (IfTestCont (t2,t3,e,k)) t1
    
let interp t = eval Env.empty EndCont t
