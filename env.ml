(* Environment *)

exception Not_found

let empty = []

let bind e x v = (x,v) :: e

let rec lookup e x = match e with
  | [] -> raise Not_found
  | (x',v) :: e' -> if x = x' then v else lookup e' x
