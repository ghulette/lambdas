(* Environment *)

exception Not_found

type 'a env

val empty : 'a env
val bind : 'a env -> string -> 'a -> 'a env
val lookup : 'a env -> string -> 'a 
