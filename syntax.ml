type var = string

type expr =
  | App of expr * expr
  | Lam of var * expr
  | Var of var
;;

(*
 https://github.com/TheLokin/Lambda-Calculus-Interpreter  
*)
