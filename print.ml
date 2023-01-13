open Syntax
(*
pretty printing
*)

let rec to_string e =
  match e with
  | App (e1, e2) -> Printf.sprintf "(%s %s)" (to_string e1) (to_string e2)
  | Lam (v, e) -> Printf.sprintf "(λ%s. %s)" v (to_string e)
  | Var v -> v

(* a bit nicer *)
let rec pretty_to_string e =
  match e with
  | Var v -> v
  | Lam (v, e) -> Printf.sprintf "λ%s. %s" v (pretty_to_string e)
  | App (e1, e2) -> 
    let s1 = pretty_to_string e1 in
    let s2 = pretty_to_string e2 in
    let s1' = match e1 with
      | Lam _ -> Printf.sprintf "(%s)" s1
      | _ -> s1
    in
    let s2' = match e2 with
      | Lam _ | App _ -> Printf.sprintf "(%s)" s2
      | _ -> s2
    in
    Printf.sprintf "%s %s" s1' s2'
