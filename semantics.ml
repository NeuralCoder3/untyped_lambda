open Syntax
(* 
alternative: call by value, call by need 
https://en.wikipedia.org/wiki/Evaluation_strategy
*)

(* smallstep call by name *)
let rec subst e v r =
  match e with
  | App (e1, e2) -> App (subst e1 v r, subst e2 v r)
  | Lam (v', e) -> if v = v' then Lam (v', e) else Lam (v', subst e v r)
  | Var v' -> if v = v' then r else Var v'

(* let rec cbn e =
  match e with
  | App (Lam (v, e), e2) -> cbn (subst e v e2)
  | App (e1, e2) -> App (cbn e1, cbn e2)
  (* normalize in lambda *)
  | Lam (v, e) -> Lam (v, cbn e)
  | _ -> e *)

let rec cbn_step e =
  match e with
  | App (Lam (v, e), e2) -> Some (subst e v e2)
  | App (e1, e2) -> 
    (match cbn_step e1 with
    | Some e1' -> Some (App (e1', e2))
    | None -> 
      (match cbn_step e2 with
      | Some e2' -> Some (App (e1, e2'))
      | None -> None
      )
    )
  | Lam (v, e) ->
    (match cbn_step e with
    | Some e' -> Some (Lam (v, e'))
    | None -> None
    )
  | _ -> None


(* bigstep *)
exception Unbound of var
let empty x = raise (Unbound x)
let update f x v y = if y = x then v else f y

let rec eval env e =
  match e with
  | Var v -> env v
  | Lam (v, e) -> Lam (v, e)
  | App (e1,e2) ->
    match eval env e1 with
    | Lam (v, e) -> 
      let env' = update env v (eval env e2) in
      eval env' e
    | _ -> raise (Failure "not a function")



open Print

let rec cbn ?(dbg=false) e =
  if dbg then Printf.printf "%s\n" (to_string e);
  match cbn_step e with
  | Some e' -> 
    if dbg then Printf.printf "  ~> ";
    cbn ~dbg e'
  | None -> e
