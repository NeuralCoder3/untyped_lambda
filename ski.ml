open Syntax

(* type sk = S | K | App of sk*sk | Var of string *)
(* | App2 of sk*expr *)
(* I = SKK *)

(*
   | Embed of expr

https://okmij.org/ftp/tagless-final/ski.pdf
https://esolangs.org/wiki/S_and_K_Turing-completeness_proof
https://crypto.stanford.edu/~blynn/lambda/kiselyov.html
https://crypto.stanford.edu/~blynn/lambda/logski.html

https://crypto.stanford.edu/~blynn/lambda/sk.html
https://tromp.github.io/cl/LC.pdf

https://math.stackexchange.com/questions/4304294/rules-for-converting-lambda-calculus-expressions-to-ski-combinator-calculus-expr
*)



(* let rec to_ski e = 
  match e with
  | Lam (x, Var y) when x = y -> I *)
  (* | Lam (x, e) -> App2 (K, e)
  | App (e1, e2) -> App2 (App2 (S, e1), e2) *)
  (* | Var x -> failwith "free var" *)

  (* for SK *)
let rec contains x e = 
  match e with 
  | Var y -> x = y
  | App (e1, e2) -> contains x e1 || contains x e2
  | _ -> false

(* type sk = S | K | App of sk*sk | Var of string *)

let _S = Var "S"
let _K = Var "K"
let _I = App (App (_S, _K), _K)

let rec to_ski e  = 
  match e with
  | Lam (x, App (y, Var z)) when z = x -> to_ski y
  | Lam (x, b) ->
    let b' = to_ski b in
    (* if b' = Var x then _I else App (K, b') *)
    if contains x b' then 
      match b' with
      | Var _ -> _I
      | App (e1, e2) -> App (App (_S, to_ski (Lam (x, e1))), to_ski (Lam (x, e2)))
      | _ -> failwith "not possible"
    else App (_K, b')
  | Var x -> Var x
  | App (e1, e2) -> App (to_ski e1, to_ski e2)

type sk = S | K | SKApp of sk*sk 

let rec extract e = 
  match e with
  | App (e1, e2) -> 
    SKApp (extract e1, extract e2)
  | Var "S" -> S
  | Var "K" -> K
  | _ -> failwith "not possible"


  
    
  (* match e with
  | Lam (x, Var y) when x = y -> App (App (S, K), K)
  | Lam (x, App (y, z)) when x = y -> to_ski y
  | Lam (x, y) -> App (K, to_ski y) *)
