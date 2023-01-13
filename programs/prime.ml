let is_prime n = 
  let rec is_prime_aux n i = 
    if i * i > n then true
    else if n mod i = 0 then false
    else is_prime_aux n (i + 1)
  in
  is_prime_aux n 2;;


(* or iter using induction *)

let rec iter f n x = 
  if n = 0 then x
  else iter f (n - 1) (f x);;

let rec is_prime_iter n = 
  let (_,b) = iter (fun (i, b) -> (i + 1, b && n mod i <> 0)) (n - 2) (2, true) in
  b
  (* if n < 2 then false
  else iter (fun i -> i + 1) (n - 2) (fun i -> n mod i <> 0);; *)

(* or using for_all with range *)
(* ... *)

(* next_prime *)
let rec next_prime n = 
  if is_prime n then n
  else next_prime (n + 1);;

(* nth prime *)
let rec nth_prime n = 
  if n = 1 then 2
  else next_prime (nth_prime (n - 1)+1);;

let _ =
  List.init 1000 (fun i -> nth_prime (i + 1)) |> List.iter (Printf.printf "%d ");;
