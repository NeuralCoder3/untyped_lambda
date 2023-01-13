let rec numbers n = n :: numbers (n + 1)

let rec take xs n =
  match xs,n with
  | [],_ -> []
  | _,0 -> []
  | x::xs,n -> x :: take xs (n - 1)

let primes =
  List.filter (fun n -> 
    List.length (List.filter (fun x -> n mod x = 0) (take (numbers 1) n)) = 2
  ) (numbers 2)

let () = List.iter (fun x -> print_int x; print_newline ()) (take primes 100)
