open Parsing;;
open Lexing;;

open Parser;;
open Lexer;;

open Syntax;;
open Print;;
open Semantics;;

let parse s = start token (from_string s)

let test_string ?(dbg=false) s = 
  Printf.printf "Input: %s\n" s;
  let expr = parse s in 
  Printf.printf "Expr: %s\n" (pretty_to_string expr)

let parse_file f =
  let id = open_in f in
  let s = from_channel id in
  let expr = start token s in
  close_in id;
  expr

let _ =
  let expr = parse_file "./programs/nums.lam" in
  Printf.printf "Expr: %s\n" (pretty_to_string expr);
  let evaluated = cbn expr in
  Printf.printf "Eval: %s\n" (pretty_to_string evaluated)

  
  


(* let _ =
  test "λx. x";
  test "(λx. x)";
  test "(λy. y)";
  test "λx. λy. x";
  test "λx. λy. y";
  test "(λx. x) (λy. y)";
  test "λx. λy. y x";
  test ~dbg:true "λx. λy. x y";
  test "λx. λy. x y";
  test "λx. (λy. y) x";
  test "O";
  test "S";
  test "S O";
  test ~dbg:true "λx. λy. λz. x y z";
  test ~dbg:true "λx. λy. λz. λa. x y z a";
  test ~dbg:true "S (S O)";
  test "5" *)
