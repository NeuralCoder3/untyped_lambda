{
  open Lexing;;
  open Parser;;
  open Bytes;;

  exception Lexical_error of string;;

  let comments = ref 0;;

  let open_comment () =
    comments := !comments+1
  ;;

  let close_comment () =
    comments := !comments-1;
    !comments > 0
  ;;

  let reset_comments () =
    comments := 1
  ;;

}

rule token = parse
  | [' ' '\t' '\r' '\n']*
      { token lexbuf }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '.'
      { DOT }
  | "*)"
      { raise (Lexical_error "Unmatched end of comment") }
  | "(*"
      {
        reset_comments ();
        comment lexbuf;
        token lexbuf
      }
  | "lambda" | "L" | "\\" | "λ"
      { LAMBDA }
  | ['A'-'Z' 'a'-'z' '_' '0'-'9' '\'']+
      { ID (lexeme lexbuf) }
  | eof
      { EOF }
  | _
      { raise (Lexical_error ("Illegal character " ^ lexeme lexbuf)) }


and comment = parse
  | "(*"
      {
        open_comment ();
        comment lexbuf
      }
  | "*)"
      { if close_comment () then comment lexbuf }
  | eof
      { raise (Lexical_error "Comment not terminated") }
  | _
      { comment lexbuf }
