%{
  open Syntax;;
%}

%token LAMBDA

%token <string> ID

%token LPAREN
%token RPAREN
%token DOT
%token EOF

%start start
%type <Syntax.expr> start

%%

start :
    term EOF
      { $1 }

term :
    appTerm
      { $1 }
  | LAMBDA ID DOT term
      { Lam ($2, $4) }

appTerm :
    atomicTerm
      { $1 }
  | appTerm atomicTerm
      { App ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | ID
      { Var ($1) }
