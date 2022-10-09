type token =
  | CSTINT of (int)
  | INT
  | CSTBOOL of (bool)
  | BOOL
  | INTERNAL
  | EXTERNAL
  | NAME of (string)
  | ASSIGNMENT
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | LBRAKE
  | RBRAKE
  | STOP
  | PLUS
  | MINUS
  | EQ
  | AND
  | OR
  | NOT
  | COMMA
  | SEMI
  | EOF
  | IF
  | ELSE
  | ROUTINE
  | LOCKED
  | PRINT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn.topdecs
