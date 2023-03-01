type token =
  | CSTINT of (int)
  | INT
  | CSTBOOL of (bool)
  | BOOL
  | CSTCHAR of (char)
  | CHAR
  | INTERNAL
  | EXTERNAL
  | ENTRY
  | NAME of (string)
  | MERGE
  | PATH of (string)
  | TYPE_VAR of (char)
  | ASSIGNMENT
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | LBRAKE
  | RBRAKE
  | STOP
  | HALT
  | PLUS
  | MINUS
  | TIMES
  | EQ
  | NEQ
  | LT
  | GT
  | LTEQ
  | GTEQ
  | LOGIC_AND
  | LOGIC_OR
  | PIPE
  | NOT
  | VALUE
  | COMMA
  | DOT
  | SEMI
  | COLON
  | EOF
  | IF
  | ELSE
  | WHILE
  | UNTIL
  | FOR
  | REPEAT
  | BREAK
  | CONTINUE
  | LOCKED
  | STRUCT
  | NULL
  | NEW
  | PRINT
  | HASH

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Absyn.topdecs
