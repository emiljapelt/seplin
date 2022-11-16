type token =
  | SECTION_END
  | GLOBAL_SECTION
  | PROGRAM_SECTION
  | ENTRY_POINT
  | LABEL
  | NAME of (string)
  | INT
  | BOOL
  | CST_INT of (int)
  | CST_BOOL of (bool)
  | HALT
  | STOP
  | CALL
  | GOTO
  | IF_TRUE
  | PLACE_BOOL
  | PLACE_INT
  | CLONE_FULL
  | CLONE_HALF
  | CLONE_SHORT
  | CLONE_BYTE
  | FETCH_FULL
  | FETCH_HALF
  | FETCH_SHORT
  | FETCH_BYTE
  | FIELD_FETCH
  | DECLARE_FULL
  | DECLARE_HALF
  | DECLARE_SHORT
  | DECLARE_BYTE
  | DECLARE_STRUCT
  | ASSIGN_FULL
  | ASSIGN_HALF
  | ASSIGN_SHORT
  | ASSIGN_BYTE
  | FIELD_ASSIGN
  | INT_ADD
  | INT_MUL
  | INT_SUB
  | INT_EQ
  | INT_LT
  | BOOL_EQ
  | BOOL_NOT
  | BOOL_AND
  | BOOL_OR
  | GETSP
  | GETBP
  | MODSP
  | FREE_VAR
  | FREE_VARS
  | PRINT_VAR
  | PRINT_INT
  | PRINT_BOOL
  | STACK_FETCH
  | BP_FETCH
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ProgramRep.program
