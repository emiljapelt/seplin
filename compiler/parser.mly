%{
  open Absyn
  open ProgramRep
  open Exceptions
%}
%token <int> CSTINT
%token INT
%token <bool> CSTBOOL
%token BOOL
%token INTERNAL EXTERNAL
%token <string> NAME
%token ASSIGNMENT
%token LPAR RPAR LBRACE RBRACE LBRAKE RBRAKE
%token STOP HALT
%token PLUS MINUS TIMES EQ NEQ LT GT LTEQ GTEQ
%token LOGIC_AND LOGIC_OR PIPE NOT VALUE
%token COMMA DOT SEMI EOF
%token IF ELSE
%token WHILE UNTIL FOR
%token BREAK CONTINUE
%token LOCKED STRUCT VAR NULL NEW
%token PRINT

%left ELSE
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left PLUS MINUS LOGIC_OR
%left TIMES LOGIC_AND
%nonassoc NOT

%start main
%type <Absyn.topdecs> main
%%
main:
  topdecs EOF     { Topdecs $1 }
;

topdecs:
                        { [] }
  | topdec  topdecs     { $1 :: $2 }
;

topdec:
    typ NAME SEMI                                             { Global (false, $1, $2) }
  | LOCKED typ NAME SEMI                                      { Global (true, $2, $3) }
  | typ NAME ASSIGNMENT assignable_expression SEMI            { GlobalAssign (false, $1, $2, $4)   }
  | LOCKED typ NAME ASSIGNMENT assignable_expression SEMI     { GlobalAssign (true, $2, $3, $5)    }
  | INTERNAL NAME LPAR params RPAR block           { Routine (Internal, $2, $4, $6) }
  | EXTERNAL NAME LPAR params RPAR block           { Routine (External, $2, $4, $6) }
  | INTERNAL NAME LPAR params RPAR chain           { Routine (Internal, $2, $4, Block $6) }
  | EXTERNAL NAME LPAR params RPAR chain           { Routine (External, $2, $4, Block $6) }
  | STRUCT NAME LBRAKE params RBRAKE SEMI          { Struct ($2, $4) }
;

typ:
    INT                 { T_Int }
  | BOOL                { T_Bool }
  | typ LBRAKE RBRAKE   { T_Array $1 }
  | NAME                { T_Struct $1 }
;

block:
  LBRACE stmtOrDecSeq RBRACE    { Block $2 }
;

chain:
    DOT NAME LPAR arguments RPAR { [Statement (Expression (Call($2, $4)))] }
  | DOT NAME LPAR arguments RPAR chain { (Statement (Expression (Call ($2, $4)))) :: $6 }
;

assignable_expression:
    reference                                     { Reference $1 }
  | value                                         { Value $1 }
  | NEW typ LBRAKE assignable_expression RBRAKE   { NewArray ($2, $4) }
  | NEW NAME LBRAKE arguments RBRAKE              { NewStruct ($2, $4) }
  | LPAR assignable_expression RPAR               { $2 }
;

reference:
   NAME                                               { VarRef $1 }
  | reference LBRAKE NAME RBRAKE                      { StructRef ($1, $3) }
  | reference LBRAKE assignable_expression RBRAKE     { ArrayRef ($1, $3) }
  | NULL                                              { Null }
;

value:
    CSTBOOL   { Bool $1 }
  | CSTINT    { Int $1 }
  | assignable_expression LOGIC_AND assignable_expression       { Binary_op ("&&", $1, $3) }
  | assignable_expression LOGIC_OR assignable_expression        { Binary_op ("||", $1, $3) }
  | assignable_expression EQ assignable_expression        { Binary_op ("=", $1, $3) }
  | assignable_expression NEQ assignable_expression       { Binary_op ("!=", $1, $3) }
  | assignable_expression LTEQ assignable_expression      { Binary_op ("<=", $1, $3) }
  | assignable_expression LT assignable_expression        { Binary_op ("<", $1, $3) }
  | assignable_expression GTEQ assignable_expression      { Binary_op (">=", $1, $3) }
  | assignable_expression GT assignable_expression        { Binary_op (">", $1, $3) }
  | assignable_expression PLUS assignable_expression      { Binary_op ("+", $1, $3) }
  | assignable_expression TIMES assignable_expression     { Binary_op ("*", $1, $3) }
  | assignable_expression MINUS assignable_expression     { Binary_op ("-", $1, $3) }
  | MINUS assignable_expression                           { Binary_op ("-", Value (Int 0), $2) }
  | NOT assignable_expression                             { Unary_op ("!", $2) }
  | PIPE reference PIPE                                   { ArraySize $2 }
  | VALUE reference                                       { Lookup $2 }
;

unassignable_expression:
    STOP reference ASSIGNMENT assignable_expression       { Assign ("", $2, $4) }
  | reference PLUS ASSIGNMENT assignable_expression  { Assign ("+", $1, $4) }
  | reference MINUS ASSIGNMENT assignable_expression { Assign ("-", $1, $4) }
  | reference TIMES ASSIGNMENT assignable_expression { Assign ("*", $1, $4) }
  | reference NOT ASSIGNMENT assignable_expression   { Assign ("!", $1, $4) }
  | NAME LPAR arguments RPAR                    { Call ($1, $3) }
  | STOP                                        { Stop }
  | HALT                                        { Halt }
  | BREAK                                       { Break }
  | CONTINUE                                    { Continue }
  | PRINT assignable_expression                 { Print $2 }
;

arguments:
                 { [] }
  | arguments1   { $1 }
;

arguments1:
    assignable_expression                     { [$1] }
  | assignable_expression COMMA arguments1    { $1 :: $3 }
;

stmtOrDecSeq:
                               { [] }
  | stmtOrDec stmtOrDecSeq     { $1 :: $2}
;

stmtOrDec:
    stmt                                                     { Statement $1 }
  | dec                                                      { Declaration $1 }
;

dec:
    typ NAME SEMI                                            { TypeDeclaration (false, $1, $2) }
  | LOCKED typ NAME SEMI                                     { TypeDeclaration (true, $2, $3) }
  | typ NAME ASSIGNMENT assignable_expression SEMI           { AssignDeclaration (false, $1, $2, $4) }
  | LOCKED typ NAME ASSIGNMENT assignable_expression SEMI    { AssignDeclaration (true, $2, $3, $5) }
  | VAR NAME ASSIGNMENT assignable_expression SEMI           { VarDeclaration (false, $2, $4) }
  | LOCKED VAR NAME ASSIGNMENT assignable_expression SEMI    { VarDeclaration (true, $3, $5) }
;

stmt:
    unassignable_expression SEMI                       { Expression $1 }
  | block                                              { $1 }
  | IF LPAR assignable_expression RPAR stmt ELSE stmt        { If ($3, $5, $7) }
  | IF LPAR assignable_expression RPAR stmt                  { If ($3, $5, Block []) }
  | WHILE LPAR assignable_expression RPAR stmt               { While ($3, $5) }
  | UNTIL LPAR assignable_expression RPAR stmt               { While (Value (Unary_op("!", $3)), $5) }
  | FOR LPAR dec assignable_expression SEMI unassignable_expression RPAR stmt    { For ($3, $4, $6, $8) }
;

params:
               { [] }
  | params1    { $1 }
;

params1:
    param                  { [$1] }
  | param COMMA params1    { $1 :: $3 }
;

param:
    LOCKED typ NAME            { (true, $2, $3) }
  | typ NAME                   { (false, $1, $2) }
;
