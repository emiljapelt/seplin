%{
  open Absyn
  open ProgramRep
%}
%token <int> CSTINT
%token INT
%token <bool> CSTBOOL
%token BOOL
%token INTERNAL EXTERNAL
%token <string> NAME
%token ASSIGNMENT
%token LPAR RPAR LBRACE RBRACE LBRAKE RBRAKE
%token STOP
%token PLUS MINUS EQ
%token AND OR NOT
%token COMMA SEMI EOF
%token IF ELSE
%token ROUTINE LOCKED
%token PRINT
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
    typ NAME ASSIGNMENT assignable_expression SEMI            { GlobalVar (false, $1, $2, $4)   }
  | LOCKED typ NAME ASSIGNMENT assignable_expression SEMI     { GlobalVar (true, $2, $3, $5)    }
  | INTERNAL ROUTINE NAME LPAR paramdecs RPAR block           { Routine (Internal, $3, $5, $7) }
  | EXTERNAL ROUTINE NAME LPAR paramdecs RPAR block           { Routine (External, $3, $5, $7) }
;

typ:
    INT       { T_Int }
  | BOOL      { T_Bool }
;

block:
  LBRACE stmtOrDecSeq RBRACE    { Block $2 }
;

assignable_expression:
    CSTBOOL     { Bool $1 }
  | CSTINT    { Int $1 }
  | NAME         { Lookup $1  }
  | assignable_expression AND assignable_expression       { Binary_op ("&", $1, $3) }
  | assignable_expression OR assignable_expression        { Binary_op ("|", $1, $3) }
  | assignable_expression EQ assignable_expression        { Binary_op ("=", $1, $3) }
  | assignable_expression PLUS assignable_expression      { Binary_op ("+", $1, $3) }
  | assignable_expression MINUS assignable_expression     { Binary_op ("-", $1, $3) }
  | assignable_expression EQ assignable_expression        { Binary_op ("=", $1, $3) }
  | NOT assignable_expression                             { Unary_op ("!", $2) }
;

unassignable_expression:
    NAME ASSIGNMENT assignable_expression     { Assign ($1, $3) }
  | NAME LPAR params RPAR                       { Call ($1, $3) }
  | STOP                                        { Stop }
  | PRINT assignable_expression                 { Print $2 }
;

params:
              { [] }
  | params1   { $1 }
;

params1:
    assignable_expression                               { [$1] }
  | assignable_expression COMMA params1    { $1 :: $3 }
;

stmtOrDecSeq:
                               { [] }
  | stmtOrDec stmtOrDecSeq     { $1 :: $2}
;

stmtOrDec:
    stmt                                                     { Statement $1 }
  | typ NAME ASSIGNMENT assignable_expression SEMI           { Declaration (false, $1, $2, $4) }
  | LOCKED typ NAME ASSIGNMENT assignable_expression SEMI    { Declaration (true, $2, $3, $5) }
;

stmt:
    unassignable_expression SEMI                       { Expression $1 }
  | block                                              { $1 }
  | IF LPAR assignable_expression RPAR stmt ELSE stmt        { If ($3, $5, $7) }
  | IF LPAR assignable_expression RPAR stmt                  { If ($3, $5, Block []) }
;

paramdecs:
                  { [] }
  | paramdecs1    { $1 }
;

paramdecs1:
    param                     { [$1] }
  | param COMMA paramdecs1    { $1 :: $3 }
  | param COMMA paramdecs1    { $1 :: $3 }
;

param:
    typ NAME              { (false, $1, $2) }
  | LOCKED typ NAME       { (true, $2, $3) }
;

