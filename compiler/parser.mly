%{
  open Absyn
  open ProgramRep
  open Exceptions

  type var_name_generator = { mutable next : int }
  let vg = ( {next = 0;} )
  let new_var () =
    let number = vg.next in
    let () = vg.next <- vg.next+1 in
    Int.to_string number

  let get_filename () = ((symbol_start_pos ()).pos_fname)
  let get_linenum () = ((symbol_start_pos ()).pos_lnum)
%}
%token <int> CSTINT
%token INT
%token <bool> CSTBOOL
%token BOOL
%token <char> CSTCHAR
%token CHAR
%token INTERNAL EXTERNAL
%token <string> NAME
%token INCLUDE
%token <string> PATH
%token <char> TYPE_VAR
%token ASSIGNMENT
%token LPAR RPAR LBRACE RBRACE LBRAKE RBRAKE
%token STOP HALT
%token PLUS MINUS TIMES EQ NEQ LT GT LTEQ GTEQ
%token LOGIC_AND LOGIC_OR PIPE NOT VALUE
%token COMMA DOT SEMI COLON EOF
%token IF ELSE
%token WHILE UNTIL FOR REPEAT
%token BREAK CONTINUE
%token LOCKED STRUCT NULL NEW
%token PRINT HASH

%right ASSIGNMENT
%left ELSE
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left PLUS MINUS LOGIC_OR
%left TIMES LOGIC_AND
%right HASH
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
    dec { GlobalDeclaration $1 }
  | INTERNAL NAME LPAR params RPAR block                  { Routine (Internal, $2, [], $4, $6) }
  | INTERNAL NAME LT typ_vars GT LPAR params RPAR block   { Routine (Internal, $2, $4, $7, $9) }
  | EXTERNAL NAME LPAR params RPAR block                  { Routine (External, $2, [], $4, $6) }
  | EXTERNAL NAME LT typ_vars GT LPAR params RPAR block   { Routine (External, $2, $4, $7, $9) }
  | STRUCT NAME LPAR params RPAR SEMI                     { Struct ($2, [], $4) }
  | STRUCT NAME LT typ_vars GT LPAR params RPAR SEMI      { Struct ($2, $4, $7) }
  | INCLUDE PATH SEMI                                     { Include $2 }
;

typ_vars:
    TYPE_VAR                { [$1] }
  | TYPE_VAR COMMA typ_vars { $1 :: $3 }
;

typ_args:
    typ                 { [$1] }
  | typ COMMA typ_args  { $1 :: $3 }
;

typ:
    INT                 { T_Int }
  | BOOL                { T_Bool }
  | CHAR                { T_Char }
  | typ LBRAKE RBRAKE   { T_Array $1 }
  | NAME                { T_Struct ($1, []) }
  | NAME LT typ_args GT { T_Struct ($1, $3) }
  | TYPE_VAR            { T_Generic $1 }
;

block:
  LBRACE stmtOrDecSeq RBRACE    { Block $2 }
;

assignable_expression:
    reference                                     { Reference $1 }
  | value                                         { Value $1 }
  | LPAR assignable_expression RPAR               { $2 }
;

reference:
   NAME                                               { VarRef $1 }
  | reference DOT NAME                                { StructRef ($1, $3) }
  | reference LBRAKE assignable_expression RBRAKE     { ArrayRef ($1, $3) }
  | NULL                                              { Null }
;

value:
    CSTBOOL   { Bool $1 }
  | CSTINT    { Int $1 }
  | CSTCHAR   { Char $1 }
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
  | HASH typ                                              { GetInput $2 }
  | VALUE reference                                       { ValueOf $2 }
  | NEW typ LBRAKE assignable_expression RBRAKE           { NewArray ($2, $4) }
  | LBRAKE arguments RBRAKE                               { ArrayLiteral $2 }
  | NEW NAME LPAR arguments RPAR                          { NewStruct ($2, [], $4) }
  | NEW NAME LT typ_args GT LPAR arguments RPAR           { NewStruct ($2, $4, $7) }
  | LBRACE arguments RBRACE                               { StructLiteral $2 }
;

unassignable_expression:
    reference ASSIGNMENT assignable_expression       { Assign ($1, $3) }
  | reference PLUS ASSIGNMENT assignable_expression  { Assign ($1, Value(Binary_op("+", Reference $1, $4))) }
  | reference MINUS ASSIGNMENT assignable_expression { Assign ($1, Value(Binary_op("-", Reference $1, $4))) }
  | reference TIMES ASSIGNMENT assignable_expression { Assign ($1, Value(Binary_op("*", Reference $1, $4))) }
  | reference NOT ASSIGNMENT assignable_expression   { Assign ($1, Value(Unary_op("!", $4))) }
  | NAME LPAR arguments RPAR                    { Call ($1, [], $3) }
  | NAME LT typ_args GT LPAR arguments RPAR     { Call ($1, $3, $6) }
  | STOP                                        { Stop }
  | HALT                                        { Halt }
  | BREAK                                       { Break }
  | CONTINUE                                    { Continue }
  | PRINT arguments1                            { Print $2 }
;

arguments:
                 { [] }
  | arguments1   { $1 }
;

arguments1:
    assignable_expression                     { [$1] }
  | assignable_expression COMMA arguments1    { $1 :: $3 }
  | error { raise_line_error "Error in arguments" (get_filename ()) (get_linenum ()) }
;

stmtOrDecSeq:
                               { [] }
  | stmtOrDec stmtOrDecSeq     { $1 :: $2 }
;

stmtOrDec:
    stmt                                                     { Statement ($1, (get_filename ()), (get_linenum ())) }
  | dec                                                      { Declaration ($1, (get_filename ()), (get_linenum ())) }
;

dec:
    NAME COLON typ SEMI                                      { TypeDeclaration (false, $3, $1) }
  | NAME COLON LOCKED typ SEMI                               { TypeDeclaration (true, $4, $1) }
  | NAME COLON typ ASSIGNMENT assignable_expression SEMI     { AssignDeclaration (false, Some $3, $1, $5) }
  | NAME COLON LOCKED typ ASSIGNMENT assignable_expression SEMI    { AssignDeclaration (true, Some $4, $1, $6) }
  | NAME COLON ASSIGNMENT assignable_expression SEMI           { AssignDeclaration (false, None, $1, $4) }
  | NAME COLON LOCKED ASSIGNMENT assignable_expression SEMI    { AssignDeclaration (true, None, $1, $5) }
;

stmt:
    unassignable_expression SEMI                       { Expression $1 }
  | block                                              { $1 }
  | IF LPAR assignable_expression RPAR stmt ELSE stmt        { If ($3, $5, $7) }
  | IF LPAR assignable_expression RPAR stmt                  { If ($3, $5, Block []) }
  | WHILE LPAR assignable_expression RPAR stmt               { While ($3, $5) }
  | UNTIL LPAR assignable_expression RPAR stmt               { While (Value (Unary_op("!", $3)), $5) }
  | FOR LPAR dec assignable_expression SEMI unassignable_expression RPAR stmt    { Block([Declaration($3, (get_filename ()), (get_linenum ())); Statement(While($4, Block([Statement($8, (get_filename ()), (get_linenum ())); Statement(Expression($6), (get_filename ()), (get_linenum ()));])), (get_filename ()), (get_linenum ()));]) }
  | REPEAT LPAR value RPAR stmt { 
    let var_name = new_var () in
    Block([
      Declaration(TypeDeclaration(false, T_Int, var_name), (get_filename ()), (get_linenum ())); 
      Statement(While(Value(Binary_op("<", Reference(VarRef var_name), Value $3)), 
        Block([
          Statement($5, (get_filename ()), (get_linenum ())); 
          Statement(Expression(Assign(VarRef(var_name), Value(Binary_op("+", Value(Int 1), Reference(VarRef var_name))))), (get_filename ()), (get_linenum ()));
        ])
      ), (get_filename ()), (get_linenum ()));
    ]) 
  }
  | REPEAT LPAR reference RPAR stmt { 
    let count_name = new_var () in
    let limit_name = new_var () in
    Block([
      Declaration(AssignDeclaration(false, Some T_Int, limit_name, Value(ValueOf($3))), (get_filename ()), (get_linenum ())); 
      Declaration(TypeDeclaration(false, T_Int, count_name), (get_filename ()), (get_linenum ())); 
      Statement(While(Value(Binary_op("<", Reference(VarRef count_name), Reference(VarRef limit_name))), 
        Block([
          Statement($5, (get_filename ()), (get_linenum ())); 
          Statement(Expression(Assign(VarRef count_name, Value(Binary_op("+", Value(Int 1), Reference(VarRef count_name))))), (get_filename ()), (get_linenum ()));
        ])
      ), (get_filename ()), (get_linenum ()));
    ]) 
  }
;

params:
               { [] }
  | params1    { $1 }
;

params1:
    param                  { [$1] }
  | param COMMA params1    { $1 :: $3 }
  | error { raise_line_error "Error in parameter declaration" (get_filename ()) (get_linenum ()) }
;

param:
    NAME COLON LOCKED typ           { (true, $4, $1) }
  | NAME COLON typ                  { (false, $3, $1) }
;
