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
%token INTERNAL EXTERNAL ENTRY
%token <string> NAME
%token REFERENCE AS
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
  | INTERNAL NAME LPAR params RPAR block                    { Routine (Internal, $2, [], $4, $6) }
  | INTERNAL NAME LT typ_vars GT LPAR params RPAR block     { Routine (Internal, $2, $4, $7, $9) }
  | EXTERNAL NAME LPAR params RPAR block                    { Routine (External, $2, [], $4, $6) }
  | EXTERNAL NAME LT typ_vars GT LPAR params RPAR block     { Routine (External, $2, $4, $7, $9) }
  | ENTRY NAME LPAR simple_params RPAR block                { Routine (Entry, $2, [], $4, $6) }
  | ENTRY NAME LT typ_vars GT LPAR simple_params RPAR block { raise_line_error "Entrypoints cannot be generic" (get_filename ()) (get_linenum ()) }
  | STRUCT NAME LPAR params RPAR SEMI                       { Struct ($2, [], $4) }
  | STRUCT NAME LT typ_vars GT LPAR params RPAR SEMI        { Struct ($2, $4, $7) }
  | REFERENCE PATH AS NAME SEMI                             { FileReference($4, $2) }
;

typ_vars:
    TYPE_VAR                { [$1] }
  | TYPE_VAR COMMA typ_vars { $1 :: $3 }
;

typ_args:
    typ                 { [$1] }
  | typ COMMA typ_args  { $1 :: $3 }
;

simple_typ:
    INT                 { T_Int }
  | BOOL                { T_Bool }
  | CHAR                { T_Char }
  | error { raise_line_error "Entrypoints can only take simple types as arguments" (get_filename ()) (get_linenum ()) }
;

typ:
    simple_typ          { $1 }
  | typ LBRAKE RBRAKE   { T_Array $1 }
  | NAME                { T_Struct ($1, []) }
  | NAME LT typ_args GT { T_Struct ($1, $3) }
  | TYPE_VAR            { T_Generic $1 }
;

block:
  LBRACE stmtOrDecSeq RBRACE    { Block $2 }
;

expression:
    reference                                     { Reference $1 }
  | value                                         { Value $1 }
;

reference:
   NAME                                               { VariableAccess $1 }
  | reference DOT NAME                                { StructAccess ($1, $3) }
  | reference LBRAKE expression RBRAKE                { ArrayAccess ($1, $3) }
  | NULL                                              { Null }
  | LPAR reference RPAR {$2}
;

simple_value:
  LPAR value RPAR { $2 }
  | CSTBOOL   { Bool $1 }
  | CSTINT    { Int $1 }
  | CSTCHAR   { Char $1 }
  | MINUS expression                                      { Binary_op ("-", Value (Int 0), $2) }
  | NOT expression                                        { Unary_op ("!", $2) }
  | PIPE reference PIPE                                   { ArraySize $2 }
  | HASH typ                                              { GetInput $2 }
  | VALUE reference                                       { ValueOf $2 }
  | NEW typ LBRAKE expression RBRAKE                      { NewArray ($2, $4) }
  | LBRAKE arguments RBRAKE                               { ArrayLiteral $2 }
  | NEW NAME LPAR arguments RPAR                          { NewStruct ($2, [], $4) }
  | NEW NAME LT typ_args GT LPAR arguments RPAR           { NewStruct ($2, $4, $7) }
  | LBRACE arguments RBRACE                               { StructLiteral $2 }
;

value:
   simple_value { $1 }
  | expression LOGIC_AND expression       { Binary_op ("&&", $1, $3) }
  | expression LOGIC_OR expression        { Binary_op ("||", $1, $3) }
  | expression EQ expression        { Binary_op ("=", $1, $3) }
  | expression NEQ expression       { Binary_op ("!=", $1, $3) }
  | expression LTEQ expression      { Binary_op ("<=", $1, $3) }
  | expression LT expression        { Binary_op ("<", $1, $3) }
  | expression GTEQ expression      { Binary_op (">=", $1, $3) }
  | expression GT expression        { Binary_op (">", $1, $3) }
  | expression PLUS expression      { Binary_op ("+", $1, $3) }
  | expression TIMES expression     { Binary_op ("*", $1, $3) }
  | expression MINUS expression     { Binary_op ("-", $1, $3) }
;

arguments:
                 { [] }
  | arguments1   { $1 }
;

arguments1:
    expression                     { [$1] }
  | expression COMMA arguments1    { $1 :: $3 }
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
  | NAME COLON typ ASSIGNMENT expression SEMI     { AssignDeclaration (false, Some $3, $1, $5) }
  | NAME COLON LOCKED typ ASSIGNMENT expression SEMI    { AssignDeclaration (true, Some $4, $1, $6) }
  | NAME COLON ASSIGNMENT expression SEMI           { AssignDeclaration (false, None, $1, $4) }
  | NAME COLON LOCKED ASSIGNMENT expression SEMI    { AssignDeclaration (true, None, $1, $5) }
;

stmt:
   block                                              { $1 }
  | IF LPAR expression RPAR stmt ELSE stmt        { If ($3, $5, $7) }
  | IF LPAR expression RPAR stmt                  { If ($3, $5, Block []) }
  | WHILE LPAR expression RPAR stmt               { While ($3, $5) }
  | UNTIL LPAR expression RPAR stmt               { While (Value (Unary_op("!", $3)), $5) }
  | FOR LPAR dec expression SEMI non_control_flow_stmt RPAR stmt    { Block([Declaration($3, (get_filename ()), (get_linenum ())); Statement(While($4, Block([Statement($8, (get_filename ()), (get_linenum ())); Statement($6, (get_filename ()), (get_linenum ()));])), (get_filename ()), (get_linenum ()));]) }
  | REPEAT LPAR value RPAR stmt { 
    let var_name = new_var () in
    Block([
      Declaration(TypeDeclaration(false, T_Int, var_name), (get_filename ()), (get_linenum ())); 
      Statement(While(Value(Binary_op("<", Reference(VariableAccess var_name), Value $3)), 
        Block([
          Statement($5, (get_filename ()), (get_linenum ())); 
          Statement(Assign(VariableAccess(var_name), Value(Binary_op("+", Value(Int 1), Reference(VariableAccess var_name)))), (get_filename ()), (get_linenum ()));
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
      Statement(While(Value(Binary_op("<", Reference(VariableAccess count_name), Reference(VariableAccess limit_name))), 
        Block([
          Statement($5, (get_filename ()), (get_linenum ())); 
          Statement(Assign(VariableAccess count_name, Value(Binary_op("+", Value(Int 1), Reference(VariableAccess count_name)))), (get_filename ()), (get_linenum ()));
        ])
      ), (get_filename ()), (get_linenum ()));
    ]) 
  }
  | STOP SEMI                                       { Stop }
  | HALT SEMI                                        { Halt }
  | BREAK SEMI                                   { Break }
  | CONTINUE SEMI                                  { Continue }
  | non_control_flow_stmt SEMI { $1 }
;

non_control_flow_stmt:
    reference ASSIGNMENT expression        { Assign ($1, $3) }
  | reference PLUS ASSIGNMENT expression   { Assign ($1, Value(Binary_op("+", Reference $1, $4))) }
  | reference MINUS ASSIGNMENT expression  { Assign ($1, Value(Binary_op("-", Reference $1, $4))) }
  | reference TIMES ASSIGNMENT expression  { Assign ($1, Value(Binary_op("*", Reference $1, $4))) }
  | reference NOT ASSIGNMENT expression    { Assign ($1, Value(Unary_op("!", $4))) }
  | NAME LPAR arguments RPAR                          { Call (None, $1, [], $3) }
  | NAME LT typ_args GT LPAR arguments RPAR           { Call (None, $1, $3, $6) }
  | NAME HASH NAME LPAR arguments RPAR                 { Call (Some($1), $3, [], $5) }
  | NAME HASH NAME LT typ_args GT LPAR arguments RPAR  { Call (Some($1), $3, $5, $8) }
  | PRINT arguments1                          { Print $2 }
;

simple_params:
                      { [] }
  | simple_params1    { $1 }
;

simple_params1:
    simple_param                         { [$1] }
  | simple_param COMMA simple_params1    { $1 :: $3 }
;

simple_param:
    NAME COLON LOCKED simple_typ           { (true, $4, $1) }
  | NAME COLON simple_typ                  { (false, $3, $1) }
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
    NAME COLON LOCKED typ           { (true, $4, $1) }
  | NAME COLON typ                  { (false, $3, $1) }
;