%{
  open Absyn
  open ProgramRep
  open Exceptions
  open Lexing

  type var_name_generator = { mutable next : int }
  let vg = ( {next = 0;} )
  let new_var () =
    let number = vg.next in
    let () = vg.next <- vg.next+1 in
    Int.to_string number

  let string_to_array_literal str =
    let rec explode idx acc =
      match idx with
      | i when i >= (String.length str)-1 -> ArrayLiteral (List.rev acc)
      | i -> explode (idx+1) ((Value(Char(str.[i])))::acc)
    in
    explode 1 []
%}
%token <int> CSTINT
%token INT
%token <bool> CSTBOOL
%token BOOL
%token <char> CSTCHAR
%token CHAR
%token <string> CSTSTRING
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
%token CONST STABLE STRUCT NULL NEW
%token PRINT READ HASH UNDERSCORE

%left ELSE
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left PLUS MINUS LOGIC_OR
%left TIMES LOGIC_AND
%nonassoc NOT HASH

%start main
%type <Absyn.file> main
%%
main:
  topdecs EOF     { File $1 }
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
  | ENTRY NAME LT typ_vars GT LPAR simple_params RPAR block { raise_error "Entrypoints cannot be generic" }
  | STRUCT NAME LPAR struct_params RPAR SEMI                       { Struct ($2, [], $4) }
  | STRUCT NAME LT typ_vars GT LPAR struct_params RPAR SEMI        { Struct ($2, $4, $7) }
  | REFERENCE PATH AS NAME SEMI                             { FileReference($4, $2) }
;

typ_vars:
    TYPE_VAR                { [$1] }
  | TYPE_VAR COMMA typ_vars { $1 :: $3 }
;

typ_args:
    typ                 { [Some $1] }
  | UNDERSCORE          { [None] }
  | typ COMMA typ_args  { (Some $1) :: $3 }
  | UNDERSCORE COMMA typ_args { None :: $3 }
;

simple_typ:
    INT                 { T_Int }
  | BOOL                { T_Bool }
  | CHAR                { T_Char }
;

typ:
    simple_typ          { $1 }
  | typ LBRAKE RBRAKE   { T_Array (Some $1) }
  | NAME                { T_Struct ($1, []) }
  | NAME LT typ_args GT { T_Struct ($1, $3) }
  | TYPE_VAR            { T_Generic $1 }
  | LPAR typ_list RPAR  { T_Routine $2 }
;

typ_list:
   { [] }
  | typ               { [(Open, $1)] }  
  | STABLE typ        { [(Stable, $2)] }
  | CONST typ         { [(Const, $2)] }
  | typ COMMA typ_list {(Open, $1)::$3}
  | STABLE typ COMMA typ_list {(Stable, $2)::$4}
  | CONST typ COMMA typ_list {(Const, $2)::$4}
;

block:
  LBRACE stmtOrDecSeq RBRACE    { Block $2 }
;

expression:
    reference                               { Reference $1 }
  | value                                   { Value $1 }
;

reference:
  NAME HASH inner_reference   { OtherContext ($1, $3) }
  | inner_reference           { LocalContext $1 }
  | NULL                      { Null }
;

inner_reference:
   NAME                                                     { Access $1 }
  | inner_reference DOT NAME                                { StructAccess ($1, $3) }
  | inner_reference LBRAKE expression RBRAKE                { ArrayAccess ($1, $3) }
  | LPAR inner_reference RPAR                               { $2 }
;

simple_value:
  LPAR value RPAR { $2 }
  | CSTBOOL   { Bool $1 }
  | CSTINT    { Int $1 }
  | CSTCHAR   { Char $1 }
  | MINUS expression                                      { Binary_op ("-", Value (Int 0), $2) }
  | NOT expression                                        { Unary_op ("!", $2) }
  | PIPE inner_reference PIPE                                   { ArraySize $2 }
  | READ LT typ GT                                        { GetInput $3 }
  | VALUE inner_reference                                       { ValueOf $2 }
  | NEW typ LBRAKE expression RBRAKE                      { NewArray ($2, $4) }
  | LBRAKE arguments RBRAKE                               { ArrayLiteral $2 }
  | CSTSTRING                                             { string_to_array_literal $1 }
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
;

stmtOrDecSeq:
                               { [] }
  | stmtOrDec stmtOrDecSeq     { $1 :: $2 }
;

stmtOrDec:
    stmt                                                     { Statement ($1, $symbolstartpos.pos_lnum) }
  | dec                                                      { Declaration ($1, $symbolstartpos.pos_lnum) }
;

dec:
    NAME COLON typ SEMI                                      { TypeDeclaration (Open, $3, $1) }
  | NAME COLON STABLE typ SEMI                               { TypeDeclaration (Stable, $4, $1) }
  | NAME COLON CONST typ SEMI                                { TypeDeclaration (Const, $4, $1) }
  | NAME COLON typ ASSIGNMENT expression SEMI                { AssignDeclaration (Open, Some $3, $1, $5) }
  | NAME COLON STABLE typ ASSIGNMENT expression SEMI         { AssignDeclaration (Stable, Some $4, $1, $6) }
  | NAME COLON CONST typ ASSIGNMENT expression SEMI          { AssignDeclaration (Const, Some $4, $1, $6) }
  | NAME COLON ASSIGNMENT expression SEMI                    { AssignDeclaration (Open, None, $1, $4) }
  | NAME COLON STABLE ASSIGNMENT expression SEMI             { AssignDeclaration (Stable, None, $1, $5) }
  | NAME COLON CONST ASSIGNMENT expression SEMI              { AssignDeclaration (Const, None, $1, $5) }
;

stmt:
   block                                              { $1 }
  | IF LPAR expression RPAR stmt ELSE stmt        { If ($3, $5, $7) }
  | IF LPAR expression RPAR stmt                  { If ($3, $5, Block []) }
  | WHILE LPAR expression RPAR stmt               { While ($3, $5) }
  | UNTIL LPAR expression RPAR stmt               { While (Value (Unary_op("!", $3)), $5) }
  | FOR LPAR dec expression SEMI non_control_flow_stmt RPAR stmt    { Block([Declaration($3, $symbolstartpos.pos_lnum); Statement(While($4, Block([Statement($8,$symbolstartpos.pos_lnum); Statement($6,$symbolstartpos.pos_lnum);])), $symbolstartpos.pos_lnum);]) }
  | REPEAT LPAR value RPAR stmt { 
    let var_name = new_var () in
    Block([
      Declaration(TypeDeclaration(Open, T_Int, var_name), $symbolstartpos.pos_lnum); 
      Statement(While(Value(Binary_op("<", Reference(LocalContext(Access var_name)), Value $3)), 
        Block([
          Statement($5,$symbolstartpos.pos_lnum); 
          Statement(Assign(Access(var_name), Value(Binary_op("+", Value(Int 1), Reference(LocalContext(Access var_name))))), $symbolstartpos.pos_lnum);
        ])
      ),$symbolstartpos.pos_lnum);
    ]) 
  }
  | REPEAT LPAR inner_reference RPAR stmt { 
    let count_name = new_var () in
    let limit_name = new_var () in
    Block([
      Declaration(AssignDeclaration(Open, Some T_Int, limit_name, Value(ValueOf($3))), $symbolstartpos.pos_lnum); 
      Declaration(TypeDeclaration(Open, T_Int, count_name), $symbolstartpos.pos_lnum); 
      Statement(While(Value(Binary_op("<", Reference(LocalContext(Access count_name)), Reference(LocalContext(Access limit_name)))), 
        Block([
          Statement($5, $symbolstartpos.pos_lnum); 
          Statement(Assign(Access count_name, Value(Binary_op("+", Value(Int 1), Reference(LocalContext(Access count_name))))), $symbolstartpos.pos_lnum);
        ])
      ), $symbolstartpos.pos_lnum);
    ]) 
  }
  | STOP SEMI                                       { Stop }
  | HALT SEMI                                        { Halt }
  | BREAK SEMI                                   { Break }
  | CONTINUE SEMI                                  { Continue }
  | non_control_flow_stmt SEMI { $1 }
;

non_control_flow_stmt:
    inner_reference ASSIGNMENT expression        { Assign ($1, $3) }
  | inner_reference PLUS ASSIGNMENT expression   { Assign ($1, Value(Binary_op("+", Reference(LocalContext $1), $4))) }
  | inner_reference MINUS ASSIGNMENT expression  { Assign ($1, Value(Binary_op("-", Reference(LocalContext $1), $4))) }
  | inner_reference TIMES ASSIGNMENT expression  { Assign ($1, Value(Binary_op("*", Reference(LocalContext $1), $4))) }
  | inner_reference NOT ASSIGNMENT expression    { Assign ($1, Value(Unary_op("!", $4))) }
  | reference LPAR arguments RPAR                      { Call ($1, [], $3) }
  | reference LT typ_args GT LPAR arguments RPAR       { Call ($1, $3, $6) }
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
    NAME COLON simple_typ                  { (Open, $3, $1) }
  | NAME COLON STABLE simple_typ           { (Stable, $4, $1) }
  | NAME COLON CONST simple_typ            { (Const, $4, $1) }
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
  | NAME COLON typ                  { (Open, $3, $1) }
  | NAME COLON STABLE typ           { (Stable, $4, $1) }
  | NAME COLON CONST typ            { (Const, $4, $1) }
;



struct_params:
               { [] }
  | struct_params1    { $1 }
;

struct_params1:
    struct_param                  { [$1] }
  | struct_param COMMA struct_params1    { $1 :: $3 }
;

struct_param:
  | NAME COLON typ                  { (Open, $3, $1) }
  | NAME COLON STABLE typ           { (Stable, $4, $1) }
  | NAME COLON CONST typ            { (Const, $4, $1) }
;