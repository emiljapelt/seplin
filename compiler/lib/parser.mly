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
      | i -> 
        if str.[i] = '\\' then ( match str.[i+1] with
          | 'n' -> explode (idx+2) ((Value(Char('\n')))::acc)
          | 't' -> explode (idx+2) ((Value(Char('\t')))::acc)
          | '\\' -> explode (idx+2) ((Value(Char('\\')))::acc)
          | _ -> raise_failure "Unsupported escape character in string literal"
        )
        else explode (idx+1) ((Value(Char(str.[i])))::acc)
    in
    explode 1 []

    let transform_when arg else_case cases =
      let rec aux arg cases = match cases with
        | [(v,s)] -> If(Value(Binary_op("=", arg, v)), s, else_case)
        | (v,s)::t -> If(Value(Binary_op("=", arg, v)), s, aux arg t)
        | _ -> raise_failure "when to if transform failed"
      in
      match cases with
      | [(v,s)] -> If(Value(Binary_op("=", arg, v)), s, else_case)
      | (v,s)::t -> If(Value(Binary_op("=", arg, v)), s, aux arg t)
      | _ -> raise_failure "Empty when-expression"

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
%token QMARK
%token IF ELSE WHEN IS
%token WHILE UNTIL FOR REPEAT
%token BREAK CONTINUE
%token CONST STABLE STRUCT NULL NEW
%token PRINT READ HASH UNDERSCORE

/*Low precedence*/
%left LOGIC_AND LOGIC_OR
%left EQ NEQ
%left GT LT GTEQ LTEQ
%left PLUS MINUS
%left TIMES 
%nonassoc NOT VALUE
/*High precedence*/

%start main
%type <Absyn.file> main
%type <Absyn.inner_reference> inner_reference
%%
main:
  topdecs EOF     { File $1 }
;

topdecs:
                        { [] }
  | topdec  topdecs     { $1 :: $2 }
;

topdec:
    accmod dec semi_opt                                           { GlobalDeclaration ($1, $2) }
  | STRUCT NAME LPAR struct_params RPAR semi_opt                  { Struct ($2, [], $4) }
  | STRUCT NAME LT typ_vars GT LPAR struct_params RPAR semi_opt   { Struct ($2, $4, $7) }
  | REFERENCE PATH AS NAME semi_opt                               { FileReference($4, $2) }
;

semi_opt:
  {}
  | SEMI {}
;

varmod:
    STABLE { Stable }
  | CONST { Const }
;

accmod:
    INTERNAL { Internal }
  | EXTERNAL { External }
  | ENTRY    { Entry }
;

typ_vars:
    TYPE_VAR                  { [$1] }
  | TYPE_VAR COMMA            { [$1] }
  | TYPE_VAR COMMA typ_vars   { $1 :: $3 }
;

typ_args:
    typ                       { [Some $1] }
  | UNDERSCORE                { [None] }
  | typ COMMA                 { [Some $1] }
  | UNDERSCORE COMMA          { [None] }
  | typ COMMA typ_args        { (Some $1) :: $3 }
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
  | LPAR typ_list RPAR  { T_Routine ([], $2) }
  | LT typ_vars GT LPAR typ_list RPAR  { T_Routine ($2, $5) }
;

typ_list:
   { [] }
  | typ                           { [(Open, $1)] }  
  | varmod typ                    { [($1, $2)] }
  | typ COMMA typ_list            { (Open, $1)::$3 }
  | varmod typ COMMA typ_list     { ($1, $2)::$4 }
;

block:
  LBRACE stmtOrDecSeq RBRACE    { Block $2 }
;

expression:
    reference                               { Reference $1 }
  | value                                   { Value $1 }
  | expression_not_ternary QMARK expression COLON expression { Ternary ($1, $3, $5) }
;

expression_not_ternary:
    reference                               { Reference $1 }
  | value                                   { Value $1 }
  | LPAR expression RPAR                    { $2 }
;

// simple_expression:
//     reference                               { Reference $1 }
//   | simple_value                            { Value $1 }
// ;

reference:
    NAME HASH inner_reference { OtherContext ($1, $3) }
  | inner_reference           { LocalContext $1 }
  | NULL                      { Null }
;

inner_reference:
    NAME                                                    { Access $1 }
  | inner_reference DOT NAME                                { StructAccess ($1, $3) }
  | inner_reference LBRAKE expression RBRAKE                { ArrayAccess ($1, $3) }
;

const_value:
  CSTBOOL                                                 { Bool $1 }
  | CSTINT                                                { Int $1 }
  | CSTCHAR                                               { Char $1 }
;

simple_value:
    LPAR value RPAR                                       { $2 }
  | const_value                                           { $1 }
  | MINUS expression_not_ternary                          { Binary_op ("-", Value (Int 0), $2) }
  | NOT expression_not_ternary                            { Unary_op ("!", $2) }
  | VALUE expression_not_ternary                          { Unary_op ("$", $2) }
  | PIPE inner_reference PIPE                             { ArraySize $2 }
  | READ LT typ GT                                        { GetInput $3 }
  | NEW typ LBRAKE expression RBRAKE                      { NewArray ($2, $4) }
  | LBRAKE arguments RBRAKE                               { ArrayLiteral $2 }
  | CSTSTRING                                             { string_to_array_literal $1 }
  | NEW NAME LPAR arguments RPAR                          { NewStruct ($2, [], $4) }
  | NEW NAME LT typ_args GT LPAR arguments RPAR           { NewStruct ($2, $4, $7) }
  | LBRACE arguments RBRACE                               { StructLiteral $2 }
;

value:
    simple_value { $1 }
  | expression_not_ternary binop expression_not_ternary { Binary_op ($2, $1, $3) }
  | LPAR params RPAR block                    { AnonRoutine ([], $2, $4) }
  | LT typ_vars GT LPAR params RPAR block     { AnonRoutine ($2, $5, $7) }
;

%inline binop:
    LOGIC_AND   { "&&" }
  | LOGIC_OR    { "||" }
  | EQ          { "="  }
  | NEQ         { "!=" }
  | LTEQ        { "<=" }
  | LT          { "<"  }
  | GTEQ        { ">=" }
  | GT          { ">"  }
  | PLUS        { "+"  }
  | TIMES       { "*"  }
  | MINUS       { "-"  }
;

arguments:
                 { [] }
  | arguments1   { $1 }
;

arguments1:
    expression                     { [$1] }
  | expression COMMA               { [$1] }
  | expression COMMA arguments1    { $1 :: $3 }
;

stmtOrDecSeq:
                               { [] }
  | stmtOrDec stmtOrDecSeq     { $1 :: $2 }
;

stmtOrDec:
    stmt                                                     { Statement ($1, $symbolstartpos.pos_lnum) }
  | dec SEMI                                                      { Declaration ($1, $symbolstartpos.pos_lnum) }
;

dec:
    NAME COLON typ                                       { TypeDeclaration (Open, $3, $1) }
  | NAME COLON varmod typ                                { TypeDeclaration ($3, $4, $1) }
  | NAME COLON typ ASSIGNMENT expression                 { AssignDeclaration (Open, Some $3, $1, $5) }
  | NAME COLON varmod typ ASSIGNMENT expression          { AssignDeclaration ($3, Some $4, $1, $6) }
  | NAME COLON ASSIGNMENT expression                     { AssignDeclaration (Open, None, $1, $4) }
  | NAME COLON varmod ASSIGNMENT expression              { AssignDeclaration ($3, None, $1, $5) }
;

stmt:
    stmt1 { $1 }
  | stmt2 { $1 }
;

stmt2:
    IF LPAR expression RPAR stmt1 ELSE stmt2       { If ($3, $5, $7) }
  | IF LPAR expression RPAR stmt                   { If ($3, $5, Block []) }
  | WHEN LPAR expression RPAR LBRACE when_cases RBRACE ELSE stmt2      { transform_when $3 $9 $6 }
  | WHEN LPAR expression RPAR LBRACE when_cases RBRACE                 { transform_when $3 (Block[]) $6 }
  | WHILE LPAR expression RPAR stmt2               { While ($3, $5) }
  | UNTIL LPAR expression RPAR stmt2               { While (Value (Unary_op("!", $3)), $5) }
  | FOR LPAR dec SEMI expression SEMI non_control_flow_stmt RPAR stmt2    { Block([Declaration($3, $symbolstartpos.pos_lnum); Statement(While($5, Block([Statement($9,$symbolstartpos.pos_lnum); Statement($7,$symbolstartpos.pos_lnum);])), $symbolstartpos.pos_lnum);]) }
  | REPEAT LPAR value RPAR stmt2 { 
    let var_name = new_var () in
    Block([
      Declaration(TypeDeclaration(Open, T_Int, var_name), $symbolstartpos.pos_lnum); 
      Statement(While(Value(Binary_op("<", Reference(LocalContext(Access var_name)), Value $3)), 
        Block([
          Statement($5,$symbolstartpos.pos_lnum); 
          Statement(Assign(LocalContext(Access var_name), Value(Binary_op("+", Value(Int 1), Reference(LocalContext(Access var_name))))), $symbolstartpos.pos_lnum);
        ])
      ),$symbolstartpos.pos_lnum);
    ]) 
  }
  | REPEAT stmt2 { While(Value(Bool(true)), $2) }
  | REPEAT LPAR inner_reference RPAR stmt2 { 
    let count_name = new_var () in
    let limit_name = new_var () in
    Block([
      Declaration(AssignDeclaration(Const, Some T_Int, limit_name, Value(Unary_op("$",Reference(LocalContext $3)))), $symbolstartpos.pos_lnum); 
      Declaration(TypeDeclaration(Open, T_Int, count_name), $symbolstartpos.pos_lnum); 
      Statement(While(Value(Binary_op("<", Reference(LocalContext(Access count_name)), Reference(LocalContext(Access limit_name)))), 
        Block([
          Statement($5, $symbolstartpos.pos_lnum); 
          Statement(Assign(LocalContext(Access count_name), Value(Binary_op("+", Value(Int 1), Reference(LocalContext(Access count_name))))), $symbolstartpos.pos_lnum);
        ])
      ), $symbolstartpos.pos_lnum);
    ]) 
  }
;

stmt1: /* No unbalanced if-else */
    block                                              { $1 }
  | IF LPAR expression RPAR stmt1 ELSE stmt1       { If ($3, $5, $7) }
  | WHEN LPAR expression RPAR LBRACE when_cases RBRACE ELSE stmt1      { transform_when $3 $9 $6 }
  | WHILE LPAR expression RPAR stmt1               { While ($3, $5) }
  | UNTIL LPAR expression RPAR stmt1               { While (Value (Unary_op("!", $3)), $5) }
  | FOR LPAR dec SEMI expression SEMI non_control_flow_stmt RPAR stmt1    { Block([Declaration($3, $symbolstartpos.pos_lnum); Statement(While($5, Block([Statement($9,$symbolstartpos.pos_lnum); Statement($7,$symbolstartpos.pos_lnum);])), $symbolstartpos.pos_lnum);]) }
  | REPEAT LPAR value RPAR stmt1 { 
    let var_name = new_var () in
    Block([
      Declaration(TypeDeclaration(Open, T_Int, var_name), $symbolstartpos.pos_lnum); 
      Statement(While(Value(Binary_op("<", Reference(LocalContext(Access var_name)), Value $3)), 
        Block([
          Statement($5,$symbolstartpos.pos_lnum); 
          Statement(Assign(LocalContext(Access var_name), Value(Binary_op("+", Value(Int 1), Reference(LocalContext(Access var_name))))), $symbolstartpos.pos_lnum);
        ])
      ),$symbolstartpos.pos_lnum);
    ]) 
  }
  | REPEAT stmt1 { While(Value(Bool(true)), $2) }
  | REPEAT LPAR inner_reference RPAR stmt1 { 
    let count_name = new_var () in
    let limit_name = new_var () in
    Block([
      Declaration(AssignDeclaration(Const, Some T_Int, limit_name, Value(Unary_op("$",Reference(LocalContext $3)))), $symbolstartpos.pos_lnum); 
      Declaration(TypeDeclaration(Open, T_Int, count_name), $symbolstartpos.pos_lnum); 
      Statement(While(Value(Binary_op("<", Reference(LocalContext(Access count_name)), Reference(LocalContext(Access limit_name)))), 
        Block([
          Statement($5, $symbolstartpos.pos_lnum); 
          Statement(Assign(LocalContext(Access count_name), Value(Binary_op("+", Value(Int 1), Reference(LocalContext(Access count_name))))), $symbolstartpos.pos_lnum);
        ])
      ), $symbolstartpos.pos_lnum);
    ]) 
  }
  | STOP SEMI                                    { Stop }
  | HALT SEMI                                    { Halt }
  | HALT arguments1 SEMI                         { Block[Statement(Print $2, $symbolstartpos.pos_lnum); Statement(Halt, $symbolstartpos.pos_lnum);] }
  | BREAK SEMI                                   { Break }
  | CONTINUE SEMI                                { Continue }
  | non_control_flow_stmt SEMI { $1 }
;

when_case:
  IS LPAR const_value RPAR stmt { (Value $3, $5) }
;

when_cases:
  when_case { [$1] }
  | when_case when_cases { $1::$2 }
;

non_control_flow_stmt:
    reference ASSIGNMENT expression        { Assign ($1, $3) }
  | reference PLUS ASSIGNMENT expression   { Assign ($1, Value(Binary_op("+", Reference $1, $4))) }
  | reference MINUS ASSIGNMENT expression  { Assign ($1, Value(Binary_op("-", Reference $1, $4))) }
  | reference TIMES ASSIGNMENT expression  { Assign ($1, Value(Binary_op("*", Reference $1, $4))) }
  | reference NOT ASSIGNMENT expression    { Assign ($1, Value(Unary_op("!", $4))) }
  | reference LPAR arguments RPAR                      { Call ($1, [], $3) }
  | reference LT typ_args GT LPAR arguments RPAR       { Call ($1, $3, $6) }
  | PRINT arguments1                          { Print $2 }
;

params:
               { [] }
  | params1    { $1 }
;

params1:
    param                  { [$1] }
  | param COMMA            { [$1] }
  | param COMMA params1    { $1 :: $3 }
;

param:
  | NAME COLON typ                  { (Open, $3, $1) }
  | NAME COLON varmod typ           { ($3, $4, $1) }
;



struct_params:
               { [] }
  | struct_params1    { $1 }
;

struct_params1:
    struct_param                        { [$1] }
  | struct_param COMMA                  { [$1] }
  | struct_param COMMA struct_params1   { $1 :: $3 }
;

struct_param:
  | NAME COLON typ                  { (Open, $3, $1) }
  | NAME COLON varmod typ           { ($3, $4, $1) }
;