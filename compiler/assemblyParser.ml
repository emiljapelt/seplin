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
  | FETCH_BOOL
  | FETCH_INT
  | DECLARE_BOOL
  | DECLARE_INT
  | ASSIGN_BOOL
  | ASSIGN_INT
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
  | CLONE_FRAME
  | FREE_VAR
  | FREE_VARS
  | PRINT_VAR
  | PRINT_INT
  | PRINT_BOOL
  | STACK_FETCH
  | BP_FETCH
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "assemblyParser.mly"
    open ProgramRep
# 56 "assemblyParser.ml"
let yytransl_const = [|
  257 (* SECTION_END *);
  258 (* GLOBAL_SECTION *);
  259 (* PROGRAM_SECTION *);
  260 (* ENTRY_POINT *);
  261 (* LABEL *);
  263 (* INT *);
  264 (* BOOL *);
  267 (* HALT *);
  268 (* STOP *);
  269 (* CALL *);
  270 (* GOTO *);
  271 (* IF_TRUE *);
  272 (* PLACE_BOOL *);
  273 (* PLACE_INT *);
  274 (* CLONE_FULL *);
  275 (* CLONE_HALF *);
  276 (* CLONE_SHORT *);
  277 (* CLONE_BYTE *);
  278 (* FETCH_BOOL *);
  279 (* FETCH_INT *);
  280 (* DECLARE_BOOL *);
  281 (* DECLARE_INT *);
  282 (* ASSIGN_BOOL *);
  283 (* ASSIGN_INT *);
  284 (* INT_ADD *);
  285 (* INT_MUL *);
  286 (* INT_SUB *);
  287 (* INT_EQ *);
  288 (* INT_LT *);
  289 (* BOOL_EQ *);
  290 (* BOOL_NOT *);
  291 (* BOOL_AND *);
  292 (* BOOL_OR *);
  293 (* GETSP *);
  294 (* GETBP *);
  295 (* MODSP *);
  296 (* CLONE_FRAME *);
  297 (* FREE_VAR *);
  298 (* FREE_VARS *);
  299 (* PRINT_VAR *);
  300 (* PRINT_INT *);
  301 (* PRINT_BOOL *);
  302 (* STACK_FETCH *);
  303 (* BP_FETCH *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  262 (* NAME *);
  265 (* CST_INT *);
  266 (* CST_BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\004\000\004\000\004\000\003\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\006\000\
\006\000\006\000\000\000"

let yylen = "\002\000\
\007\000\007\000\004\000\001\000\000\000\003\000\003\000\001\000\
\000\000\004\000\003\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\002\000\003\000\002\000\002\000\002\000\003\000\003\000\000\000\
\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\051\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\012\000\013\000\
\000\000\000\000\000\000\000\000\000\000\019\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\000\000\041\000\000\000\043\000\044\000\045\000\
\000\000\000\000\000\000\006\000\007\000\000\000\000\000\000\000\
\000\000\011\000\014\000\015\000\016\000\017\000\018\000\040\000\
\042\000\046\000\047\000\000\000\003\000\000\000\049\000\050\000\
\010\000\000\000\000\000\000\000\001\000\002\000"

let yydgoto = "\002\000\
\005\000\008\000\048\000\009\000\049\000\097\000"

let yysindex = "\255\255\
\002\255\000\000\255\254\121\255\000\000\021\255\254\254\031\255\
\000\000\036\255\037\255\121\255\121\255\039\255\040\255\046\255\
\043\255\045\255\121\255\121\255\121\255\121\255\121\255\121\255\
\121\255\121\255\121\255\121\255\121\255\121\255\121\255\121\255\
\121\255\121\255\121\255\121\255\121\255\121\255\121\255\047\255\
\121\255\048\255\121\255\121\255\121\255\049\255\050\255\054\255\
\000\000\255\254\255\254\057\255\029\255\121\255\000\000\000\000\
\121\255\121\255\121\255\121\255\121\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\121\255\000\000\121\255\000\000\000\000\000\000\
\121\255\121\255\001\000\000\000\000\000\121\255\029\255\029\255\
\121\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\255\254\000\000\060\255\000\000\000\000\
\000\000\061\255\063\000\064\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\064\255\065\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\065\255\065\255\000\000\000\000\000\000\
\000\000\000\000\065\255\065\255\065\255\065\255\065\255\065\255\
\065\255\065\255\065\255\065\255\065\255\065\255\065\255\065\255\
\065\255\065\255\065\255\065\255\065\255\065\255\065\255\000\000\
\065\255\000\000\065\255\065\255\065\255\000\000\000\000\000\000\
\000\000\064\255\064\255\000\000\077\255\065\255\000\000\000\000\
\065\255\065\255\065\255\065\255\065\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\065\255\000\000\065\255\000\000\000\000\000\000\
\065\255\065\255\000\000\000\000\000\000\065\255\077\255\077\255\
\065\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\064\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\215\255\230\255\244\255\246\255\201\255"

let yytablesize = 259
let yytable = "\001\000\
\109\000\055\000\056\000\003\000\004\000\006\000\007\000\051\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\072\000\073\000\074\000\075\000\076\000\077\000\
\078\000\079\000\080\000\081\000\082\000\050\000\084\000\052\000\
\086\000\087\000\088\000\095\000\096\000\092\000\093\000\111\000\
\112\000\053\000\054\000\098\000\057\000\058\000\099\000\100\000\
\101\000\102\000\103\000\059\000\060\000\061\000\091\000\083\000\
\085\000\089\000\090\000\094\000\115\000\116\000\117\000\118\000\
\005\000\009\000\114\000\110\000\000\000\000\000\000\000\000\000\
\104\000\000\000\105\000\000\000\000\000\048\000\106\000\107\000\
\048\000\048\000\000\000\000\000\000\000\000\000\113\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\000\000\048\000\048\000\048\000\
\048\000\048\000\048\000\048\000\010\000\011\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\014\000\015\000\016\000\
\017\000\018\000\019\000\020\000\021\000\022\000\023\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\000\000\041\000\042\000\043\000\044\000\045\000\046\000\047\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\108\000"

let yycheck = "\001\000\
\000\000\012\000\013\000\002\001\003\001\007\001\008\001\010\001\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\009\001\041\000\001\001\
\043\000\044\000\045\000\007\001\008\001\050\000\051\000\095\000\
\096\000\006\001\006\001\054\000\006\001\006\001\057\000\058\000\
\059\000\060\000\061\000\006\001\010\001\009\001\001\001\009\001\
\009\001\009\001\009\001\003\001\001\001\001\001\000\000\000\000\
\001\001\001\001\108\000\094\000\255\255\255\255\255\255\255\255\
\083\000\255\255\085\000\255\255\255\255\001\001\089\000\090\000\
\004\001\005\001\255\255\255\255\255\255\255\255\097\000\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\039\001\255\255\041\001\042\001\043\001\
\044\001\045\001\046\001\047\001\004\001\005\001\255\255\255\255\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\255\255\041\001\042\001\043\001\044\001\045\001\046\001\047\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\002\001"

let yynames_const = "\
  SECTION_END\000\
  GLOBAL_SECTION\000\
  PROGRAM_SECTION\000\
  ENTRY_POINT\000\
  LABEL\000\
  INT\000\
  BOOL\000\
  HALT\000\
  STOP\000\
  CALL\000\
  GOTO\000\
  IF_TRUE\000\
  PLACE_BOOL\000\
  PLACE_INT\000\
  CLONE_FULL\000\
  CLONE_HALF\000\
  CLONE_SHORT\000\
  CLONE_BYTE\000\
  FETCH_BOOL\000\
  FETCH_INT\000\
  DECLARE_BOOL\000\
  DECLARE_INT\000\
  ASSIGN_BOOL\000\
  ASSIGN_INT\000\
  INT_ADD\000\
  INT_MUL\000\
  INT_SUB\000\
  INT_EQ\000\
  INT_LT\000\
  BOOL_EQ\000\
  BOOL_NOT\000\
  BOOL_AND\000\
  BOOL_OR\000\
  GETSP\000\
  GETBP\000\
  MODSP\000\
  CLONE_FRAME\000\
  FREE_VAR\000\
  FREE_VARS\000\
  PRINT_VAR\000\
  PRINT_INT\000\
  PRINT_BOOL\000\
  STACK_FETCH\000\
  BP_FETCH\000\
  EOF\000\
  "

let yynames_block = "\
  NAME\000\
  CST_INT\000\
  CST_BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'global_section) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'program_section) in
    Obj.repr(
# 58 "assemblyParser.mly"
                                                                                                 ( Program (_2, _5) )
# 319 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'program_section) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'global_section) in
    Obj.repr(
# 59 "assemblyParser.mly"
                                                                                                ( Program (_5, _2) )
# 327 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'program_section) in
    Obj.repr(
# 60 "assemblyParser.mly"
                                                       ( Program ([], _2) )
# 334 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 64 "assemblyParser.mly"
                     ( _1 )
# 341 "assemblyParser.ml"
               : 'global_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "assemblyParser.mly"
        ([])
# 347 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 69 "assemblyParser.mly"
                                   ( (G_Int _2) :: _3 )
# 355 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 70 "assemblyParser.mly"
                                     ( (G_Bool _2) :: _3 )
# 363 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 74 "assemblyParser.mly"
            ( _1 )
# 370 "assemblyParser.ml"
               : 'program_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "assemblyParser.mly"
        ([])
# 376 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 79 "assemblyParser.mly"
                                         ( (EntryPoint (_2, _3)) :: _4 )
# 385 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 80 "assemblyParser.mly"
                         ( (Label _2) :: _3)
# 393 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 81 "assemblyParser.mly"
                   ( (Instruction(0)) :: _2 )
# 400 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 82 "assemblyParser.mly"
                   ( (Instruction(1)) :: _2 )
# 407 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 83 "assemblyParser.mly"
                        ( (LabelInstruction(2, _2)) :: _3 )
# 415 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 84 "assemblyParser.mly"
                        ( (LabelInstruction(3, _2)) :: _3 )
# 423 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 85 "assemblyParser.mly"
                           ( (LabelInstruction(4, _2)) :: _3 )
# 431 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 86 "assemblyParser.mly"
                                  ( BoolInstruction(5, _2) :: _3 )
# 439 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 87 "assemblyParser.mly"
                                ( IntInstruction(6, _2) :: _3 )
# 447 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 88 "assemblyParser.mly"
                         ( Instruction(7) :: _2 )
# 454 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 89 "assemblyParser.mly"
                         ( Instruction(8) :: _2 )
# 461 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 90 "assemblyParser.mly"
                          ( Instruction(9) :: _2 )
# 468 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 91 "assemblyParser.mly"
                         ( Instruction(10) :: _2 )
# 475 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 92 "assemblyParser.mly"
                         ( Instruction(11) :: _2 )
# 482 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 93 "assemblyParser.mly"
                        ( Instruction(12) :: _2 )
# 489 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 94 "assemblyParser.mly"
                           ( Instruction(13) :: _2 )
# 496 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 95 "assemblyParser.mly"
                          ( Instruction(14) :: _2 )
# 503 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 96 "assemblyParser.mly"
                          ( Instruction(15) :: _2 )
# 510 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 97 "assemblyParser.mly"
                         ( Instruction(16) :: _2 )
# 517 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 98 "assemblyParser.mly"
                      ( Instruction(17) :: _2 )
# 524 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 99 "assemblyParser.mly"
                      (Instruction(18) :: _2 )
# 531 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 100 "assemblyParser.mly"
                      ( Instruction(19) :: _2 )
# 538 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 101 "assemblyParser.mly"
                     ( Instruction(20) :: _2 )
# 545 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 102 "assemblyParser.mly"
                     ( Instruction(21) :: _2 )
# 552 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 103 "assemblyParser.mly"
                      ( Instruction(22) :: _2 )
# 559 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 104 "assemblyParser.mly"
                       ( Instruction(23) :: _2 )
# 566 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 105 "assemblyParser.mly"
                       ( Instruction(24) :: _2 )
# 573 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 106 "assemblyParser.mly"
                      ( Instruction(25) :: _2 )
# 580 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 107 "assemblyParser.mly"
                    ( Instruction(26) :: _2 )
# 587 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 108 "assemblyParser.mly"
                    ( Instruction(27) :: _2 )
# 594 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 109 "assemblyParser.mly"
                            ( IntInstruction(28, _2) :: _3 )
# 602 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 110 "assemblyParser.mly"
                       ( Instruction(29) :: _2 )
# 609 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 111 "assemblyParser.mly"
                                ( IntInstruction(30, _2) :: _3 )
# 617 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 112 "assemblyParser.mly"
                        ( Instruction(31) :: _2 )
# 624 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 113 "assemblyParser.mly"
                        ( Instruction(32) :: _2 )
# 631 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 114 "assemblyParser.mly"
                         ( Instruction(33) :: _2 )
# 638 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 115 "assemblyParser.mly"
                                  ( IntInstruction(34, _2) :: _3 )
# 646 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 116 "assemblyParser.mly"
                               ( IntInstruction(35, _2) :: _3 )
# 654 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    Obj.repr(
# 120 "assemblyParser.mly"
        ([])
# 660 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 121 "assemblyParser.mly"
                    ( T_Int :: _2)
# 667 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 122 "assemblyParser.mly"
                     ( T_Bool :: _2)
# 674 "assemblyParser.ml"
               : 'type_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ProgramRep.program)
