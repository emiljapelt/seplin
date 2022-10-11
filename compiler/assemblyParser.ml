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
  | FETCH_ADDR
  | FREE_VAR
  | FREE_VARS
  | PRINT_VAR
  | PRINT_INT
  | PRINT_BOOL
  | STACK_FETCH
  | BP_FETCH
  | STACK_TRANSFER
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "assemblyParser.mly"
    open ProgramRep
# 58 "assemblyParser.ml"
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
  297 (* FETCH_ADDR *);
  298 (* FREE_VAR *);
  299 (* FREE_VARS *);
  300 (* PRINT_VAR *);
  301 (* PRINT_INT *);
  302 (* PRINT_BOOL *);
  303 (* STACK_FETCH *);
  304 (* BP_FETCH *);
  305 (* STACK_TRANSFER *);
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
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\007\000\007\000\004\000\001\000\000\000\003\000\003\000\001\000\
\000\000\004\000\003\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\003\000\
\002\000\002\000\003\000\002\000\002\000\002\000\003\000\003\000\
\003\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\053\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\008\000\000\000\000\000\000\000\000\000\000\000\
\012\000\013\000\000\000\000\000\000\000\000\000\000\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\000\000\041\000\042\000\000\000\
\044\000\045\000\046\000\000\000\000\000\000\000\000\000\006\000\
\007\000\000\000\000\000\000\000\000\000\011\000\014\000\015\000\
\016\000\017\000\018\000\040\000\043\000\047\000\048\000\049\000\
\000\000\003\000\000\000\051\000\052\000\010\000\000\000\000\000\
\000\000\001\000\002\000"

let yydgoto = "\002\000\
\005\000\008\000\050\000\009\000\051\000\101\000"

let yysindex = "\255\255\
\002\255\000\000\255\254\130\255\000\000\021\255\254\254\032\255\
\000\000\037\255\038\255\130\255\130\255\039\255\041\255\042\255\
\044\255\046\255\130\255\130\255\130\255\130\255\130\255\130\255\
\130\255\130\255\130\255\130\255\130\255\130\255\130\255\130\255\
\130\255\130\255\130\255\130\255\130\255\130\255\130\255\047\255\
\130\255\130\255\048\255\130\255\130\255\130\255\049\255\050\255\
\051\255\060\255\000\000\255\254\255\254\059\255\030\255\130\255\
\000\000\000\000\130\255\130\255\130\255\130\255\130\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\130\255\000\000\000\000\130\255\
\000\000\000\000\000\000\130\255\130\255\130\255\001\000\000\000\
\000\000\130\255\030\255\030\255\130\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\255\254\000\000\062\255\000\000\000\000\000\000\063\255\065\000\
\066\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\066\255\067\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\067\255\067\255\000\000\000\000\000\000\
\000\000\000\000\067\255\067\255\067\255\067\255\067\255\067\255\
\067\255\067\255\067\255\067\255\067\255\067\255\067\255\067\255\
\067\255\067\255\067\255\067\255\067\255\067\255\067\255\000\000\
\067\255\067\255\000\000\067\255\067\255\067\255\000\000\000\000\
\000\000\000\000\000\000\066\255\066\255\000\000\084\255\067\255\
\000\000\000\000\067\255\067\255\067\255\067\255\067\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\067\255\000\000\000\000\067\255\
\000\000\000\000\000\000\067\255\067\255\067\255\000\000\000\000\
\000\000\067\255\084\255\084\255\067\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\066\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\212\255\228\255\243\255\246\255\198\255"

let yytablesize = 259
let yytable = "\001\000\
\114\000\057\000\058\000\003\000\004\000\006\000\007\000\053\000\
\064\000\065\000\066\000\067\000\068\000\069\000\070\000\071\000\
\072\000\073\000\074\000\075\000\076\000\077\000\078\000\079\000\
\080\000\081\000\082\000\083\000\084\000\052\000\086\000\087\000\
\054\000\089\000\090\000\091\000\099\000\100\000\096\000\097\000\
\116\000\117\000\055\000\056\000\059\000\102\000\060\000\061\000\
\103\000\104\000\105\000\106\000\107\000\062\000\063\000\085\000\
\088\000\092\000\093\000\094\000\095\000\098\000\120\000\121\000\
\122\000\123\000\005\000\009\000\119\000\115\000\000\000\000\000\
\000\000\000\000\108\000\000\000\000\000\109\000\000\000\000\000\
\000\000\110\000\111\000\112\000\050\000\000\000\000\000\050\000\
\050\000\000\000\118\000\000\000\000\000\000\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\000\000\050\000\050\000\050\000\050\000\
\050\000\050\000\050\000\050\000\050\000\010\000\011\000\000\000\
\000\000\000\000\000\000\000\000\012\000\013\000\014\000\015\000\
\016\000\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\000\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\113\000"

let yycheck = "\001\000\
\000\000\012\000\013\000\002\001\003\001\007\001\008\001\010\001\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\009\001\041\000\042\000\
\001\001\044\000\045\000\046\000\007\001\008\001\052\000\053\000\
\099\000\100\000\006\001\006\001\006\001\056\000\006\001\006\001\
\059\000\060\000\061\000\062\000\063\000\010\001\009\001\009\001\
\009\001\009\001\009\001\009\001\001\001\003\001\001\001\001\001\
\000\000\000\000\001\001\001\001\113\000\098\000\255\255\255\255\
\255\255\255\255\085\000\255\255\255\255\088\000\255\255\255\255\
\255\255\092\000\093\000\094\000\001\001\255\255\255\255\004\001\
\005\001\255\255\101\000\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\255\255\041\001\042\001\043\001\044\001\
\045\001\046\001\047\001\048\001\049\001\004\001\005\001\255\255\
\255\255\255\255\255\255\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\255\255\041\001\042\001\043\001\044\001\045\001\046\001\
\047\001\048\001\049\001\255\255\255\255\255\255\255\255\255\255\
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
  FETCH_ADDR\000\
  FREE_VAR\000\
  FREE_VARS\000\
  PRINT_VAR\000\
  PRINT_INT\000\
  PRINT_BOOL\000\
  STACK_FETCH\000\
  BP_FETCH\000\
  STACK_TRANSFER\000\
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
# 60 "assemblyParser.mly"
                                                                                                 ( Program (_2, _5) )
# 328 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'program_section) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'global_section) in
    Obj.repr(
# 61 "assemblyParser.mly"
                                                                                                ( Program (_5, _2) )
# 336 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'program_section) in
    Obj.repr(
# 62 "assemblyParser.mly"
                                                       ( Program ([], _2) )
# 343 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 66 "assemblyParser.mly"
                     ( _1 )
# 350 "assemblyParser.ml"
               : 'global_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "assemblyParser.mly"
        ([])
# 356 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 71 "assemblyParser.mly"
                                   ( (G_Int _2) :: _3 )
# 364 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 72 "assemblyParser.mly"
                                     ( (G_Bool _2) :: _3 )
# 372 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 76 "assemblyParser.mly"
            ( _1 )
# 379 "assemblyParser.ml"
               : 'program_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "assemblyParser.mly"
        ([])
# 385 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 81 "assemblyParser.mly"
                                         ( (EntryPoint (_2, _3)) :: _4 )
# 394 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 82 "assemblyParser.mly"
                         ( (Label _2) :: _3)
# 402 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 83 "assemblyParser.mly"
                   ( (Instruction(0)) :: _2 )
# 409 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 84 "assemblyParser.mly"
                   ( (Instruction(1)) :: _2 )
# 416 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 85 "assemblyParser.mly"
                        ( (LabelInstruction(2, _2)) :: _3 )
# 424 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 86 "assemblyParser.mly"
                        ( (LabelInstruction(3, _2)) :: _3 )
# 432 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 87 "assemblyParser.mly"
                           ( (LabelInstruction(4, _2)) :: _3 )
# 440 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 88 "assemblyParser.mly"
                                  ( BoolInstruction(5, _2) :: _3 )
# 448 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 89 "assemblyParser.mly"
                                ( IntInstruction(6, _2) :: _3 )
# 456 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 90 "assemblyParser.mly"
                         ( Instruction(7) :: _2 )
# 463 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 91 "assemblyParser.mly"
                         ( Instruction(8) :: _2 )
# 470 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 92 "assemblyParser.mly"
                          ( Instruction(9) :: _2 )
# 477 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 93 "assemblyParser.mly"
                         ( Instruction(10) :: _2 )
# 484 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 94 "assemblyParser.mly"
                         ( Instruction(11) :: _2 )
# 491 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 95 "assemblyParser.mly"
                        ( Instruction(12) :: _2 )
# 498 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 96 "assemblyParser.mly"
                           ( Instruction(13) :: _2 )
# 505 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 97 "assemblyParser.mly"
                          ( Instruction(14) :: _2 )
# 512 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 98 "assemblyParser.mly"
                          ( Instruction(15) :: _2 )
# 519 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 99 "assemblyParser.mly"
                         ( Instruction(16) :: _2 )
# 526 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 100 "assemblyParser.mly"
                      ( Instruction(17) :: _2 )
# 533 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 101 "assemblyParser.mly"
                      (Instruction(18) :: _2 )
# 540 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 102 "assemblyParser.mly"
                      ( Instruction(19) :: _2 )
# 547 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 103 "assemblyParser.mly"
                     ( Instruction(20) :: _2 )
# 554 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 104 "assemblyParser.mly"
                     ( Instruction(21) :: _2 )
# 561 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 105 "assemblyParser.mly"
                      ( Instruction(22) :: _2 )
# 568 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 106 "assemblyParser.mly"
                       ( Instruction(23) :: _2 )
# 575 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 107 "assemblyParser.mly"
                       ( Instruction(24) :: _2 )
# 582 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 108 "assemblyParser.mly"
                      ( Instruction(25) :: _2 )
# 589 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 109 "assemblyParser.mly"
                    ( Instruction(26) :: _2 )
# 596 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 110 "assemblyParser.mly"
                    ( Instruction(27) :: _2 )
# 603 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 111 "assemblyParser.mly"
                            ( IntInstruction(28, _2) :: _3 )
# 611 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 112 "assemblyParser.mly"
                         ( Instruction(29) :: _2 )
# 618 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 113 "assemblyParser.mly"
                       ( Instruction(30) :: _2 )
# 625 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 114 "assemblyParser.mly"
                                ( IntInstruction(31, _2) :: _3 )
# 633 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 115 "assemblyParser.mly"
                        ( Instruction(32) :: _2 )
# 640 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 116 "assemblyParser.mly"
                        ( Instruction(33) :: _2 )
# 647 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 117 "assemblyParser.mly"
                         ( Instruction(34) :: _2 )
# 654 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 118 "assemblyParser.mly"
                                  ( IntInstruction(35, _2) :: _3 )
# 662 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 119 "assemblyParser.mly"
                               ( IntInstruction(36, _2) :: _3 )
# 670 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 120 "assemblyParser.mly"
                                     ( IntInstruction(37, _2) :: _3)
# 678 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "assemblyParser.mly"
        ([])
# 684 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 125 "assemblyParser.mly"
                    ( T_Int :: _2)
# 691 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 126 "assemblyParser.mly"
                     ( T_Bool :: _2)
# 698 "assemblyParser.ml"
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
