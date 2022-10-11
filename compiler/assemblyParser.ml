type token =
  | SECTION_END
  | GLOBAL_SECTION
  | PROGRAM_SECTION
  | ENTRY_POINT
  | LABEL
  | NAME of (string)
  | INT
  | BOOL
  | LOCKED
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
  | LOCK
  | UNLOCK
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "assemblyParser.mly"
    open ProgramRep
# 60 "assemblyParser.ml"
let yytransl_const = [|
  257 (* SECTION_END *);
  258 (* GLOBAL_SECTION *);
  259 (* PROGRAM_SECTION *);
  260 (* ENTRY_POINT *);
  261 (* LABEL *);
  263 (* INT *);
  264 (* BOOL *);
  265 (* LOCKED *);
  268 (* HALT *);
  269 (* STOP *);
  270 (* CALL *);
  271 (* GOTO *);
  272 (* IF_TRUE *);
  273 (* PLACE_BOOL *);
  274 (* PLACE_INT *);
  275 (* CLONE_FULL *);
  276 (* CLONE_HALF *);
  277 (* CLONE_SHORT *);
  278 (* CLONE_BYTE *);
  279 (* FETCH_BOOL *);
  280 (* FETCH_INT *);
  281 (* DECLARE_BOOL *);
  282 (* DECLARE_INT *);
  283 (* ASSIGN_BOOL *);
  284 (* ASSIGN_INT *);
  285 (* INT_ADD *);
  286 (* INT_MUL *);
  287 (* INT_SUB *);
  288 (* INT_EQ *);
  289 (* INT_LT *);
  290 (* BOOL_EQ *);
  291 (* BOOL_NOT *);
  292 (* BOOL_AND *);
  293 (* BOOL_OR *);
  294 (* GETSP *);
  295 (* GETBP *);
  296 (* MODSP *);
  297 (* CLONE_FRAME *);
  298 (* FETCH_ADDR *);
  299 (* FREE_VAR *);
  300 (* FREE_VARS *);
  301 (* PRINT_VAR *);
  302 (* PRINT_INT *);
  303 (* PRINT_BOOL *);
  304 (* STACK_FETCH *);
  305 (* BP_FETCH *);
  306 (* LOCK *);
  307 (* UNLOCK *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  262 (* NAME *);
  266 (* CST_INT *);
  267 (* CST_BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\002\000\004\000\004\000\004\000\004\000\
\004\000\003\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\007\000\007\000\004\000\001\000\000\000\004\000\003\000\004\000\
\003\000\001\000\000\000\004\000\003\000\002\000\002\000\003\000\
\003\000\003\000\003\000\003\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\003\000\002\000\002\000\002\000\003\000\002\000\002\000\
\002\000\003\000\003\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\055\000\000\000\000\000\000\000\
\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\014\000\015\000\000\000\000\000\000\000\
\000\000\000\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\000\000\
\043\000\044\000\045\000\000\000\047\000\048\000\049\000\000\000\
\000\000\000\000\007\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\013\000\016\000\017\000\018\000\019\000\020\000\
\042\000\046\000\050\000\051\000\000\000\003\000\006\000\008\000\
\000\000\053\000\054\000\012\000\000\000\000\000\000\000\001\000\
\002\000"

let yydgoto = "\002\000\
\005\000\009\000\051\000\010\000\052\000\106\000"

let yysindex = "\255\255\
\005\255\000\000\253\254\134\255\000\000\020\255\023\255\034\255\
\037\255\000\000\041\255\043\255\134\255\134\255\044\255\050\255\
\051\255\047\255\049\255\134\255\134\255\134\255\134\255\134\255\
\134\255\134\255\134\255\134\255\134\255\134\255\134\255\134\255\
\134\255\134\255\134\255\134\255\134\255\134\255\134\255\134\255\
\052\255\134\255\134\255\134\255\053\255\134\255\134\255\134\255\
\054\255\055\255\059\255\000\000\253\254\253\254\056\255\057\255\
\058\255\036\255\134\255\000\000\000\000\134\255\134\255\134\255\
\134\255\134\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\134\255\
\000\000\000\000\000\000\134\255\000\000\000\000\000\000\134\255\
\134\255\001\000\000\000\000\000\253\254\253\254\134\255\036\255\
\036\255\134\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\253\254\000\000\000\000\000\000\
\066\255\000\000\000\000\000\000\068\255\070\000\071\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\071\255\072\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\072\255\072\255\000\000\000\000\
\000\000\000\000\000\000\072\255\072\255\072\255\072\255\072\255\
\072\255\072\255\072\255\072\255\072\255\072\255\072\255\072\255\
\072\255\072\255\072\255\072\255\072\255\072\255\072\255\072\255\
\000\000\072\255\072\255\072\255\000\000\072\255\072\255\072\255\
\000\000\000\000\000\000\000\000\071\255\071\255\000\000\000\000\
\000\000\088\255\072\255\000\000\000\000\072\255\072\255\072\255\
\072\255\072\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\072\255\
\000\000\000\000\000\000\072\255\000\000\000\000\000\000\072\255\
\072\255\000\000\000\000\000\000\071\255\071\255\072\255\088\255\
\088\255\072\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\071\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\213\255\228\255\242\255\245\255\197\255"

let yytablesize = 259
let yytable = "\001\000\
\118\000\060\000\061\000\006\000\007\000\008\000\003\000\004\000\
\067\000\068\000\069\000\070\000\071\000\072\000\073\000\074\000\
\075\000\076\000\077\000\078\000\079\000\080\000\081\000\082\000\
\083\000\084\000\085\000\086\000\087\000\053\000\089\000\090\000\
\091\000\054\000\093\000\094\000\095\000\057\000\099\000\100\000\
\055\000\056\000\104\000\105\000\122\000\123\000\058\000\107\000\
\059\000\062\000\108\000\109\000\110\000\111\000\112\000\063\000\
\064\000\065\000\066\000\098\000\103\000\088\000\092\000\096\000\
\097\000\101\000\126\000\102\000\127\000\128\000\129\000\005\000\
\011\000\125\000\121\000\000\000\113\000\000\000\000\000\000\000\
\114\000\000\000\000\000\000\000\115\000\116\000\119\000\120\000\
\052\000\000\000\000\000\052\000\052\000\000\000\124\000\000\000\
\000\000\000\000\000\000\052\000\052\000\052\000\052\000\052\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
\052\000\052\000\052\000\052\000\052\000\052\000\052\000\052\000\
\052\000\011\000\012\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\117\000"

let yycheck = "\001\000\
\000\000\013\000\014\000\007\001\008\001\009\001\002\001\003\001\
\020\000\021\000\022\000\023\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\033\000\034\000\035\000\
\036\000\037\000\038\000\039\000\040\000\010\001\042\000\043\000\
\044\000\011\001\046\000\047\000\048\000\001\001\053\000\054\000\
\007\001\008\001\007\001\008\001\104\000\105\000\006\001\059\000\
\006\001\006\001\062\000\063\000\064\000\065\000\066\000\006\001\
\006\001\011\001\010\001\001\001\003\001\010\001\010\001\010\001\
\010\001\010\001\001\001\011\001\001\001\000\000\000\000\001\001\
\001\001\117\000\103\000\255\255\088\000\255\255\255\255\255\255\
\092\000\255\255\255\255\255\255\096\000\097\000\101\000\102\000\
\001\001\255\255\255\255\004\001\005\001\255\255\106\000\255\255\
\255\255\255\255\255\255\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001\044\001\045\001\046\001\047\001\048\001\
\049\001\004\001\005\001\255\255\255\255\255\255\255\255\255\255\
\255\255\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\042\001\
\043\001\044\001\045\001\046\001\047\001\048\001\049\001\255\255\
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
  LOCKED\000\
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
  LOCK\000\
  UNLOCK\000\
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
# 62 "assemblyParser.mly"
                                                                                                 ( Program (_2, _5) )
# 337 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'program_section) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'global_section) in
    Obj.repr(
# 63 "assemblyParser.mly"
                                                                                                ( Program (_5, _2) )
# 345 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'program_section) in
    Obj.repr(
# 64 "assemblyParser.mly"
                                                       ( Program ([], _2) )
# 352 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 68 "assemblyParser.mly"
                     ( _1 )
# 359 "assemblyParser.ml"
               : 'global_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "assemblyParser.mly"
        ([])
# 365 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 73 "assemblyParser.mly"
                                          ( (G_Int (true, _3)) :: _4 )
# 373 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 74 "assemblyParser.mly"
                                   ( (G_Int (false, _2)) :: _3 )
# 381 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 75 "assemblyParser.mly"
                                            ( (G_Bool (true, _3)) :: _4 )
# 389 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 76 "assemblyParser.mly"
                                     ( (G_Bool (false, _2)) :: _3 )
# 397 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 80 "assemblyParser.mly"
            ( _1 )
# 404 "assemblyParser.ml"
               : 'program_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "assemblyParser.mly"
        ([])
# 410 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 85 "assemblyParser.mly"
                                         ( (EntryPoint (_2, _3)) :: _4 )
# 419 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 86 "assemblyParser.mly"
                         ( (Label _2) :: _3)
# 427 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 87 "assemblyParser.mly"
                   ( (Instruction(0)) :: _2 )
# 434 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 88 "assemblyParser.mly"
                   ( (Instruction(1)) :: _2 )
# 441 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 89 "assemblyParser.mly"
                        ( (LabelInstruction(2, _2)) :: _3 )
# 449 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 90 "assemblyParser.mly"
                        ( (LabelInstruction(3, _2)) :: _3 )
# 457 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 91 "assemblyParser.mly"
                           ( (LabelInstruction(4, _2)) :: _3 )
# 465 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 92 "assemblyParser.mly"
                                  ( BoolInstruction(5, _2) :: _3 )
# 473 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 93 "assemblyParser.mly"
                                ( IntInstruction(6, _2) :: _3 )
# 481 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 94 "assemblyParser.mly"
                         ( Instruction(7) :: _2 )
# 488 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 95 "assemblyParser.mly"
                         ( Instruction(8) :: _2 )
# 495 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 96 "assemblyParser.mly"
                          ( Instruction(9) :: _2 )
# 502 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 97 "assemblyParser.mly"
                         ( Instruction(10) :: _2 )
# 509 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 98 "assemblyParser.mly"
                         ( Instruction(11) :: _2 )
# 516 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 99 "assemblyParser.mly"
                        ( Instruction(12) :: _2 )
# 523 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 100 "assemblyParser.mly"
                           ( Instruction(13) :: _2 )
# 530 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 101 "assemblyParser.mly"
                          ( Instruction(14) :: _2 )
# 537 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 102 "assemblyParser.mly"
                          ( Instruction(15) :: _2 )
# 544 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 103 "assemblyParser.mly"
                         ( Instruction(16) :: _2 )
# 551 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 104 "assemblyParser.mly"
                      ( Instruction(17) :: _2 )
# 558 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 105 "assemblyParser.mly"
                      (Instruction(18) :: _2 )
# 565 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 106 "assemblyParser.mly"
                      ( Instruction(19) :: _2 )
# 572 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 107 "assemblyParser.mly"
                     ( Instruction(20) :: _2 )
# 579 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 108 "assemblyParser.mly"
                     ( Instruction(21) :: _2 )
# 586 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 109 "assemblyParser.mly"
                      ( Instruction(22) :: _2 )
# 593 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 110 "assemblyParser.mly"
                       ( Instruction(23) :: _2 )
# 600 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 111 "assemblyParser.mly"
                       ( Instruction(24) :: _2 )
# 607 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 112 "assemblyParser.mly"
                      ( Instruction(25) :: _2 )
# 614 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 113 "assemblyParser.mly"
                    ( Instruction(26) :: _2 )
# 621 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 114 "assemblyParser.mly"
                    ( Instruction(27) :: _2 )
# 628 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 115 "assemblyParser.mly"
                            ( IntInstruction(28, _2) :: _3 )
# 636 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 116 "assemblyParser.mly"
                          ( Instruction(29) :: _2 )
# 643 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 117 "assemblyParser.mly"
                         ( Instruction(30) :: _2 )
# 650 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 118 "assemblyParser.mly"
                       ( Instruction(31) :: _2 )
# 657 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 119 "assemblyParser.mly"
                                ( IntInstruction(32, _2) :: _3 )
# 665 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 120 "assemblyParser.mly"
                        ( Instruction(33) :: _2 )
# 672 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 121 "assemblyParser.mly"
                        ( Instruction(34) :: _2 )
# 679 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 122 "assemblyParser.mly"
                         ( Instruction(35) :: _2 )
# 686 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 123 "assemblyParser.mly"
                                  ( IntInstruction(36, _2) :: _3 )
# 694 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 124 "assemblyParser.mly"
                               ( IntInstruction(37, _2) :: _3 )
# 702 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "assemblyParser.mly"
        ([])
# 708 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 129 "assemblyParser.mly"
                    ( T_Int :: _2)
# 715 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 130 "assemblyParser.mly"
                     ( T_Bool :: _2)
# 722 "assemblyParser.ml"
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
