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

open Parsing;;
let _ = parse_error;;
# 2 "assemblyParser.mly"
    open ProgramRep
# 64 "assemblyParser.ml"
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
  278 (* FETCH_FULL *);
  279 (* FETCH_HALF *);
  280 (* FETCH_SHORT *);
  281 (* FETCH_BYTE *);
  282 (* FIELD_FETCH *);
  283 (* DECLARE_FULL *);
  284 (* DECLARE_HALF *);
  285 (* DECLARE_SHORT *);
  286 (* DECLARE_BYTE *);
  287 (* DECLARE_STRUCT *);
  288 (* ASSIGN_FULL *);
  289 (* ASSIGN_HALF *);
  290 (* ASSIGN_SHORT *);
  291 (* ASSIGN_BYTE *);
  292 (* FIELD_ASSIGN *);
  293 (* INT_ADD *);
  294 (* INT_MUL *);
  295 (* INT_SUB *);
  296 (* INT_EQ *);
  297 (* INT_LT *);
  298 (* BOOL_EQ *);
  299 (* BOOL_NOT *);
  300 (* BOOL_AND *);
  301 (* BOOL_OR *);
  302 (* GETSP *);
  303 (* GETBP *);
  304 (* MODSP *);
  305 (* FREE_VAR *);
  306 (* FREE_VARS *);
  307 (* PRINT_VAR *);
  308 (* PRINT_INT *);
  309 (* PRINT_BOOL *);
  310 (* STACK_FETCH *);
  311 (* BP_FETCH *);
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
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\006\000\006\000\006\000\000\000"

let yylen = "\002\000\
\007\000\007\000\004\000\001\000\000\000\003\000\003\000\001\000\
\000\000\004\000\003\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\003\000\002\000\003\000\002\000\002\000\002\000\003\000\003\000\
\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\060\000\000\000\000\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\000\000\000\000\000\000\000\000\000\000\012\000\
\013\000\000\000\000\000\000\000\000\000\000\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\041\000\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\000\000\050\000\000\000\052\000\
\053\000\054\000\000\000\000\000\000\000\006\000\007\000\000\000\
\000\000\000\000\000\000\011\000\014\000\015\000\016\000\017\000\
\018\000\049\000\051\000\055\000\056\000\000\000\003\000\000\000\
\058\000\059\000\010\000\000\000\000\000\000\000\001\000\002\000"

let yydgoto = "\002\000\
\005\000\008\000\057\000\009\000\058\000\115\000"

let yysindex = "\255\255\
\002\255\000\000\255\254\147\255\000\000\030\255\254\254\040\255\
\000\000\045\255\046\255\147\255\147\255\048\255\049\255\055\255\
\052\255\054\255\147\255\147\255\147\255\147\255\147\255\147\255\
\147\255\147\255\147\255\147\255\147\255\147\255\147\255\147\255\
\147\255\147\255\147\255\147\255\147\255\147\255\147\255\147\255\
\147\255\147\255\147\255\147\255\147\255\147\255\147\255\147\255\
\056\255\147\255\057\255\147\255\147\255\147\255\058\255\059\255\
\063\255\000\000\255\254\255\254\066\255\038\255\147\255\000\000\
\000\000\147\255\147\255\147\255\147\255\147\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\147\255\000\000\147\255\000\000\
\000\000\000\000\147\255\147\255\001\000\000\000\000\000\147\255\
\038\255\038\255\147\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\255\254\000\000\069\255\
\000\000\000\000\000\000\070\255\072\000\073\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\073\255\074\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\074\255\074\255\000\000\000\000\000\000\
\000\000\000\000\074\255\074\255\074\255\074\255\074\255\074\255\
\074\255\074\255\074\255\074\255\074\255\074\255\074\255\074\255\
\074\255\074\255\074\255\074\255\074\255\074\255\074\255\074\255\
\074\255\074\255\074\255\074\255\074\255\074\255\074\255\074\255\
\000\000\074\255\000\000\074\255\074\255\074\255\000\000\000\000\
\000\000\000\000\073\255\073\255\000\000\095\255\074\255\000\000\
\000\000\074\255\074\255\074\255\074\255\074\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\074\255\000\000\074\255\000\000\
\000\000\000\000\074\255\074\255\000\000\000\000\000\000\074\255\
\095\255\095\255\074\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\073\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\206\255\221\255\244\255\246\255\192\255"

let yytablesize = 259
let yytable = "\001\000\
\127\000\064\000\065\000\003\000\004\000\006\000\007\000\060\000\
\071\000\072\000\073\000\074\000\075\000\076\000\077\000\078\000\
\079\000\080\000\081\000\082\000\083\000\084\000\085\000\086\000\
\087\000\088\000\089\000\090\000\091\000\092\000\093\000\094\000\
\095\000\096\000\097\000\098\000\099\000\100\000\059\000\102\000\
\061\000\104\000\105\000\106\000\113\000\114\000\110\000\111\000\
\129\000\130\000\062\000\063\000\116\000\066\000\067\000\117\000\
\118\000\119\000\120\000\121\000\068\000\069\000\070\000\109\000\
\101\000\103\000\107\000\108\000\112\000\133\000\134\000\135\000\
\136\000\005\000\009\000\132\000\128\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\122\000\000\000\123\000\000\000\000\000\057\000\
\124\000\125\000\057\000\057\000\000\000\000\000\000\000\000\000\
\131\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\057\000\057\000\
\057\000\057\000\057\000\057\000\057\000\057\000\010\000\011\000\
\000\000\000\000\000\000\000\000\000\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\126\000"

let yycheck = "\001\000\
\000\000\012\000\013\000\002\001\003\001\007\001\008\001\010\001\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\041\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\009\001\050\000\
\001\001\052\000\053\000\054\000\007\001\008\001\059\000\060\000\
\113\000\114\000\006\001\006\001\063\000\006\001\006\001\066\000\
\067\000\068\000\069\000\070\000\006\001\010\001\009\001\001\001\
\009\001\009\001\009\001\009\001\003\001\001\001\001\001\000\000\
\000\000\001\001\001\001\126\000\112\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\101\000\255\255\103\000\255\255\255\255\001\001\
\107\000\108\000\004\001\005\001\255\255\255\255\255\255\255\255\
\115\000\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\038\001\039\001\040\001\041\001\
\042\001\043\001\044\001\045\001\046\001\047\001\048\001\049\001\
\050\001\051\001\052\001\053\001\054\001\055\001\004\001\005\001\
\255\255\255\255\255\255\255\255\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\046\001\047\001\048\001\049\001\050\001\051\001\052\001\053\001\
\054\001\055\001\255\255\255\255\255\255\255\255\255\255\255\255\
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
  FETCH_FULL\000\
  FETCH_HALF\000\
  FETCH_SHORT\000\
  FETCH_BYTE\000\
  FIELD_FETCH\000\
  DECLARE_FULL\000\
  DECLARE_HALF\000\
  DECLARE_SHORT\000\
  DECLARE_BYTE\000\
  DECLARE_STRUCT\000\
  ASSIGN_FULL\000\
  ASSIGN_HALF\000\
  ASSIGN_SHORT\000\
  ASSIGN_BYTE\000\
  FIELD_ASSIGN\000\
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
# 66 "assemblyParser.mly"
                                                                                                 ( Program (_2, _5) )
# 351 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'program_section) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'global_section) in
    Obj.repr(
# 67 "assemblyParser.mly"
                                                                                                ( Program (_5, _2) )
# 359 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'program_section) in
    Obj.repr(
# 68 "assemblyParser.mly"
                                                       ( Program ([], _2) )
# 366 "assemblyParser.ml"
               : ProgramRep.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 72 "assemblyParser.mly"
                     ( _1 )
# 373 "assemblyParser.ml"
               : 'global_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "assemblyParser.mly"
        ([])
# 379 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 77 "assemblyParser.mly"
                                   ( (G_Int _2) :: _3 )
# 387 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'global_variables) in
    Obj.repr(
# 78 "assemblyParser.mly"
                                     ( (G_Bool _2) :: _3 )
# 395 "assemblyParser.ml"
               : 'global_variables))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 82 "assemblyParser.mly"
            ( _1 )
# 402 "assemblyParser.ml"
               : 'program_section))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "assemblyParser.mly"
        ([])
# 408 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'type_list) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 87 "assemblyParser.mly"
                                         ( (EntryPoint (_2, _3)) :: _4 )
# 417 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 88 "assemblyParser.mly"
                         ( (Label _2) :: _3)
# 425 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 89 "assemblyParser.mly"
                   ( Instruction(0) :: _2 )
# 432 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 90 "assemblyParser.mly"
                   ( Instruction(1) :: _2 )
# 439 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 91 "assemblyParser.mly"
                        ( LabelInstruction(2, _2) :: _3 )
# 447 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 92 "assemblyParser.mly"
                        ( LabelInstruction(3, _2) :: _3 )
# 455 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 93 "assemblyParser.mly"
                           ( LabelInstruction(4, _2) :: _3 )
# 463 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bool) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 94 "assemblyParser.mly"
                                  ( BoolInstruction(5, _2) :: _3 )
# 471 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 95 "assemblyParser.mly"
                                ( IntInstruction(6, _2) :: _3 )
# 479 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 96 "assemblyParser.mly"
                         ( Instruction(7) :: _2 )
# 486 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 97 "assemblyParser.mly"
                         ( Instruction(8) :: _2 )
# 493 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 98 "assemblyParser.mly"
                          ( Instruction(9) :: _2 )
# 500 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 99 "assemblyParser.mly"
                         ( Instruction(10) :: _2 )
# 507 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 100 "assemblyParser.mly"
                         ( Instruction(11) :: _2 )
# 514 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 101 "assemblyParser.mly"
                         ( Instruction(12) :: _2 )
# 521 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 102 "assemblyParser.mly"
                          ( Instruction(13) :: _2 )
# 528 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 103 "assemblyParser.mly"
                         ( Instruction(14) :: _2 )
# 535 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 104 "assemblyParser.mly"
                          ( Instruction(15) :: _2 )
# 542 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 105 "assemblyParser.mly"
                           ( Instruction(16) :: _2 )
# 549 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 106 "assemblyParser.mly"
                           ( Instruction(17) :: _2 )
# 556 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 107 "assemblyParser.mly"
                            ( Instruction(18) :: _2 )
# 563 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 108 "assemblyParser.mly"
                           ( Instruction(19) :: _2 )
# 570 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 109 "assemblyParser.mly"
                             ( Instruction(20) :: _2 )
# 577 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 110 "assemblyParser.mly"
                          ( Instruction(21) :: _2 )
# 584 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 111 "assemblyParser.mly"
                          ( Instruction(22) :: _2 )
# 591 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 112 "assemblyParser.mly"
                           ( Instruction(23) :: _2 )
# 598 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 113 "assemblyParser.mly"
                          ( Instruction(24) :: _2 )
# 605 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 114 "assemblyParser.mly"
                           ( Instruction(25) :: _2 )
# 612 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 115 "assemblyParser.mly"
                      ( Instruction(26) :: _2 )
# 619 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 116 "assemblyParser.mly"
                      ( Instruction(27) :: _2 )
# 626 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 117 "assemblyParser.mly"
                      ( Instruction(28) :: _2 )
# 633 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 118 "assemblyParser.mly"
                     ( Instruction(29) :: _2 )
# 640 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 119 "assemblyParser.mly"
                     ( Instruction(30) :: _2 )
# 647 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 120 "assemblyParser.mly"
                      ( Instruction(31) :: _2 )
# 654 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 121 "assemblyParser.mly"
                       ( Instruction(32) :: _2 )
# 661 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 122 "assemblyParser.mly"
                       ( Instruction(33) :: _2 )
# 668 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 123 "assemblyParser.mly"
                      ( Instruction(34) :: _2 )
# 675 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 124 "assemblyParser.mly"
                    ( Instruction(35) :: _2 )
# 682 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 125 "assemblyParser.mly"
                    ( Instruction(36) :: _2 )
# 689 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 126 "assemblyParser.mly"
                            ( IntInstruction(37, _2) :: _3 )
# 697 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 127 "assemblyParser.mly"
                       ( Instruction(38) :: _2 )
# 704 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 128 "assemblyParser.mly"
                                ( IntInstruction(39, _2) :: _3 )
# 712 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 129 "assemblyParser.mly"
                        ( Instruction(40) :: _2 )
# 719 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 130 "assemblyParser.mly"
                        ( Instruction(41) :: _2 )
# 726 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 131 "assemblyParser.mly"
                         ( Instruction(42) :: _2 )
# 733 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 132 "assemblyParser.mly"
                                  ( IntInstruction(43, _2) :: _3 )
# 741 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 133 "assemblyParser.mly"
                               ( IntInstruction(44, _2) :: _3 )
# 749 "assemblyParser.ml"
               : 'program))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "assemblyParser.mly"
        ([])
# 755 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 138 "assemblyParser.mly"
                    ( T_Int :: _2)
# 762 "assemblyParser.ml"
               : 'type_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'type_list) in
    Obj.repr(
# 139 "assemblyParser.mly"
                     ( T_Bool :: _2)
# 769 "assemblyParser.ml"
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
