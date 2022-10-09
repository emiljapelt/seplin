type token =
  | CSTINT of (int)
  | INT
  | CSTBOOL of (bool)
  | BOOL
  | INTERNAL
  | EXTERNAL
  | NAME of (string)
  | ASSIGNMENT
  | LPAR
  | RPAR
  | LBRACE
  | RBRACE
  | LBRAKE
  | RBRAKE
  | STOP
  | PLUS
  | MINUS
  | EQ
  | AND
  | OR
  | NOT
  | COMMA
  | SEMI
  | EOF
  | IF
  | ELSE
  | ROUTINE
  | LOCKED
  | PRINT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Absyn
  open ProgramRep
# 38 "parser.ml"
let yytransl_const = [|
  258 (* INT *);
  260 (* BOOL *);
  261 (* INTERNAL *);
  262 (* EXTERNAL *);
  264 (* ASSIGNMENT *);
  265 (* LPAR *);
  266 (* RPAR *);
  267 (* LBRACE *);
  268 (* RBRACE *);
  269 (* LBRAKE *);
  270 (* RBRAKE *);
  271 (* STOP *);
  272 (* PLUS *);
  273 (* MINUS *);
  274 (* EQ *);
  275 (* AND *);
  276 (* OR *);
  277 (* NOT *);
  278 (* COMMA *);
  279 (* SEMI *);
    0 (* EOF *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* ROUTINE *);
  283 (* LOCKED *);
  284 (* PRINT *);
    0|]

let yytransl_block = [|
  257 (* CSTINT *);
  259 (* CSTBOOL *);
  263 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\004\000\
\004\000\007\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\009\000\009\000\009\000\009\000\
\010\000\010\000\011\000\011\000\008\000\008\000\012\000\012\000\
\012\000\013\000\013\000\013\000\013\000\006\000\006\000\014\000\
\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\005\000\006\000\007\000\007\000\001\000\
\001\000\003\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\002\000\003\000\004\000\001\000\002\000\
\000\000\001\000\001\000\003\000\000\000\002\000\001\000\005\000\
\006\000\002\000\001\000\007\000\005\000\000\000\001\000\001\000\
\003\000\003\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\000\000\000\000\000\000\045\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\011\000\013\000\000\000\000\000\000\000\000\000\000\000\
\039\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\004\000\000\000\043\000\000\000\000\000\000\000\
\005\000\000\000\000\000\000\000\000\000\000\000\044\000\000\000\
\006\000\041\000\007\000\000\000\023\000\000\000\000\000\000\000\
\000\000\035\000\000\000\000\000\000\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\034\000\030\000\000\000\
\000\000\000\000\026\000\000\000\000\000\000\000\000\000\022\000\
\000\000\000\000\000\000\028\000\000\000\000\000\032\000\000\000\
\033\000\036\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\031\000\081\000\032\000\066\000\067\000\
\068\000\082\000\083\000\069\000\070\000\033\000\034\000"

let yysindex = "\004\000\
\008\255\000\000\000\000\000\000\231\254\245\254\020\255\000\000\
\041\000\008\255\037\255\047\255\054\255\056\255\000\000\000\000\
\048\255\065\255\068\255\059\255\039\255\002\255\002\255\039\255\
\000\000\000\000\000\000\039\255\087\255\020\255\076\255\077\255\
\000\000\069\255\080\255\095\255\126\255\039\255\039\255\039\255\
\039\255\039\255\000\000\086\255\000\000\097\255\002\255\097\255\
\000\000\126\255\126\255\126\255\126\255\126\255\000\000\051\255\
\000\000\000\000\000\000\018\255\000\000\100\255\020\255\039\255\
\109\255\000\000\105\255\101\255\051\255\000\000\039\255\039\255\
\039\255\118\255\126\255\124\255\000\000\000\000\000\000\126\255\
\119\255\123\255\000\000\082\255\132\255\039\255\039\255\000\000\
\023\255\039\255\103\255\000\000\122\255\111\255\000\000\023\255\
\000\000\000\000"

let yyrindex = "\000\000\
\148\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\148\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\139\255\139\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\140\255\000\000\000\000\049\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\058\255\063\255\066\255\072\255\074\255\000\000\141\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\141\255\000\000\000\000\142\255\
\000\000\000\000\128\255\000\000\000\000\000\000\000\000\131\255\
\145\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\021\255\000\000\000\000\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\146\000\000\000\001\000\235\255\134\000\247\255\089\000\
\000\000\000\000\072\000\000\000\176\255\113\000\000\000"

let yytablesize = 160
let yytable = "\029\000\
\012\000\011\000\036\000\003\000\001\000\004\000\037\000\014\000\
\093\000\003\000\011\000\004\000\005\000\006\000\013\000\098\000\
\050\000\051\000\052\000\053\000\054\000\003\000\037\000\004\000\
\037\000\071\000\072\000\037\000\030\000\060\000\044\000\037\000\
\037\000\056\000\007\000\037\000\057\000\061\000\059\000\025\000\
\015\000\026\000\075\000\017\000\037\000\027\000\062\000\037\000\
\037\000\080\000\064\000\084\000\003\000\018\000\004\000\021\000\
\065\000\060\000\020\000\028\000\019\000\056\000\020\000\074\000\
\091\000\061\000\024\000\017\000\094\000\065\000\020\000\020\000\
\018\000\022\000\062\000\016\000\023\000\063\000\064\000\017\000\
\017\000\014\000\045\000\015\000\018\000\018\000\046\000\016\000\
\016\000\048\000\047\000\089\000\055\000\014\000\014\000\015\000\
\015\000\038\000\039\000\040\000\041\000\042\000\038\000\039\000\
\040\000\041\000\042\000\056\000\073\000\043\000\038\000\039\000\
\040\000\041\000\042\000\076\000\077\000\049\000\038\000\039\000\
\040\000\041\000\042\000\078\000\085\000\095\000\038\000\039\000\
\040\000\041\000\042\000\086\000\088\000\097\000\038\000\039\000\
\040\000\041\000\042\000\090\000\087\000\038\000\039\000\040\000\
\041\000\042\000\096\000\002\000\038\000\040\000\024\000\025\000\
\029\000\021\000\027\000\016\000\035\000\079\000\092\000\058\000"

let yycheck = "\021\000\
\026\001\001\000\024\000\002\001\001\000\004\001\028\000\007\000\
\089\000\002\001\010\000\004\001\005\001\006\001\026\001\096\000\
\038\000\039\000\040\000\041\000\042\000\002\001\002\001\004\001\
\004\001\008\001\009\001\007\001\027\001\007\001\030\000\011\001\
\012\001\011\001\027\001\015\001\046\000\015\001\048\000\001\001\
\000\000\003\001\064\000\007\001\024\001\007\001\024\001\027\001\
\028\001\071\000\028\001\073\000\002\001\007\001\004\001\008\001\
\056\000\007\001\010\001\021\001\007\001\011\001\007\001\063\000\
\086\000\015\001\008\001\010\001\090\000\069\000\022\001\023\001\
\010\001\009\001\024\001\010\001\009\001\027\001\028\001\022\001\
\023\001\010\001\007\001\010\001\022\001\023\001\010\001\022\001\
\023\001\010\001\022\001\010\001\007\001\022\001\023\001\022\001\
\023\001\016\001\017\001\018\001\019\001\020\001\016\001\017\001\
\018\001\019\001\020\001\011\001\009\001\023\001\016\001\017\001\
\018\001\019\001\020\001\007\001\012\001\023\001\016\001\017\001\
\018\001\019\001\020\001\023\001\007\001\023\001\016\001\017\001\
\018\001\019\001\020\001\008\001\010\001\023\001\016\001\017\001\
\018\001\019\001\020\001\008\001\022\001\016\001\017\001\018\001\
\019\001\020\001\025\001\000\000\010\001\010\001\023\001\010\001\
\012\001\023\001\010\001\010\000\023\000\069\000\087\000\047\000"

let yynames_const = "\
  INT\000\
  BOOL\000\
  INTERNAL\000\
  EXTERNAL\000\
  ASSIGNMENT\000\
  LPAR\000\
  RPAR\000\
  LBRACE\000\
  RBRACE\000\
  LBRAKE\000\
  RBRAKE\000\
  STOP\000\
  PLUS\000\
  MINUS\000\
  EQ\000\
  AND\000\
  OR\000\
  NOT\000\
  COMMA\000\
  SEMI\000\
  EOF\000\
  IF\000\
  ELSE\000\
  ROUTINE\000\
  LOCKED\000\
  PRINT\000\
  "

let yynames_block = "\
  CSTINT\000\
  CSTBOOL\000\
  NAME\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdecs) in
    Obj.repr(
# 24 "parser.mly"
                  ( Topdecs _1 )
# 230 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 28 "parser.mly"
                        ( [] )
# 236 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 29 "parser.mly"
                        ( _1 :: _2 )
# 244 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 33 "parser.mly"
                                                              ( GlobalVar (false, _1, _2, _4)   )
# 253 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 34 "parser.mly"
                                                              ( GlobalVar (true, _2, _3, _5)    )
# 262 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 35 "parser.mly"
                                                              ( Routine (Internal, _3, _5, _7) )
# 271 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 36 "parser.mly"
                                                              ( Routine (External, _3, _5, _7) )
# 280 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 40 "parser.mly"
              ( T_Int )
# 286 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
              ( T_Bool )
# 292 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 45 "parser.mly"
                                ( Block _2 )
# 299 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 49 "parser.mly"
                ( Bool _1 )
# 306 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 50 "parser.mly"
              ( Int _1 )
# 313 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 51 "parser.mly"
                 ( Lookup _1  )
# 320 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 52 "parser.mly"
                                                      ( Binary_op ("&", _1, _3) )
# 328 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 53 "parser.mly"
                                                     ( Binary_op ("|", _1, _3) )
# 336 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 54 "parser.mly"
                                                     ( Binary_op ("=", _1, _3) )
# 344 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 55 "parser.mly"
                                                         ( Binary_op ("+", _1, _3) )
# 352 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 56 "parser.mly"
                                                         ( Binary_op ("-", _1, _3) )
# 360 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 57 "parser.mly"
                                                      ( Binary_op ("=", _1, _3) )
# 368 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 58 "parser.mly"
                                              ( Unary_op ("!", _2) )
# 375 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 77 "parser.mly"
                                              ( Assign (_1, _3) )
# 383 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 78 "parser.mly"
                                                ( Call (_1, _3) )
# 391 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                                                ( Stop )
# 397 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 80 "parser.mly"
                                                ( Print _2 )
# 404 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
              ( [] )
# 410 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 85 "parser.mly"
              ( _1 )
# 417 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 89 "parser.mly"
                                                        ( [_1] )
# 424 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 90 "parser.mly"
                                           ( _1 :: _3 )
# 432 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                               ( [] )
# 438 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 95 "parser.mly"
                               ( _1 :: _2)
# 446 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "parser.mly"
                                                             ( Statement _1 )
# 453 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 100 "parser.mly"
                                                             ( Declaration (false, _1, _2, _4) )
# 462 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 101 "parser.mly"
                                                             ( Declaration (true, _2, _3, _5) )
# 471 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 105 "parser.mly"
                                                       ( Expression _1 )
# 478 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 106 "parser.mly"
                                                       ( _1 )
# 485 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 494 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 108 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 502 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                  ( [] )
# 508 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 113 "parser.mly"
                  ( _1 )
# 515 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 117 "parser.mly"
                              ( [_1] )
# 522 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 118 "parser.mly"
                              ( _1 :: _3 )
# 530 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 119 "parser.mly"
                              ( _1 :: _3 )
# 538 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 123 "parser.mly"
                          ( (false, _1, _2) )
# 546 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 124 "parser.mly"
                          ( (true, _2, _3) )
# 554 "parser.ml"
               : 'param))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Absyn.topdecs)
