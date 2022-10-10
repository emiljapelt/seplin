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
  | HALT
  | PLUS
  | MINUS
  | TIMES
  | EQ
  | NEQ
  | LT
  | GT
  | LTEQ
  | GTEQ
  | AND
  | OR
  | NOT
  | COMMA
  | SEMI
  | EOF
  | IF
  | ELSE
  | WHILE
  | LOCKED
  | PRINT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Absyn
  open ProgramRep
# 45 "parser.ml"
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
  272 (* HALT *);
  273 (* PLUS *);
  274 (* MINUS *);
  275 (* TIMES *);
  276 (* EQ *);
  277 (* NEQ *);
  278 (* LT *);
  279 (* GT *);
  280 (* LTEQ *);
  281 (* GTEQ *);
  282 (* AND *);
  283 (* OR *);
  284 (* NOT *);
  285 (* COMMA *);
  286 (* SEMI *);
    0 (* EOF *);
  287 (* IF *);
  288 (* ELSE *);
  289 (* WHILE *);
  290 (* LOCKED *);
  291 (* PRINT *);
    0|]

let yytransl_block = [|
  257 (* CSTINT *);
  259 (* CSTBOOL *);
  263 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\004\000\
\004\000\007\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\009\000\009\000\009\000\009\000\009\000\
\010\000\010\000\011\000\011\000\008\000\008\000\012\000\012\000\
\012\000\013\000\013\000\013\000\013\000\013\000\006\000\006\000\
\014\000\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\005\000\006\000\006\000\006\000\001\000\
\001\000\003\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\002\000\002\000\003\000\003\000\004\000\001\000\001\000\002\000\
\000\000\001\000\001\000\003\000\000\000\002\000\001\000\005\000\
\006\000\002\000\001\000\007\000\005\000\005\000\000\000\001\000\
\001\000\003\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\000\000\000\000\000\000\053\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\048\000\000\000\000\000\000\000\012\000\011\000\013\000\000\000\
\000\000\000\000\000\000\000\000\051\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\052\000\000\000\006\000\050\000\007\000\005\000\027\000\000\000\
\000\000\023\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\030\000\031\000\000\000\000\000\000\000\
\000\000\000\000\043\000\000\000\000\000\000\000\039\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\042\000\
\038\000\000\000\000\000\000\000\034\000\000\000\000\000\000\000\
\000\000\000\000\029\000\000\000\000\000\000\000\000\000\036\000\
\000\000\046\000\000\000\040\000\000\000\041\000\044\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\023\000\099\000\024\000\083\000\084\000\
\085\000\100\000\101\000\086\000\087\000\025\000\026\000"

let yysindex = "\004\000\
\004\255\000\000\000\000\000\000\009\255\014\255\255\254\000\000\
\022\000\004\255\029\255\030\255\031\255\039\255\000\000\000\000\
\051\255\013\255\013\255\053\255\091\255\255\254\048\255\054\255\
\000\000\036\255\056\255\091\255\000\000\000\000\000\000\091\255\
\091\255\091\255\035\000\061\255\000\000\062\255\013\255\062\255\
\049\000\244\255\018\255\000\000\091\255\091\255\091\255\091\255\
\091\255\091\255\091\255\091\255\091\255\091\255\091\255\000\000\
\000\000\047\255\000\000\000\000\000\000\000\000\000\000\018\255\
\018\255\000\000\115\000\115\000\226\255\226\255\226\255\226\255\
\000\000\018\255\010\255\000\000\000\000\068\255\076\255\255\254\
\091\255\079\255\000\000\078\255\065\255\047\255\000\000\091\255\
\091\255\091\255\091\255\084\255\104\000\088\255\000\000\000\000\
\000\000\104\000\091\000\087\255\000\000\006\000\024\000\093\255\
\091\255\091\255\000\000\072\255\072\255\091\255\063\000\000\000\
\070\255\000\000\077\000\000\000\072\255\000\000\000\000"

let yyrindex = "\000\000\
\106\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\106\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\094\255\094\255\000\000\000\000\000\000\000\000\000\000\
\000\000\098\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\100\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\099\255\000\000\000\000\000\000\000\000\000\000\116\255\
\132\255\000\000\218\255\230\255\164\255\180\255\196\255\212\255\
\000\000\148\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\099\255\000\000\000\000\
\102\255\000\000\000\000\000\000\083\255\000\000\000\000\000\000\
\000\000\085\255\104\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\255\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\118\000\000\000\013\000\235\255\097\000\220\255\045\000\
\000\000\000\000\026\000\000\000\189\255\096\000\000\000"

let yytablesize = 398
let yytable = "\035\000\
\003\000\059\000\004\000\061\000\001\000\003\000\041\000\004\000\
\005\000\006\000\042\000\043\000\044\000\011\000\003\000\012\000\
\004\000\088\000\089\000\014\000\013\000\015\000\011\000\064\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\036\000\017\000\047\000\007\000\018\000\019\000\
\113\000\114\000\045\000\054\000\045\000\020\000\022\000\045\000\
\003\000\119\000\004\000\045\000\045\000\075\000\037\000\045\000\
\045\000\058\000\021\000\093\000\028\000\076\000\077\000\038\000\
\039\000\040\000\098\000\057\000\102\000\103\000\082\000\045\000\
\058\000\045\000\045\000\045\000\090\000\078\000\075\000\079\000\
\080\000\081\000\058\000\111\000\091\000\094\000\076\000\077\000\
\115\000\095\000\104\000\029\000\092\000\030\000\096\000\105\000\
\107\000\031\000\082\000\032\000\110\000\117\000\078\000\047\000\
\079\000\002\000\081\000\049\000\033\000\025\000\037\000\033\000\
\032\000\035\000\028\000\027\000\025\000\025\000\034\000\025\000\
\025\000\025\000\025\000\025\000\025\000\022\000\025\000\016\000\
\025\000\025\000\097\000\112\000\022\000\022\000\060\000\022\000\
\022\000\022\000\022\000\022\000\022\000\024\000\022\000\000\000\
\022\000\022\000\000\000\000\000\024\000\024\000\000\000\024\000\
\024\000\024\000\024\000\024\000\024\000\015\000\024\000\000\000\
\024\000\024\000\000\000\000\000\015\000\015\000\000\000\015\000\
\015\000\015\000\015\000\015\000\015\000\019\000\015\000\000\000\
\015\000\015\000\000\000\000\000\000\000\000\000\000\000\019\000\
\019\000\019\000\019\000\019\000\019\000\021\000\000\000\000\000\
\019\000\019\000\000\000\000\000\000\000\000\000\000\000\021\000\
\021\000\021\000\021\000\021\000\021\000\018\000\000\000\000\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\018\000\
\018\000\018\000\018\000\018\000\018\000\020\000\000\000\000\000\
\018\000\018\000\000\000\016\000\000\000\000\000\000\000\020\000\
\020\000\020\000\020\000\020\000\020\000\016\000\016\000\017\000\
\020\000\020\000\045\000\046\000\047\000\000\000\016\000\016\000\
\000\000\017\000\017\000\054\000\055\000\063\000\000\000\000\000\
\000\000\000\000\017\000\017\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\108\000\
\000\000\000\000\000\000\000\000\000\000\000\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\109\000\000\000\000\000\000\000\000\000\000\000\000\000\
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\000\000\000\000\
\056\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\000\000\000\000\062\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\000\000\000\000\116\000\045\000\046\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\000\000\000\000\118\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\000\000\106\000\
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\045\000\046\000\047\000\000\000\000\000\
\050\000\051\000\052\000\053\000\054\000\055\000"

let yycheck = "\021\000\
\002\001\038\000\004\001\040\000\001\000\002\001\028\000\004\001\
\005\001\006\001\032\000\033\000\034\000\001\000\002\001\007\001\
\004\001\008\001\009\001\007\000\007\001\000\000\010\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\022\000\007\001\019\001\034\001\009\001\009\001\
\108\000\109\000\002\001\026\001\004\001\007\001\034\001\007\001\
\002\001\117\000\004\001\011\001\012\001\007\001\007\001\015\001\
\016\001\011\001\008\001\081\000\008\001\015\001\016\001\010\001\
\029\001\010\001\088\000\007\001\090\000\091\000\058\000\031\001\
\011\001\033\001\034\001\035\001\009\001\031\001\007\001\033\001\
\034\001\035\001\011\001\105\000\009\001\007\001\015\001\016\001\
\110\000\012\001\007\001\001\001\080\000\003\001\030\001\008\001\
\010\001\007\001\086\000\009\001\008\001\032\001\031\001\010\001\
\033\001\000\000\035\001\010\001\018\001\010\001\012\001\010\001\
\030\001\010\001\030\001\019\000\017\001\018\001\028\001\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\027\001\010\000\
\029\001\030\001\086\000\106\000\017\001\018\001\039\000\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\027\001\255\255\
\029\001\030\001\255\255\255\255\017\001\018\001\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\027\001\255\255\
\029\001\030\001\255\255\255\255\017\001\018\001\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\027\001\255\255\
\029\001\030\001\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\255\255\255\255\
\029\001\030\001\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\255\255\255\255\
\029\001\030\001\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\255\255\255\255\
\029\001\030\001\255\255\010\001\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\020\001\021\001\010\001\
\029\001\030\001\017\001\018\001\019\001\255\255\029\001\030\001\
\255\255\020\001\021\001\026\001\027\001\010\001\255\255\255\255\
\255\255\255\255\029\001\030\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\010\001\
\255\255\255\255\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\010\001\255\255\255\255\255\255\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\255\255\255\255\
\030\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\255\255\030\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\255\255\255\255\030\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\255\255\030\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\255\255\029\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\017\001\018\001\019\001\255\255\255\255\
\022\001\023\001\024\001\025\001\026\001\027\001"

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
  HALT\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  EQ\000\
  NEQ\000\
  LT\000\
  GT\000\
  LTEQ\000\
  GTEQ\000\
  AND\000\
  OR\000\
  NOT\000\
  COMMA\000\
  SEMI\000\
  EOF\000\
  IF\000\
  ELSE\000\
  WHILE\000\
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
# 33 "parser.mly"
                  ( Topdecs _1 )
# 319 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                        ( [] )
# 325 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 38 "parser.mly"
                        ( _1 :: _2 )
# 333 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 42 "parser.mly"
                                                              ( GlobalVar (false, _1, _2, _4)   )
# 342 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 43 "parser.mly"
                                                              ( GlobalVar (true, _2, _3, _5)    )
# 351 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 44 "parser.mly"
                                                      ( Routine (Internal, _2, _4, _6) )
# 360 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 45 "parser.mly"
                                                      ( Routine (External, _2, _4, _6) )
# 369 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
              ( T_Int )
# 375 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
              ( T_Bool )
# 381 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 54 "parser.mly"
                                ( Block _2 )
# 388 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 58 "parser.mly"
                ( Bool _1 )
# 395 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 59 "parser.mly"
              ( Int _1 )
# 402 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "parser.mly"
                 ( Lookup _1  )
# 409 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 61 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 417 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 62 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 425 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 63 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 433 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 64 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 441 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 65 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 449 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 66 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 457 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 67 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 465 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 68 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 473 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 69 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 481 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 70 "parser.mly"
                                                          ( Binary_op ("*", _1, _3) )
# 489 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 71 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 497 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 504 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 73 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 511 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 74 "parser.mly"
                                                          ( _2 )
# 518 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 78 "parser.mly"
                                                ( Assign (_1, _3) )
# 526 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 79 "parser.mly"
                                                ( Call (_1, _3) )
# 534 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                                                ( Stop )
# 540 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                                                ( Halt )
# 546 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 82 "parser.mly"
                                                ( Print _2 )
# 553 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
              ( [] )
# 559 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 87 "parser.mly"
              ( _1 )
# 566 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 91 "parser.mly"
                                                        ( [_1] )
# 573 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 92 "parser.mly"
                                           ( _1 :: _3 )
# 581 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                               ( [] )
# 587 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 97 "parser.mly"
                               ( _1 :: _2)
# 595 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                                                             ( Statement _1 )
# 602 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 102 "parser.mly"
                                                             ( Declaration (false, _1, _2, _4) )
# 611 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 103 "parser.mly"
                                                             ( Declaration (true, _2, _3, _5) )
# 620 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 107 "parser.mly"
                                                       ( Expression _1 )
# 627 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 108 "parser.mly"
                                                       ( _1 )
# 634 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 109 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 643 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 110 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 651 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 111 "parser.mly"
                                                             ( While (_3, _5) )
# 659 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 115 "parser.mly"
                  ( [] )
# 665 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 116 "parser.mly"
                  ( _1 )
# 672 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 120 "parser.mly"
                              ( [_1] )
# 679 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 121 "parser.mly"
                              ( _1 :: _3 )
# 687 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 125 "parser.mly"
                          ( (false, _1, _2) )
# 695 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 126 "parser.mly"
                          ( (true, _2, _3) )
# 703 "parser.ml"
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
