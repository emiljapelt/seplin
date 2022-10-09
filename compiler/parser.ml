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
  | ROUTINE
  | LOCKED
  | PRINT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Absyn
  open ProgramRep
# 44 "parser.ml"
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
  275 (* NEQ *);
  276 (* LT *);
  277 (* GT *);
  278 (* LTEQ *);
  279 (* GTEQ *);
  280 (* AND *);
  281 (* OR *);
  282 (* NOT *);
  283 (* COMMA *);
  284 (* SEMI *);
    0 (* EOF *);
  285 (* IF *);
  286 (* ELSE *);
  287 (* WHILE *);
  288 (* ROUTINE *);
  289 (* LOCKED *);
  290 (* PRINT *);
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
\005\000\005\000\009\000\009\000\009\000\009\000\010\000\010\000\
\011\000\011\000\008\000\008\000\012\000\012\000\012\000\013\000\
\013\000\013\000\013\000\013\000\006\000\006\000\014\000\014\000\
\014\000\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\005\000\006\000\007\000\007\000\001\000\
\001\000\003\000\001\000\001\000\001\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\002\000\003\000\003\000\004\000\001\000\002\000\000\000\001\000\
\001\000\003\000\000\000\002\000\001\000\005\000\006\000\002\000\
\001\000\007\000\005\000\005\000\000\000\001\000\001\000\003\000\
\003\000\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\000\000\000\000\000\000\052\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\011\000\013\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\004\000\000\000\050\000\000\000\000\000\
\000\000\005\000\026\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\000\000\006\000\
\048\000\007\000\000\000\029\000\000\000\000\000\000\000\000\000\
\000\000\041\000\000\000\000\000\000\000\037\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\010\000\040\000\036\000\
\000\000\000\000\000\000\032\000\000\000\000\000\000\000\000\000\
\000\000\028\000\000\000\000\000\000\000\000\000\034\000\000\000\
\044\000\000\000\038\000\000\000\039\000\042\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\033\000\098\000\034\000\082\000\083\000\
\084\000\099\000\100\000\085\000\086\000\035\000\036\000"

let yysindex = "\013\000\
\014\255\000\000\000\000\000\000\006\255\023\255\049\255\000\000\
\039\000\014\255\039\255\047\255\050\255\057\255\000\000\000\000\
\063\255\071\255\073\255\091\255\084\255\009\255\009\255\084\255\
\000\000\000\000\000\000\084\255\084\255\084\255\162\255\049\255\
\085\255\099\255\000\000\067\255\102\255\175\255\120\255\226\255\
\226\255\084\255\084\255\084\255\084\255\084\255\084\255\084\255\
\084\255\084\255\084\255\000\000\111\255\000\000\110\255\009\255\
\110\255\000\000\000\000\226\255\226\255\226\255\226\255\226\255\
\226\255\226\255\226\255\226\255\226\255\000\000\041\255\000\000\
\000\000\000\000\252\254\000\000\115\255\118\255\049\255\084\255\
\124\255\000\000\121\255\104\255\041\255\000\000\084\255\084\255\
\084\255\084\255\140\255\226\255\141\255\000\000\000\000\000\000\
\226\255\214\255\138\255\000\000\136\255\152\255\142\255\084\255\
\084\255\000\000\066\255\066\255\084\255\188\255\000\000\133\255\
\000\000\201\255\000\000\066\255\000\000\000\000"

let yyrindex = "\000\000\
\151\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\151\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\154\255\154\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\155\255\000\000\000\000\000\000\007\255\
\022\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\051\255\076\255\079\255\086\255\088\255\
\092\255\095\255\098\255\101\255\107\255\000\000\176\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\176\255\000\000\000\000\156\255\
\000\000\000\000\000\000\139\255\000\000\000\000\000\000\000\000\
\161\255\191\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\255\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\192\000\000\000\005\000\235\255\191\000\010\000\130\000\
\000\000\000\000\122\000\000\000\150\255\172\000\000\000"

let yytablesize = 251
let yytable = "\031\000\
\112\000\113\000\038\000\087\000\088\000\011\000\039\000\040\000\
\041\000\118\000\003\000\014\000\004\000\001\000\011\000\003\000\
\024\000\004\000\005\000\006\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\043\000\025\000\
\043\000\024\000\024\000\043\000\053\000\012\000\015\000\043\000\
\043\000\032\000\003\000\043\000\004\000\017\000\007\000\075\000\
\025\000\025\000\003\000\071\000\004\000\018\000\013\000\076\000\
\019\000\043\000\092\000\043\000\022\000\043\000\043\000\020\000\
\072\000\097\000\074\000\101\000\102\000\077\000\021\000\078\000\
\075\000\079\000\080\000\081\000\071\000\022\000\022\000\022\000\
\076\000\023\000\110\000\091\000\025\000\023\000\026\000\114\000\
\016\000\081\000\027\000\054\000\028\000\056\000\077\000\017\000\
\078\000\019\000\024\000\080\000\029\000\021\000\023\000\023\000\
\018\000\016\000\016\000\020\000\055\000\030\000\014\000\057\000\
\017\000\017\000\019\000\019\000\015\000\070\000\021\000\021\000\
\071\000\018\000\018\000\089\000\020\000\020\000\090\000\014\000\
\014\000\059\000\093\000\095\000\094\000\015\000\015\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\107\000\103\000\106\000\104\000\109\000\002\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\108\000\116\000\045\000\047\000\031\000\030\000\042\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000\035\000\027\000\052\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\033\000\016\000\058\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\037\000\096\000\115\000\
\042\000\043\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\111\000\073\000\117\000\042\000\043\000\044\000\
\045\000\046\000\047\000\048\000\049\000\050\000\051\000\000\000\
\105\000\042\000\043\000\044\000\045\000\046\000\047\000\048\000\
\049\000\050\000\051\000"

let yycheck = "\021\000\
\107\000\108\000\024\000\008\001\009\001\001\000\028\000\029\000\
\030\000\116\000\002\001\007\000\004\001\001\000\010\000\002\001\
\010\001\004\001\005\001\006\001\042\000\043\000\044\000\045\000\
\046\000\047\000\048\000\049\000\050\000\051\000\002\001\010\001\
\004\001\027\001\028\001\007\001\032\000\032\001\000\000\011\001\
\012\001\033\001\002\001\015\001\004\001\007\001\033\001\007\001\
\027\001\028\001\002\001\011\001\004\001\007\001\032\001\015\001\
\007\001\029\001\080\000\031\001\010\001\033\001\034\001\007\001\
\055\000\087\000\057\000\089\000\090\000\029\001\008\001\031\001\
\007\001\033\001\034\001\071\000\011\001\027\001\028\001\009\001\
\015\001\009\001\104\000\079\000\001\001\010\001\003\001\109\000\
\010\001\085\000\007\001\007\001\009\001\027\001\029\001\010\001\
\031\001\010\001\008\001\034\001\017\001\010\001\027\001\028\001\
\010\001\027\001\028\001\010\001\010\001\026\001\010\001\010\001\
\027\001\028\001\027\001\028\001\010\001\007\001\027\001\028\001\
\011\001\027\001\028\001\009\001\027\001\028\001\009\001\027\001\
\028\001\010\001\007\001\028\001\012\001\027\001\028\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\007\001\010\001\008\001\008\001\000\000\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\030\001\010\001\010\001\010\001\028\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\012\001\028\001\028\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\010\001\010\000\028\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\023\000\085\000\028\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\105\000\056\000\028\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\255\255\
\027\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001"

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
# 25 "parser.mly"
                  ( Topdecs _1 )
# 280 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
                        ( [] )
# 286 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 30 "parser.mly"
                        ( _1 :: _2 )
# 294 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 34 "parser.mly"
                                                              ( GlobalVar (false, _1, _2, _4)   )
# 303 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 35 "parser.mly"
                                                              ( GlobalVar (true, _2, _3, _5)    )
# 312 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 36 "parser.mly"
                                                              ( Routine (Internal, _3, _5, _7) )
# 321 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 37 "parser.mly"
                                                              ( Routine (External, _3, _5, _7) )
# 330 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
              ( T_Int )
# 336 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "parser.mly"
              ( T_Bool )
# 342 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 46 "parser.mly"
                                ( Block _2 )
# 349 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 50 "parser.mly"
                ( Bool _1 )
# 356 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 51 "parser.mly"
              ( Int _1 )
# 363 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                 ( Lookup _1  )
# 370 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 53 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 378 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 54 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 386 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 55 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 394 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 56 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 402 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 57 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 410 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 58 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 418 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 59 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 426 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 60 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 434 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 61 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 442 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 62 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 450 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 63 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 457 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 64 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 464 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 65 "parser.mly"
                                                          ( _2 )
# 471 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 69 "parser.mly"
                                              ( Assign (_1, _3) )
# 479 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 70 "parser.mly"
                                                ( Call (_1, _3) )
# 487 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                                                ( Stop )
# 493 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                ( Print _2 )
# 500 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
              ( [] )
# 506 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 77 "parser.mly"
              ( _1 )
# 513 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 81 "parser.mly"
                                                        ( [_1] )
# 520 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 82 "parser.mly"
                                           ( _1 :: _3 )
# 528 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                               ( [] )
# 534 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 87 "parser.mly"
                               ( _1 :: _2)
# 542 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 91 "parser.mly"
                                                             ( Statement _1 )
# 549 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 92 "parser.mly"
                                                             ( Declaration (false, _1, _2, _4) )
# 558 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 93 "parser.mly"
                                                             ( Declaration (true, _2, _3, _5) )
# 567 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 97 "parser.mly"
                                                       ( Expression _1 )
# 574 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 98 "parser.mly"
                                                       ( _1 )
# 581 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 99 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 590 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 100 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 598 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 101 "parser.mly"
                                                             ( While (_3, _5) )
# 606 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                  ( [] )
# 612 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 106 "parser.mly"
                  ( _1 )
# 619 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 110 "parser.mly"
                              ( [_1] )
# 626 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 111 "parser.mly"
                              ( _1 :: _3 )
# 634 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 112 "parser.mly"
                              ( _1 :: _3 )
# 642 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 116 "parser.mly"
                          ( (false, _1, _2) )
# 650 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "parser.mly"
                          ( (true, _2, _3) )
# 658 "parser.ml"
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
