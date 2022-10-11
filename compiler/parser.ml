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
  | TRANSFER
  | VAR
  | PRINT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Absyn
  open ProgramRep
# 47 "parser.ml"
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
  291 (* TRANSFER *);
  292 (* VAR *);
  293 (* PRINT *);
    0|]

let yytransl_block = [|
  257 (* CSTINT *);
  259 (* CSTBOOL *);
  263 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\004\000\004\000\007\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\009\000\009\000\009\000\
\009\000\009\000\010\000\010\000\011\000\011\000\008\000\008\000\
\012\000\012\000\012\000\012\000\012\000\012\000\012\000\013\000\
\013\000\013\000\013\000\013\000\006\000\006\000\014\000\014\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\003\000\004\000\005\000\006\000\006\000\
\006\000\001\000\001\000\003\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\003\000\004\000\001\000\
\001\000\002\000\000\000\001\000\001\000\003\000\000\000\002\000\
\001\000\003\000\004\000\005\000\006\000\005\000\006\000\002\000\
\001\000\007\000\005\000\005\000\000\000\001\000\001\000\003\000\
\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\011\000\000\000\000\000\000\000\059\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\054\000\000\000\000\000\000\000\005\000\014\000\013\000\
\015\000\000\000\000\000\000\000\000\000\000\000\058\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\057\000\000\000\008\000\056\000\009\000\007\000\
\029\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\032\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\049\000\000\000\000\000\
\000\000\041\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\048\000\040\000\000\000\000\000\
\000\000\036\000\000\000\000\000\000\000\000\000\000\000\000\000\
\042\000\000\000\031\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\038\000\000\000\052\000\000\000\000\000\046\000\
\044\000\000\000\047\000\045\000\050\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\024\000\104\000\025\000\086\000\087\000\
\088\000\105\000\106\000\089\000\090\000\026\000\027\000"

let yysindex = "\016\000\
\005\255\000\000\000\000\000\000\033\255\035\255\018\255\000\000\
\021\000\005\255\043\255\053\255\055\255\058\255\000\000\000\000\
\249\254\014\255\014\255\250\254\112\255\000\000\018\255\059\255\
\057\255\000\000\039\255\061\255\112\255\000\000\000\000\000\000\
\000\000\112\255\112\255\112\255\060\000\062\255\000\000\063\255\
\014\255\063\255\074\000\013\000\242\254\000\000\112\255\112\255\
\112\255\112\255\112\255\112\255\112\255\112\255\112\255\112\255\
\112\255\000\000\000\000\073\255\000\000\000\000\000\000\000\000\
\000\000\242\254\242\254\000\000\168\000\168\000\068\255\068\255\
\068\255\068\255\000\000\242\254\046\255\000\000\000\000\074\255\
\083\255\002\255\086\255\112\255\092\255\000\000\088\255\071\255\
\073\255\000\000\112\255\112\255\112\255\112\255\095\255\096\255\
\097\255\157\000\251\254\000\000\000\000\000\000\157\000\144\000\
\104\255\000\000\031\000\049\000\103\255\011\255\112\255\112\255\
\000\000\112\255\000\000\101\255\101\255\112\255\112\255\000\000\
\088\000\102\000\000\000\090\255\000\000\116\000\130\000\000\000\
\000\000\101\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\120\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\120\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\113\255\113\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\114\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\125\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\115\255\000\000\000\000\000\000\000\000\
\000\000\141\255\157\255\000\000\243\255\255\255\189\255\205\255\
\221\255\237\255\000\000\173\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\115\255\000\000\000\000\116\255\000\000\000\000\000\000\000\000\
\000\000\098\255\000\000\000\000\000\000\000\000\099\255\121\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\123\000\000\000\036\000\235\255\117\000\011\000\048\000\
\000\000\000\000\025\000\000\000\184\255\100\000\000\000"

let yytablesize = 451
let yytable = "\037\000\
\021\000\029\000\112\000\003\000\049\000\004\000\003\000\043\000\
\004\000\005\000\006\000\056\000\044\000\045\000\046\000\003\000\
\001\000\004\000\119\000\003\000\015\000\004\000\022\000\030\000\
\113\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\011\000\095\000\007\000\012\000\
\120\000\013\000\014\000\124\000\125\000\011\000\051\000\023\000\
\051\000\017\000\061\000\051\000\063\000\091\000\092\000\051\000\
\051\000\133\000\038\000\051\000\051\000\018\000\098\000\019\000\
\020\000\039\000\040\000\041\000\059\000\103\000\042\000\107\000\
\108\000\060\000\003\000\051\000\004\000\051\000\051\000\077\000\
\051\000\051\000\093\000\060\000\047\000\048\000\049\000\078\000\
\079\000\121\000\122\000\094\000\097\000\056\000\057\000\085\000\
\126\000\127\000\099\000\100\000\101\000\109\000\110\000\080\000\
\111\000\081\000\082\000\077\000\083\000\084\000\118\000\060\000\
\031\000\115\000\032\000\078\000\079\000\096\000\033\000\002\000\
\034\000\130\000\053\000\055\000\085\000\035\000\039\000\034\000\
\030\000\035\000\037\000\080\000\016\000\081\000\027\000\028\000\
\102\000\084\000\123\000\036\000\062\000\027\000\027\000\000\000\
\027\000\027\000\027\000\027\000\027\000\027\000\024\000\027\000\
\000\000\027\000\027\000\000\000\000\000\024\000\024\000\000\000\
\024\000\024\000\024\000\024\000\024\000\024\000\026\000\024\000\
\000\000\024\000\024\000\000\000\000\000\026\000\026\000\000\000\
\026\000\026\000\026\000\026\000\026\000\026\000\017\000\026\000\
\000\000\026\000\026\000\000\000\000\000\017\000\017\000\000\000\
\017\000\017\000\017\000\017\000\017\000\017\000\021\000\017\000\
\000\000\017\000\017\000\000\000\000\000\000\000\000\000\000\000\
\021\000\021\000\021\000\021\000\021\000\021\000\023\000\000\000\
\000\000\021\000\021\000\000\000\000\000\000\000\000\000\000\000\
\023\000\023\000\023\000\023\000\023\000\023\000\020\000\000\000\
\000\000\023\000\023\000\000\000\000\000\000\000\000\000\000\000\
\020\000\020\000\020\000\020\000\020\000\020\000\022\000\000\000\
\000\000\020\000\020\000\000\000\018\000\000\000\000\000\000\000\
\022\000\022\000\022\000\022\000\022\000\022\000\018\000\018\000\
\019\000\022\000\022\000\000\000\000\000\000\000\000\000\018\000\
\018\000\000\000\019\000\019\000\000\000\000\000\065\000\000\000\
\000\000\000\000\000\000\019\000\019\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\116\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\117\000\000\000\000\000\000\000\000\000\000\000\
\000\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\000\000\058\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\064\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\128\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\129\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\000\000\131\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\132\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\114\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\047\000\048\000\049\000\000\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000"

let yycheck = "\021\000\
\008\001\008\001\008\001\002\001\019\001\004\001\002\001\029\000\
\004\001\005\001\006\001\026\001\034\000\035\000\036\000\002\001\
\001\000\004\001\008\001\002\001\000\000\004\001\030\001\030\001\
\030\001\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\001\000\036\001\034\001\007\001\
\030\001\007\001\007\000\116\000\117\000\010\000\002\001\034\001\
\004\001\007\001\040\000\007\001\042\000\008\001\009\001\011\001\
\012\001\130\000\023\000\015\001\016\001\009\001\084\000\009\001\
\007\001\007\001\010\001\029\001\007\001\091\000\010\001\093\000\
\094\000\011\001\002\001\031\001\004\001\033\001\034\001\007\001\
\036\001\037\001\009\001\011\001\017\001\018\001\019\001\015\001\
\016\001\111\000\112\000\009\001\007\001\026\001\027\001\060\000\
\118\000\119\000\007\001\012\001\030\001\007\001\007\001\031\001\
\008\001\033\001\034\001\007\001\036\001\037\001\008\001\011\001\
\001\001\010\001\003\001\015\001\016\001\082\000\007\001\000\000\
\009\001\032\001\010\001\010\001\089\000\010\001\012\001\030\001\
\030\001\018\001\010\001\031\001\010\000\033\001\010\001\019\000\
\089\000\037\001\114\000\028\001\041\000\017\001\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\027\001\
\255\255\029\001\030\001\255\255\255\255\017\001\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\027\001\
\255\255\029\001\030\001\255\255\255\255\017\001\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\027\001\
\255\255\029\001\030\001\255\255\255\255\017\001\018\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\027\001\
\255\255\029\001\030\001\255\255\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\255\255\
\255\255\029\001\030\001\255\255\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\255\255\
\255\255\029\001\030\001\255\255\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\255\255\
\255\255\029\001\030\001\255\255\010\001\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\020\001\021\001\
\010\001\029\001\030\001\255\255\255\255\255\255\255\255\029\001\
\030\001\255\255\020\001\021\001\255\255\255\255\010\001\255\255\
\255\255\255\255\255\255\029\001\030\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\010\001\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\010\001\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\255\255\030\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\255\255\255\255\030\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\255\255\255\255\030\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\255\255\255\255\030\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\255\255\030\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\255\255\255\255\030\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\255\255\029\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\017\001\018\001\019\001\255\255\255\255\022\001\023\001\024\001\
\025\001\026\001\027\001"

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
  TRANSFER\000\
  VAR\000\
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
# 347 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                        ( [] )
# 353 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 38 "parser.mly"
                        ( _1 :: _2 )
# 361 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 42 "parser.mly"
                                                              ( Global (false, _1, _2) )
# 369 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parser.mly"
                                                              ( Global (true, _2, _3) )
# 377 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 44 "parser.mly"
                                                              ( GlobalAssign (false, _1, _2, _4)   )
# 386 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 45 "parser.mly"
                                                              ( GlobalAssign (true, _2, _3, _5)    )
# 395 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 46 "parser.mly"
                                                   ( Routine (Internal, _2, _4, _6) )
# 404 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 47 "parser.mly"
                                                   ( Routine (External, _2, _4, _6) )
# 413 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
              ( T_Int )
# 419 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
              ( T_Bool )
# 425 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 56 "parser.mly"
                                ( Block _2 )
# 432 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 60 "parser.mly"
                ( Bool _1 )
# 439 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "parser.mly"
              ( Int _1 )
# 446 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                 ( Lookup _1  )
# 453 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 63 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 461 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 64 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 469 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 65 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 477 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 66 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 485 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 67 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 493 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 68 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 501 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 69 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 509 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 70 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 517 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 71 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 525 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                          ( Binary_op ("*", _1, _3) )
# 533 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 73 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 541 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 74 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 548 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 75 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 555 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 76 "parser.mly"
                                                          ( _2 )
# 562 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 80 "parser.mly"
                                                ( Assign (_1, _3) )
# 570 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 81 "parser.mly"
                                                ( Call (_1, _3) )
# 578 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                                ( Stop )
# 584 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                                                ( Halt )
# 590 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 84 "parser.mly"
                                                ( Print _2 )
# 597 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
                 ( [] )
# 603 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 89 "parser.mly"
                 ( _1 )
# 610 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 93 "parser.mly"
                                              ( [_1] )
# 617 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 94 "parser.mly"
                                              ( _1 :: _3 )
# 625 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                               ( [] )
# 631 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 99 "parser.mly"
                               ( _1 :: _2)
# 639 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                                             ( Statement _1 )
# 646 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 104 "parser.mly"
                                                             ( Declaration (false, _1, _2) )
# 654 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "parser.mly"
                                                             ( Declaration (true, _2, _3) )
# 662 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 106 "parser.mly"
                                                             ( AssignDeclaration (false, _1, _2, _4) )
# 671 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 107 "parser.mly"
                                                             ( AssignDeclaration (true, _2, _3, _5) )
# 680 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 108 "parser.mly"
                                                             ( VarDeclaration (false, _2, _4) )
# 688 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 109 "parser.mly"
                                                             ( VarDeclaration (true, _3, _5) )
# 696 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 113 "parser.mly"
                                                       ( Expression _1 )
# 703 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 114 "parser.mly"
                                                       ( _1 )
# 710 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 115 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 719 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 116 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 727 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                                                             ( While (_3, _5) )
# 735 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
               ( [] )
# 741 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 122 "parser.mly"
               ( _1 )
# 748 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 126 "parser.mly"
                           ( [_1] )
# 755 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 127 "parser.mly"
                           ( _1 :: _3 )
# 763 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
                               ( (true, _2, _3) )
# 771 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "parser.mly"
                               ( (false, _1, _2) )
# 779 "parser.ml"
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
