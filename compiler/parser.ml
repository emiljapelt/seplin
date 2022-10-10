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
  | VAR
  | PRINT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Absyn
  open ProgramRep
# 46 "parser.ml"
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
  291 (* VAR *);
  292 (* PRINT *);
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
\002\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\011\000\000\000\000\000\000\000\059\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\054\000\000\000\000\000\000\000\005\000\014\000\013\000\
\015\000\000\000\000\000\000\000\000\000\000\000\057\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\058\000\000\000\008\000\056\000\009\000\007\000\
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

let yysindex = "\039\000\
\014\255\000\000\000\000\000\000\035\255\037\255\002\255\000\000\
\045\000\014\255\047\255\056\255\060\255\064\255\000\000\000\000\
\249\254\007\255\007\255\250\254\099\255\000\000\002\255\067\255\
\073\255\000\000\048\255\078\255\099\255\000\000\000\000\000\000\
\000\000\099\255\099\255\099\255\048\000\082\255\000\000\081\255\
\007\255\081\255\062\000\001\000\049\255\000\000\099\255\099\255\
\099\255\099\255\099\255\099\255\099\255\099\255\099\255\099\255\
\099\255\000\000\000\000\051\255\000\000\000\000\000\000\000\000\
\000\000\049\255\049\255\000\000\156\000\156\000\238\255\238\255\
\238\255\238\255\000\000\049\255\013\255\000\000\000\000\084\255\
\085\255\003\255\094\255\099\255\098\255\000\000\095\255\079\255\
\051\255\000\000\099\255\099\255\099\255\099\255\103\255\104\255\
\105\255\145\000\251\254\000\000\000\000\000\000\145\000\132\000\
\102\255\000\000\019\000\037\000\106\255\009\255\099\255\099\255\
\000\000\099\255\000\000\088\255\088\255\099\255\099\255\000\000\
\076\000\090\000\000\000\083\255\000\000\104\000\118\000\000\000\
\000\000\088\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\116\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\116\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\110\255\110\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\113\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\112\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\114\255\000\000\000\000\000\000\000\000\
\000\000\128\255\144\255\000\000\230\255\242\255\176\255\192\255\
\208\255\224\255\000\000\160\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\114\255\000\000\000\000\118\255\000\000\000\000\000\000\000\000\
\000\000\101\255\000\000\000\000\000\000\000\000\117\255\130\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\133\000\000\000\036\000\235\255\125\000\226\255\067\000\
\000\000\000\000\046\000\000\000\190\255\122\000\000\000"

let yytablesize = 439
let yytable = "\037\000\
\021\000\029\000\112\000\003\000\003\000\004\000\004\000\043\000\
\003\000\061\000\004\000\063\000\044\000\045\000\046\000\003\000\
\119\000\004\000\005\000\006\000\091\000\092\000\022\000\030\000\
\113\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\011\000\095\000\120\000\001\000\
\023\000\012\000\014\000\013\000\015\000\011\000\051\000\007\000\
\051\000\124\000\125\000\051\000\003\000\017\000\004\000\051\000\
\051\000\077\000\038\000\051\000\051\000\060\000\098\000\133\000\
\018\000\078\000\079\000\049\000\019\000\103\000\020\000\107\000\
\108\000\039\000\056\000\051\000\041\000\051\000\051\000\051\000\
\051\000\080\000\040\000\081\000\082\000\083\000\084\000\042\000\
\059\000\121\000\122\000\060\000\093\000\094\000\077\000\085\000\
\126\000\127\000\060\000\031\000\097\000\032\000\078\000\079\000\
\099\000\033\000\100\000\034\000\101\000\109\000\110\000\115\000\
\111\000\118\000\130\000\002\000\035\000\096\000\080\000\053\000\
\081\000\027\000\055\000\084\000\085\000\039\000\036\000\035\000\
\027\000\027\000\034\000\027\000\027\000\027\000\027\000\027\000\
\027\000\024\000\027\000\037\000\027\000\027\000\016\000\028\000\
\024\000\024\000\030\000\024\000\024\000\024\000\024\000\024\000\
\024\000\026\000\024\000\102\000\024\000\024\000\000\000\123\000\
\026\000\026\000\062\000\026\000\026\000\026\000\026\000\026\000\
\026\000\017\000\026\000\000\000\026\000\026\000\000\000\000\000\
\017\000\017\000\000\000\017\000\017\000\017\000\017\000\017\000\
\017\000\021\000\017\000\000\000\017\000\017\000\000\000\000\000\
\000\000\000\000\000\000\021\000\021\000\021\000\021\000\021\000\
\021\000\023\000\000\000\000\000\021\000\021\000\000\000\000\000\
\000\000\000\000\000\000\023\000\023\000\023\000\023\000\023\000\
\023\000\020\000\000\000\000\000\023\000\023\000\000\000\000\000\
\000\000\000\000\000\000\020\000\020\000\020\000\020\000\020\000\
\020\000\022\000\000\000\000\000\020\000\020\000\000\000\018\000\
\000\000\000\000\000\000\022\000\022\000\022\000\022\000\022\000\
\022\000\018\000\018\000\019\000\022\000\022\000\047\000\048\000\
\049\000\000\000\018\000\018\000\000\000\019\000\019\000\056\000\
\057\000\000\000\065\000\000\000\000\000\000\000\019\000\019\000\
\000\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\116\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\117\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\058\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\064\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\000\000\128\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\129\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\131\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\132\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\114\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\047\000\048\000\049\000\000\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000"

let yycheck = "\021\000\
\008\001\008\001\008\001\002\001\002\001\004\001\004\001\029\000\
\002\001\040\000\004\001\042\000\034\000\035\000\036\000\002\001\
\008\001\004\001\005\001\006\001\008\001\009\001\030\001\030\001\
\030\001\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\001\000\035\001\030\001\001\000\
\034\001\007\001\007\000\007\001\000\000\010\000\002\001\034\001\
\004\001\116\000\117\000\007\001\002\001\007\001\004\001\011\001\
\012\001\007\001\023\000\015\001\016\001\011\001\084\000\130\000\
\009\001\015\001\016\001\019\001\009\001\091\000\007\001\093\000\
\094\000\007\001\026\001\031\001\029\001\033\001\034\001\035\001\
\036\001\031\001\010\001\033\001\034\001\035\001\036\001\010\001\
\007\001\111\000\112\000\011\001\009\001\009\001\007\001\060\000\
\118\000\119\000\011\001\001\001\007\001\003\001\015\001\016\001\
\007\001\007\001\012\001\009\001\030\001\007\001\007\001\010\001\
\008\001\008\001\032\001\000\000\018\001\082\000\031\001\010\001\
\033\001\010\001\010\001\036\001\089\000\012\001\028\001\010\001\
\017\001\018\001\030\001\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\027\001\010\001\029\001\030\001\010\000\019\000\
\017\001\018\001\030\001\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\027\001\089\000\029\001\030\001\255\255\114\000\
\017\001\018\001\041\000\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\027\001\255\255\029\001\030\001\255\255\255\255\
\017\001\018\001\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\027\001\255\255\029\001\030\001\255\255\255\255\
\255\255\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\255\255\255\255\029\001\030\001\255\255\255\255\
\255\255\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\255\255\255\255\029\001\030\001\255\255\255\255\
\255\255\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\010\001\255\255\255\255\029\001\030\001\255\255\010\001\
\255\255\255\255\255\255\020\001\021\001\022\001\023\001\024\001\
\025\001\020\001\021\001\010\001\029\001\030\001\017\001\018\001\
\019\001\255\255\029\001\030\001\255\255\020\001\021\001\026\001\
\027\001\255\255\010\001\255\255\255\255\255\255\029\001\030\001\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\010\001\255\255\255\255\255\255\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\010\001\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\255\255\255\255\030\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\255\255\255\255\030\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\255\255\030\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\255\255\255\255\030\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\255\255\255\255\030\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\255\255\255\255\030\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\029\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\017\001\018\001\019\001\255\255\
\255\255\022\001\023\001\024\001\025\001\026\001\027\001"

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
# 340 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                        ( [] )
# 346 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 38 "parser.mly"
                        ( _1 :: _2 )
# 354 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 42 "parser.mly"
                                                              ( Global (false, _1, _2) )
# 362 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parser.mly"
                                                              ( Global (true, _2, _3) )
# 370 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 44 "parser.mly"
                                                              ( GlobalAssign (false, _1, _2, _4)   )
# 379 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 45 "parser.mly"
                                                              ( GlobalAssign (true, _2, _3, _5)    )
# 388 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 46 "parser.mly"
                                                      ( Routine (Internal, _2, _4, _6) )
# 397 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'paramdecs) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 47 "parser.mly"
                                                      ( Routine (External, _2, _4, _6) )
# 406 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 51 "parser.mly"
              ( T_Int )
# 412 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
              ( T_Bool )
# 418 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 56 "parser.mly"
                                ( Block _2 )
# 425 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 60 "parser.mly"
                ( Bool _1 )
# 432 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 61 "parser.mly"
              ( Int _1 )
# 439 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 62 "parser.mly"
                 ( Lookup _1  )
# 446 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 63 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 454 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 64 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 462 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 65 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 470 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 66 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 478 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 67 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 486 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 68 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 494 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 69 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 502 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 70 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 510 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 71 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 518 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                          ( Binary_op ("*", _1, _3) )
# 526 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 73 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 534 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 74 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 541 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 75 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 548 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 76 "parser.mly"
                                                          ( _2 )
# 555 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 80 "parser.mly"
                                                ( Assign (_1, _3) )
# 563 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'params) in
    Obj.repr(
# 81 "parser.mly"
                                                ( Call (_1, _3) )
# 571 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                                                ( Stop )
# 577 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                                                ( Halt )
# 583 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 84 "parser.mly"
                                                ( Print _2 )
# 590 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser.mly"
              ( [] )
# 596 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 89 "parser.mly"
              ( _1 )
# 603 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 93 "parser.mly"
                                                        ( [_1] )
# 610 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 94 "parser.mly"
                                           ( _1 :: _3 )
# 618 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                               ( [] )
# 624 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 99 "parser.mly"
                               ( _1 :: _2)
# 632 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 103 "parser.mly"
                                                             ( Statement _1 )
# 639 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 104 "parser.mly"
                                                             ( Declaration (false, _1, _2) )
# 647 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 105 "parser.mly"
                                                             ( Declaration (true, _2, _3) )
# 655 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 106 "parser.mly"
                                                             ( AssignDeclaration (false, _1, _2, _4) )
# 664 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 107 "parser.mly"
                                                             ( AssignDeclaration (true, _2, _3, _5) )
# 673 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 108 "parser.mly"
                                                             ( VarDeclaration (false, _2, _4) )
# 681 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 109 "parser.mly"
                                                             ( VarDeclaration (true, _3, _5) )
# 689 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 113 "parser.mly"
                                                       ( Expression _1 )
# 696 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 114 "parser.mly"
                                                       ( _1 )
# 703 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 115 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 712 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 116 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 720 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 117 "parser.mly"
                                                             ( While (_3, _5) )
# 728 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "parser.mly"
                  ( [] )
# 734 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 122 "parser.mly"
                  ( _1 )
# 741 "parser.ml"
               : 'paramdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 126 "parser.mly"
                              ( [_1] )
# 748 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'paramdecs1) in
    Obj.repr(
# 127 "parser.mly"
                              ( _1 :: _3 )
# 756 "parser.ml"
               : 'paramdecs1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 131 "parser.mly"
                          ( (false, _1, _2) )
# 764 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 132 "parser.mly"
                          ( (true, _2, _3) )
# 772 "parser.ml"
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
