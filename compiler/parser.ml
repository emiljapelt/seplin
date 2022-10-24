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
  | DOT
  | SEMI
  | EOF
  | IF
  | ELSE
  | WHILE
  | UNTIL
  | FOR
  | BREAK
  | CONTINUE
  | LOCKED
  | TRANSFER
  | VAR
  | PRINT

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
  open Absyn
  open ProgramRep
  open Exceptions
# 53 "parser.ml"
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
  286 (* DOT *);
  287 (* SEMI *);
    0 (* EOF *);
  288 (* IF *);
  289 (* ELSE *);
  290 (* WHILE *);
  291 (* UNTIL *);
  292 (* FOR *);
  293 (* BREAK *);
  294 (* CONTINUE *);
  295 (* LOCKED *);
  296 (* TRANSFER *);
  297 (* VAR *);
  298 (* PRINT *);
    0|]

let yytransl_block = [|
  257 (* CSTINT *);
  259 (* CSTBOOL *);
  263 (* NAME *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\007\000\008\000\008\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\010\000\010\000\012\000\012\000\009\000\009\000\013\000\013\000\
\015\000\015\000\015\000\015\000\015\000\015\000\014\000\014\000\
\014\000\014\000\014\000\014\000\014\000\006\000\006\000\016\000\
\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\003\000\004\000\005\000\006\000\006\000\
\006\000\006\000\006\000\001\000\001\000\003\000\005\000\006\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\003\000\004\000\001\000\001\000\001\000\001\000\002\000\
\000\000\001\000\001\000\003\000\000\000\002\000\001\000\001\000\
\003\000\004\000\005\000\006\000\005\000\006\000\002\000\001\000\
\007\000\005\000\005\000\005\000\008\000\000\000\001\000\001\000\
\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\013\000\000\000\000\000\000\000\068\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\063\000\000\000\000\000\000\000\005\000\018\000\017\000\
\019\000\000\000\000\000\000\000\000\000\000\000\067\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\066\000\000\000\000\000\008\000\010\000\065\000\
\009\000\011\000\007\000\033\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\036\000\037\000\000\000\000\000\000\000\000\000\038\000\039\000\
\000\000\000\000\000\000\000\000\056\000\000\000\000\000\000\000\
\047\000\048\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\055\000\
\046\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\000\000\000\
\000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\000\000\000\000\000\000\000\044\000\000\000\059\000\060\000\
\000\000\000\000\000\000\053\000\051\000\016\000\000\000\000\000\
\054\000\052\000\057\000\000\000\061\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\024\000\116\000\025\000\093\000\063\000\
\094\000\117\000\095\000\118\000\096\000\097\000\098\000\026\000\
\027\000"

let yysindex = "\010\000\
\014\255\000\000\000\000\000\000\032\255\043\255\020\255\000\000\
\057\000\014\255\052\255\001\255\075\255\078\255\000\000\000\000\
\009\255\019\255\019\255\029\255\071\255\000\000\020\255\083\255\
\082\255\000\000\080\255\102\255\071\255\000\000\000\000\000\000\
\000\000\071\255\071\255\071\255\119\000\109\255\000\000\034\255\
\019\255\034\255\134\000\054\000\035\255\000\000\071\255\071\255\
\071\255\071\255\071\255\071\255\071\255\071\255\071\255\071\255\
\071\255\000\000\000\000\121\255\112\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\255\035\255\000\000\248\000\
\248\000\076\255\076\255\076\255\076\255\000\000\035\255\088\255\
\000\000\000\000\111\255\115\255\117\255\118\255\000\000\000\000\
\003\255\122\255\071\255\123\255\000\000\119\255\103\255\121\255\
\000\000\000\000\124\255\071\255\071\255\071\255\071\255\071\255\
\002\255\128\255\132\255\138\255\237\000\040\255\000\000\000\000\
\000\000\071\255\237\000\224\000\139\255\000\000\072\000\090\000\
\108\000\071\255\142\255\044\255\071\255\071\255\000\000\141\255\
\071\255\000\000\031\255\031\255\031\255\149\000\071\255\071\255\
\000\000\164\000\179\000\134\255\000\000\133\255\000\000\000\000\
\154\255\194\000\209\000\000\000\000\000\000\000\031\255\144\255\
\000\000\000\000\000\000\031\255\000\000"

let yyrindex = "\000\000\
\152\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\152\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\157\255\157\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\158\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\155\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\159\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\177\255\193\255\000\000\027\000\
\039\000\225\255\245\255\005\000\021\000\000\000\209\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\159\255\
\000\000\000\000\000\000\000\000\164\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\045\255\000\000\000\000\000\000\
\000\000\164\255\046\255\171\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\001\000\000\000\106\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\173\000\000\000\002\000\235\255\166\000\009\000\216\255\
\092\000\075\000\045\000\064\000\000\000\211\255\100\000\168\000\
\000\000"

let yytablesize = 531
let yytable = "\037\000\
\015\000\066\000\011\000\003\000\003\000\004\000\004\000\043\000\
\014\000\018\000\001\000\011\000\044\000\045\000\046\000\003\000\
\021\000\004\000\005\000\006\000\003\000\003\000\004\000\004\000\
\038\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\029\000\080\000\012\000\022\000\
\089\000\060\000\090\000\106\000\060\000\081\000\082\000\126\000\
\062\000\013\000\065\000\136\000\007\000\049\000\040\000\034\000\
\015\000\023\000\017\000\030\000\056\000\092\000\083\000\061\000\
\084\000\085\000\086\000\087\000\088\000\109\000\127\000\031\000\
\091\000\032\000\137\000\040\000\034\000\033\000\115\000\034\000\
\119\000\120\000\121\000\019\000\020\000\142\000\143\000\144\000\
\035\000\039\000\107\000\040\000\047\000\048\000\049\000\100\000\
\101\000\092\000\036\000\150\000\134\000\056\000\057\000\138\000\
\139\000\155\000\092\000\058\000\041\000\058\000\157\000\042\000\
\058\000\146\000\147\000\059\000\058\000\058\000\099\000\102\000\
\058\000\058\000\003\000\103\000\004\000\104\000\105\000\080\000\
\108\000\110\000\111\000\060\000\114\000\112\000\123\000\081\000\
\082\000\058\000\124\000\058\000\058\000\058\000\058\000\058\000\
\058\000\125\000\058\000\058\000\130\000\135\000\140\000\002\000\
\083\000\156\000\084\000\085\000\086\000\087\000\088\000\089\000\
\080\000\090\000\091\000\061\000\031\000\151\000\062\000\064\000\
\081\000\082\000\045\000\031\000\031\000\041\000\031\000\031\000\
\031\000\031\000\031\000\031\000\043\000\031\000\016\000\031\000\
\028\000\031\000\028\000\113\000\128\000\152\000\087\000\088\000\
\141\000\028\000\028\000\091\000\028\000\028\000\028\000\028\000\
\028\000\028\000\030\000\028\000\122\000\028\000\000\000\028\000\
\064\000\030\000\030\000\000\000\030\000\030\000\030\000\030\000\
\030\000\030\000\021\000\030\000\000\000\030\000\000\000\030\000\
\000\000\021\000\021\000\000\000\021\000\021\000\021\000\021\000\
\021\000\021\000\025\000\021\000\000\000\021\000\000\000\021\000\
\000\000\000\000\000\000\000\000\025\000\025\000\025\000\025\000\
\025\000\025\000\000\000\000\000\000\000\025\000\027\000\025\000\
\000\000\000\000\015\000\000\000\015\000\015\000\015\000\000\000\
\027\000\027\000\027\000\027\000\027\000\027\000\024\000\000\000\
\000\000\027\000\000\000\027\000\000\000\000\000\000\000\000\000\
\024\000\024\000\024\000\024\000\024\000\024\000\026\000\000\000\
\000\000\024\000\000\000\024\000\022\000\000\000\000\000\015\000\
\026\000\026\000\026\000\026\000\026\000\026\000\022\000\022\000\
\023\000\026\000\000\000\026\000\000\000\000\000\000\000\022\000\
\000\000\022\000\023\000\023\000\000\000\000\000\000\000\068\000\
\000\000\000\000\000\000\023\000\000\000\023\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\131\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\132\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\133\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\000\000\000\000\000\000\058\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\000\000\067\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\000\000\000\000\000\000\145\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\000\000\000\000\148\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\000\000\000\000\
\000\000\149\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\000\000\
\153\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\000\000\000\000\000\000\154\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\129\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\047\000\048\000\049\000\000\000\000\000\052\000\053\000\054\000\
\055\000\056\000\057\000"

let yycheck = "\021\000\
\000\000\042\000\001\000\002\001\002\001\004\001\004\001\029\000\
\007\000\009\001\001\000\010\000\034\000\035\000\036\000\002\001\
\008\001\004\001\005\001\006\001\002\001\002\001\004\001\004\001\
\023\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\008\001\007\001\007\001\031\001\
\039\001\011\001\041\001\041\001\011\001\015\001\016\001\008\001\
\040\000\007\001\042\000\008\001\039\001\019\001\010\001\010\001\
\000\000\039\001\007\001\031\001\026\001\060\000\032\001\030\001\
\034\001\035\001\036\001\037\001\038\001\091\000\031\001\001\001\
\042\001\003\001\031\001\031\001\031\001\007\001\100\000\009\001\
\102\000\103\000\104\000\009\001\007\001\131\000\132\000\133\000\
\018\001\007\001\089\000\010\001\017\001\018\001\019\001\008\001\
\009\001\096\000\028\001\140\000\122\000\026\001\027\001\125\000\
\126\000\151\000\105\000\002\001\029\001\004\001\156\000\010\001\
\007\001\135\000\136\000\007\001\011\001\012\001\007\001\009\001\
\015\001\016\001\002\001\009\001\004\001\009\001\009\001\007\001\
\007\001\007\001\012\001\011\001\009\001\031\001\007\001\015\001\
\016\001\032\001\007\001\034\001\035\001\036\001\037\001\038\001\
\039\001\008\001\041\001\042\001\010\001\008\001\010\001\000\000\
\032\001\010\001\034\001\035\001\036\001\037\001\038\001\039\001\
\007\001\041\001\042\001\030\001\010\001\033\001\010\001\010\001\
\015\001\016\001\012\001\017\001\018\001\010\001\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\010\000\029\001\
\019\000\031\001\010\001\096\000\114\000\145\000\037\001\038\001\
\129\000\017\001\018\001\042\001\020\001\021\001\022\001\023\001\
\024\001\025\001\010\001\027\001\105\000\029\001\255\255\031\001\
\041\000\017\001\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\010\001\027\001\255\255\029\001\255\255\031\001\
\255\255\017\001\018\001\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\010\001\027\001\255\255\029\001\255\255\031\001\
\255\255\255\255\255\255\255\255\020\001\021\001\022\001\023\001\
\024\001\025\001\255\255\255\255\255\255\029\001\010\001\031\001\
\255\255\255\255\002\001\255\255\004\001\005\001\006\001\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\255\255\
\255\255\029\001\255\255\031\001\255\255\255\255\255\255\255\255\
\020\001\021\001\022\001\023\001\024\001\025\001\010\001\255\255\
\255\255\029\001\255\255\031\001\010\001\255\255\255\255\039\001\
\020\001\021\001\022\001\023\001\024\001\025\001\020\001\021\001\
\010\001\029\001\255\255\031\001\255\255\255\255\255\255\029\001\
\255\255\031\001\020\001\021\001\255\255\255\255\255\255\010\001\
\255\255\255\255\255\255\029\001\255\255\031\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\010\001\255\255\255\255\255\255\255\255\255\255\255\255\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\010\001\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\255\255\255\255\255\255\031\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\255\255\255\255\255\255\031\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\255\255\255\255\255\255\031\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\255\255\
\255\255\255\255\031\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\255\255\255\255\
\255\255\031\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\255\255\255\255\255\255\
\031\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\255\255\255\255\031\001\
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
  DOT\000\
  SEMI\000\
  EOF\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  UNTIL\000\
  FOR\000\
  BREAK\000\
  CONTINUE\000\
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
# 35 "parser.mly"
                  ( Topdecs _1 )
# 396 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                        ( [] )
# 402 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 40 "parser.mly"
                        ( _1 :: _2 )
# 410 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 44 "parser.mly"
                                                              ( Global (false, _1, _2) )
# 418 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 45 "parser.mly"
                                                              ( Global (true, _2, _3) )
# 426 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 46 "parser.mly"
                                                              ( GlobalAssign (false, _1, _2, _4)   )
# 435 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 47 "parser.mly"
                                                              ( GlobalAssign (true, _2, _3, _5)    )
# 444 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 48 "parser.mly"
                                                   ( Routine (Internal, _2, _4, _6) )
# 453 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 49 "parser.mly"
                                                   ( Routine (External, _2, _4, _6) )
# 462 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'chain) in
    Obj.repr(
# 50 "parser.mly"
                                                   ( Routine (Internal, _2, _4, Block _6) )
# 471 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'chain) in
    Obj.repr(
# 51 "parser.mly"
                                                   ( Routine (External, _2, _4, Block _6) )
# 480 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
              ( T_Int )
# 486 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
              ( T_Bool )
# 492 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 60 "parser.mly"
                                ( Block _2 )
# 499 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 64 "parser.mly"
                                 ( [Statement (Expression (Call(_2, _4)))] )
# 507 "parser.ml"
               : 'chain))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'arguments) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'chain) in
    Obj.repr(
# 65 "parser.mly"
                                       ( (Statement (Expression (Call (_2, _4)))) :: _6 )
# 516 "parser.ml"
               : 'chain))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 69 "parser.mly"
                ( Bool _1 )
# 523 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
              ( Int _1 )
# 530 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                 ( Lookup _1  )
# 537 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 545 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 73 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 553 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 74 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 561 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 75 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 569 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 76 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 577 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 77 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 585 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 78 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 593 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 79 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 601 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 80 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 609 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 81 "parser.mly"
                                                          ( Binary_op ("*", _1, _3) )
# 617 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 82 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 625 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 83 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 632 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 84 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 639 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 85 "parser.mly"
                                                          ( _2 )
# 646 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 89 "parser.mly"
                                                ( Assign (_1, _3) )
# 654 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 90 "parser.mly"
                                                ( Call (_1, _3) )
# 662 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                                                ( Stop )
# 668 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
                                                ( Halt )
# 674 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 93 "parser.mly"
                                                ( Break )
# 680 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                                                ( Continue )
# 686 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 95 "parser.mly"
                                                ( Print _2 )
# 693 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                 ( [] )
# 699 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 100 "parser.mly"
                 ( _1 )
# 706 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 104 "parser.mly"
                                              ( [_1] )
# 713 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 105 "parser.mly"
                                              ( _1 :: _3 )
# 721 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                               ( [] )
# 727 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 110 "parser.mly"
                               ( _1 :: _2)
# 735 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 114 "parser.mly"
                                                             ( Statement _1 )
# 742 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 115 "parser.mly"
                                                             ( Declaration _1 )
# 749 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 119 "parser.mly"
                                                             ( TypeDeclaration (false, _1, _2) )
# 757 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 120 "parser.mly"
                                                             ( TypeDeclaration (true, _2, _3) )
# 765 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 121 "parser.mly"
                                                             ( AssignDeclaration (false, _1, _2, _4) )
# 774 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 122 "parser.mly"
                                                             ( AssignDeclaration (true, _2, _3, _5) )
# 783 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 123 "parser.mly"
                                                             ( VarDeclaration (false, _2, _4) )
# 791 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 124 "parser.mly"
                                                             ( VarDeclaration (true, _3, _5) )
# 799 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 128 "parser.mly"
                                                       ( Expression _1 )
# 806 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 129 "parser.mly"
                                                       ( _1 )
# 813 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 130 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 822 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 131 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 830 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 132 "parser.mly"
                                                             ( While (_3, _5) )
# 838 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 133 "parser.mly"
                                                             ( While (Unary_op("!", _3), _5) )
# 846 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'unassignable_expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 134 "parser.mly"
                                                                                 ( For (_3, _4, _6, _8) )
# 856 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "parser.mly"
               ( [] )
# 862 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 139 "parser.mly"
               ( _1 )
# 869 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 143 "parser.mly"
                           ( [_1] )
# 876 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 144 "parser.mly"
                           ( _1 :: _3 )
# 884 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 148 "parser.mly"
                               ( (true, _2, _3) )
# 892 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 149 "parser.mly"
                               ( (false, _1, _2) )
# 900 "parser.ml"
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
