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
\011\000\011\000\011\000\011\000\010\000\010\000\012\000\012\000\
\009\000\009\000\013\000\013\000\015\000\015\000\015\000\015\000\
\015\000\015\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\006\000\006\000\016\000\016\000\017\000\017\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\003\000\004\000\005\000\006\000\006\000\
\006\000\006\000\006\000\001\000\001\000\003\000\005\000\006\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\002\000\002\000\
\003\000\003\000\004\000\004\000\004\000\004\000\004\000\001\000\
\001\000\001\000\001\000\002\000\000\000\001\000\001\000\003\000\
\000\000\002\000\001\000\001\000\003\000\004\000\005\000\006\000\
\005\000\006\000\002\000\001\000\007\000\005\000\005\000\005\000\
\008\000\000\000\001\000\001\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\012\000\013\000\000\000\000\000\000\000\072\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\067\000\000\000\000\000\000\000\005\000\018\000\017\000\
\019\000\000\000\000\000\000\000\000\000\000\000\071\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\070\000\000\000\000\000\008\000\010\000\069\000\
\009\000\011\000\007\000\033\000\000\000\000\000\029\000\000\000\
\000\000\000\000\000\000\000\000\000\000\020\000\000\000\000\000\
\040\000\041\000\000\000\000\000\000\000\000\000\042\000\043\000\
\000\000\000\000\000\000\000\000\060\000\000\000\000\000\000\000\
\051\000\052\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\014\000\059\000\050\000\000\000\000\000\000\000\
\000\000\046\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\000\000\000\
\000\000\039\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\054\000\000\000\000\000\000\000\
\048\000\000\000\063\000\064\000\000\000\000\000\000\000\057\000\
\055\000\016\000\000\000\000\000\058\000\056\000\061\000\000\000\
\065\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\024\000\120\000\025\000\093\000\063\000\
\094\000\121\000\095\000\122\000\096\000\097\000\098\000\026\000\
\027\000"

let yysindex = "\005\000\
\018\255\000\000\000\000\000\000\004\255\057\255\045\255\000\000\
\039\000\018\255\058\255\001\255\063\255\076\255\000\000\000\000\
\029\255\014\255\014\255\030\255\135\255\000\000\045\255\083\255\
\082\255\000\000\065\255\085\255\135\255\000\000\000\000\000\000\
\000\000\135\255\135\255\135\255\105\000\089\255\000\000\247\254\
\014\255\247\254\120\000\040\000\033\255\000\000\135\255\135\255\
\135\255\135\255\135\255\135\255\135\255\135\255\135\255\135\255\
\135\255\000\000\000\000\113\255\092\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\033\255\033\255\000\000\234\000\
\234\000\025\000\025\000\025\000\025\000\000\000\033\255\028\000\
\000\000\000\000\091\255\097\255\101\255\110\255\000\000\000\000\
\015\255\094\255\135\255\118\255\000\000\120\255\099\255\113\255\
\000\000\000\000\125\255\135\255\135\255\129\255\171\255\174\255\
\187\255\135\255\135\255\135\255\003\255\126\255\136\255\203\255\
\223\000\032\255\000\000\000\000\000\000\135\255\223\000\210\000\
\165\255\000\000\135\255\135\255\135\255\135\255\058\000\076\000\
\094\000\135\255\217\255\037\255\135\255\135\255\000\000\181\255\
\135\255\000\000\223\000\223\000\223\000\223\000\124\255\124\255\
\124\255\135\000\135\255\135\255\000\000\150\000\165\000\208\255\
\000\000\194\255\000\000\000\000\051\255\180\000\195\000\000\000\
\000\000\000\000\124\255\219\255\000\000\000\000\000\000\124\255\
\000\000"

let yyrindex = "\000\000\
\239\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\239\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\231\255\231\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\233\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\147\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\241\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\163\255\179\255\000\000\087\255\
\010\000\211\255\227\255\244\255\004\000\000\000\195\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\241\255\
\000\000\000\000\000\000\000\000\236\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\038\255\000\000\000\000\000\000\000\000\236\255\167\255\245\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\183\255\197\255\199\255\213\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\000\000\039\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\235\000\000\000\002\000\235\255\238\000\042\000\218\255\
\164\000\153\000\115\000\137\000\000\000\234\255\168\000\237\000\
\000\000"

let yytablesize = 517
let yytable = "\037\000\
\015\000\060\000\011\000\066\000\003\000\001\000\004\000\043\000\
\014\000\018\000\012\000\011\000\044\000\045\000\046\000\003\000\
\003\000\004\000\004\000\003\000\061\000\004\000\005\000\006\000\
\038\000\069\000\070\000\071\000\072\000\073\000\074\000\075\000\
\076\000\077\000\078\000\079\000\021\000\029\000\015\000\134\000\
\062\000\089\000\062\000\090\000\148\000\062\000\003\000\044\000\
\004\000\062\000\062\000\049\000\023\000\062\000\062\000\110\000\
\007\000\080\000\056\000\022\000\030\000\092\000\135\000\013\000\
\017\000\081\000\082\000\149\000\044\000\113\000\062\000\019\000\
\062\000\062\000\062\000\062\000\062\000\062\000\119\000\062\000\
\062\000\062\000\020\000\065\000\127\000\128\000\129\000\087\000\
\088\000\039\000\111\000\040\000\091\000\041\000\042\000\059\000\
\022\000\092\000\099\000\106\000\112\000\139\000\140\000\141\000\
\142\000\107\000\022\000\022\000\146\000\108\000\092\000\150\000\
\151\000\162\000\003\000\022\000\004\000\022\000\109\000\080\000\
\154\000\155\000\156\000\060\000\114\000\158\000\159\000\081\000\
\082\000\116\000\080\000\115\000\131\000\118\000\060\000\031\000\
\123\000\032\000\081\000\082\000\167\000\033\000\132\000\034\000\
\083\000\169\000\084\000\085\000\086\000\087\000\088\000\089\000\
\035\000\090\000\091\000\083\000\031\000\084\000\085\000\086\000\
\087\000\088\000\036\000\031\000\031\000\091\000\031\000\031\000\
\031\000\031\000\031\000\031\000\028\000\031\000\138\000\031\000\
\034\000\031\000\124\000\028\000\028\000\125\000\028\000\028\000\
\028\000\028\000\028\000\028\000\030\000\028\000\152\000\028\000\
\035\000\028\000\126\000\030\000\030\000\034\000\030\000\030\000\
\030\000\030\000\030\000\030\000\021\000\030\000\036\000\030\000\
\037\000\030\000\133\000\021\000\021\000\035\000\021\000\021\000\
\021\000\021\000\021\000\021\000\025\000\021\000\038\000\021\000\
\147\000\021\000\163\000\036\000\168\000\037\000\025\000\025\000\
\025\000\025\000\025\000\025\000\027\000\061\000\002\000\025\000\
\066\000\025\000\068\000\038\000\016\000\045\000\027\000\027\000\
\027\000\027\000\027\000\027\000\049\000\024\000\047\000\027\000\
\028\000\027\000\015\000\117\000\015\000\015\000\015\000\024\000\
\024\000\024\000\024\000\024\000\024\000\026\000\136\000\164\000\
\024\000\153\000\024\000\023\000\130\000\064\000\000\000\026\000\
\026\000\026\000\026\000\026\000\026\000\023\000\023\000\000\000\
\026\000\000\000\026\000\100\000\101\000\000\000\023\000\015\000\
\023\000\047\000\048\000\049\000\102\000\103\000\104\000\000\000\
\000\000\068\000\056\000\057\000\000\000\000\000\000\000\105\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\143\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\144\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\145\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\000\000\000\000\000\000\058\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\000\000\067\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\000\000\000\000\000\000\157\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\000\000\160\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\000\000\000\000\000\000\161\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\000\000\000\000\165\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\000\000\000\000\
\000\000\166\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\137\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\047\000\048\000\049\000\000\000\000\000\052\000\
\053\000\054\000\055\000\056\000\057\000"

let yycheck = "\021\000\
\000\000\011\001\001\000\042\000\002\001\001\000\004\001\029\000\
\007\000\009\001\007\001\010\000\034\000\035\000\036\000\002\001\
\002\001\004\001\004\001\002\001\030\001\004\001\005\001\006\001\
\023\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\008\001\008\001\000\000\008\001\
\002\001\039\001\004\001\041\001\008\001\007\001\002\001\010\001\
\004\001\011\001\012\001\019\001\039\001\015\001\016\001\041\001\
\039\001\007\001\026\001\031\001\031\001\060\000\031\001\007\001\
\007\001\015\001\016\001\031\001\031\001\091\000\032\001\009\001\
\034\001\035\001\036\001\037\001\038\001\039\001\100\000\041\001\
\042\001\040\000\007\001\042\000\106\000\107\000\108\000\037\001\
\038\001\007\001\089\000\010\001\042\001\029\001\010\001\007\001\
\010\001\096\000\007\001\009\001\007\001\123\000\124\000\125\000\
\126\000\009\001\020\001\021\001\130\000\009\001\109\000\133\000\
\134\000\152\000\002\001\029\001\004\001\031\001\009\001\007\001\
\143\000\144\000\145\000\011\001\007\001\147\000\148\000\015\001\
\016\001\031\001\007\001\012\001\007\001\009\001\011\001\001\001\
\008\001\003\001\015\001\016\001\163\000\007\001\007\001\009\001\
\032\001\168\000\034\001\035\001\036\001\037\001\038\001\039\001\
\018\001\041\001\042\001\032\001\010\001\034\001\035\001\036\001\
\037\001\038\001\028\001\017\001\018\001\042\001\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\010\001\029\001\
\010\001\031\001\008\001\017\001\018\001\008\001\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\010\001\029\001\
\010\001\031\001\008\001\017\001\018\001\031\001\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\010\001\029\001\
\010\001\031\001\008\001\017\001\018\001\031\001\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\010\001\029\001\
\008\001\031\001\033\001\031\001\010\001\031\001\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\030\001\000\000\029\001\
\010\001\031\001\010\001\031\001\010\000\010\001\020\001\021\001\
\022\001\023\001\024\001\025\001\012\001\010\001\010\001\029\001\
\019\000\031\001\002\001\096\000\004\001\005\001\006\001\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\118\000\157\000\
\029\001\137\000\031\001\010\001\109\000\041\000\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\020\001\021\001\255\255\
\029\001\255\255\031\001\008\001\009\001\255\255\029\001\039\001\
\031\001\017\001\018\001\019\001\017\001\018\001\019\001\255\255\
\255\255\010\001\026\001\027\001\255\255\255\255\255\255\028\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\010\001\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\010\001\
\255\255\255\255\255\255\255\255\255\255\255\255\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\255\255\255\255\255\255\031\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\255\255\255\255\255\255\031\001\017\001\
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
\023\001\024\001\025\001\026\001\027\001\255\255\029\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\017\001\018\001\019\001\255\255\255\255\022\001\
\023\001\024\001\025\001\026\001\027\001"

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
# 398 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                        ( [] )
# 404 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 40 "parser.mly"
                        ( _1 :: _2 )
# 412 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 44 "parser.mly"
                                                              ( Global (false, _1, _2) )
# 420 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 45 "parser.mly"
                                                              ( Global (true, _2, _3) )
# 428 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 46 "parser.mly"
                                                              ( GlobalAssign (false, _1, _2, _4)   )
# 437 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 47 "parser.mly"
                                                              ( GlobalAssign (true, _2, _3, _5)    )
# 446 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 48 "parser.mly"
                                                   ( Routine (Internal, _2, _4, _6) )
# 455 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 49 "parser.mly"
                                                   ( Routine (External, _2, _4, _6) )
# 464 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'chain) in
    Obj.repr(
# 50 "parser.mly"
                                                   ( Routine (Internal, _2, _4, Block _6) )
# 473 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'chain) in
    Obj.repr(
# 51 "parser.mly"
                                                   ( Routine (External, _2, _4, Block _6) )
# 482 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "parser.mly"
              ( T_Int )
# 488 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "parser.mly"
              ( T_Bool )
# 494 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 60 "parser.mly"
                                ( Block _2 )
# 501 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 64 "parser.mly"
                                 ( [Statement (Expression (Call(_2, _4)))] )
# 509 "parser.ml"
               : 'chain))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'arguments) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'chain) in
    Obj.repr(
# 65 "parser.mly"
                                       ( (Statement (Expression (Call (_2, _4)))) :: _6 )
# 518 "parser.ml"
               : 'chain))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 69 "parser.mly"
                ( Bool _1 )
# 525 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 70 "parser.mly"
              ( Int _1 )
# 532 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 71 "parser.mly"
                 ( Lookup _1  )
# 539 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 547 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 73 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 555 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 74 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 563 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 75 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 571 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 76 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 579 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 77 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 587 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 78 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 595 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 79 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 603 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 80 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 611 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 81 "parser.mly"
                                                          ( Binary_op ("*", _1, _3) )
# 619 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 82 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 627 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 83 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 634 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 84 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 641 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 85 "parser.mly"
                                                          ( _2 )
# 648 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 89 "parser.mly"
                                                ( Assign ("", _1, _3) )
# 656 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 90 "parser.mly"
                                                ( Assign ("+", _1, _4) )
# 664 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 91 "parser.mly"
                                                ( Assign ("-", _1, _4) )
# 672 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 92 "parser.mly"
                                                ( Assign ("*", _1, _4) )
# 680 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 93 "parser.mly"
                                                ( Assign ("!", _1, _4) )
# 688 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 94 "parser.mly"
                                                ( Call (_1, _3) )
# 696 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                                                ( Stop )
# 702 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                                                ( Halt )
# 708 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                                                ( Break )
# 714 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                                                ( Continue )
# 720 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 99 "parser.mly"
                                                ( Print _2 )
# 727 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "parser.mly"
                 ( [] )
# 733 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 104 "parser.mly"
                 ( _1 )
# 740 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 108 "parser.mly"
                                              ( [_1] )
# 747 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 109 "parser.mly"
                                              ( _1 :: _3 )
# 755 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                               ( [] )
# 761 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 114 "parser.mly"
                               ( _1 :: _2)
# 769 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 118 "parser.mly"
                                                             ( Statement _1 )
# 776 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 119 "parser.mly"
                                                             ( Declaration _1 )
# 783 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 123 "parser.mly"
                                                             ( TypeDeclaration (false, _1, _2) )
# 791 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 124 "parser.mly"
                                                             ( TypeDeclaration (true, _2, _3) )
# 799 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 125 "parser.mly"
                                                             ( AssignDeclaration (false, _1, _2, _4) )
# 808 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 126 "parser.mly"
                                                             ( AssignDeclaration (true, _2, _3, _5) )
# 817 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 127 "parser.mly"
                                                             ( VarDeclaration (false, _2, _4) )
# 825 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 128 "parser.mly"
                                                             ( VarDeclaration (true, _3, _5) )
# 833 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 132 "parser.mly"
                                                       ( Expression _1 )
# 840 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 133 "parser.mly"
                                                       ( _1 )
# 847 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 134 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 856 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 135 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 864 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 136 "parser.mly"
                                                             ( While (_3, _5) )
# 872 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 137 "parser.mly"
                                                             ( While (Unary_op("!", _3), _5) )
# 880 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'unassignable_expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 138 "parser.mly"
                                                                                 ( For (_3, _4, _6, _8) )
# 890 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "parser.mly"
               ( [] )
# 896 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 143 "parser.mly"
               ( _1 )
# 903 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 147 "parser.mly"
                           ( [_1] )
# 910 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 148 "parser.mly"
                           ( _1 :: _3 )
# 918 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 152 "parser.mly"
                               ( (true, _2, _3) )
# 926 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 153 "parser.mly"
                               ( (false, _1, _2) )
# 934 "parser.ml"
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
