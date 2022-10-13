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
# 52 "parser.ml"
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
  290 (* UNTIL *);
  291 (* FOR *);
  292 (* BREAK *);
  293 (* CONTINUE *);
  294 (* LOCKED *);
  295 (* TRANSFER *);
  296 (* VAR *);
  297 (* PRINT *);
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
\009\000\009\000\009\000\009\000\010\000\010\000\011\000\011\000\
\008\000\008\000\012\000\012\000\014\000\014\000\014\000\014\000\
\014\000\014\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\006\000\006\000\015\000\015\000\016\000\016\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\003\000\004\000\005\000\006\000\006\000\
\006\000\001\000\001\000\003\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\003\000\004\000\001\000\
\001\000\001\000\001\000\002\000\000\000\001\000\001\000\003\000\
\000\000\002\000\001\000\001\000\003\000\004\000\005\000\006\000\
\005\000\006\000\002\000\001\000\007\000\005\000\005\000\005\000\
\008\000\000\000\001\000\001\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\011\000\000\000\000\000\000\000\064\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\059\000\000\000\000\000\000\000\005\000\014\000\013\000\
\015\000\000\000\000\000\000\000\000\000\000\000\063\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\062\000\000\000\008\000\061\000\009\000\007\000\
\029\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\032\000\033\000\000\000\
\000\000\000\000\000\000\034\000\035\000\000\000\000\000\000\000\
\000\000\052\000\000\000\000\000\000\000\043\000\044\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\012\000\051\000\042\000\000\000\000\000\000\000\
\038\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\000\000\000\031\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\000\000\000\000\040\000\000\000\055\000\
\056\000\000\000\000\000\000\000\049\000\047\000\000\000\000\000\
\050\000\048\000\053\000\000\000\057\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\024\000\111\000\025\000\090\000\091\000\
\092\000\112\000\113\000\093\000\094\000\095\000\026\000\027\000"

let yysindex = "\005\000\
\014\255\000\000\000\000\000\000\250\254\004\255\070\255\000\000\
\045\000\014\255\042\255\062\255\067\255\051\255\000\000\000\000\
\016\255\019\255\019\255\029\255\140\255\000\000\070\255\074\255\
\081\255\000\000\064\255\088\255\140\255\000\000\000\000\000\000\
\000\000\140\255\140\255\140\255\098\000\100\255\000\000\101\255\
\019\255\101\255\112\000\033\000\247\254\000\000\140\255\140\255\
\140\255\140\255\140\255\140\255\140\255\140\255\140\255\140\255\
\140\255\000\000\000\000\090\255\000\000\000\000\000\000\000\000\
\000\000\247\254\247\254\000\000\220\000\220\000\144\255\144\255\
\144\255\144\255\000\000\247\254\046\255\000\000\000\000\102\255\
\104\255\106\255\107\255\000\000\000\000\003\255\110\255\140\255\
\113\255\000\000\117\255\092\255\090\255\000\000\000\000\140\255\
\140\255\140\255\140\255\140\255\000\255\126\255\135\255\124\255\
\209\000\033\255\000\000\000\000\000\000\209\000\196\000\138\255\
\000\000\051\000\069\000\087\000\140\255\142\255\036\255\140\255\
\140\255\000\000\140\255\000\000\103\255\103\255\103\255\126\000\
\140\255\140\255\000\000\140\000\154\000\000\000\127\255\000\000\
\000\000\032\255\168\000\182\000\000\000\000\000\103\255\146\255\
\000\000\000\000\000\000\103\255\000\000"

let yyrindex = "\000\000\
\157\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\157\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\150\255\150\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\154\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\155\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\174\255\000\000\000\000\000\000\000\000\
\000\000\171\255\187\255\000\000\125\255\018\000\219\255\236\255\
\252\255\012\000\000\000\203\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\174\255\000\000\000\000\000\000\
\156\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\255\000\000\000\000\000\000\000\000\040\255\157\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\049\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\173\000\000\000\002\000\235\255\171\000\062\000\094\000\
\061\000\000\000\079\000\000\000\026\000\102\000\165\000\000\000"

let yytablesize = 503
let yytable = "\037\000\
\012\000\003\000\011\000\004\000\003\000\001\000\004\000\043\000\
\014\000\049\000\013\000\011\000\044\000\045\000\046\000\003\000\
\056\000\004\000\005\000\006\000\003\000\036\000\004\000\021\000\
\038\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\029\000\086\000\077\000\087\000\
\121\000\036\000\102\000\130\000\015\000\022\000\078\000\079\000\
\017\000\030\000\054\000\007\000\054\000\096\000\097\000\054\000\
\023\000\020\000\030\000\054\000\054\000\089\000\122\000\054\000\
\054\000\131\000\105\000\084\000\085\000\030\000\018\000\003\000\
\088\000\004\000\110\000\019\000\114\000\115\000\116\000\054\000\
\039\000\054\000\054\000\054\000\054\000\054\000\054\000\103\000\
\054\000\054\000\040\000\003\000\041\000\004\000\089\000\128\000\
\077\000\042\000\132\000\133\000\060\000\061\000\089\000\063\000\
\078\000\079\000\059\000\139\000\140\000\077\000\098\000\060\000\
\099\000\060\000\100\000\101\000\104\000\078\000\079\000\106\000\
\080\000\108\000\081\000\082\000\083\000\084\000\085\000\086\000\
\107\000\087\000\088\000\120\000\118\000\080\000\018\000\081\000\
\082\000\083\000\084\000\085\000\031\000\119\000\032\000\088\000\
\018\000\018\000\033\000\124\000\034\000\129\000\135\000\136\000\
\137\000\018\000\018\000\148\000\002\000\035\000\143\000\058\000\
\047\000\048\000\049\000\060\000\027\000\037\000\039\000\036\000\
\147\000\056\000\057\000\027\000\027\000\149\000\027\000\027\000\
\027\000\027\000\027\000\027\000\024\000\027\000\016\000\027\000\
\027\000\041\000\109\000\024\000\024\000\028\000\024\000\024\000\
\024\000\024\000\024\000\024\000\026\000\024\000\144\000\024\000\
\024\000\134\000\117\000\026\000\026\000\062\000\026\000\026\000\
\026\000\026\000\026\000\026\000\017\000\026\000\000\000\026\000\
\026\000\000\000\000\000\017\000\017\000\000\000\017\000\017\000\
\017\000\017\000\017\000\017\000\021\000\017\000\000\000\017\000\
\017\000\000\000\000\000\000\000\000\000\000\000\021\000\021\000\
\021\000\021\000\021\000\021\000\000\000\023\000\000\000\021\000\
\021\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\023\000\023\000\023\000\020\000\000\000\000\000\
\023\000\023\000\000\000\000\000\000\000\000\000\000\000\020\000\
\020\000\020\000\020\000\020\000\020\000\022\000\000\000\000\000\
\020\000\020\000\000\000\019\000\000\000\000\000\000\000\022\000\
\022\000\022\000\022\000\022\000\022\000\019\000\019\000\000\000\
\022\000\022\000\065\000\000\000\000\000\000\000\019\000\019\000\
\000\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\125\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\126\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\127\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\058\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\064\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\138\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\000\000\141\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\142\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\145\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\146\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\123\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\047\000\048\000\049\000\000\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000"

let yycheck = "\021\000\
\007\001\002\001\001\000\004\001\002\001\001\000\004\001\029\000\
\007\000\019\001\007\001\010\000\034\000\035\000\036\000\002\001\
\026\001\004\001\005\001\006\001\002\001\010\001\004\001\008\001\
\023\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\008\001\038\001\007\001\040\001\
\008\001\030\001\040\001\008\001\000\000\030\001\015\001\016\001\
\007\001\010\001\002\001\038\001\004\001\008\001\009\001\007\001\
\038\001\007\001\030\001\011\001\012\001\060\000\030\001\015\001\
\016\001\030\001\088\000\036\001\037\001\030\001\009\001\002\001\
\041\001\004\001\096\000\009\001\098\000\099\000\100\000\031\001\
\007\001\033\001\034\001\035\001\036\001\037\001\038\001\086\000\
\040\001\041\001\010\001\002\001\029\001\004\001\093\000\117\000\
\007\001\010\001\120\000\121\000\011\001\040\000\101\000\042\000\
\015\001\016\001\007\001\129\000\130\000\007\001\009\001\011\001\
\009\001\011\001\009\001\009\001\007\001\015\001\016\001\007\001\
\031\001\030\001\033\001\034\001\035\001\036\001\037\001\038\001\
\012\001\040\001\041\001\008\001\007\001\031\001\010\001\033\001\
\034\001\035\001\036\001\037\001\001\001\007\001\003\001\041\001\
\020\001\021\001\007\001\010\001\009\001\008\001\125\000\126\000\
\127\000\029\001\030\001\010\001\000\000\018\001\032\001\010\001\
\017\001\018\001\019\001\010\001\010\001\010\001\010\001\028\001\
\143\000\026\001\027\001\017\001\018\001\148\000\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\010\000\029\001\
\030\001\012\001\093\000\017\001\018\001\019\000\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\138\000\029\001\
\030\001\123\000\101\000\017\001\018\001\041\000\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\255\255\029\001\
\030\001\255\255\255\255\017\001\018\001\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\010\001\027\001\255\255\029\001\
\030\001\255\255\255\255\255\255\255\255\255\255\020\001\021\001\
\022\001\023\001\024\001\025\001\255\255\010\001\255\255\029\001\
\030\001\255\255\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\255\255\255\255\
\029\001\030\001\255\255\255\255\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\255\255\255\255\
\029\001\030\001\255\255\010\001\255\255\255\255\255\255\020\001\
\021\001\022\001\023\001\024\001\025\001\020\001\021\001\255\255\
\029\001\030\001\010\001\255\255\255\255\255\255\029\001\030\001\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\010\001\255\255\255\255\255\255\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\010\001\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\010\001\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\024\001\025\001\
\026\001\027\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\024\001\025\001\026\001\027\001\255\255\255\255\030\001\
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
# 378 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 39 "parser.mly"
                        ( [] )
# 384 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 40 "parser.mly"
                        ( _1 :: _2 )
# 392 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 44 "parser.mly"
                                                              ( Global (false, _1, _2) )
# 400 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 45 "parser.mly"
                                                              ( Global (true, _2, _3) )
# 408 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 46 "parser.mly"
                                                              ( GlobalAssign (false, _1, _2, _4)   )
# 417 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 47 "parser.mly"
                                                              ( GlobalAssign (true, _2, _3, _5)    )
# 426 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 48 "parser.mly"
                                                   ( Routine (Internal, _2, _4, _6) )
# 435 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 49 "parser.mly"
                                                   ( Routine (External, _2, _4, _6) )
# 444 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
              ( T_Int )
# 450 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
              ( T_Bool )
# 456 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 58 "parser.mly"
                                ( Block _2 )
# 463 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 62 "parser.mly"
                ( Bool _1 )
# 470 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 63 "parser.mly"
              ( Int _1 )
# 477 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "parser.mly"
                 ( Lookup _1  )
# 484 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 65 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 492 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 66 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 500 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 67 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 508 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 68 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 516 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 69 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 524 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 70 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 532 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 71 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 540 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 548 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 73 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 556 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 74 "parser.mly"
                                                          ( Binary_op ("*", _1, _3) )
# 564 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 75 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 572 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 76 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 579 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 77 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 586 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 78 "parser.mly"
                                                          ( _2 )
# 593 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 82 "parser.mly"
                                                ( Assign (_1, _3) )
# 601 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 83 "parser.mly"
                                                ( Call (_1, _3) )
# 609 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                                                ( Stop )
# 615 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                                                ( Halt )
# 621 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                                                ( Break )
# 627 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                                                ( Continue )
# 633 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 88 "parser.mly"
                                                ( Print _2 )
# 640 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "parser.mly"
                 ( [] )
# 646 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 93 "parser.mly"
                 ( _1 )
# 653 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 97 "parser.mly"
                                              ( [_1] )
# 660 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 98 "parser.mly"
                                              ( _1 :: _3 )
# 668 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    Obj.repr(
# 102 "parser.mly"
                               ( [] )
# 674 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 103 "parser.mly"
                               ( _1 :: _2)
# 682 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 107 "parser.mly"
                                                             ( Statement _1 )
# 689 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 108 "parser.mly"
                                                             ( Declaration _1 )
# 696 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 112 "parser.mly"
                                                             ( TypeDeclaration (false, _1, _2) )
# 704 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 113 "parser.mly"
                                                             ( TypeDeclaration (true, _2, _3) )
# 712 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 114 "parser.mly"
                                                             ( AssignDeclaration (false, _1, _2, _4) )
# 721 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 115 "parser.mly"
                                                             ( AssignDeclaration (true, _2, _3, _5) )
# 730 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 116 "parser.mly"
                                                             ( VarDeclaration (false, _2, _4) )
# 738 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 117 "parser.mly"
                                                             ( VarDeclaration (true, _3, _5) )
# 746 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 121 "parser.mly"
                                                       ( Expression _1 )
# 753 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 122 "parser.mly"
                                                       ( _1 )
# 760 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 123 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 769 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 124 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 777 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 125 "parser.mly"
                                                             ( While (_3, _5) )
# 785 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 126 "parser.mly"
                                                             ( While (Unary_op("!", _3), _5) )
# 793 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'unassignable_expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 127 "parser.mly"
                                                                                 ( For (_3, _4, _6, _8) )
# 803 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
               ( [] )
# 809 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 132 "parser.mly"
               ( _1 )
# 816 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 136 "parser.mly"
                           ( [_1] )
# 823 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 137 "parser.mly"
                           ( _1 :: _3 )
# 831 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 141 "parser.mly"
                               ( (true, _2, _3) )
# 839 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 142 "parser.mly"
                               ( (false, _1, _2) )
# 847 "parser.ml"
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
