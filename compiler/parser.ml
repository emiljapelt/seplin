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
# 50 "parser.ml"
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
  292 (* LOCKED *);
  293 (* TRANSFER *);
  294 (* VAR *);
  295 (* PRINT *);
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
\012\000\012\000\014\000\014\000\014\000\014\000\014\000\014\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\006\000\
\006\000\015\000\015\000\016\000\016\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\003\000\004\000\005\000\006\000\006\000\
\006\000\001\000\001\000\003\000\001\000\001\000\001\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\002\000\002\000\003\000\003\000\004\000\001\000\
\001\000\002\000\000\000\001\000\001\000\003\000\000\000\002\000\
\001\000\001\000\003\000\004\000\005\000\006\000\005\000\006\000\
\002\000\001\000\007\000\005\000\005\000\005\000\008\000\000\000\
\001\000\001\000\003\000\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\010\000\011\000\000\000\000\000\000\000\062\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\003\000\
\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\
\000\000\057\000\000\000\000\000\000\000\005\000\014\000\013\000\
\015\000\000\000\000\000\000\000\000\000\000\000\061\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\060\000\000\000\008\000\059\000\009\000\007\000\
\029\000\000\000\000\000\025\000\000\000\000\000\000\000\000\000\
\000\000\000\000\016\000\000\000\000\000\032\000\033\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\050\000\
\000\000\000\000\000\000\041\000\042\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\012\000\049\000\040\000\000\000\000\000\000\000\036\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\000\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\044\000\000\000\000\000\038\000\000\000\053\000\054\000\000\000\
\000\000\000\000\047\000\045\000\000\000\000\000\048\000\046\000\
\051\000\000\000\055\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\024\000\109\000\025\000\088\000\089\000\
\090\000\110\000\111\000\091\000\092\000\093\000\026\000\027\000"

let yysindex = "\009\000\
\014\255\000\000\000\000\000\000\004\255\015\255\052\255\000\000\
\001\000\014\255\045\255\061\255\065\255\072\255\000\000\000\000\
\250\254\019\255\019\255\009\255\050\255\000\000\052\255\078\255\
\079\255\000\000\059\255\089\255\050\255\000\000\000\000\000\000\
\000\000\050\255\050\255\050\255\090\000\095\255\000\000\092\255\
\019\255\092\255\104\000\025\000\064\255\000\000\050\255\050\255\
\050\255\050\255\050\255\050\255\050\255\050\255\050\255\050\255\
\050\255\000\000\000\000\118\255\000\000\000\000\000\000\000\000\
\000\000\064\255\064\255\000\000\212\000\212\000\223\000\223\000\
\223\000\223\000\000\000\064\255\040\255\000\000\000\000\096\255\
\101\255\103\255\108\255\003\255\114\255\050\255\116\255\000\000\
\119\255\100\255\118\255\000\000\000\000\050\255\050\255\050\255\
\050\255\050\255\002\255\128\255\129\255\132\255\201\000\034\255\
\000\000\000\000\000\000\201\000\188\000\131\255\000\000\043\000\
\061\000\079\000\050\255\134\255\036\255\050\255\050\255\000\000\
\050\255\000\000\093\255\093\255\093\255\118\000\050\255\050\255\
\000\000\132\000\146\000\000\000\111\255\000\000\000\000\030\255\
\160\000\174\000\000\000\000\000\093\255\135\255\000\000\000\000\
\000\000\093\255\000\000"

let yyrindex = "\000\000\
\144\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\144\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\136\255\136\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\137\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\148\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\138\255\000\000\000\000\000\000\000\000\
\000\000\164\255\180\255\000\000\051\255\010\000\212\255\228\255\
\244\255\004\000\000\000\196\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\138\255\000\000\000\000\000\000\149\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\033\255\000\000\
\000\000\000\000\000\000\037\255\151\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\080\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\138\000\000\000\002\000\235\255\143\000\018\000\072\000\
\028\000\000\000\046\000\000\000\014\000\077\000\139\000\000\000"

let yytablesize = 506
let yytable = "\037\000\
\015\000\021\000\011\000\003\000\003\000\004\000\004\000\043\000\
\014\000\001\000\012\000\011\000\044\000\045\000\046\000\003\000\
\029\000\004\000\005\000\006\000\003\000\013\000\004\000\022\000\
\038\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\075\000\076\000\077\000\084\000\030\000\085\000\
\100\000\119\000\034\000\128\000\078\000\079\000\030\000\094\000\
\095\000\007\000\031\000\017\000\032\000\003\000\023\000\004\000\
\033\000\061\000\034\000\063\000\018\000\087\000\034\000\120\000\
\103\000\129\000\030\000\035\000\086\000\018\000\018\000\018\000\
\108\000\019\000\112\000\113\000\114\000\036\000\020\000\018\000\
\018\000\052\000\049\000\052\000\039\000\101\000\052\000\041\000\
\040\000\056\000\052\000\052\000\087\000\126\000\052\000\052\000\
\130\000\131\000\042\000\077\000\087\000\059\000\060\000\060\000\
\096\000\137\000\138\000\078\000\079\000\097\000\052\000\098\000\
\052\000\052\000\052\000\052\000\099\000\052\000\052\000\003\000\
\102\000\004\000\104\000\080\000\077\000\081\000\082\000\083\000\
\060\000\106\000\105\000\086\000\078\000\079\000\116\000\117\000\
\133\000\134\000\135\000\118\000\122\000\127\000\141\000\002\000\
\146\000\056\000\058\000\016\000\080\000\039\000\081\000\082\000\
\083\000\084\000\145\000\085\000\086\000\027\000\035\000\147\000\
\037\000\028\000\107\000\142\000\027\000\027\000\132\000\027\000\
\027\000\027\000\027\000\027\000\027\000\024\000\027\000\115\000\
\027\000\027\000\000\000\062\000\024\000\024\000\000\000\024\000\
\024\000\024\000\024\000\024\000\024\000\026\000\024\000\000\000\
\024\000\024\000\000\000\000\000\026\000\026\000\000\000\026\000\
\026\000\026\000\026\000\026\000\026\000\017\000\026\000\000\000\
\026\000\026\000\000\000\000\000\017\000\017\000\000\000\017\000\
\017\000\017\000\017\000\017\000\017\000\021\000\017\000\000\000\
\017\000\017\000\000\000\000\000\000\000\000\000\000\000\021\000\
\021\000\021\000\021\000\021\000\021\000\023\000\000\000\000\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\023\000\
\023\000\023\000\023\000\023\000\023\000\020\000\000\000\000\000\
\023\000\023\000\000\000\000\000\000\000\000\000\000\000\020\000\
\020\000\020\000\020\000\020\000\020\000\022\000\000\000\000\000\
\020\000\020\000\000\000\019\000\000\000\000\000\000\000\022\000\
\022\000\022\000\022\000\022\000\022\000\019\000\019\000\000\000\
\022\000\022\000\065\000\000\000\000\000\000\000\019\000\019\000\
\000\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\123\000\000\000\000\000\000\000\
\000\000\000\000\000\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\124\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\054\000\055\000\056\000\057\000\
\125\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
\048\000\049\000\050\000\051\000\052\000\053\000\054\000\055\000\
\056\000\057\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\058\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\064\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\136\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\000\000\139\000\047\000\048\000\049\000\050\000\051\000\052\000\
\053\000\054\000\055\000\056\000\057\000\000\000\000\000\140\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000\054\000\
\055\000\056\000\057\000\000\000\000\000\143\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\000\000\000\000\144\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\000\000\
\121\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\047\000\048\000\049\000\000\000\
\000\000\052\000\053\000\054\000\055\000\056\000\057\000\047\000\
\048\000\049\000\000\000\000\000\000\000\000\000\000\000\000\000\
\056\000\057\000"

let yycheck = "\021\000\
\000\000\008\001\001\000\002\001\002\001\004\001\004\001\029\000\
\007\000\001\000\007\001\010\000\034\000\035\000\036\000\002\001\
\008\001\004\001\005\001\006\001\002\001\007\001\004\001\030\001\
\023\000\047\000\048\000\049\000\050\000\051\000\052\000\053\000\
\054\000\055\000\056\000\057\000\007\001\036\001\030\001\038\001\
\038\001\008\001\010\001\008\001\015\001\016\001\010\001\008\001\
\009\001\036\001\001\001\007\001\003\001\002\001\036\001\004\001\
\007\001\040\000\009\001\042\000\010\001\060\000\030\001\030\001\
\086\000\030\001\030\001\018\001\039\001\009\001\020\001\021\001\
\094\000\009\001\096\000\097\000\098\000\028\001\007\001\029\001\
\030\001\002\001\019\001\004\001\007\001\084\000\007\001\029\001\
\010\001\026\001\011\001\012\001\091\000\115\000\015\001\016\001\
\118\000\119\000\010\001\007\001\099\000\007\001\011\001\011\001\
\009\001\127\000\128\000\015\001\016\001\009\001\031\001\009\001\
\033\001\034\001\035\001\036\001\009\001\038\001\039\001\002\001\
\007\001\004\001\007\001\031\001\007\001\033\001\034\001\035\001\
\011\001\030\001\012\001\039\001\015\001\016\001\007\001\007\001\
\123\000\124\000\125\000\008\001\010\001\008\001\032\001\000\000\
\010\001\010\001\010\001\010\000\031\001\012\001\033\001\034\001\
\035\001\036\001\141\000\038\001\039\001\010\001\010\001\146\000\
\010\001\019\000\091\000\136\000\017\001\018\001\121\000\020\001\
\021\001\022\001\023\001\024\001\025\001\010\001\027\001\099\000\
\029\001\030\001\255\255\041\000\017\001\018\001\255\255\020\001\
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
\255\255\022\001\023\001\024\001\025\001\026\001\027\001\017\001\
\018\001\019\001\255\255\255\255\255\255\255\255\255\255\255\255\
\026\001\027\001"

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
# 34 "parser.mly"
                  ( Topdecs _1 )
# 374 "parser.ml"
               : Absyn.topdecs))
; (fun __caml_parser_env ->
    Obj.repr(
# 38 "parser.mly"
                        ( [] )
# 380 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'topdec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'topdecs) in
    Obj.repr(
# 39 "parser.mly"
                        ( _1 :: _2 )
# 388 "parser.ml"
               : 'topdecs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 43 "parser.mly"
                                                              ( Global (false, _1, _2) )
# 396 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 44 "parser.mly"
                                                              ( Global (true, _2, _3) )
# 404 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 45 "parser.mly"
                                                              ( GlobalAssign (false, _1, _2, _4)   )
# 413 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 46 "parser.mly"
                                                              ( GlobalAssign (true, _2, _3, _5)    )
# 422 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 47 "parser.mly"
                                                   ( Routine (Internal, _2, _4, _6) )
# 431 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 48 "parser.mly"
                                                   ( Routine (External, _2, _4, _6) )
# 440 "parser.ml"
               : 'topdec))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
              ( T_Int )
# 446 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
              ( T_Bool )
# 452 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDecSeq) in
    Obj.repr(
# 57 "parser.mly"
                                ( Block _2 )
# 459 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 61 "parser.mly"
                ( Bool _1 )
# 466 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parser.mly"
              ( Int _1 )
# 473 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parser.mly"
                 ( Lookup _1  )
# 480 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 64 "parser.mly"
                                                          ( Binary_op ("&", _1, _3) )
# 488 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 65 "parser.mly"
                                                          ( Binary_op ("|", _1, _3) )
# 496 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 66 "parser.mly"
                                                          ( Binary_op ("=", _1, _3) )
# 504 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 67 "parser.mly"
                                                          ( Binary_op ("!=", _1, _3) )
# 512 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 68 "parser.mly"
                                                          ( Binary_op ("<=", _1, _3) )
# 520 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 69 "parser.mly"
                                                          ( Binary_op ("<", _1, _3) )
# 528 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 70 "parser.mly"
                                                          ( Binary_op (">=", _1, _3) )
# 536 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 71 "parser.mly"
                                                          ( Binary_op (">", _1, _3) )
# 544 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 72 "parser.mly"
                                                          ( Binary_op ("+", _1, _3) )
# 552 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 73 "parser.mly"
                                                          ( Binary_op ("*", _1, _3) )
# 560 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 74 "parser.mly"
                                                          ( Binary_op ("-", _1, _3) )
# 568 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 75 "parser.mly"
                                                          ( Binary_op ("-", Int 0, _2) )
# 575 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 76 "parser.mly"
                                                          ( Unary_op ("!", _2) )
# 582 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 77 "parser.mly"
                                                          ( _2 )
# 589 "parser.ml"
               : 'assignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 81 "parser.mly"
                                                ( Assign (_1, _3) )
# 597 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'arguments) in
    Obj.repr(
# 82 "parser.mly"
                                                ( Call (_1, _3) )
# 605 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                                                ( Stop )
# 611 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                                                ( Halt )
# 617 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 85 "parser.mly"
                                                ( Print _2 )
# 624 "parser.ml"
               : 'unassignable_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                 ( [] )
# 630 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 90 "parser.mly"
                 ( _1 )
# 637 "parser.ml"
               : 'arguments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'assignable_expression) in
    Obj.repr(
# 94 "parser.mly"
                                              ( [_1] )
# 644 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arguments1) in
    Obj.repr(
# 95 "parser.mly"
                                              ( _1 :: _3 )
# 652 "parser.ml"
               : 'arguments1))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                               ( [] )
# 658 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmtOrDec) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmtOrDecSeq) in
    Obj.repr(
# 100 "parser.mly"
                               ( _1 :: _2)
# 666 "parser.ml"
               : 'stmtOrDecSeq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 104 "parser.mly"
                                                             ( Statement _1 )
# 673 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 105 "parser.mly"
                                                             ( Declaration _1 )
# 680 "parser.ml"
               : 'stmtOrDec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 109 "parser.mly"
                                                             ( TypeDeclaration (false, _1, _2) )
# 688 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 110 "parser.mly"
                                                             ( TypeDeclaration (true, _2, _3) )
# 696 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 111 "parser.mly"
                                                             ( AssignDeclaration (false, _1, _2, _4) )
# 705 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 112 "parser.mly"
                                                             ( AssignDeclaration (true, _2, _3, _5) )
# 714 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 113 "parser.mly"
                                                             ( VarDeclaration (false, _2, _4) )
# 722 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'assignable_expression) in
    Obj.repr(
# 114 "parser.mly"
                                                             ( VarDeclaration (true, _3, _5) )
# 730 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'unassignable_expression) in
    Obj.repr(
# 118 "parser.mly"
                                                       ( Expression _1 )
# 737 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 119 "parser.mly"
                                                       ( _1 )
# 744 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 120 "parser.mly"
                                                             ( If (_3, _5, _7) )
# 753 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 121 "parser.mly"
                                                             ( If (_3, _5, Block []) )
# 761 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 122 "parser.mly"
                                                             ( While (_3, _5) )
# 769 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'assignable_expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 123 "parser.mly"
                                                             ( While (Unary_op("!", _3), _5) )
# 777 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'assignable_expression) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'unassignable_expression) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 124 "parser.mly"
                                                                                 ( For (_3, _4, _6, _8) )
# 787 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "parser.mly"
               ( [] )
# 793 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 129 "parser.mly"
               ( _1 )
# 800 "parser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 133 "parser.mly"
                           ( [_1] )
# 807 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'params1) in
    Obj.repr(
# 134 "parser.mly"
                           ( _1 :: _3 )
# 815 "parser.ml"
               : 'params1))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 138 "parser.mly"
                               ( (true, _2, _3) )
# 823 "parser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 139 "parser.mly"
                               ( (false, _1, _2) )
# 831 "parser.ml"
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
