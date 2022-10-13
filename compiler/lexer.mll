{
    open Parser
    open Exceptions
    let keyword_table = Hashtbl.create 53
    let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
                        [ "int", INT;
                          "bool", BOOL;
                          "internal", INTERNAL;
                          "external", EXTERNAL;
                          "locked", LOCKED;
                          "var", VAR;
                          "if", IF;
                          "else", ELSE;
                          "while", WHILE;
                          "until", UNTIL;
                          "for", FOR;
                          "stop", STOP;
                          "break", BREAK;
                          "continue", CONTINUE;
                          "halt", HALT;
                          "print", PRINT]


  let line_num = ref 1
}
rule lex = parse
        [' ' '\t' '\r']        { lex lexbuf }
    |   '\n'        { incr line_num; lex lexbuf }
    |   ['0'-'9']+ as lxm { CSTINT (int_of_string lxm) }
    |   "true"            { CSTBOOL true}
    |   "false"           { CSTBOOL false}
    |   ['A'-'Z' 'a'-'z' '''] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                { try
                    Hashtbl.find keyword_table id
                  with Not_found -> NAME id }
    |   '+'           { PLUS }
    |   '*'           { TIMES }
    |   '-'           { MINUS }
    |   '='           { EQ }
    |   "!="          { NEQ }
    |   "<="          { LTEQ }
    |   "<"           { LT }
    |   ">="          { GTEQ }
    |   ">"           { GT }
    |   '&'           { AND }
    |   '|'           { OR }
    |   '!'           { NOT }
    |   ":="          { ASSIGNMENT }
    |   '('           { LPAR }
    |   ')'           { RPAR }
    |   '{'           { LBRACE }
    |   '}'           { RBRACE }
    |   '['           { LBRAKE }
    |   ']'           { RBRAKE }
    |   ','           { COMMA }
    |   ';'           { SEMI }
    |   _             { syntax_error "Unknown token" line_num }
    |   eof           { EOF }
