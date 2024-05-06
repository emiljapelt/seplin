{
  open Parser
  open Exceptions
  let keyword_table = Hashtbl.create 53
  let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
                      [ "int", INT;
                        "bool", BOOL;
                        "char", CHAR;
                        "intern", INTERNAL;
                        "extern", EXTERNAL;
                        "entry", ENTRY;
                        "struct", STRUCT;
                        "reference", REFERENCE;
                        "as", AS;
                        "new", NEW;
                        "null", NULL;
                        "const", CONST;
                        "stable", STABLE;
                        "if", IF;
                        "else", ELSE;
                        "is",  IS;
                        "while", WHILE;
                        "until", UNTIL;
                        "for", FOR;
                        "repeat", REPEAT;
                        "stop", STOP;
                        "break", BREAK;
                        "continue", CONTINUE;
                        "halt", HALT;
                        "print", PRINT;
                        "read", READ]
  
  let char_of_string s lexbuf = match s with
  | "\'\\n\'" -> '\n'
  | "\'\\t\'" -> '\t'
  | "\'\\\\'" -> '\\'
  | _ when s.[1] = '\\' -> raise (Failure (Some((Lexing.lexeme_start_p lexbuf).pos_fname), Some((Lexing.lexeme_start_p lexbuf).pos_lnum), ("Unknown escape character: " ^ s)))
  | _ -> s.[1]

  let incr_linenum lexbuf = 
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

  let set_filename filename lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_fname = filename;
    }
}

rule lex = parse
        [' ' '\t']               { lex lexbuf }
    |   ('\r''\n' | '\n')        { incr_linenum lexbuf ; lex lexbuf }
    |   "//" [^ '\n' '\r']* ('\r''\n' | '\n' | eof)       { incr_linenum lexbuf ; lex lexbuf }
    |   ['0'-'9']+ as lxm { CSTINT (int_of_string lxm) }
    |   ''' ['\\']? _ ''' as lxm { CSTCHAR (char_of_string lxm lexbuf) }
    |   "true"            { CSTBOOL true }
    |   "false"           { CSTBOOL false }
    |   '"' [^ '"' '\n' '\r']* '"' as str { CSTSTRING str }
    |   ['A'-'Z'] as lxm { TYPE_VAR (lxm) }
    |   ['A'-'Z' 'a'-'z' '''] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id
                { try
                    Hashtbl.find keyword_table id
                  with Not_found -> NAME id }
    |   ('.' '.'?)? ('/' ('.' '.'? | ['A'-'Z' 'a'-'z' '0'-'9' '_']+))* '/' ['A'-'Z' 'a'-'z' '0'-'9' '_' '.']+ '.' ['a' - 'z']+ as path { 
        if Filename.extension path = ".sep" then PATH path else raise (Failure (Some((Lexing.lexeme_start_p lexbuf).pos_fname), Some((Lexing.lexeme_start_p lexbuf).pos_lnum), ("Referenced files must with .sep")))
    }
    |   '+'           { PLUS }
    |   '*'           { TIMES }
    |   '-'           { MINUS }
    |   '/'           { FSLASH }
    |   '%'           { PCT }
    |   '='           { EQ }
    |   "!="          { NEQ }
    |   "<="          { LTEQ }
    |   "<"           { LT }
    |   ">="          { GTEQ }
    |   ">"           { GT }
    |   "&&"          { LOGIC_AND }
    |   "||"          { LOGIC_OR }
    |   '$'           { VALUE }
    |   '|'           { PIPE }
    |   '!'           { NOT }
    |   '?'           { QMARK }
    |   ":="          { ASSIGNMENT }
    |   '('           { LPAR }
    |   ')'           { RPAR }
    |   '{'           { LBRACE }
    |   '}'           { RBRACE }
    |   '['           { LBRAKE }
    |   ']'           { RBRAKE }
    |   ','           { COMMA }
    |   '.'           { DOT }
    |   ';'           { SEMI }
    |   ':'           { COLON }
    |   '#'           { HASH }
    |   '_'           { UNDERSCORE }
    |   _             { raise (Failure(Some((Lexing.lexeme_start_p lexbuf).pos_fname), Some((Lexing.lexeme_start_p lexbuf).pos_lnum), ("Unknown token"))) }
    |   eof           { EOF }

and start filename = parse
       "" { set_filename filename lexbuf ; lex lexbuf }
