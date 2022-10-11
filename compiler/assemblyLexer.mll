{
    open AssemblyParser
    let keyword_table = Hashtbl.create 53
    let () = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
                        [ 
                            "#",              SECTION_END;
                            "#GLOBAL",        GLOBAL_SECTION;
                            "#PROGRAM",       PROGRAM_SECTION;
                            "#ENTRY",         ENTRY_POINT;
                            "#LABEL",         LABEL;
                            "INT" ,           INT;
                            "BOOL" ,          BOOL;
                            "LOCKED",         LOCKED;
                            "HALT",           HALT;
                            "STOP",           STOP;
                            "CALL",           CALL;
                            "GOTO",           GOTO;
                            "IF_TRUE",        IF_TRUE;
                            "PLACE_BOOL",     PLACE_BOOL;
                            "PLACE_INT",      PLACE_INT;
                            "CLONE_FULL",     CLONE_FULL;
                            "CLONE_HALF",     CLONE_HALF;
                            "CLONE_SHORT",    CLONE_SHORT;
                            "CLONE_BYTE",     CLONE_BYTE;
                            "FETCH_BOOL",     FETCH_BOOL;
                            "FETCH_INT",      FETCH_INT;
                            "DECLARE_BOOL",   DECLARE_BOOL;
                            "DECLARE_INT",    DECLARE_INT;
                            "ASSIGN_BOOL",    ASSIGN_BOOL;
                            "ASSIGN_INT",     ASSIGN_INT;
                            "INT_ADD",        INT_ADD;
                            "INT_MUL",        INT_MUL;
                            "INT_SUB",        INT_SUB;
                            "INT_EQ",         INT_EQ;
                            "INT_LT",         INT_LT;
                            "BOOL_EQ",        BOOL_EQ;
                            "BOOL_NOT",       BOOL_NOT;
                            "BOOL_AND",       BOOL_AND;
                            "BOOL_OR",        BOOL_OR;
                            "GETSP",          GETSP;
                            "GETBP",          GETBP;
                            "MODSP",          MODSP;
                            "CLONE_FRAME",    CLONE_FRAME;
                            "FETCH_ADDR",     FETCH_ADDR;
                            "FREE_VAR",       FREE_VAR;
                            "FREE_VARS",      FREE_VARS;
                            "PRINT_VAR",      PRINT_VAR;
                            "PRINT_INT",      PRINT_INT;
                            "PRINT_BOOL",     PRINT_BOOL;
                            "STACK_FETCH",    STACK_FETCH;
                            "BP_FETCH",       BP_FETCH;
                        ]
}
rule lex = parse
        [' ' '\t' '\r' '\n']        { lex lexbuf }
    |   ['-']?['0'-'9']+ as lxm           { CST_INT (int_of_string lxm) }
    | "true"               { CST_BOOL true }
    | "false"               { CST_BOOL false }
    |   ['A'-'Z' 'a'-'z' '#'] ['A'-'Z' 'a'-'z' '0'-'9' '_' ] * as id
                { try
                    Hashtbl.find keyword_table id
                  with Not_found -> NAME id }
    | eof             { EOF }
