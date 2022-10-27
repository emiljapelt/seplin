{
    open AssemblyParser
    open Exceptions
    let meta_table = Hashtbl.create 10
    let () = List.iter (fun (kwd, tok) -> Hashtbl.add meta_table kwd tok) 
                        [
                            "#",              SECTION_END;
                            "#GLOBAL",        GLOBAL_SECTION;
                            "#PROGRAM",       PROGRAM_SECTION;
                            "#ENTRY",         ENTRY_POINT;
                            "#LABEL",         LABEL;
                        ]
    let instruction_table = Hashtbl.create 53
    let () = List.iter (fun (kwd, tok) -> Hashtbl.add instruction_table kwd tok)
                        [ 
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
                            "FREE_VAR",       FREE_VAR;
                            "FREE_VARS",      FREE_VARS;
                            "PRINT_VAR",      PRINT_VAR;
                            "PRINT_INT",      PRINT_INT;
                            "PRINT_BOOL",     PRINT_BOOL;
                            "STACK_FETCH",    STACK_FETCH;
                            "BP_FETCH",       BP_FETCH;
                        ]

    let line_num = ref 1
}
rule lex = parse
        [' ' '\t' '\r']                         { lex lexbuf }
    |   '\n'                                    { incr line_num; lex lexbuf }
    |   ['-']?['0'-'9']+ as lxm                 { CST_INT (int_of_string lxm) }
    | "true"                                    { CST_BOOL true }
    | "false"                                   { CST_BOOL false }
    | "INT"                                     { INT }
    | "BOOL"                                    { BOOL }
    |   ['#'] ['A'-'Z'] * as id 
                { try
                    Hashtbl.find meta_table id
                  with Not_found -> syntax_error ("Unknown meta symbol \'" ^ id ^ "\'") line_num}
    |   ['A'-'Z'] ['A'-'Z' '_' ] * as id
                { try
                    Hashtbl.find instruction_table id
                  with Not_found -> syntax_error ("Unknown instruction \'" ^ id ^ "\'") line_num }
    |   ['A'-'Z' 'a'-'z' '#'] ['A'-'Z' 'a'-'z' '0'-'9' '_' ] * as name   { NAME name }
    | _                 { syntax_error "Unknown token" line_num}
    | eof               { EOF }
