%{
    open ProgramRep
%}
%token SECTION_END
%token GLOBAL_SECTION
%token PROGRAM_SECTION
%token ENTRY_POINT
%token LABEL
%token <string> NAME
%token INT
%token BOOL
%token <int> CST_INT
%token <bool> CST_BOOL
%token HALT
%token STOP
%token CALL
%token GOTO
%token IF_TRUE
%token PLACE_BOOL
%token PLACE_INT
%token CLONE_FULL
%token CLONE_HALF
%token CLONE_SHORT
%token CLONE_BYTE
%token FETCH_FULL
%token FETCH_HALF
%token FETCH_SHORT
%token FETCH_BYTE
%token FIELD_FETCH
%token DECLARE_FULL
%token DECLARE_HALF
%token DECLARE_SHORT
%token DECLARE_BYTE
%token DECLARE_STRUCT
%token ASSIGN_FULL
%token ASSIGN_HALF
%token ASSIGN_SHORT
%token ASSIGN_BYTE
%token REF_ASSIGN
%token FIELD_ASSIGN
%token INT_ADD
%token INT_MUL
%token INT_SUB
%token INT_EQ
%token INT_LT
%token BOOL_EQ
%token BOOL_NOT
%token BOOL_AND
%token BOOL_OR
%token GETSP
%token GETBP
%token MODSP
%token FREE_VAR
%token FREE_VARS
%token PRINT_VAR
%token PRINT_INT
%token PRINT_BOOL
%token STACK_FETCH
%token BP_FETCH
%token SIZE_OF
%token TO_START
%token REF_FETCH
%token INCR_REF
%token EOF

%start main
%type <ProgramRep.program> main
%%

main:
    PROGRAM_SECTION program_section SECTION_END EOF  { Program ([], [], $2) }
;

program_section:
    program { $1 }
;

program:
        {[]}
    | ENTRY_POINT NAME type_list program { (EntryPoint ($2, $2, $3)) :: $4 }
    | LABEL NAME program { (Label $2) :: $3}
    | HALT program { Instruction(0) :: $2 }
    | STOP program { Instruction(1) :: $2 }
    | CALL NAME program { LabelInstruction(2, $2) :: $3 }
    | GOTO NAME program { LabelInstruction(3, $2) :: $3 }
    | IF_TRUE NAME program { LabelInstruction(4, $2) :: $3 }
    | PLACE_BOOL CST_BOOL program { BoolInstruction(5, $2) :: $3 }
    | PLACE_INT CST_INT program { IntInstruction(6, $2) :: $3 }
    | CLONE_FULL program { Instruction(7) :: $2 }
    | CLONE_HALF program { Instruction(8) :: $2 }
    | CLONE_SHORT program { Instruction(9) :: $2 }
    | CLONE_BYTE program { Instruction(10) :: $2 }
    | FETCH_FULL program { Instruction(11) :: $2 }
    | FETCH_HALF program { Instruction(12) :: $2 }
    | FETCH_SHORT program { Instruction(13) :: $2 }
    | FETCH_BYTE program { Instruction(14) :: $2 }
    | FIELD_FETCH program { Instruction(15) :: $2 }
    | DECLARE_FULL program { Instruction(16) :: $2 }
    | DECLARE_HALF program { Instruction(17) :: $2 }
    | DECLARE_SHORT program { Instruction(18) :: $2 }
    | DECLARE_BYTE program { Instruction(19) :: $2 }
    | DECLARE_STRUCT program { Instruction(20) :: $2 }
    | ASSIGN_FULL program { Instruction(21) :: $2 }
    | ASSIGN_HALF program { Instruction(22) :: $2 }
    | ASSIGN_SHORT program { Instruction(23) :: $2 }
    | ASSIGN_BYTE program { Instruction(24) :: $2 }
    | REF_ASSIGN program { Instruction(25) :: $2 }
    | FIELD_ASSIGN program { Instruction(26) :: $2 }
    | INT_ADD program { Instruction(27) :: $2 }
    | INT_MUL program { Instruction(28) :: $2 }
    | INT_SUB program { Instruction(29) :: $2 }
    | INT_EQ program { Instruction(30) :: $2 }
    | INT_LT program { Instruction(31) :: $2 }
    | BOOL_EQ program { Instruction(32) :: $2 }
    | BOOL_NOT program { Instruction(33) :: $2 }
    | BOOL_AND program { Instruction(34) :: $2 }
    | BOOL_OR program { Instruction(35) :: $2 }
    | GETSP program { Instruction(36) :: $2 }
    | GETBP program { Instruction(37) :: $2 }
    | MODSP CST_INT program { IntInstruction(38, $2) :: $3 }
    | FREE_VAR program { Instruction(39) :: $2 }
    | FREE_VARS CST_INT program { IntInstruction(40, $2) :: $3 }
    | PRINT_VAR program { Instruction(41) :: $2 }
    | PRINT_INT program { Instruction(42) :: $2 }
    | PRINT_BOOL program { Instruction(43) :: $2 }
    | STACK_FETCH CST_INT program { IntInstruction(44, $2) :: $3 }
    | BP_FETCH CST_INT program { IntInstruction(45, $2) :: $3 }
    | SIZE_OF program { Instruction(46) :: $2 }
    | TO_START program { Instruction(47) :: $2 }
    | REF_FETCH program { Instruction(48) :: $2 }
    | INCR_REF program { Instruction(49) :: $2 }
;

type_list:
        {[]}
    | INT type_list { (Open, T_Int) :: $2 }
    | BOOL type_list { (Open, T_Bool) :: $2 }
;