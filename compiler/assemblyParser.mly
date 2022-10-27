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
%token FETCH_BOOL
%token FETCH_INT
%token DECLARE_BOOL
%token DECLARE_INT
%token ASSIGN_BOOL
%token ASSIGN_INT
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
%token CLONE_FRAME
%token FREE_VAR
%token FREE_VARS
%token PRINT_VAR
%token PRINT_INT
%token PRINT_BOOL
%token STACK_FETCH
%token BP_FETCH
%token EOF

%start main
%type <ProgramRep.program> main
%%

main:
      GLOBAL_SECTION global_section SECTION_END PROGRAM_SECTION program_section SECTION_END EOF  { Program ($2, $5) }
    | PROGRAM_SECTION program_section SECTION_END GLOBAL_SECTION global_section SECTION_END EOF { Program ($5, $2) }
    | PROGRAM_SECTION program_section SECTION_END EOF  { Program ([], $2) }
;

global_section:
    global_variables { $1 }
;

global_variables:
        {[]}
    | INT CST_INT global_variables { (G_Int $2) :: $3 }
    | BOOL CST_BOOL global_variables { (G_Bool $2) :: $3 }
;

program_section:
    program { $1 }
;

program:
        {[]}
    | ENTRY_POINT NAME type_list program { (EntryPoint ($2, $3)) :: $4 }
    | LABEL NAME program { (Label $2) :: $3}
    | HALT program { (Instruction(0)) :: $2 }
    | STOP program { (Instruction(1)) :: $2 }
    | CALL NAME program { (LabelInstruction(2, $2)) :: $3 }
    | GOTO NAME program { (LabelInstruction(3, $2)) :: $3 }
    | IF_TRUE NAME program { (LabelInstruction(4, $2)) :: $3 }
    | PLACE_BOOL CST_BOOL program { BoolInstruction(5, $2) :: $3 }
    | PLACE_INT CST_INT program { IntInstruction(6, $2) :: $3 }
    | CLONE_FULL program { Instruction(7) :: $2 }
    | CLONE_HALF program { Instruction(8) :: $2 }
    | CLONE_SHORT program { Instruction(9) :: $2 }
    | CLONE_BYTE program { Instruction(10) :: $2 }
    | FETCH_BOOL program { Instruction(11) :: $2 }
    | FETCH_INT program { Instruction(12) :: $2 }
    | DECLARE_BOOL program { Instruction(13) :: $2 }
    | DECLARE_INT program { Instruction(14) :: $2 }
    | ASSIGN_BOOL program { Instruction(15) :: $2 }
    | ASSIGN_INT program { Instruction(16) :: $2 }
    | INT_ADD program { Instruction(17) :: $2 }
    | INT_MUL program {Instruction(18) :: $2 }
    | INT_SUB program { Instruction(19) :: $2 }
    | INT_EQ program { Instruction(20) :: $2 }
    | INT_LT program { Instruction(21) :: $2 }
    | BOOL_EQ program { Instruction(22) :: $2 }
    | BOOL_NOT program { Instruction(23) :: $2 }
    | BOOL_AND program { Instruction(24) :: $2 }
    | BOOL_OR program { Instruction(25) :: $2 }
    | GETSP program { Instruction(26) :: $2 }
    | GETBP program { Instruction(27) :: $2 }
    | MODSP CST_INT program { IntInstruction(28, $2) :: $3 }
    | FREE_VAR program { Instruction(29) :: $2 }
    | FREE_VARS CST_INT program { IntInstruction(30, $2) :: $3 }
    | PRINT_VAR program { Instruction(31) :: $2 }
    | PRINT_INT program { Instruction(32) :: $2 }
    | PRINT_BOOL program { Instruction(33) :: $2 }
    | STACK_FETCH CST_INT program { IntInstruction(34, $2) :: $3 }
    | BP_FETCH CST_INT program { IntInstruction(35, $2) :: $3 }
;

type_list:
        {[]}
    | INT type_list { T_Int :: $2}
    | BOOL type_list { T_Bool :: $2}
;