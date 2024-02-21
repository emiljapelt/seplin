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
    | LABEL NAME program { (CLabel $2) :: $3}
    | HALT program { CHalt :: $2 }
    | STOP program { CStop :: $2 }
    | CALL NAME program { CPlaceLabel $2 :: Call :: $3 }
    | GOTO NAME program { GoTo $2 :: $3 }
    | IF_TRUE NAME program { IfTrue $2 :: $3 }
    | PLACE_BOOL CST_BOOL program { PlaceByte(C_Bool $2) :: $3 }
    | PLACE_INT CST_INT program { PlaceFull(C_Int $2) :: $3 }
    | CLONE_FULL program { CloneFull :: $2 }
    | CLONE_HALF program { CloneHalf :: $2 }
    | CLONE_SHORT program { CloneShort :: $2 }
    | CLONE_BYTE program { CloneByte :: $2 }
    | FETCH_FULL program { FetchFull :: $2 }
    | FETCH_HALF program { FetchHalf :: $2 }
    | FETCH_SHORT program { FetchShort :: $2 }
    | FETCH_BYTE program { FetchByte :: $2 }
    | FIELD_FETCH program { FieldFetch :: $2 }
    | DECLARE_FULL program { DeclareFull :: $2 }
    | DECLARE_HALF program { DeclareHalf :: $2 }
    | DECLARE_SHORT program { DeclareShort :: $2 }
    | DECLARE_BYTE program { DeclareByte :: $2 }
    | DECLARE_STRUCT program { DeclareStruct :: $2 }
    | ASSIGN_FULL program { AssignFull :: $2 }
    | ASSIGN_HALF program { AssignHalf :: $2 }
    | ASSIGN_SHORT program { AssignShort :: $2 }
    | ASSIGN_BYTE program { AssignByte :: $2 }
    | REF_ASSIGN program { RefAssign :: $2 }
    | FIELD_ASSIGN program { FieldAssign :: $2 }
    | INT_ADD program { IntAdd :: $2 }
    | INT_MUL program { IntMul :: $2 }
    | INT_SUB program { IntSub :: $2 }
    | INT_EQ program { FullEq :: $2 }
    | INT_LT program { IntLt :: $2 }
    | BOOL_EQ program { BoolEq :: $2 }
    | BOOL_NOT program { BoolNot :: $2 }
    | BOOL_AND program { BoolAnd :: $2 }
    | BOOL_OR program { BoolOr :: $2 }
    | GETSP program { GetSP :: $2 }
    | GETBP program { GetBP :: $2 }
    | MODSP CST_INT program { ModSP $2 :: $3 }
    | FREE_VAR program { FreeVar :: $2 }
    | FREE_VARS CST_INT program { FreeVars $2 :: $3 }
    | PRINT_VAR program { PrintInt :: $2 }
    | PRINT_INT program { PrintInt :: $2 }
    | PRINT_BOOL program { PrintBool :: $2 }
    | STACK_FETCH CST_INT program { StackFetch $2 :: $3 }
    | BP_FETCH CST_INT program { BPFetch $2 :: $3 }
    | SIZE_OF program { SizeOf :: $2 }
    | TO_START program { Start :: $2 }
    | REF_FETCH program { RefFetch :: $2 }
    | INCR_REF program { IncrRef :: $2 }
;

type_list:
        {[]}
    | INT type_list { (Open, T_Int) :: $2 }
    | BOOL type_list { (Open, T_Bool) :: $2 }
;