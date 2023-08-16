open ProgramRep

type statement =
    | If of expression * statement * statement
    | While of expression * statement
    | Block of statement_or_declaration list
    | Assign of reference * expression
    | Call of string option * string * typ option list * expression list (* context_alias * routine_name * type_arguments * arguments *)
    | Stop
    | Halt
    | Break
    | Continue
    | Print of expression list

and declaration =
    | TypeDeclaration of var_mod * typ * string
    | AssignDeclaration of var_mod * typ option * string * expression

and statement_or_declaration =
    | Statement of statement * int (*   statement * line_number   *)
    | Declaration of declaration * int (*   declaration * line_number   *)
    
and expression =
    | Reference of reference
    | Value of value

and reference =
    | VariableAccess of string
    | StructAccess of reference * string
    | ArrayAccess of reference * expression
    | Null

and value =
    | Binary_op of string * expression * expression
    | Unary_op of string * expression
    | ArraySize of reference
    | GetInput of typ
    | Bool of bool
    | Int of int
    | Char of char
    | ValueOf of reference
    | NewArray of typ * expression
    | ArrayLiteral of expression list
    | NewStruct of string * typ option list * expression list
    | StructLiteral of expression list

and top_declaration =
    | Routine of access_mod * string * char list * (var_mod * typ * string) list * statement
    | GlobalDeclaration of declaration
    | Struct of string * char list * ((var_mod * typ * string) list)
    | FileReference of string * string (* alias * file_path *)

and access_mod =
    | Internal
    | External
    | Entry

and file = 
    | File of top_declaration list