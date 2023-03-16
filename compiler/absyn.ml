open ProgramRep

type statement =
    | If of expression * statement * statement
    | While of expression * statement
    | Block of statement_or_declaration list
    | Assign of reference * expression
    | Call of string * typ list * expression list
    | Stop
    | Halt
    | Break
    | Continue
    | Print of expression list

and declaration =
    | TypeDeclaration of bool * typ * string
    | AssignDeclaration of bool * typ option * string * expression

and statement_or_declaration =
    | Statement of statement * string * int (*   statement * file_name * line_number   *)
    | Declaration of declaration * string * int (*   declaration * file_name * line_number   *)
    
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
    | NewStruct of string * typ list * expression list
    | StructLiteral of expression list

and top_declaration =
    | Routine of access_mod * string * char list * (bool * typ * string) list * statement
    | GlobalDeclaration of declaration
    | Struct of string * char list * ((bool * typ * string) list)
    | Merge of string
    | FileReference of string * string (* nick_name * file_path *)

and access_mod =
    | Internal
    | External
    | Entry

and topdecs = 
    | Topdecs of top_declaration list