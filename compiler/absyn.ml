open ProgramRep

type statement =
    | If of assignable_expression * statement * statement
    | While of assignable_expression * statement
    | For of declaration * assignable_expression * unassignable_expression * statement
    | Block of statement_or_declaration list
    | Expression of unassignable_expression

and declaration =
    | TypeDeclaration of bool * typ * string
    | AssignDeclaration of bool * typ * string * assignable_expression
    | VarDeclaration of bool * string * assignable_expression

and statement_or_declaration =
    | Statement of statement
    | Declaration of declaration

and unassignable_expression =
    | Assign of reference * assignable_expression
    | Call of string * assignable_expression list
    | Stop
    | Halt
    | Break
    | Continue
    | Print of assignable_expression

and assignable_expression =
    | Reference of reference
    | Value of value

and reference =
    | VarRef of string
    | StructRef of reference * string
    | ArrayRef of reference * assignable_expression
    | Null

and value =
    | Binary_op of string * assignable_expression * assignable_expression
    | Unary_op of string * assignable_expression
    | ArraySize of reference
    | Bool of bool
    | Int of int
    | Lookup of reference
    | NewArray of typ * assignable_expression
    | NewStruct of string * assignable_expression list

and top_declaration =
    | Routine of access_mod * string * (bool * typ * string) list * statement
    | GlobalDeclaration of declaration
    | Struct of string * ((bool * typ * string) list)

and access_mod =
    | Internal
    | External

and topdecs = 
    | Topdecs of top_declaration list