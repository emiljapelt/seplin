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

(* Add assignmet-types, reassignments, field-assignments *)
and unassignable_expression =
    | Assign of reference * string * assignable_expression
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
    | Lookup of string
    | StructLookup of string * string
    | ArrayLookup of srting * int

and value =
    | Binary_op of string * assignable_expression * assignable_expression
    | Unary_op of string * assignable_expression
    | ArraySize of string
    | Bool of bool
    | Int of int

and top_declaration =
    | Routine of access_mod * string * (bool * typ * string) list * statement
    | Global of bool * typ * string
    | GlobalAssign of bool * typ * string * assignable_expression
    | Struct of string * declaration list

and access_mod =
    | Internal
    | External

and topdecs = 
    | Topdecs of top_declaration list