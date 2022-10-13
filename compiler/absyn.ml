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

(* Might not be necessary *)
and expression =
    | Assignable_expression of assignable_expression
    | Unassignable_expression of unassignable_expression

and unassignable_expression =
    | Assign of string * assignable_expression
    | Call of string * assignable_expression list
    | Stop
    | Halt
    | Print of assignable_expression

and assignable_expression =
    | Binary_op of string * assignable_expression * assignable_expression
    | Unary_op of string * assignable_expression
    | Bool of bool
    | Int of int
    | Lookup of string

and top_declaration =
    | Routine of access_mod * string * (bool * typ * string) list * statement
    | Global of bool * typ * string
    | GlobalAssign of bool * typ * string * assignable_expression

and access_mod =
    | Internal
    | External

and topdecs = 
    | Topdecs of top_declaration list