open ProgramRep

type statement =
    | If of assignable_expression * statement * statement
    | Block of statement_or_declaration list
    | Expression of unassignable_expression

and statement_or_declaration =
    | Statement of statement
    | Declaration of bool * typ * string * assignable_expression

(* Might not be necessary *)
and expression =
    | Assignable_expression of assignable_expression
    | Unassignable_expression of unassignable_expression

and unassignable_expression =
    | Assign of string * assignable_expression
    | Call of string * assignable_expression list
    | Stop
    | Print of assignable_expression

and assignable_expression =
    | Binary_op of string * assignable_expression * assignable_expression
    | Unary_op of string * assignable_expression
    | Bool of bool
    | Int of int
    | Lookup of string

(* and bool_expression =
    | Bool of bool
    | Binary_bool_op of string * assignable_expression * assignable_expression
    | Unary_bool_op of string * assignable_expression

and int_expression =
    | Int of int
    | Binary_int_op of string * assignable_expression * assignable_expression
    | Unary_int_op of string * assignable_expression *)

and top_declaration =
    | Routine of access_mod * string * (bool * typ * string) list * statement
    | GlobalVar of bool * typ * string * assignable_expression

and access_mod =
    | Internal
    | External

and topdecs = 
    | Topdecs of top_declaration list