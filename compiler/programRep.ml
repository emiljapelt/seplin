type program =
  | Program of global_var list * program_part list

and global_var =
  | G_Int of int
  | G_Bool of bool

and program_part =
  | EntryPoint of string * typ list
  | Label of string
  | Instruction of int
  | IntInstruction of int * int
  | BoolInstruction of int * bool
  | LabelInstruction of int * string

and typ =
  | T_Int
  | T_Bool