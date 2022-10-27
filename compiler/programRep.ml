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


type concrete_program_part =
  | CEntryPoint of string * typ list
  | CLabel of string
  | CHalt 
  | CStop 
  | Call of string
  | GoTo of string
  | IfTrue of string
  | PlaceBool of bool
  | PlaceInt of int
  | CloneFull 
  | CloneHalf 
  | CloneShort 
  | CloneByte 
  | FetchBool 
  | FetchInt 
  | DeclareBool 
  | DeclareInt 
  | AssignBool 
  | AssignInt 
  | IntAdd
  | IntMul 
  | IntSub 
  | IntEq
  | IntLt 
  | BoolEq 
  | BoolNot 
  | BoolAnd 
  | BoolOr 
  | GetSP
  | GetBP 
  | ModSP of int
  | FreeVar
  | FreeVars of int
  | PrintVar 
  | PrintInt
  | PrintBool 
  | StackFetch of int
  | BPFetch of int

let translate concrete_list =
  let rec aux cl acc =
  match cl with
  | [] -> List.rev acc
  | h::t -> (
    match h with
    | CEntryPoint (s, tl) -> aux t (EntryPoint(s,tl)::acc)
    | CLabel (s) -> aux t (Label(s)::acc)
    | CHalt -> aux t (Instruction(0)::acc)
    | CStop -> aux t (Instruction(1)::acc)
    | Call (s) -> aux t (LabelInstruction(2, s)::acc)
    | GoTo (s) -> aux t (LabelInstruction(3, s)::acc)
    | IfTrue (s) -> aux t (LabelInstruction(4, s)::acc)
    | PlaceBool (b) -> aux t (BoolInstruction(5, b)::acc)
    | PlaceInt (i) -> aux t (IntInstruction(6, i)::acc)
    | CloneFull -> aux t (Instruction(7)::acc)
    | CloneHalf -> aux t (Instruction(8)::acc)
    | CloneShort -> aux t (Instruction(9)::acc)
    | CloneByte -> aux t (Instruction(10)::acc)
    | FetchBool -> aux t (Instruction(11)::acc)
    | FetchInt -> aux t (Instruction(12)::acc)
    | DeclareBool -> aux t (Instruction(13)::acc)
    | DeclareInt -> aux t (Instruction(14)::acc)
    | AssignBool -> aux t (Instruction(15)::acc)
    | AssignInt -> aux t (Instruction(16)::acc)
    | IntAdd -> aux t (Instruction(17)::acc)
    | IntMul -> aux t (Instruction(18)::acc)
    | IntSub -> aux t (Instruction(19)::acc)
    | IntEq -> aux t (Instruction(20)::acc)
    | IntLt -> aux t (Instruction(21)::acc)
    | BoolEq -> aux t (Instruction(22)::acc)
    | BoolNot -> aux t (Instruction(23)::acc)
    | BoolAnd -> aux t (Instruction(24)::acc)
    | BoolOr -> aux t (Instruction(25)::acc)
    | GetSP -> aux t (Instruction(26)::acc)
    | GetBP -> aux t (Instruction(27)::acc)
    | ModSP i -> aux t (IntInstruction(28, i)::acc)
    | FreeVar -> aux t (Instruction(29)::acc)
    | FreeVars i -> aux t (IntInstruction(30, i)::acc)
    | PrintVar -> aux t (Instruction(31)::acc)
    | PrintInt -> aux t (Instruction(32)::acc)
    | PrintBool -> aux t (Instruction(33)::acc)
    | StackFetch i -> aux t (IntInstruction(34, i)::acc)
    | BPFetch i -> aux t (IntInstruction(35, i)::acc)
  )
  in aux concrete_list []