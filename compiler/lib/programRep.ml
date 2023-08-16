
type byte_container =
  | C_Bool of bool
  | C_Char of char

type full_container =
  | C_Int of int

type program =
  | Program of (string * char list * (var_mod * typ * string) list) list * (var_mod * typ * string) list * program_part list

and program_part =
  | EntryPoint of string * string * (var_mod * typ) list
  | Label of string
  | Instruction of int
  | FullInstruction of int * full_container
  | ByteInstruction of int * byte_container
  | LabelInstruction of int * string

and typ =
  | T_Int
  | T_Bool
  | T_Char
  | T_Array of typ
  | T_Struct of string * typ option list
  | T_Null
  | T_Generic of char

and var_mod =
  | Open
  | Stable
  | Const

let type_index ty =
  match ty with
  | T_Int -> 0
  | T_Bool -> 1
  | T_Char -> 2
  | T_Array _ -> 3
  | T_Struct _ -> 4
  | T_Generic _ -> 5
  | T_Null -> failwith "typing a null"

type concrete_program_part =
  | CEntryPoint of string * string * (var_mod * typ) list
  | CLabel of string
  | CHalt
  | CStop
  | Call of string
  | GoTo of string
  | IfTrue of string
  | PlaceByte of byte_container
  | PlaceFull of full_container
  | CloneFull
  | CloneHalf
  | CloneShort
  | CloneByte
  | FetchFull
  | FetchHalf
  | FetchShort
  | FetchByte
  | FieldFetch
  | DeclareFull
  | DeclareHalf
  | DeclareShort
  | DeclareByte
  | DeclareStruct
  | AssignFull
  | AssignHalf
  | AssignShort
  | AssignByte
  | RefAssign
  | FieldAssign
  | IntAdd
  | IntMul
  | IntSub
  | FullEq
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
  | PrintInt
  | PrintBool
  | StackFetch of int
  | BPFetch of int
  | SizeOf
  | ToStart
  | RefFetch
  | IncrRef
  | PrintChar
  | GetInput of int
  | HalfEq
  | ShortEq
  | ByteEq



let translate concrete_list =
  let rec aux cl acc =
  match cl with
  | [] -> List.rev acc
  | h::t -> (
    match h with
    | CEntryPoint (name, label, tl) -> aux t (EntryPoint(name,label,tl)::acc)
    | CLabel (s) -> aux t (Label(s)::acc)
    | CHalt -> aux t (Instruction(0)::acc)
    | CStop -> aux t (Instruction(1)::acc)
    | Call (s) -> aux t (LabelInstruction(2, s)::acc)
    | GoTo (s) -> aux t (LabelInstruction(3, s)::acc)
    | IfTrue (s) -> aux t (LabelInstruction(4, s)::acc)
    | PlaceByte (b) -> aux t (ByteInstruction(5, b)::acc)
    | PlaceFull (i) -> aux t (FullInstruction(6, i)::acc)
    | CloneFull -> aux t (Instruction(7)::acc)
    | CloneHalf -> aux t (Instruction(8)::acc)
    | CloneShort -> aux t (Instruction(9)::acc)
    | CloneByte -> aux t (Instruction(10)::acc)
    | FetchFull -> aux t (Instruction(11)::acc)
    | FetchHalf -> aux t (Instruction(12)::acc)
    | FetchShort -> aux t (Instruction(13)::acc)
    | FetchByte -> aux t (Instruction(14)::acc)
    | FieldFetch -> aux t (Instruction(15)::acc)
    | DeclareFull -> aux t (Instruction(16)::acc)
    | DeclareHalf -> aux t (Instruction(17)::acc)
    | DeclareShort -> aux t (Instruction(18)::acc)
    | DeclareByte -> aux t (Instruction(19)::acc)
    | DeclareStruct -> aux t (Instruction(20)::acc)
    | AssignFull -> aux t (Instruction(21)::acc)
    | AssignHalf -> aux t (Instruction(22)::acc)
    | AssignShort -> aux t (Instruction(23)::acc)
    | AssignByte -> aux t (Instruction(24)::acc)
    | RefAssign -> aux t (Instruction(25)::acc)
    | FieldAssign -> aux t (Instruction(26)::acc)
    | IntAdd -> aux t (Instruction(27)::acc)
    | IntMul -> aux t (Instruction(28)::acc)
    | IntSub -> aux t (Instruction(29)::acc)
    | FullEq -> aux t (Instruction(30)::acc)
    | IntLt -> aux t (Instruction(31)::acc)
    | BoolEq -> aux t (Instruction(32)::acc)
    | BoolNot -> aux t (Instruction(33)::acc)
    | BoolAnd -> aux t (Instruction(34)::acc)
    | BoolOr -> aux t (Instruction(35)::acc)
    | GetSP -> aux t (Instruction(36)::acc)
    | GetBP -> aux t (Instruction(37)::acc)
    | ModSP (i) -> aux t (FullInstruction(38, C_Int i)::acc)
    | FreeVar -> aux t (Instruction(39)::acc)
    | FreeVars (i) -> aux t (FullInstruction(40, C_Int i)::acc)
    | PrintInt -> aux t (Instruction(41)::acc)
    | PrintBool -> aux t (Instruction(42)::acc)
    | StackFetch (i) -> aux t (FullInstruction(43, C_Int i)::acc)
    | BPFetch (i) -> aux t (FullInstruction(44, C_Int i)::acc)
    | SizeOf -> aux t (Instruction(45)::acc)
    | ToStart -> aux t (Instruction(46)::acc)
    | RefFetch -> aux t (Instruction(47)::acc)
    | IncrRef -> aux t (Instruction(48)::acc)
    | PrintChar -> aux t (Instruction(49)::acc)
    | GetInput (i) -> aux t (FullInstruction(50, C_Int i)::acc)
    | HalfEq -> aux t (Instruction(51)::acc)
    | ShortEq -> aux t (Instruction(52)::acc)
    | ByteEq -> aux t (Instruction(53)::acc)
  )
  in aux concrete_list []