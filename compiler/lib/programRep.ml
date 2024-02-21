
type byte_container =
  | C_Bool of bool
  | C_Char of char

type full_container =
  | C_Int of int
  
type program =
  | Program of (string * char list * (var_mod * typ * string) list) list * (access_mod * var_mod * typ * int * string) list * concrete_program_part list

and program_part =
  | Label of string
  | PlaceLabel of string
  | Instruction of int
  | FullInstruction of int * full_container
  | ByteInstruction of int * byte_container
  | LabelInstruction of int * string

and typ =
  | T_Int
  | T_Bool
  | T_Char
  | T_Array of typ option
  | T_Struct of string * typ option list
  | T_Null
  | T_Generic of char
  | T_Routine of char list * (var_mod * typ) list

and op_typ =
  | NOp_T of typ
  | BinOp_T of string * op_typ * op_typ
  | UnOp_T of string * op_typ
  | TernaryOp_T of op_typ * op_typ * op_typ

and var_mod =
  | Open
  | Stable
  | Const

and access_mod =
  | Internal
  | External
  | Entry

and concrete_program_part =
  | CLabel of string
  | CHalt
  | CStop
  | CPlaceLabel of string
  | Call
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
  | Start
  | RefFetch
  | IncrRef
  | PrintChar
  | GetInput of int
  | HalfEq
  | ShortEq
  | ByteEq

let type_index ty = match ty with
  | T_Int -> 0
  | T_Bool -> 1
  | T_Char -> 2
  | T_Array _ -> 3
  | T_Struct _ -> 4
  | T_Generic _ -> 5
  | T_Routine _ -> 6
  | T_Null -> failwith "typing a null"

let type_input_index ty = match ty with
  | T_Int -> 0
  | T_Bool -> 1
  | T_Char -> 2
  | T_Array (Some T_Char) -> 3
  | _ -> failwith "Not an inputable type"

let translate_single c = match c with
    | CLabel (s) -> Label(s)
    | CHalt -> Instruction(0)
    | CStop -> Instruction(1)
    | CPlaceLabel (s) -> PlaceLabel(s)
    | Call -> Instruction(2)
    | GoTo (s) -> LabelInstruction(3, s)
    | IfTrue (s) -> LabelInstruction(4, s)
    | PlaceByte (b) -> ByteInstruction(5, b)
    | PlaceFull (i) -> FullInstruction(6, i)
    | CloneFull -> Instruction(7)
    | CloneHalf -> Instruction(8)
    | CloneShort -> Instruction(9)
    | CloneByte -> Instruction(10)
    | FetchFull -> Instruction(11)
    | FetchHalf -> Instruction(12)
    | FetchShort -> Instruction(13)
    | FetchByte -> Instruction(14)
    | FieldFetch -> Instruction(15)
    | DeclareFull -> Instruction(16)
    | DeclareHalf -> Instruction(17)
    | DeclareShort -> Instruction(18)
    | DeclareByte -> Instruction(19)
    | DeclareStruct -> Instruction(20)
    | AssignFull -> Instruction(21)
    | AssignHalf -> Instruction(22)
    | AssignShort -> Instruction(23)
    | AssignByte -> Instruction(24)
    | RefAssign -> Instruction(25)
    | FieldAssign -> Instruction(26)
    | IntAdd -> Instruction(27)
    | IntMul -> Instruction(28)
    | IntSub -> Instruction(29)
    | FullEq -> Instruction(30)
    | IntLt -> Instruction(31)
    | BoolEq -> Instruction(32)
    | BoolNot -> Instruction(33)
    | BoolAnd -> Instruction(34)
    | BoolOr -> Instruction(35)
    | GetSP -> Instruction(36)
    | GetBP -> Instruction(37)
    | ModSP (i) -> FullInstruction(38, C_Int i)
    | FreeVar -> Instruction(39)
    | FreeVars (i) -> FullInstruction(40, C_Int i)
    | PrintInt -> Instruction(41)
    | PrintBool -> Instruction(42)
    | StackFetch (i) -> FullInstruction(43, C_Int i)
    | BPFetch (i) -> FullInstruction(44, C_Int i)
    | SizeOf -> Instruction(45)
    | Start -> Instruction(46)
    | RefFetch -> Instruction(47)
    | IncrRef -> Instruction(48)
    | PrintChar -> Instruction(49)
    | GetInput (i) -> FullInstruction(50, C_Int i)
    | HalfEq -> Instruction(51)
    | ShortEq -> Instruction(52)
    | ByteEq -> Instruction(53)

let translate concrete_list =
  let rec aux cl acc =
  match cl with
  | [] -> List.rev acc
  | h::t -> aux t (translate_single h :: acc)
  in aux concrete_list []
