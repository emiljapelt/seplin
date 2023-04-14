open Absyn
open ProgramRep
open Exceptions
open Typing
open Helpers


(*** Helper functions ***)
let addFreeVars amount acc =
  match (amount, acc) with
  | (0, _) -> acc
  | (1, FreeVar :: accr) -> FreeVars(2) :: accr
  | (1, FreeVars(y) :: accr) -> FreeVars(y+1) :: accr
  | (1, _) -> FreeVar :: acc
  | (x, FreeVar :: accr) -> FreeVars(x+1) :: accr 
  | (x, FreeVars(y) :: accr) -> FreeVars(x+y) :: accr 
  | (x, _) -> FreeVars(x) :: acc

let addStop acc =
  match acc with
  | CStop::_ -> acc
  | CHalt::acc1 -> CStop :: acc1
  | _ -> CStop :: acc

let addHalt acc =
  match acc with
  | CHalt::_ -> acc
  | CStop::acc1 -> CHalt :: acc1
  | _ -> CHalt :: acc

(* Scanning *)
let count_decl stmt_dec_list =
  let rec aux sdl c =
    match sdl with
    | [] -> c
    | (Declaration(dec,_))::t -> (
      match dec with
      | TypeDeclaration _ -> aux t (c+1)
      | AssignDeclaration _ -> aux t (c+1)
    )
    | _::t -> aux t (c)
  in
  aux stmt_dec_list 0

(*    list of: string * access_mod * char list * (bool * typ * string) list * statement    *)
let get_routines file =
  let rec aux topdecs acc =
    match topdecs with
    | [] -> acc
    | h::t -> (
      match h with
      | Routine (accmod, name, typ_vars, params, stmt) -> (
        if routine_exists name acc then raise_error ("Duplicate routine name: " ^ name)
        else aux t ((accmod,name,"",typ_vars,params,stmt)::acc)
        )
      | _ -> aux t acc
    )
  in match file with
  | File (tds) -> aux tds []

(*    list of: string * char list * (bool * typ * string) list   *)
let get_structs file =
  let rec aux topdecs acc =
    match topdecs with
    | [] -> acc
    | h::t -> (
      match h with
      | Struct (name, typ_vars, params) -> (
        if struct_exists name acc then raise_error ("Duplicate struct name: " ^ name)
        else aux t ((name, typ_vars, params)::acc)
        )
      | _ -> aux t acc
    )
  in match file with
  | File (tds) -> aux tds []


(*** Global variable handling ***)
(*    Compute the list of variable dependencies for each global variable    *)
let get_globvar_dependencies gvs =
  let rec dependencies_from_assignable expr acc =
    match expr with
    | Reference r -> ( match r with
      | VariableAccess (name) -> name::acc
      | ArrayAccess (refer,_) -> dependencies_from_assignable (Reference refer) acc
      | StructAccess (refer,_) -> dependencies_from_assignable (Reference refer) acc
      | Null -> acc
    )
    | Value v -> ( match v with
      | Binary_op (_, expr1, expr2) -> dependencies_from_assignable expr1 (dependencies_from_assignable expr2 acc)
      | Unary_op (_, expr1) -> dependencies_from_assignable expr1 acc
      | ArraySize (refer) -> dependencies_from_assignable (Reference refer) acc
      | Bool _ -> acc
      | Int _ -> acc
      | Char _ -> acc
      | GetInput _ -> acc
      | ValueOf (refer) -> dependencies_from_assignable (Reference refer) acc
      | NewArray (_,expr1) -> dependencies_from_assignable expr1 acc
      | ArrayLiteral exprs -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
      | NewStruct (_,_,exprs) -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
      | StructLiteral (exprs) -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
    )
  in
  let dependencies_from_declaration dec =
    match dec with
    | TypeDeclaration _ -> []
    | AssignDeclaration (_,_,_,expr) -> dependencies_from_assignable expr []
  in
  List.map (fun (name,context_name,lock,ty,dec) -> ((name,context_name,lock,ty,dec), dependencies_from_declaration dec)) gvs

let extract_name t =
  match t with
  | (f,_,_,_,_,_) -> f

(*    Compute an ordering of the global variables, according to their dependencies    *)
let order_dep_globvars dep_gvs =
  let rec aux dep_globvars count prev_count remain acc =
    match dep_globvars with
    | [] when remain = [] -> acc
    | [] when count = prev_count -> raise_error "Could not resolve a global variable order, there might be a circular dependency"
    | [] -> aux remain count count [] acc
    | h::t -> ( match h with
      | ((name,context,lock,ty,dec), deps) -> (
        if List.for_all (fun dep -> List.exists (fun a -> dep = extract_name a) acc) deps then aux t (count+1) prev_count remain ((name,context,count,lock,ty,dec)::acc)
        else aux t count prev_count (h::remain) acc
      )
    )
  in
  List.rev (aux dep_gvs 0 0 [] [])

let gather_globvar_info gvs =
  List.map (fun (name,_,_,lock,ty,_) -> (lock, ty, name)) gvs


(*** Optimizing functions ***)
let rec optimize_assignable_expr expr var_env =
  match expr with
  | Reference _ -> expr
  | Value val_expr -> optimize_value val_expr var_env

and optimize_value expr var_env =
  match expr with
  | Binary_op (op, e1, e2) -> ( 
    let opte1 = optimize_assignable_expr e1 var_env in
    let opte2 = optimize_assignable_expr e2 var_env in
    match (op, opte1, opte2) with
    | ("&&", Value(Bool b1), Value(Bool b2)) -> Value(Bool(b1&&b2))
    | ("&&", Value(Bool true), _) -> opte2
    | ("&&", _, Value(Bool true)) -> opte1
    | ("&&", Value(Bool false), _) -> Value(Bool false)
    | ("&&", _, Value(Bool false)) -> Value(Bool false)
    | ("||", Value(Bool b1), Value(Bool b2)) -> Value(Bool(b1||b2))
    | ("||", Value(Bool true), _) -> Value(Bool true)
    | ("||", _, Value(Bool true)) -> Value(Bool true)
    | ("||", Value(Bool false), _) -> opte2
    | ("||", _, Value(Bool false)) -> opte1
    | ("+", Value(Int i1), Value(Int i2)) -> Value(Int (i1+i2))
    | ("+", Value(Int 0), _) -> opte2
    | ("+", _, Value(Int 0)) -> opte1
    | ("-", Value(Int 0), Value(Int i)) -> Value(Int (-i))
    | ("-", Value(Int i1), Value(Int i2)) -> Value(Int (i1-i2))
    | ("-", _, Value(Int 0)) -> opte1
    | ("*", Value(Int i1), Value(Int i2)) -> Value(Int (i1*i2))
    | ("*", Value(Int 0), _) -> Value(Int 0)
    | ("*", _, Value(Int 0)) -> Value(Int 0)
    | ("*", Value(Int 1), _) -> opte2
    | ("*", _, Value(Int 1)) -> opte1
    | ("=", Value(Int i1), Value(Int i2)) -> Value(Bool (i1=i2))
    | ("=", Value(Char c1), Value(Char c2)) -> Value(Bool (c1=c2))
    | ("=", Value(Bool b1), Value(Bool b2)) -> Value(Bool (b1=b2))
    | ("!=", Value(Int i1), Value(Int i2)) -> Value(Bool (i1!=i2))
    | ("!=", Value(Char c1), Value(Char c2)) -> Value(Bool (c1!=c2))
    | ("!=", Value(Bool b1), Value(Bool b2)) -> Value(Bool (b1!=b2))
    | ("<", Value(Int i1), Value(Int i2)) -> Value(Bool (i1<i2))
    | ("<=", Value(Int i1), Value(Int i2)) -> Value(Bool (i1<=i2))
    | (">", Value(Int i1), Value(Int i2)) -> Value(Bool (i1>i2))
    | (">=", Value(Int i1), Value(Int i2)) -> Value(Bool (i1>=i2))
    | _ -> Value(Binary_op(op, opte1, opte2))
  )
  | Unary_op (op, e) -> ( 
    let opte = optimize_assignable_expr e var_env in
    match (op, opte) with
    | ("!", Value(Bool b)) -> Value(Bool (not b))
    | _ -> opte
  )
  | ArraySize _ -> Value(expr)
  | GetInput _ -> Value(expr)
  | Bool _ -> Value(expr)
  | Int _ -> Value(expr)
  | Char _ -> Value(expr)
  | ValueOf _ -> Value(expr)
  | NewArray _ -> Value(expr)
  | ArrayLiteral _ -> Value(expr)
  | NewStruct (_,_,_) -> Value(expr)
  | StructLiteral(exprs) -> Value(StructLiteral( List.map (fun e -> optimize_assignable_expr e var_env) exprs ))


(*** Compiling functions ***)
let routine_head accmod name context base_context params =
  match accmod with
  | Internal -> CLabel(context^"#"^name)
  | External -> CLabel(context^"#"^name)
  | Entry -> (
    if (context = base_context) then CEntryPoint(name, (context^"#"^name), List.map (fun (lock,ty,_) -> (lock,ty)) params)
    else CLabel(context^"#"^name)
  )

let fetch_var_index (name: string) globvars localvars = 
  match lookup_localvar name localvars with
  | Some (lc,_,_) -> BPFetch(lc)
  | None -> 
    match lookup_globvar name globvars with
    | Some (gc,_,_) -> StackFetch(gc)
    | None -> raise_error ("No such variable '" ^ name ^ "'")

let rec compile_assignable_expr expr var_env acc =
  match expr with
  | Reference ref_expr -> compile_reference ref_expr var_env acc
  | Value val_expr -> compile_value val_expr var_env acc

and compile_reference ref_expr var_env acc =
  match ref_expr with
  | VariableAccess name -> (fetch_var_index name var_env.globals var_env.locals) :: RefFetch :: acc
  | StructAccess (refer, field) -> ( 
    let (_, ref_ty) = Typing.type_reference refer var_env in
    match ref_ty with
    | T_Struct (name, _) -> (
      match lookup_struct name var_env.structs with
      | Some (_, params) -> (
        let (_, _, index) = struct_field field params in
        compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: acc)
      )
      | None -> raise_error ("No such struct '" ^ name ^ "'")
    )
    | _ -> raise_error ("Struct field lookup on non-struct value")
  )
  | ArrayAccess (refer, index) -> (
    let (_, ref_ty) = Typing.type_reference refer var_env in
    let (_, idx_ty) = Typing.type_assignable_expr index var_env in
    match (ref_ty, idx_ty) with
    | (T_Array (_), T_Int) -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: acc)))
    | _ -> raise_error ("Array indexed with non-integer value")
  )
  | Null -> PlaceInt(0) :: acc

and compile_assignable_expr_as_value expr var_env acc =
  match expr with
  | Reference r -> (
    let (_, ref_ty) = Typing.type_reference r var_env in
    match ref_ty with
    | T_Int -> compile_reference r var_env (FetchFull :: FetchFull :: acc)
    | T_Bool -> compile_reference r var_env (FetchFull :: FetchByte :: acc)
    | T_Char -> compile_reference r var_env (FetchFull :: FetchByte :: acc)
    | T_Array _ -> compile_reference r var_env (FetchFull :: acc)
    | T_Struct _ -> compile_reference r var_env (FetchFull :: acc)
    | T_Generic _ -> compile_reference r var_env (FetchFull :: acc)
    | T_Null -> compile_reference r var_env acc
  )
  | _ -> compile_assignable_expr expr var_env acc

and compile_value val_expr var_env acc =
  match val_expr with
  | Bool b -> PlaceBool(b) :: acc
  | Int i -> PlaceInt(i) :: acc
  | Char c -> PlaceChar(c) :: acc
  | ArraySize refer -> (
    let (_, ref_ty) = Typing.type_reference refer var_env in
    match ref_ty with
    | T_Array _ -> compile_reference refer var_env  (FetchFull :: SizeOf :: acc)
    | _ -> raise_error "Array size called on non-array value"
  )
  | GetInput ty -> ( match type_index ty with
    | -1 -> raise_error "Unsupported GetInput variant"
    | x -> GetInput(x) :: acc
  )
  | ValueOf refer -> (
    let (_, ref_ty) = Typing.type_reference refer var_env in
    match ref_ty with
    | T_Int -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Bool -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Char -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Array _ -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Struct _ -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Generic _ -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Null -> raise_error ("Direct null pointer dereferencing")
  )
  | NewArray (_, size_expr) -> (
    let (_, s_ty) = Typing.type_assignable_expr size_expr var_env in
    match s_ty with
    | T_Int -> compile_assignable_expr_as_value (optimize_assignable_expr size_expr var_env) var_env (DeclareStruct :: IncrRef :: acc)
    | _ -> raise_error ("Initializing array with non-integer size")
  )
  | ArrayLiteral elements -> (
    let (_,ty) = Typing.type_array_literal (List.map (fun e -> Typing.type_assignable_expr e var_env) elements) in
    let rec aux es c acc =
      match es with
      | [] -> acc
      | h::t -> ( match h with
        | Value _ -> ( match ty with
          | T_Int -> aux t (c+1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr h var_env (AssignFull :: FieldAssign :: acc))
          | T_Char -> aux t (c+1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr h var_env (AssignByte :: FieldAssign :: acc))
          | T_Bool -> aux t (c+1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr h var_env (AssignByte :: FieldAssign :: acc))
          | _ -> aux t (c+1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr h var_env (IncrRef :: FieldAssign :: acc))
        )
        | Reference r -> ( match r with
          | Null -> aux t (c+1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr h var_env (FieldAssign :: acc))
          | _ -> aux t (c+1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr h var_env (FetchFull :: IncrRef :: FieldAssign :: acc))
        )
      )
    in
    PlaceInt(List.length elements) :: DeclareStruct :: (aux elements 0 acc)
  )
  | NewStruct (name, typ_args, args) -> (
    let rec aux ags fields c acc =
      match (ags, fields) with
      | ([], []) -> acc
      | (ha::ta, (field_lock,field_ty,_)::tf) -> (
        let (ha_lock, ha_ty) = Typing.type_assignable_expr ha var_env in
        if not (Typing.type_equal field_ty ha_ty) then raise_error ("Struct argument type mismatch")
        else if ha_lock && (not field_lock) then raise_error "Locked variable given as a parameter that is not locked"
        else let optha = optimize_assignable_expr ha var_env in
        match optha with
        | Value _ -> (
          match ha_ty with
          | T_Int -> aux ta tf (c-1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr optha var_env (AssignFull :: FieldAssign :: acc))
          | T_Char -> aux ta tf (c-1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr optha var_env (AssignByte :: FieldAssign :: acc))
          | T_Bool -> aux ta tf (c-1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr optha var_env (AssignByte :: FieldAssign :: acc))
          | _ -> aux ta tf (c-1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr optha var_env (IncrRef :: FieldAssign :: acc))
        )
        | Reference r -> (
          match r with
          | Null -> aux ta tf (c-1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr optha var_env (FieldAssign :: acc))
          | _ -> aux ta tf (c-1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr optha var_env (FetchFull :: IncrRef :: FieldAssign :: acc))
        )
      )
      | (_,_) -> raise_error ("Expected " ^(string_of_int (List.length fields))^ " arguments, but was given " ^(string_of_int (List.length args))) 
    in
    match lookup_struct name var_env.structs with
    | Some (typ_vars, params) -> (
      if (List.length typ_vars > 0) then ( (* Generic *)
        if (typ_args = []) then (
          let typ_args = infere_generics typ_vars params args var_env in
          PlaceInt(List.length params) :: DeclareStruct :: (aux (List.rev args) (List.rev (replace_generics params typ_vars typ_args)) ((List.length params)-1) acc)
        )
        else if (List.length typ_args) = (List.length typ_vars) then (
          PlaceInt(List.length params) :: DeclareStruct :: (aux (List.rev args) (List.rev (replace_generics params typ_vars typ_args)) ((List.length params)-1) acc)
        )
        else raise_error ("Expected " ^(string_of_int (List.length typ_vars))^ " type arguments, but was given " ^(string_of_int (List.length typ_args))) 
      )
      else ( (* Not generic *)
        PlaceInt(List.length params) :: DeclareStruct :: (aux (List.rev args) (List.rev params) ((List.length params)-1) acc)
      ) 
    )
    | None -> raise_error ("No such struct '" ^ name ^ "'")
  )
  | StructLiteral (exprs) -> (
    let rec aux es c acc =
      match es with
      | [] -> acc
      | h::t -> (
        let opt_h = optimize_assignable_expr h var_env in
        match opt_h with
        | Value(StructLiteral _) -> aux t (c-1) (CloneFull :: PlaceInt(c) :: (compile_assignable_expr opt_h var_env (FieldAssign :: acc)))
        | Value _ -> (
          let (_, h_ty) = type_assignable_expr opt_h var_env in
          match h_ty with
          | T_Int -> aux t (c-1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr opt_h var_env (AssignFull :: FieldAssign :: acc))
          | T_Char -> aux t (c-1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr opt_h var_env (AssignByte :: FieldAssign :: acc))
          | T_Bool -> aux t (c-1) (CloneFull :: PlaceInt(c) :: DeclareFull :: IncrRef :: CloneFull :: compile_assignable_expr opt_h var_env (AssignByte :: FieldAssign :: acc))
          | _ -> aux t (c-1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr opt_h var_env (IncrRef :: FieldAssign :: acc))
        )
        | Reference r -> (
          match r with
          | Null -> aux t (c-1) (CloneFull :: PlaceInt(c) :: PlaceInt(0) :: FieldAssign :: acc)
          | _ -> aux t (c-1) (CloneFull :: PlaceInt(c) :: compile_assignable_expr opt_h var_env (FetchFull :: IncrRef :: FieldAssign :: acc))
        )
      )
    in
    PlaceInt(List.length exprs) :: DeclareStruct :: (aux (List.rev exprs) ((List.length exprs)-1) acc)
  )
  | Binary_op (op, e1, e2) -> (
      let (_, t1) = Typing.type_assignable_expr e1 var_env in
      let (_, t2) = Typing.type_assignable_expr e2 var_env in
      match (op, t1, t2, e1, e2) with
      | ("&&", T_Bool, T_Bool, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (BoolAnd :: acc))
      | ("||", T_Bool, T_Bool, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (BoolOr :: acc))
      | ("=", _, _, Reference(r), Reference(Null)) -> compile_reference r var_env (FetchFull :: PlaceInt(0) :: FullEq :: acc)
      | ("=", _, _, Reference(Null), Reference(r)) -> compile_reference r var_env (FetchFull :: PlaceInt(0) :: FullEq :: acc)
      | ("=", T_Bool, T_Bool, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (BoolEq :: acc))
      | ("=", T_Char, T_Char, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (ByteEq :: acc))
      | ("=", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (FullEq :: acc))
      | ("!=", _, _, Reference(r), Reference(Null)) -> compile_reference r var_env (FetchFull :: PlaceInt(0) :: FullEq :: BoolNot :: acc)
      | ("!=", _, _, Reference(Null), Reference(r)) -> compile_reference r var_env (FetchFull :: PlaceInt(0) :: FullEq :: BoolNot :: acc)
      | ("!=", T_Bool, T_Bool, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (BoolEq :: BoolNot :: acc))
      | ("!=", T_Char, T_Char, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (ByteEq :: BoolNot :: acc))
      | ("!=", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (FullEq :: BoolNot :: acc))
      | ("<=", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (IntLt :: BoolNot :: acc))
      | ("<", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e2 var_env (compile_assignable_expr_as_value e1 var_env (IntLt :: acc))
      | (">=", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e2 var_env (compile_assignable_expr_as_value e1 var_env (IntLt :: BoolNot :: acc))
      | (">", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (IntLt :: acc))
      | ("+", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (IntAdd :: acc))
      | ("-", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e2 var_env (compile_assignable_expr_as_value e1 var_env (IntSub :: acc))
      | ("*", T_Int, T_Int, _, _) -> compile_assignable_expr_as_value e1 var_env (compile_assignable_expr_as_value e2 var_env (IntMul :: acc))
      | _ -> raise_error "Unknown binary operator, or type mismatch"
    )
  | Unary_op (op, e) -> (
    let (_, t) = Typing.type_assignable_expr e var_env in
    match (op, t, e) with
    | ("!", T_Bool, _) -> compile_assignable_expr_as_value e var_env (BoolNot :: acc)
    | _ -> raise_error "Unknown unary operator, or type mismatch"
  )

let compile_arguments args var_env acc =
  let rec aux ars acc =
    match ars with
    | ([]) -> acc
    | (((plock, pty, _),eh)::t) -> (
      let opteh = optimize_assignable_expr eh var_env in
      let typ = Typing.argument_type_check plock (Some(pty)) opteh var_env in
      match opteh with
      | Value _ -> ( match typ with
        | T_Int -> aux t (DeclareFull :: IncrRef :: CloneFull :: (compile_assignable_expr_as_value opteh var_env (AssignFull :: acc)))
        | T_Bool -> aux t (DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr_as_value opteh var_env (AssignByte :: acc)))
        | T_Char -> aux t (DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr_as_value opteh var_env (AssignByte :: acc)))
        | T_Array _ -> aux t (compile_assignable_expr_as_value opteh var_env (IncrRef :: acc))
        | T_Struct _ -> aux t (compile_assignable_expr_as_value opteh var_env (IncrRef :: acc))
        | T_Null -> aux t (compile_assignable_expr_as_value opteh var_env (acc))
        | T_Generic _ -> aux t (compile_assignable_expr_as_value opteh var_env (IncrRef :: acc))(*raise_error "Generic variables not yet supported, in call arguments"*)
      )
      | Reference r -> ( match r with
        | VariableAccess _ -> aux t (compile_reference r var_env (IncrRef :: acc)) 
        | StructAccess _ -> aux t (compile_reference r var_env (FetchFull :: IncrRef :: acc)) 
        | ArrayAccess _ -> aux t (compile_reference r var_env (FetchFull :: IncrRef :: acc)) 
        | Null -> aux t (compile_reference r var_env acc) 
      )
    )
  in
  aux (List.rev args) acc

let compile_assignment target assign var_env acc =
  let assign_type = Typing.assignment_type_check target assign var_env in
  match (target, assign) with
  | (Null, _) -> raise_error "Assignment to null"
  | (VariableAccess _, Value v) -> ( match assign_type with 
    | T_Int ->  compile_reference target var_env (FetchFull :: (compile_value v var_env (AssignFull :: acc)))
    | T_Bool -> compile_reference target var_env (FetchFull :: (compile_value v var_env (AssignByte :: acc)))
    | T_Char -> compile_reference target var_env (FetchFull :: (compile_value v var_env (AssignByte :: acc)))
    | T_Array _ -> compile_reference target var_env (compile_value v var_env (IncrRef :: RefAssign :: acc))
    | T_Struct _ -> compile_reference target var_env (compile_value v var_env (IncrRef :: RefAssign :: acc))
    | T_Null -> compile_reference target var_env (compile_value v var_env (RefAssign :: acc))
    | T_Generic _ -> compile_reference target var_env (compile_value v var_env (IncrRef :: RefAssign :: acc))
  )
  | (VariableAccess _, Reference re) -> ( match assign_type with 
    | T_Int -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Bool -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Char -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Array _ -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Struct _ -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Null -> compile_reference target var_env (compile_reference re var_env (RefAssign :: acc))
    | T_Generic _ -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
  )
  | (StructAccess(refer, field), Value v) -> ( match Typing.type_reference refer var_env with
    | (_,T_Struct (str_name, _)) -> ( match lookup_struct str_name var_env.structs with
      | None -> raise_error ("Could not find struct '" ^ str_name ^ "'")
      | Some (_, fields) -> ( match struct_field field fields with
        | (_,_,index) -> ( match assign_type with
          | T_Int -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: FetchFull :: (compile_value v var_env (AssignFull :: acc)))
          | T_Bool -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))
          | T_Char -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))
          | T_Array _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_value v var_env (IncrRef :: FieldAssign :: acc)))
          | T_Struct _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_value v var_env (IncrRef :: FieldAssign :: acc)))
          | T_Null  -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_value v var_env (FieldAssign :: acc)))
          | T_Generic _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_value v var_env (IncrRef :: FieldAssign :: acc)))
        )
      )
    )
    | (_,t) -> raise_error ("Struct field assignment to variable of type '" ^ Typing.type_string t ^ "'") 
  )
  | (StructAccess(refer, field), Reference re) -> ( match Typing.type_reference refer var_env with
    | (_,T_Struct (str_name, _)) -> ( match lookup_struct str_name var_env.structs with
      | None -> raise_error ("Could not find struct '" ^ str_name ^ "'")
      | Some (_, fields) -> ( match struct_field field fields with
        | (_, _, index) -> ( match assign_type with
          | T_Int -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Bool -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Char -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Array _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Struct _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Null  -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FieldAssign :: acc)))
          | T_Generic _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
        )
      )
    )
    | (_,t) -> raise_error ("Struct field assignment to variable of type '" ^ Typing.type_string t ^ "'") 
  )
  | (ArrayAccess(refer, index), Value v) -> ( match Typing.type_reference refer var_env with
    | (_,T_Array _) -> ( match Typing.type_assignable_expr index var_env with
      | (_,T_Int) -> ( match assign_type with
        | T_Int -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: FetchFull :: (compile_value v var_env (AssignFull :: acc)))))
        | T_Bool -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))))
        | T_Char -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))))
        | T_Array _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_value v var_env (IncrRef :: FieldAssign :: acc))))
        | T_Struct _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_value v var_env (IncrRef :: FieldAssign :: acc))))
        | T_Null -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_value v var_env (FieldAssign :: acc))))
        | T_Generic _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_value v var_env (IncrRef :: FieldAssign :: acc))))
      )
      | (_,_) -> raise_error "Array index must be of type 'int'"
    )
    | (_,t) -> raise_error ("Array assignment to variable of type '" ^ Typing.type_string t ^ "'") 
  )
  | (ArrayAccess(refer, index), Reference re) -> ( match Typing.type_reference refer var_env with
    | (_,T_Array _) -> ( match Typing.type_assignable_expr index var_env with 
      | (_,T_Int) -> ( match assign_type with
        | T_Int -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Bool -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Char -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Array _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Struct _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Null -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FieldAssign :: acc))))
        | T_Generic _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))) (*raise_error "Declaring generic arrays is not supported yet"*)
      )
      | (_,_) -> raise_error "Array index must be of type 'int'"
    )
    | (_,t) -> raise_error ("Array assignment to variable of type '" ^ Typing.type_string t ^ "'") 
  )
  

let update_locals env lock typ name =
  ({ env with var_env = ({ env.var_env with locals = (lock, typ, name)::env.var_env.locals }) })

let compile_declaration dec env =
  match dec with
  | TypeDeclaration (lock, typ, name) -> (
    if localvar_exists name env.var_env.locals then raise_error ("Duplicate variable name '" ^ name ^ "'")
    else if not(well_defined_type typ env.var_env) then raise_error "Ill defined type"
    else 
    ( update_locals env lock typ name,
      match typ with
      | T_Int -> fun a -> DeclareFull :: IncrRef :: CloneFull :: PlaceInt(0) :: AssignFull :: a
      | T_Bool -> fun a -> DeclareByte :: IncrRef :: CloneFull :: PlaceBool(false) :: AssignByte :: a
      | T_Char -> fun a -> DeclareByte :: IncrRef :: CloneFull :: PlaceChar('0') :: AssignByte :: a
      | T_Array _
      | T_Struct _
      | T_Generic _ -> fun a -> PlaceInt(0) :: a
      | T_Null -> raise_error "Cannot declare the 'null' type"
    )
  )
  | AssignDeclaration (lock, typ, name, expr) -> (
    let opt_expr = optimize_assignable_expr expr env.var_env in
    let typ = declaration_type_check name lock typ expr env.var_env in
    ( update_locals env lock typ name,
      match opt_expr with
      | Reference _ -> fun a -> compile_assignable_expr opt_expr env.var_env (IncrRef :: a)
      | Value _ -> (
        match typ with
        | T_Int -> fun a -> DeclareFull :: IncrRef :: CloneFull :: (compile_assignable_expr opt_expr env.var_env (AssignFull :: a))
        | T_Bool -> fun a -> DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr opt_expr env.var_env (AssignByte :: a))
        | T_Char -> fun a -> DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr opt_expr env.var_env (AssignByte :: a))
        | T_Array _ -> fun a -> compile_assignable_expr opt_expr env.var_env (IncrRef :: a)
        | T_Struct _ -> fun a -> compile_assignable_expr opt_expr env.var_env (IncrRef :: a)
        | T_Generic _ -> fun a -> compile_assignable_expr opt_expr env.var_env (IncrRef :: a)
        | T_Null -> fun a -> compile_assignable_expr opt_expr env.var_env a
      )
    )
  )

let rec compile_sod_list sod_list env contexts break continue cleanup acc =
  match sod_list with
  | [] -> acc
  | h::t -> (
    match h with
    | Statement(stmt, line) -> ( try (
        compile_stmt stmt env contexts break continue cleanup (compile_sod_list t env contexts break continue cleanup (acc))
      ) with
      | Error(_,line_opt,expl) when Option.is_none line_opt -> raise (Error(None, Some(line), expl))
      | e -> raise e
    )
    | Declaration(dec, line) -> ( try (
        let (new_env, f) = compile_declaration dec env in
        f (compile_sod_list t new_env contexts break continue (cleanup+1) acc)
      ) with
      | Error(_,line_opt,expl) when Option.is_none line_opt -> raise (Error(None, Some(line), expl))
      | e -> raise e
    )
  )

and compile_stmt stmt env contexts break continue cleanup acc =
  match stmt with
  | If (expr, s1, s2) -> (
    let label_true = Helpers.new_label () in
    let label_stop = Helpers.new_label () in
    let (_, t) = Typing.type_assignable_expr expr env.var_env in
    if t != T_Bool then raise_error "Condition not of type 'bool'"
    else compile_assignable_expr_as_value expr env.var_env (IfTrue(label_true) :: (compile_stmt s2 env contexts break continue cleanup (GoTo(label_stop) :: CLabel(label_true) :: (compile_stmt s1 env contexts break continue cleanup (CLabel(label_stop) :: acc)))))
  )
  | While (expr, s) -> (
    let label_cond = Helpers.new_label () in
    let label_start = Helpers.new_label () in
    let label_stop = Helpers.new_label () in
    let (_, t) = Typing.type_assignable_expr expr env.var_env in
    if t != T_Bool then raise_error "Condition not of type 'bool'"
    else GoTo(label_cond) :: CLabel(label_start) :: (compile_stmt s env contexts (Some label_stop) (Some label_cond) 0 (CLabel(label_cond) :: (compile_assignable_expr_as_value expr env.var_env (IfTrue(label_start) :: CLabel(label_stop) :: acc))))
  )
  | Block (sod_list) -> (
    let decs = count_decl sod_list in
    if decs = 0 then compile_sod_list sod_list env contexts break continue cleanup acc
    else compile_sod_list sod_list env contexts break continue cleanup (addFreeVars decs acc)
  )
  | Assign (target, aexpr) -> compile_assignment target (optimize_assignable_expr aexpr env.var_env) env.var_env acc
  | Call (context_opt, name, typ_args, args) -> (
    let context_env = 
      if Option.is_none context_opt then env
      else match List.find_opt (fun (alias, _) -> alias = (Option.get context_opt)) env.file_refs with
      | None -> raise_error ("No such context '" ^ (Option.get context_opt) ^ "'")
      | Some((_,c)) -> ( match List.find_opt (fun e -> match e with Context(cn,_) -> cn = c) contexts with
        | None -> raise_error ("Failed lookup of context '" ^ c ^ "'")
        | Some(Context(_,c_env)) -> c_env
      )
    in
    match lookup_routine name context_env.routine_env with
    | None -> raise_error ("Call to undefined routine '" ^ name ^ "'")
    | Some (accmod,typ_vars,params) -> (
      if Option.is_some context_opt && accmod = Internal then raise_error ("Call to internal routine of another context") else
      if List.length params != List.length args then raise_error (name ^ "(...) requires " ^ (Int.to_string (List.length params)) ^ " arguments, but was given " ^  (Int.to_string (List.length args)))
      else if typ_vars = [] then compile_arguments (List.combine params args) env.var_env (PlaceInt(List.length params) :: Call((context_env.context_name)^"#"^name) :: acc) 
      else (
        let typ_args = (
          if List.length typ_args = List.length typ_vars then typ_args 
          else if typ_args = [] then infere_generics typ_vars params args env.var_env 
          else raise_error (name ^ "(...) requires " ^ (Int.to_string (List.length typ_vars)) ^ " type arguments, but was given " ^  (Int.to_string (List.length typ_args)))
        ) in
        compile_arguments (List.combine (replace_generics params typ_vars typ_args) args) env.var_env (PlaceInt(List.length params) :: Call((context_env.context_name)^"#"^name) :: acc)
      )
    )
  )
  | Stop -> addStop(acc)
  | Halt -> addHalt(acc)
  | Break -> (
    match break with
    | Some name when cleanup = 0 -> GoTo(name) :: acc
    | Some name -> addFreeVars cleanup (GoTo(name) :: acc)
    | None -> raise_error "No loop to break out of"
  )
  | Continue -> (
    match continue with
    | Some name when cleanup = 0 -> GoTo(name) :: acc
    | Some name -> addFreeVars cleanup (GoTo(name) :: acc)
    | None -> raise_error "No loop to continue in"
  )
  | Print exprs -> (
    let rec aux es acc =
      match es with
      | [] -> acc
      | h::t -> (
        let (_, expr_ty) = Typing.type_assignable_expr h env.var_env in
        let opte = optimize_assignable_expr h env.var_env in
        match expr_ty with
        | T_Bool -> aux t (compile_assignable_expr_as_value opte env.var_env (PrintBool :: acc))
        | T_Int -> aux t (compile_assignable_expr_as_value opte env.var_env (PrintInt :: acc))
        | T_Char -> aux t (compile_assignable_expr_as_value opte env.var_env (PrintChar :: acc))
        | _ -> aux t (compile_assignable_expr opte env.var_env (PrintInt :: acc))
      )
    in
    aux (List.rev exprs) acc
  )

let rec compile_globalvars globvars structs contexts acc =
  match globvars with
  | [] -> acc
  | (_,context_name,_,_,_,dec)::t -> (
    try (
      match List.find_opt (fun c -> match c with Context(name,_) -> name = context_name) contexts with
      | None -> raise_error "Failed context lookup"
      | Some(Context(_,env)) -> (
        let (_,f) = compile_declaration dec ({ context_name = context_name; var_env = ({ locals = []; globals = env.var_env.globals; structs = structs; typ_vars = []}); routine_env = []; file_refs = [] }) in
        compile_globalvars t structs contexts (f acc)
      )
    )
    with
    | e -> raise e
  )

let compress_path path =
  let rec compress parts acc =
    match parts with
    | [] -> List.rev acc
    | h::t when h = "." -> compress t (acc)
    | _::h2::t when h2 = ".." -> compress t acc
    | h::t -> compress t (h::acc) 
  in
  String.concat "/" (compress (String.split_on_char '/' path) [])

let total_path path =
  if path.[0] = '.' then Sys.getcwd () ^ "/" ^ path
  else path

let complete_path base path = compress_path (if path.[0] = '.' then (String.sub base 0 ((String.rindex base '/')+1) ^ path) else path)

let gather_context_infos base_path parse =
  let rec get_context_environment path topdecs file_refs globals structs routines =
    match topdecs with
    | [] -> (complete_path base_path path, globals, structs, routines, file_refs)
    | (FileReference(alias, ref_path))::t -> (
      if List.exists (fun (a,_) -> a = alias) file_refs then failwith ("Duplicate context alias '" ^ alias ^ "'") else
      let ref_path = complete_path path ref_path in
      get_context_environment path t ((alias,ref_path)::file_refs) globals structs routines
    )
    | (Routine(access_mod, name, typ_vars, params, stmt))::t -> get_context_environment path t file_refs globals structs ((access_mod,name,(complete_path base_path path),typ_vars,params,stmt)::routines)
    | (Struct(name, typ_vars, fields))::t -> get_context_environment path t file_refs globals ((name, typ_vars, fields)::structs) routines
    | (GlobalDeclaration(declaration))::t -> ( match declaration with
      | TypeDeclaration(lock, typ, name) -> get_context_environment path t file_refs ((name,(complete_path base_path path),lock,typ,declaration)::globals) structs routines
      | AssignDeclaration(lock, typ_opt, name, _) -> ( match typ_opt with
        | Some(typ) -> get_context_environment path t file_refs ((name,(complete_path base_path path),lock,typ,declaration)::globals) structs routines
        | None -> failwith "Cannot infere types for global variables"
      )
    )
  in
  let rec get_contexts path parse acc =
    let path = complete_path base_path path in
    let file = parse path in
    let context_env = get_context_environment path (match file with File(t) -> t) [][][][] in
    let (_,_,_,_,file_refs) = context_env in
    List.fold_right (fun (_,ref_path) acc -> 
      if List.exists (fun (_,(p,_,_,_,_)) -> p = ref_path) acc then acc
      else get_contexts ref_path parse (acc)
    ) file_refs ((file,context_env)::acc)
  in
  get_contexts base_path parse []

let merge_contexts contexts =
  let rec aux cs topdecs globals structs =
    match cs with
    | [] -> (File(topdecs), globals, structs)
    | (File(tds),(_,c_globals,c_structs,_,_))::t -> (
      aux t 
        (List.rev_append topdecs tds) 
        (List.rev_append globals c_globals) 
        (List.rev_append structs c_structs) 
    )
  in
  aux contexts [][][]

let create_contexts globals context_infos : context list =
  let get_globalvar_info var_name context_name = 
    match List.find_opt (fun (n,cn,_,_,_,_) -> n = var_name && cn = context_name) globals with
    | None -> failwith "Failed global variable lookup"
    | Some((n,cn,idx,lock,typ,dec)) -> (n,cn,idx,lock,typ,dec)
  in
  let rec aux c_infos acc =
    match c_infos with
    | [] -> acc
    | (_,(context_name, globs, structs, routines, file_refs))::t -> (
      aux t (Context(context_name, ({ context_name = context_name; var_env = { locals = []; globals = (List.map (fun (n,cn,_,_,_) -> get_globalvar_info n cn) globs); structs = structs; typ_vars = []}; routine_env = routines; file_refs = file_refs }))::acc)
    )
  in
  aux context_infos []

let compile path parse =
  let path = (compress_path (total_path path)) in
  let context_infos = gather_context_infos path parse in
  let (topdecs,globals,structs) = merge_contexts context_infos in
  let globals_ordered = (order_dep_globvars (get_globvar_dependencies globals)) in
  let contexts = create_contexts globals_ordered context_infos in
  let () = check_topdecs topdecs structs in
  let () = check_structs structs in
  let rec compile_routines routines acc =
    match routines with
    | [] -> acc
    | (accmod,name,context_name,typ_vars,params,stmt)::t -> ( match List.find (fun c -> match c with Context(cn,_) -> cn = context_name) contexts with 
      | Context(context_name, context_env) ->
        compile_routines t ((routine_head accmod name context_name path params)::(compile_stmt stmt ({ context_name = context_name; var_env = ({ locals = (List.rev params); globals = context_env.var_env.globals; structs = structs; typ_vars = typ_vars;}); routine_env = context_env.routine_env; file_refs = context_env.file_refs}) contexts None None 0 (addStop(acc))))
      )
  in
  let rec compile_contexts cts acc =
    match cts with
    | [] -> acc
    | Context(path, env)::t -> ( 
      try (
        compile_contexts t (compile_routines env.routine_env acc)
      ) with
        | Error(_,line_opt,expl_opt) -> raise (Error(Some(path),line_opt,expl_opt))
    )
  in
  Program(structs, (gather_globvar_info (match (List.find (fun c -> match c with Context(cn,_) -> cn = path) contexts) with Context(_,env) -> env.var_env.globals)), ProgramRep.translate(compile_globalvars (List.rev globals_ordered) structs contexts ((ToStart :: (compile_contexts contexts [])))))