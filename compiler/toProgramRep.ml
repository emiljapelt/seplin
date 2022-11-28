open Absyn
open ProgramRep
open Exceptions

let lookup_routine (name: string) routines =
  let rec aux li =
    match li with
    | [] -> None
    | (n,ps)::t -> if n = name then Some(ps) else aux t
  in
  aux routines

let lookup_globvar (name: string) globvars =
  let rec aux li c =
    match li with
    | [] -> None
    | (n,c,l,ty,_)::t -> if n = name then Some((c,ty,l)) else aux t (c-1)
  in
  aux globvars ((List.length globvars) - 1)

let lookup_localvar (name: string) localvars =
  let rec aux li c =
    match li with
    | [] -> None
    | (l,ty,n)::t -> if n = name then Some((c,ty,l)) else aux t (c-1)
  in
  aux localvars ((List.length localvars) - 1)

let lookup_struct (name: string) structs =
  let rec aux li = 
    match li with
    | [] -> None
    | (n,ps)::t -> if n = name then Some(ps) else aux t
  in
  aux structs

let count_decl stmt_dec_list =
  let rec aux sdl c =
    match sdl with
    | [] -> c
    | (Declaration dec)::t -> (
      match dec with
      | TypeDeclaration _ -> aux t (c+1)
      | AssignDeclaration _ -> aux t (c+1)
      | VarDeclaration _ -> aux t (c+1)
    )
    | _::t -> aux t (c)
  in
  aux stmt_dec_list 0

let rec type_string t =
  match t with
  | T_Bool -> "bool"
  | T_Int -> "int"
  | T_Array arr_ty -> (type_string arr_ty)^"[]"
  | T_Struct n -> n
  | T_Null -> "null"

let rec type_equal type1 type2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | (T_Int, T_Int) -> true
    | (T_Bool, T_Bool) -> true
    | (T_Array at1, T_Array at2) -> type_equal at1 at2
    | (T_Struct n1, T_Struct n2) when n1 = n2 -> true
    | (T_Null, _) -> true
    | _ -> false
  in aux type1 type2 || aux type2 type1

let routine_head accmod name params =
  match accmod with
  | Internal -> CLabel(name)
  | External -> CEntryPoint(name, List.map (fun (l,t,n) -> t) params)

type label_generator = { mutable next : int }

let lg = ( {next = 0;} )

let new_label () =
  let number = lg.next in
  let () = lg.next <- lg.next+1 in
  Int.to_string number

let var_index (name: string) globvars localvars = 
  match lookup_localvar name localvars with
  | Some (lc,_,_) -> lc
  | None -> 
    match lookup_globvar name globvars with
    | Some (gc,_,_) -> gc
    | None -> compile_error ("No such variable " ^ name)

let fetch_var_index (name: string) globvars localvars = 
  match lookup_localvar name localvars with
  | Some (lc,_,_) -> BPFetch(lc)
  | None -> 
    match lookup_globvar name globvars with
    | Some (gc,_,_) -> StackFetch(gc)
    | None -> compile_error ("No such variable " ^ name)

let fetch_var (name: string) globvars localvars = 
  let t = match lookup_localvar name localvars with
    | Some (_,lt,_) -> lt
    | None -> 
      match lookup_globvar name globvars with
      | Some (_,gt,_) -> gt
      | None -> compile_error ("No such variable " ^ name)
  in
  (t, (fetch_var_index name globvars localvars))


let struct_field field params =
  let rec aux ps c =
    match ps with
    | [] -> compile_error ("No such field, " ^ field)
    | (l,ty,n)::t -> if n = field then (l,ty,c) else aux t (c+1)
  in
  aux params 0

let var_locked (name: string) globvars localvars = 
  match lookup_localvar name localvars with
    | Some (_,_,ll) -> ll
    | None -> 
      match lookup_globvar name globvars with
      | Some (_,_,gl) -> gl
      | None -> compile_error ("No such variable " ^ name)

let globvar_exists (name: string) globvars =
  match lookup_globvar name globvars with
  | Some _ -> true
  | None -> false
  
let localvar_exists (name: string) localvars =
  match lookup_localvar name localvars with
  | Some _ -> true
  | None -> false

let routine_exists (name: string) routines =
  match lookup_routine name routines with
  | Some _ -> true
  | None -> false

let struct_exists (name: string) structs =
  match lookup_struct name structs with
  | Some _ -> true
  | None -> false

let default_value t =
  match t with
  | T_Int -> Value (Int 0)
  | T_Bool -> Value (Bool false)
  | _ -> Reference (Null)

let simple_type t =
  match t with
    | T_Int | T_Bool -> true
    | _ -> false

(*    list of: string * int * bool * typ * declaration    *)
let get_globvars (tds : topdecs) = 
  let rec aux topdecs acc count =
    match topdecs with
    | [] -> acc
    | (GlobalDeclaration dec)::t -> ( match dec with
      | TypeDeclaration (lock,ty,name) -> ( 
        if globvar_exists name acc then compile_error ("Duplicate global variable name: " ^ name)
        else aux t ((name, count, lock, ty, dec)::acc) (count+1)
      )
      | AssignDeclaration (lock,ty,name,expr) -> ( 
        if globvar_exists name acc then compile_error ("Duplicate global variable name: " ^ name)
        else aux t ((name, count, lock, ty, dec)::acc) (count+1)
      )
      | VarDeclaration (lock,name,expr) -> compile_error "var is not supported for global variables"
    )
    | _::t -> aux t acc count
  in match tds with 
  | Topdecs l -> aux l [] 0

(*    list of: string * access_mod * (bool * typ * string) list * statement    *)
let get_routines (tds : topdecs) =
  let rec aux topdecs acc =
    match topdecs with
    | [] -> acc
    | h::t -> (
      match h with
      | Routine (accmod, name, params, stmt) -> (
        if routine_exists name acc then compile_error ("Duplicate routine name: " ^ name)
        else aux t ((name, params)::acc)
        )
      | _ -> aux t acc
    )
  in match tds with
  | Topdecs l -> aux l []

(*    list of: string * (bool, typ * string) list   *)
let get_structs (tds : topdecs) =
  let rec aux topdecs acc =
    match topdecs with
    | [] -> acc
    | h::t -> (
      match h with
      | Struct (name, params) -> (
        if struct_exists name acc then compile_error ("Duplicate struct name: " ^ name)
        else aux t ((name, params)::acc)
        )
      | _ -> aux t acc
    )
  in match tds with
  | Topdecs l -> aux l []


(* lock, type, instructions *)
let rec compile_assignable_expr expr globvars localvars structs =
  match expr with
  | Reference ref_expr -> compile_reference ref_expr globvars localvars structs
  | Value val_expr -> compile_value val_expr globvars localvars structs

and compile_reference ref_expr globvars localvars structs =
  match ref_expr with
  | VarRef name -> (
    let (var_ty, inst) = fetch_var name globvars localvars in
    (var_locked name globvars localvars, var_ty, [inst])
  )
  | StructRef (refer, field) -> (
    let (ref_lock, ref_ty, inst) = compile_reference refer globvars localvars structs in
    match ref_ty with
    | T_Struct name -> (
      match lookup_struct name structs with
      | Some params -> (
        let (field_lock, field_ty, index) = struct_field field params in
        (ref_lock || field_lock, field_ty, (inst) @ [FetchFull; PlaceInt(index); FieldFetch;])
      )
      | None -> compile_error ("No such struct: " ^ name)
    )
    | _ -> compile_error ("Struct field lookup type failure")
  )
  | ArrayRef (refer, index) -> (
    let (ref_lock, ref_ty, inst) = compile_reference refer globvars localvars structs in
    let (idx_ty, index_inst) = compile_assignable_expr_as_value index globvars localvars structs in
    match (ref_ty, idx_ty) with
    | (T_Array (sub_ty), T_Int) -> (ref_lock, sub_ty, inst @ (FetchFull :: index_inst) @ [FieldFetch;])
    | _ -> compile_error ("Array lookup type failure")
  )
  | Null -> (false, T_Null, [PlaceInt(0)])

and compile_assignable_expr_as_value aexpr globvars localvars structs =
  match aexpr with
  | Reference r -> (
    let (_, ref_ty, inst) = compile_reference r globvars localvars structs in
    match ref_ty with
    | T_Int -> (ref_ty, inst @ [FetchFull;FetchFull;])
    | T_Bool -> (ref_ty, inst @ [FetchFull;FetchByte;])
    | T_Array _ -> (ref_ty, inst @ [FetchFull;])
    | T_Struct _ -> (ref_ty, inst @ [FetchFull;])
    | T_Null -> (ref_ty, inst)
  )
  | _ -> (
    let (expr_lock, expr_ty, inst) = compile_assignable_expr aexpr globvars localvars structs in
    (expr_ty, inst)
  )

and compile_value val_expr globvars localvars structs =
  match val_expr with
  | Bool b -> (false, T_Bool, [PlaceBool(b)])
  | Int i -> (false, T_Int, [PlaceInt(i)])
  | ArraySize refer -> (
    let (_, ref_ty, inst) = compile_reference refer globvars localvars structs in
    match ref_ty with
    | T_Array _ -> (false, T_Int, inst @ [FetchFull; SizeOf;])
    | _ -> compile_error "Array size only makes sense for arrays"
  )
  | Lookup refer -> (
    let (ref_lock, ref_ty, inst) = compile_reference refer globvars localvars structs in
    match ref_ty with
    | T_Int -> (ref_lock, T_Int, inst @ [FetchFull; FetchFull;])
    | T_Bool -> (ref_lock, T_Bool, inst @ [FetchFull; FetchByte])
    | T_Array ty -> (ref_lock, T_Array ty, inst @ [FetchFull;])
    | T_Struct n -> (ref_lock, T_Struct n, inst @ [FetchFull;])
    | T_Null -> compile_error ("Direct null pointer dereferencing")
  )
  | NewArray (arr_ty, size_expr) -> (
    let (s_ty, size_inst) = compile_assignable_expr_as_value size_expr globvars localvars structs in
    match s_ty with
    | T_Int -> (false, T_Array arr_ty, size_inst @ [DeclareStruct])
    | _ -> compile_error ("Init array with non-int size")
  )
  | NewStruct (name, args) -> (
    let rec aux ags fields c acc =
      match (ags, fields) with
      | ([], []) -> acc
      | (ha::ta, (field_lock,field_ty,_)::tf) -> (
        let (ha_lock, ha_ty, ha_inst) = compile_assignable_expr ha globvars localvars structs in
        if not (type_equal field_ty ha_ty) then compile_error ("Struct argument type mismatch")
        else if ha_lock && (not field_lock) then compile_error "Cannot give a locked variable as a parameter that is not locked"
        else match ha with
        | Value _ -> (
          match ha_ty with
          | T_Int -> aux ta tf (c+1) ((CloneFull :: PlaceInt(c) :: DeclareFull :: CloneFull :: ha_inst) @ (AssignFull :: FieldAssign :: acc))
          | T_Bool -> aux ta tf (c+1) ((CloneFull :: PlaceInt(c) :: DeclareByte :: CloneFull :: ha_inst) @ (AssignByte :: FieldAssign :: acc))
          | _ -> aux ta tf (c+1) ((CloneFull :: PlaceInt(c) :: ha_inst) @ (FieldAssign :: acc))
        )
        | Reference r -> (
          match r with
          | Null -> aux ta tf (c+1) ((CloneFull :: PlaceInt(c) :: ha_inst) @ (FieldAssign :: acc))
          | _ -> aux ta tf (c+1) ((CloneFull :: PlaceInt(c) :: ha_inst) @ (FetchFull :: FieldAssign :: acc))
        )
      )
      | (_,_) -> compile_error ("Struct argument count mismatch")
    in
    match lookup_struct name structs with
    | Some params -> (false, T_Struct name, PlaceInt(List.length params) :: DeclareStruct :: (aux args params 0 []) )
    | None -> compile_error ("No such struct: " ^ name)
  )
  | Binary_op (op, e1, e2) -> (
      let (t1, ins1) = compile_assignable_expr_as_value e1 globvars localvars structs in
      let (t2, ins2) = compile_assignable_expr_as_value e2 globvars localvars structs in
      match (op, t1, t2, e1, e2) with
      | ("&&", T_Bool, T_Bool, Value(Bool true), _) ->  (false, T_Bool, ins2)
      | ("&&", T_Bool, T_Bool, _, Value(Bool true)) ->  (false, T_Bool, ins1)
      | ("&&", T_Bool, T_Bool, Value(Bool false), _) ->  (false, T_Bool, [PlaceBool(false)])
      | ("&&", T_Bool, T_Bool, _, Value(Bool false)) ->  (false, T_Bool, [PlaceBool(false)])
      | ("&&", T_Bool, T_Bool, _, _) ->  (false, T_Bool, ins1 @ ins2 @ [BoolAnd])
      | ("||", T_Bool, T_Bool, Value(Bool true), _) -> (false, T_Bool, [PlaceBool(true)])
      | ("||", T_Bool, T_Bool, _, Value(Bool true)) -> (false, T_Bool, [PlaceBool(true)])
      | ("||", T_Bool, T_Bool, Value(Bool false), _) -> (false, T_Bool, ins2)
      | ("||", T_Bool, T_Bool, _, Value(Bool false)) -> (false, T_Bool, ins1)
      | ("||", T_Bool, T_Bool, _, _) -> (false, T_Bool, ins1 @ ins2 @ [BoolOr])
      | ("=", _, _, Reference(r), Reference(Null)) -> (
        let (_,_,r_inst) = compile_reference r globvars localvars structs in
        (false, T_Bool, (r_inst) @ [FetchFull;PlaceInt(0); IntEq;])
      )
      | ("=", _, _, Reference(Null), Reference(r)) -> (
        let (_,_,r_inst) = compile_reference r globvars localvars structs in
        (false, T_Bool, (r_inst) @ [FetchFull;PlaceInt(0); IntEq;])
      )
      | ("=", T_Bool, T_Bool, _, _) -> (false, T_Bool, ins1 @ ins2 @ [BoolEq])
      | ("=", T_Int, T_Int, _, _) -> (false, T_Bool, ins1 @ ins2 @ [IntEq])
      | ("!=", T_Bool, T_Bool, _, _) -> (false, T_Bool, ins1 @ ins2 @ [BoolEq] @ [BoolNot])
      | ("!=", T_Int, T_Int, _, _) -> (false, T_Bool, ins1 @ ins2 @ [IntEq] @ [BoolNot])
      | ("<=", T_Int, T_Int, _, _) -> (false, T_Bool, ins1 @ ins2 @ [IntLt] @ [BoolNot]) 
      | ("<", T_Int, T_Int, _, _) -> (false, T_Bool, ins2 @ ins1 @ [IntLt])
      | (">=", T_Int, T_Int, _, _) -> (false, T_Bool, ins2 @ ins1 @ [IntLt] @ [BoolNot])
      | (">", T_Int, T_Int, _, _) -> (false, T_Bool, ins1 @ ins2 @ [IntLt])
      | ("+", T_Int, T_Int, Value(Int 0), _) -> (false, T_Int, ins2)
      | ("+", T_Int, T_Int, _, Value(Int 0)) -> (false, T_Int, ins1)
      | ("+", T_Int, T_Int, _, _) -> (false, T_Int, ins1 @ ins2 @ [IntAdd])
      | ("-", T_Int, T_Int, _, Value(Int 0)) -> (false, T_Int, ins1)
      | ("-", T_Int, T_Int, _, _) -> (false, T_Int, ins2 @ ins1 @ [IntSub])
      | ("*", T_Int, T_Int, Value(Int 0), _) -> (false, T_Int, [PlaceInt(0)])
      | ("*", T_Int, T_Int, _, Value(Int 0)) -> (false, T_Int, [PlaceInt(0)])
      | ("*", T_Int, T_Int, Value(Int 1), _) -> (false, T_Int, ins2)
      | ("*", T_Int, T_Int, _, Value(Int 1)) -> (false, T_Int, ins1)
      | ("*", T_Int, T_Int, _, _) -> (false, T_Int, ins1 @ ins2 @ [IntMul])
      | _ -> compile_error "Unknown binary operator, or type mismatch"
    )
  | Unary_op (op, e) -> (
    let (t, ins) = compile_assignable_expr_as_value e globvars localvars structs in
    match (op, t) with
    | ("!", T_Bool) -> (false, T_Bool, ins @ [BoolNot])
    | _ -> compile_error "Unknown unary operator, or type mismatch"
  )

let compile_arguments params exprs globvars localvars structs =
  let rec aux ps es acc =
    match (ps, es) with
    | ([],[]) -> acc
    | ((plock, pty, pname)::pt,eh::et) -> (
        let (expr_lock, expr_ty, inst) = compile_assignable_expr eh globvars localvars structs in
        if not (type_equal pty expr_ty) then compile_error ("Type mismatch on assignment: expected " ^ (type_string pty) ^ ", got " ^ (type_string expr_ty)) 
        else if expr_lock && (not plock) then compile_error "Cannot give a locked variable as a parameter that is not locked"
        else match eh with
        | Value _ -> (
          match expr_ty with
          | T_Int -> aux pt et (DeclareFull :: CloneFull :: inst @ (AssignFull :: acc))
          | T_Bool -> aux pt et (DeclareByte :: CloneFull :: inst @ (AssignByte :: acc))
          | T_Array _ -> aux pt et (inst @ acc)
          | T_Struct _ -> aux pt et (inst @ acc)
          | T_Null -> aux pt et (inst @ acc)
        )
        | _ -> aux pt et ((inst @ [FetchFull]) @ acc)
      )
    | _ -> compile_error "Insufficient arguments in call"
  in
  aux (List.rev params) (List.rev exprs) []

let rec compile_assignment target assign globvars localvars structs =
  match (target, assign) with
  | (Null, _) -> compile_error "Cannot assign to null"
  | (VarRef name, Value v) -> (
    let (refer_lock, refer_ty, refer_inst) = compile_reference target globvars localvars structs in
    let (val_lock, val_ty, val_inst) = compile_value v globvars localvars structs in
    if refer_lock then compile_error "Cannot assign to locked variable"
    else if val_lock then compile_error "Cannot assign locked value to new variable"
    else if not (type_equal refer_ty val_ty) then compile_error "Type mismatch in variable assignment"
    else match val_ty with 
    | T_Int -> (refer_inst @ [FetchFull]) @ (val_inst @ [AssignFull])
    | T_Bool -> (refer_inst @ [FetchFull]) @ (val_inst @ [AssignByte])
    | T_Array _ -> (refer_inst) @ (val_inst @ [RefAssign]) 
    | T_Struct _ -> (refer_inst) @ (val_inst @ [RefAssign]) 
    | T_Null -> (refer_inst) @ (val_inst @ [RefAssign]) 
  )
  | (VarRef name, Reference re) -> (
    let (refer_lock, refer_ty, refer_inst) = compile_reference target globvars localvars structs in
    let (val_lock, val_ty, val_inst) = compile_reference re globvars localvars structs in
    if refer_lock || val_lock then compile_error "Cannot assign locked"
    else if not (type_equal refer_ty val_ty) then compile_error "Type mismatch in variable assignment"
    else match val_ty with 
    | T_Int -> (refer_inst) @ (val_inst @ [FetchFull; RefAssign;]) 
    | T_Bool -> (refer_inst) @ (val_inst @ [FetchFull; RefAssign;]) 
    | T_Array _ -> (refer_inst) @ (val_inst @ [FetchFull; RefAssign;]) 
    | T_Struct _ -> (refer_inst) @ (val_inst @ [FetchFull; RefAssign;]) 
    | T_Null -> (refer_inst) @ (val_inst @ [FetchFull; RefAssign;]) 
  )
  | (StructRef(refer, field), Value v) -> (
    let (refer_lock, refer_ty, refer_inst) = compile_reference refer globvars localvars structs in
    let (val_lock, val_ty, val_inst) = compile_value v globvars localvars structs in
    match refer_ty with
    | T_Struct str_name -> ( match lookup_struct str_name structs with
      | Some fields -> ( match struct_field field fields with
        | (field_lock,field_ty,index) -> (
          if field_lock || refer_lock then compile_error "Cannot assign to locked field"
          else if val_lock then compile_error "Cannot assign locked value to new variable"
          else if not (type_equal field_ty val_ty) then compile_error "Type mismatch in field assignment"
          else match val_ty with
          | T_Int -> (refer_inst @ [FetchFull; PlaceInt(index); FieldFetch; FetchFull;]) @ (val_inst @ [AssignFull])
          | T_Bool -> (refer_inst @ [FetchFull; PlaceInt(index); FieldFetch; FetchFull;]) @ (val_inst @ [AssignByte])
          | T_Array _ -> (refer_inst @ [FetchFull; PlaceInt(index)]) @ (val_inst @ [FieldAssign])
          | T_Struct _ -> (refer_inst @ [FetchFull; PlaceInt(index)]) @ (val_inst @ [FieldAssign])
          | T_Null  -> (refer_inst @ [FetchFull; PlaceInt(index)]) @ (val_inst @ [FieldAssign])
        )
      )
      | None -> compile_error ("Could not find struct: " ^ str_name)
    )
    | _ -> compile_error "Struct assignment to non-struct" 
  )
  | (StructRef(refer, field), Reference re) -> (
    let (refer_lock, refer_ty, refer_inst) = compile_reference refer globvars localvars structs in
    let (val_lock, val_ty, val_inst) = compile_reference re globvars localvars structs in
    match refer_ty with
    | T_Struct str_name -> ( match lookup_struct str_name structs with
      | Some fields -> ( match struct_field field fields with
        | (field_lock, field_ty, index) -> (
          if field_lock || refer_lock then compile_error "Cannot assign to locked field"
          else if not (type_equal field_ty val_ty) then compile_error "Type mismatch in field assignment"
          else match val_ty with
          | T_Int -> (refer_inst @ [FetchFull; PlaceInt(index);]) @ (val_inst @ [FetchFull; FieldAssign;])
          | T_Bool -> (refer_inst @ [FetchFull; PlaceInt(index); ]) @ (val_inst @ [FetchFull; FieldAssign;])
          | T_Array _ -> (refer_inst @ [FetchFull; PlaceInt(index);]) @ (val_inst @ [FetchFull; FieldAssign;])
          | T_Struct _ -> (refer_inst @ [FetchFull; PlaceInt(index);]) @ (val_inst @ [FetchFull; FieldAssign;])
          | T_Null  -> (refer_inst @ [FetchFull; PlaceInt(index);]) @ (val_inst @ [FieldAssign])
        )
      )
      | None -> compile_error ("Could not find struct: " ^ str_name)
    )
    | _ -> compile_error "Struct assignment to non-struct" 
  )
  | (ArrayRef(refer, index), Value v) -> (
    let (refer_lock, refer_ty, refer_inst) = compile_reference refer globvars localvars structs in
    let (val_lock, val_ty, val_inst) = compile_value v globvars localvars structs in
    let (index_ty, index_inst) = compile_assignable_expr_as_value index globvars localvars structs in
    match refer_ty with
    | T_Array arr_ty -> ( 
      if refer_lock then compile_error "Cannot assign to locked array"
      else if val_lock then compile_error "Cannot assign locked value to new variable"
      else if not (type_equal index_ty T_Int) then compile_error "Array index must be of type int"
      else if not (type_equal arr_ty val_ty) then compile_error "Type mismatch in array field assignment"
      else match val_ty with
      | T_Int -> (refer_inst @ [FetchFull] @ index_inst @ [FieldFetch; FetchFull;]) @ (val_inst @ [AssignFull])
      | T_Bool -> (refer_inst @ [FetchFull] @ index_inst @ [FieldFetch; FetchFull;]) @ (val_inst @ [AssignByte])
      | T_Array _ -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FieldAssign])
      | T_Struct _ -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FieldAssign])
      | T_Null -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FieldAssign])
    )
    | _ -> compile_error "Array assignment to non-array" 
  )
  | (ArrayRef(refer, index), Reference re) -> (
    let (refer_lock, refer_ty, refer_inst) = compile_reference refer globvars localvars structs in
    let (val_lock, val_ty, val_inst) = compile_reference re globvars localvars structs in
    let (index_ty, index_inst) = compile_assignable_expr_as_value index globvars localvars structs in
    match refer_ty with
    | T_Array arr_ty -> ( 
      if refer_lock then compile_error "Cannot assign to locked array"
      else if not (type_equal index_ty T_Int) then compile_error "Array index must be of type int"
      else if not (type_equal arr_ty val_ty) then compile_error "Type mismatch in array field assignment"
      else match val_ty with
      | T_Int -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FetchFull; FieldAssign;])
      | T_Bool -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FetchFull; FieldAssign;])
      | T_Array _ -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FetchFull; FieldAssign;])
      | T_Struct _ -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FetchFull; FieldAssign;])
      | T_Null -> (refer_inst @ [FetchFull] @ index_inst) @ (val_inst @ [FieldAssign])
    )
    | _ -> compile_error "Array assignment to non-array" 
  )

let compile_unassignable_expr expr globvars localvars structs routines break continue cleanup =
  match expr with
  | Assign (target, aexpr) -> compile_assignment target aexpr globvars localvars structs
  | Call (n, aexprs) -> (
    match lookup_routine n routines with
    | None -> compile_error ("No such routine: " ^ n)
    | Some (ps) when (List.length ps) = (List.length aexprs) -> (
      (compile_arguments ps aexprs globvars localvars structs) @ (PlaceInt(List.length ps) :: [Call(n)])
    )
    | Some (ps) -> compile_error (n ^ " requires " ^ (Int.to_string (List.length ps)) ^ " arguments, but was given " ^  (Int.to_string (List.length aexprs)))
  )
  | Stop -> [CStop]
  | Halt -> [CHalt]
  | Break -> (
    match break with
    | Some name when cleanup = 0 -> [GoTo(name)]
    | Some name -> FreeVars(cleanup) :: [GoTo(name)]
    | None -> compile_error "No loop to break out of"
  )
  | Continue -> (
    match continue with
    | Some name when cleanup = 0 -> [GoTo(name)]
    | Some name -> FreeVars(cleanup) :: [GoTo(name)]
    | None -> compile_error "No loop to continue in"
  )
  | Print expr -> (
    let (expr_ty, inst) = compile_assignable_expr_as_value expr globvars localvars structs in
    match expr_ty with
    | T_Bool -> inst @ [PrintBool]
    | T_Int -> inst @ [PrintInt]
    | _ -> [PlaceBool(false); PrintBool;] (* This is not as intended! *)
  )

let rec compile_declaration dec globvars localvars structs =
  match dec with
  | TypeDeclaration (l, ty, n) -> (
    if localvar_exists n localvars then compile_error ("Duplicate variable name: " ^ n)
    else match ty with
    | T_Int -> (DeclareFull :: CloneFull :: PlaceInt(0) :: [AssignFull], (l,ty,n)::localvars)
    | T_Bool -> (DeclareByte :: CloneFull :: PlaceBool(false) :: [AssignByte], (l,ty,n)::localvars)
    | T_Array _ -> ([PlaceInt(0)], (l,ty,n)::localvars)
    | T_Struct _ -> ([PlaceInt(0)], (l,ty,n)::localvars)
    | T_Null -> compile_error "Cannot declare the 'null' type"
  )
  | AssignDeclaration (l, ty, n, expr) -> (
    if localvar_exists n localvars then compile_error ("Duplicate variable name: " ^ n)
    else let (expr_lock, expr_ty, ins) = compile_assignable_expr expr globvars localvars structs in
    if expr_lock && (not l) then compile_error "Cannot "
    else match ty with
    | T_Int when expr_ty = T_Int -> (DeclareFull :: [CloneFull] @ ins @ [AssignFull], (l,ty,n)::localvars)
    | T_Bool when expr_ty = T_Bool -> (DeclareByte :: [CloneFull] @ ins @ [AssignByte], (l,ty,n)::localvars)
    | T_Array arr_ty when expr_ty = T_Array(arr_ty) -> (ins, (l,ty,n)::localvars)
    | T_Struct str_ty when expr_ty = T_Struct(str_ty) -> (ins, (l,ty,n)::localvars)
    | _ -> compile_error ("Type mismatch on declaration: expected " ^ (type_string ty) ^ ", got " ^ (type_string expr_ty)) 
  )
  | VarDeclaration (l, n, expr) -> (
    if localvar_exists n localvars then compile_error ("Duplicate variable name: " ^ n)
    else let (expr_lock, expr_ty, ins) = compile_assignable_expr expr globvars localvars structs in
    match expr_ty with
    | T_Int -> (DeclareFull :: [CloneFull] @ ins @ [AssignFull], (l,expr_ty,n)::localvars)
    | T_Bool -> (DeclareByte :: [CloneFull] @ ins @ [AssignByte], (l,expr_ty,n)::localvars)
    | T_Array _ -> (ins, (l,expr_ty,n)::localvars)
    | T_Struct _ -> (ins, (l,expr_ty,n)::localvars)
    | T_Null -> compile_error "Cannot declare the 'null' type"
  )

let rec compile_sod_list sod_list globvars localvars structs routines break continue cleanup =
  match sod_list with
  | [] -> []
  | h::t -> (
    match h with
    | Statement s -> compile_stmt s globvars localvars structs routines break continue cleanup @ compile_sod_list t globvars localvars structs routines break continue cleanup
    | Declaration dec -> (
      let (dec_ins, new_localvars) = compile_declaration dec globvars localvars structs in
      dec_ins @ compile_sod_list t globvars new_localvars structs routines break continue (cleanup+1)
    )
  )

and compile_stmt stmt globvars localvars structs routines break continue cleanup =
  match stmt with
  | If (e, s1, s2) -> (
    let label_true = new_label () in
    let label_stop = new_label () in
    let (t, ins) = compile_assignable_expr_as_value e globvars localvars structs in
    if t != T_Bool then compile_error "Conditional requires 'bool'"
    else ins @ [IfTrue(label_true)] @ (compile_stmt s2 globvars localvars structs routines break continue cleanup) @ [GoTo(label_stop)] @ [CLabel(label_true)] @ (compile_stmt s1 globvars localvars structs routines break continue cleanup) @ [CLabel(label_stop)]
  )
  | While (e, s) -> (
    let label_cond = new_label () in
    let label_start = new_label () in
    let label_stop = new_label () in
    let (t, ins) = compile_assignable_expr_as_value e globvars localvars structs in
    if t != T_Bool then compile_error "Conditional requires 'bool'"
    else (GoTo(label_cond)) :: CLabel(label_start) :: (compile_stmt s globvars localvars structs routines (Some label_stop) (Some label_cond) 0) @ [CLabel(label_cond)] @ ins @ (IfTrue(label_start) :: [CLabel(label_stop)])
  )
  | For (dec, con, modi, s) -> (
    let label_cond = new_label () in
    let label_start = new_label () in
    let label_modi = new_label () in
    let label_stop = new_label () in
    let (dec_ins, new_localvars) = compile_declaration dec globvars localvars structs in
    let (con_t, con_ins) = compile_assignable_expr_as_value con globvars new_localvars structs in
    let modi_ins = compile_unassignable_expr modi globvars new_localvars structs routines None None cleanup in
    if con_t != T_Bool then compile_error "Conditional requires 'bool'"
    else dec_ins @ (GoTo(label_cond) :: CLabel(label_start) :: compile_stmt s globvars new_localvars structs routines (Some label_stop) (Some label_modi) 0) @ (CLabel(label_modi) :: modi_ins) @ [CLabel(label_cond)] @ con_ins @ (IfTrue(label_start) :: CLabel(label_stop) :: [FreeVar])
  )
  | Block (sod_list) -> (
    let decs = count_decl sod_list in
    let block_ins = compile_sod_list sod_list globvars localvars structs routines break continue cleanup in 
    if decs = 0 then block_ins else block_ins @ [FreeVars(count_decl sod_list)]
  )
  | Expression (expr) -> compile_unassignable_expr expr globvars localvars structs routines break continue cleanup

(*    Compute the list of variable dependencies for each global variable    *)
let get_globvar_dependencies gvs =
  let rec dependencies_from_assignable expr acc =
    match expr with
    | Reference r -> ( match r with
      | VarRef (name) -> name::acc
      | ArrayRef (refer,_) -> dependencies_from_assignable (Reference refer) acc
      | StructRef (refer,_) -> dependencies_from_assignable (Reference refer) acc
      | Null -> acc
    )
    | Value v -> ( match v with
      | Binary_op (_, expr1, expr2) -> dependencies_from_assignable expr1 (dependencies_from_assignable expr2 acc)
      | Unary_op (_, expr1) -> dependencies_from_assignable expr1 acc
      | ArraySize (refer) -> dependencies_from_assignable (Reference refer) acc
      | Bool _ -> acc
      | Int _ -> acc
      | Lookup (refer) -> dependencies_from_assignable (Reference refer) acc
      | NewArray (_,expr1) -> dependencies_from_assignable expr1 acc
      | NewStruct (_,exprs) -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
    )
  in
  let rec dependencies_from_declaration dec =
    match dec with
    | TypeDeclaration _ -> []
    | AssignDeclaration (_,_,_,expr) -> dependencies_from_assignable expr []
    | VarDeclaration (_,_,expr) -> dependencies_from_assignable expr []
  in
  List.map (fun (name,cnt,lock,ty,dec) -> ((name,cnt,lock,ty,dec), dependencies_from_declaration dec)) gvs

let extract_name t =
  match t with
  | (f,_,_,_,_) -> f

(*    Compute an ordering of the global variables, according to their dependancies    *)
let order_dep_globvars dep_gvs =
  let rec aux dep_globvars count prev_count remain acc =
    match dep_globvars with
    | [] when List.length remain = 0 -> acc
    | [] when count = prev_count -> compile_error "Cannot resolve an ordering of the global variables"
    | [] -> aux remain count count [] acc
    | h::t -> ( match h with
      | ((name,cnt,lock,ty,dec), deps) -> (
        if List.for_all (fun dep -> List.exists (fun a -> dep = extract_name a) acc) deps then aux t (count+1) prev_count remain ((name,count,lock,ty,dec)::acc)
        else aux t count prev_count (h::remain) acc
      )
    )
  in
  List.rev (aux dep_gvs 0 0 [] [])

let compile topdecs =
  let globvars = (order_dep_globvars (get_globvar_dependencies (get_globvars topdecs))) in
  let routines = get_routines topdecs in
  let structs = get_structs topdecs in
  let globvar_inst = compile_sod_list (List.map (fun (_,_,_,_,dec) -> Declaration dec) globvars) globvars [] structs [] None None 0 in
  let rec aux tds acc =
    match tds with
    | [] -> acc
    | h::t -> match h with
      | Routine (accmod, n, params, stmt) -> 
        aux t ((routine_head accmod n params)::(compile_stmt stmt globvars (List.rev params) structs routines None None 0) @ [CStop] @ acc)
      | _ -> aux t acc
  in
  match topdecs with
  | Topdecs tds -> Program([], ProgramRep.translate(globvar_inst @ (ToStart :: (aux tds []))))