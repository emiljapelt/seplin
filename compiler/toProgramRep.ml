open Absyn
open ProgramRep
open Exceptions

(*** Types ***)
type variable_environment = { 
  locals: (bool * typ * string) list; 
  globals: (string * int * bool * typ * declaration) list; 
  structs: (string * char list * (bool * typ * string) list) list; 
}

type environment = { 
  var_env: variable_environment;
  routine_env: (string * char list * (bool * typ * string) list) list; 
}

type label_generator = { mutable next : int }



(*** Helper functions ***)
(* Lookup *)
let lookup_routine (name: string) routines =
  let rec aux li =
    match li with
    | [] -> None
    | (n,tvs,ps)::t -> if n = name then Some(tvs,ps) else aux t
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
    | (n,tvs,ps)::t -> if n = name then Some(tvs,ps) else aux t
  in
  aux structs

let struct_field field params =
  let rec aux ps c =
    match ps with
    | [] -> raise_error ("No such field, " ^ field)
    | (l,ty,n)::t -> if n = field then (l,ty,c) else aux t (c+1)
  in
  aux params 0

let var_locked (name: string) var_env = 
  match lookup_localvar name var_env.locals with
    | Some (_,_,ll) -> ll
    | None -> 
      match lookup_globvar name var_env.globals with
      | Some (_,_,gl) -> gl
      | None -> raise_error ("No such variable " ^ name)

let var_type (name: string) var_env = 
  match lookup_localvar name var_env.locals with
  | Some (_,lty,_) -> lty
  | None -> 
    match lookup_globvar name var_env.globals with
    | Some (_,gty,_) -> gty
    | None -> raise_error ("No such variable " ^ name)

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
  | CStop::acc1 -> acc
  | CHalt::acc1 -> CStop :: acc1
  | _ -> CStop :: acc

let addHalt acc =
  match acc with
  | CHalt::acc1 -> acc
  | CStop::acc1 -> CHalt :: acc1
  | _ -> CHalt :: acc

(* Scanning *)
let count_decl stmt_dec_list =
  let rec aux sdl c =
    match sdl with
    | [] -> c
    | (Declaration (dec, _))::t -> (
      match dec with
      | TypeDeclaration _ -> aux t (c+1)
      | AssignDeclaration _ -> aux t (c+1)
      | VarDeclaration _ -> aux t (c+1)
    )
    | _::t -> aux t (c)
  in
  aux stmt_dec_list 0

(*    list of: string * int * bool * typ * declaration    *)
let get_globvars (tds : topdecs) = 
  let rec aux topdecs acc count =
    match topdecs with
    | [] -> acc
    | (GlobalDeclaration dec)::t -> ( match dec with
      | TypeDeclaration (lock,ty,name) -> ( 
        if globvar_exists name acc then raise_error ("Duplicate global variable name: " ^ name)
        else aux t ((name, count, lock, ty, dec)::acc) (count+1)
      )
      | AssignDeclaration (lock,ty,name,expr) -> ( 
        if globvar_exists name acc then raise_error ("Duplicate global variable name: " ^ name)
        else aux t ((name, count, lock, ty, dec)::acc) (count+1)
      )
      | VarDeclaration (lock,name,expr) -> raise_error "var is not supported for global variables"
    )
    | _::t -> aux t acc count
  in match tds with 
  | Topdecs l -> aux l [] 0

(*    list of: string * access_mod * char list * (bool * typ * string) list * statement    *)
let get_routines (tds : topdecs) =
  let rec aux topdecs acc =
    match topdecs with
    | [] -> acc
    | h::t -> (
      match h with
      | Routine (accmod, name, typ_vars, params, stmt) -> (
        if routine_exists name acc then raise_error ("Duplicate routine name: " ^ name)
        else aux t ((name, typ_vars, params)::acc)
        )
      | _ -> aux t acc
    )
  in match tds with
  | Topdecs l -> aux l []

(*    list of: string * char list * (bool * typ * string) list   *)
let get_structs (tds : topdecs) =
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
  in match tds with
  | Topdecs l -> aux l []



(* Typing *)
let rec type_string t =
  match t with
  | T_Bool -> "bool"
  | T_Int -> "int"
  | T_Char -> "char"
  | T_Array arr_ty -> (type_string arr_ty)^"[]"
  | T_Struct(n,typ_args) -> n ^ "<" ^ (String.concat ","  (List.map (fun e -> (type_string e)) typ_args)) ^ ">"
  | T_Null -> "null"
  | T_Generic c -> String.make 1 c

let rec type_equal type1 type2 =
  let rec aux t1 t2 =
    match (t1, t2) with
    | (T_Int, T_Int) -> true
    | (T_Bool, T_Bool) -> true
    | (T_Char, T_Char) -> true
    | (T_Array at1, T_Array at2) -> type_equal at1 at2
    | (T_Struct(n1,ta1), T_Struct(n2, ta2)) when n1 = n2 && (List.length ta1) = (List.length ta2) -> true && (List.fold_right (fun e acc -> (type_equal (fst e) (snd e)) && acc) (List.combine ta1 ta2) true)
    | (T_Null, _) -> true
    | (T_Generic c1, T_Generic c2) -> c1 = c2
    | _ -> false
  in aux type1 type2 || aux type2 type1

let type_array_literal lst =
  let rec aux_type ty lo li =
    match li with
    | [] -> (lo,ty)
    | (lock,h)::t -> (
      if not (type_equal h ty) then raise_error "Array literal containing expressions of differing types"
      else if lock then aux_type ty true t
      else aux_type ty lo t
    )
  in
  match lst with 
  | [] -> (false, T_Null)
  | (lock,h)::t -> aux_type h lock t

let default_value t =
  match t with
  | T_Int -> Value (Int 0)
  | T_Bool -> Value (Bool false)
  | T_Char -> Value (Char '0')
  | _ -> Reference (Null)

let simple_type t =
  match t with
    | T_Int | T_Bool | T_Char -> true
    | _ -> false

let resolve_generic c typ_vars typ_args = 
  let rec aux lst = 
    match lst with
    | [] -> failwith "Could not resolve generic index"
    | (v,a)::t -> if v = c then a else aux t
  in
  aux (List.combine typ_vars typ_args)

let rec replace_generics lst typ_vars typ_args structs = 
  let rec replace element = 
    match element with
    | T_Generic(c) -> resolve_generic c typ_vars typ_args
    | T_Array(sub) -> T_Array(replace sub)
    | T_Struct(str_name, ta) -> T_Struct(str_name, List.map (fun e -> replace e) ta)
    | e -> e
  in
  let aux element =
    match element with
    | (lock, ty, name) -> (lock, replace ty, name)
  in
  List.map (fun e -> aux e) lst
  

let rec type_assignable_expr expr var_env =
  match expr with
  | Reference ref_expr -> type_reference ref_expr var_env
  | Value val_expr -> type_value val_expr var_env

and type_reference ref_expr var_env =
  match ref_expr with
  | VarRef name -> (var_locked name var_env, var_type name var_env)
  | StructRef (refer, field) -> (
    let (lock, ty) =  type_reference refer var_env in
    match ty with 
    | T_Struct (str_name, typ_args) -> (match lookup_struct str_name var_env.structs with
      | Some (typ_vars, fields) -> (
        let resolved_fields = replace_generics fields typ_vars typ_args var_env.structs in
        let (field_lock, field_ty,_) = struct_field field resolved_fields in
        match field_ty with
        | T_Generic c -> (lock || field_lock, resolve_generic c typ_vars typ_args)
        | _ -> (lock || field_lock, field_ty)
      )
      | None -> raise_error ("No such struct: " ^ str_name)
    )
    | _ -> raise_error ("Field access on non-struct variable")
  )
  | ArrayRef (refer, expr) -> (
    let (lock, ty) =  type_reference refer var_env in
    match ty with 
    | T_Array array_typ -> (lock, array_typ)
    | _ -> raise_error ("Array access on non-array variable")
  )
  | Null -> (false, T_Null)

and type_value val_expr var_env =
  match val_expr with
  | Binary_op (op, expr1, expr2) -> (
    let (lock1, ty1) = type_assignable_expr expr1 var_env in
    let (lock2, ty2) = type_assignable_expr expr2 var_env in
    match (op, ty1, ty2) with
    | ("&&", T_Bool, T_Bool) ->  (false, T_Bool)
    | ("||", T_Bool, T_Bool) -> (false, T_Bool)
    | ("=", _, T_Null) -> (false, T_Bool)
    | ("=", T_Null, _) -> (false, T_Bool)
    | ("=", T_Bool, T_Bool) -> (false, T_Bool)
    | ("=", T_Char, T_Char) -> (false, T_Bool)
    | ("=", T_Int, T_Int) -> (false, T_Bool)
    | ("!=", _, T_Null) -> (false, T_Bool)
    | ("!=", T_Null, _) -> (false, T_Bool)
    | ("!=", T_Bool, T_Bool) -> (false, T_Bool)
    | ("!=", T_Char, T_Char) -> (false, T_Bool)
    | ("!=", T_Int, T_Int) -> (false, T_Bool)
    | ("<=", T_Int, T_Int) -> (false, T_Bool) 
    | ("<", T_Int, T_Int) -> (false, T_Bool)
    | (">=", T_Int, T_Int) -> (false, T_Bool)
    | (">", T_Int, T_Int) -> (false, T_Bool)
    | ("+", T_Int, T_Int) -> (false, T_Int)
    | ("-", T_Int, T_Int) -> (false, T_Int)
    | ("*", T_Int, T_Int) -> (false, T_Int)
    | _ -> raise_error "Unknown binary operator, or type mismatch"
  )
  | Unary_op (op, expr) -> (
    let (lock, ty) = type_assignable_expr expr var_env in
    match (op, ty) with
    | ("!", T_Bool) -> (false, T_Bool)
    | _ -> raise_error "Unknown unary operator, or type mismatch"
  )
  | ArraySize (refer) ->  (
    let (lock, ty) = type_reference refer var_env in
    match ty with
    | T_Array _ -> (false, T_Int)
    | _ -> raise_error "Array size of non-array variable"
  )
  | GetInput ty -> (false, ty)
  | Bool _ -> (false, T_Bool)
  | Int _ -> (false, T_Int)
  | Char _ -> (false, T_Char)
  | Lookup (refer) -> type_reference refer var_env
  | NewArray (ty, _) -> (false, T_Array(ty))
  | ArrayLiteral elements -> (match type_array_literal (List.map (fun e -> type_assignable_expr e var_env) elements) with (lo,ety) -> (lo, T_Array(ety)))
  | NewStruct (name, typ_args, _) -> (false, T_Struct(name, typ_args))



(* Labels *)
let lg = ( {next = 0;} )

let new_label () =
  let number = lg.next in
  let () = lg.next <- lg.next+1 in
  Int.to_string number



(*** Global variable handling ***)
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
      | Char _ -> acc
      | GetInput _ -> acc
      | Lookup (refer) -> dependencies_from_assignable (Reference refer) acc
      | NewArray (_,expr1) -> dependencies_from_assignable expr1 acc
      | ArrayLiteral exprs -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
      | NewStruct (_,_,exprs) -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
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
    | [] when count = prev_count -> raise_error "Cannot resolve an ordering of the global variables"
    | [] -> aux remain count count [] acc
    | h::t -> ( match h with
      | ((name,cnt,lock,ty,dec), deps) -> (
        if List.for_all (fun dep -> List.exists (fun a -> dep = extract_name a) acc) deps then aux t (count+1) prev_count remain ((name,count,lock,ty,dec)::acc)
        else aux t count prev_count (h::remain) acc
      )
    )
  in
  List.rev (aux dep_gvs 0 0 [] [])

let gather_globvar_info gvs =
  List.map (fun (name,count,lock,ty,dec) -> (lock, ty, name)) gvs


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
  | Lookup _ -> Value(expr)
  | NewArray _ -> Value(expr)
  | ArrayLiteral _ -> Value(expr)
  | NewStruct (_,_,_) -> Value(expr)


(*** Compiling functions ***)
let routine_head accmod name params =
  match accmod with
  | Internal -> CLabel(name)
  | External -> CEntryPoint(name, List.map (fun (lock,ty,_) -> (lock,ty)) params)

let fetch_var_index (name: string) globvars localvars = 
  match lookup_localvar name localvars with
  | Some (lc,_,_) -> BPFetch(lc)
  | None -> 
    match lookup_globvar name globvars with
    | Some (gc,_,_) -> StackFetch(gc)
    | None -> raise_error ("No such variable " ^ name)

let rec compile_assignable_expr expr var_env acc =
  match expr with
  | Reference ref_expr -> compile_reference ref_expr var_env acc
  | Value val_expr -> compile_value val_expr var_env acc

and compile_reference ref_expr var_env acc =
  match ref_expr with
  | VarRef name -> (fetch_var_index name var_env.globals var_env.locals) :: RefFetch :: acc
  | StructRef (refer, field) -> ( 
    let (ref_lock, ref_ty) = type_reference refer var_env in
    match ref_ty with
    | T_Struct (name, _) -> (
      match lookup_struct name var_env.structs with
      | Some (_, params) -> (
        let (_, _, index) = struct_field field params in
        compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: acc)
      )
      | None -> raise_error ("No such struct: " ^ name)
    )
    | _ -> raise_error ("Struct field lookup type failure")
  )
  | ArrayRef (refer, index) -> (
    let (_, ref_ty) = type_reference refer var_env in
    let (_, idx_ty) = type_assignable_expr index var_env in
    match (ref_ty, idx_ty) with
    | (T_Array (_), T_Int) -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: acc)))
    | _ -> raise_error ("Array lookup type failure")
  )
  | Null -> PlaceInt(0) :: acc

and compile_assignable_expr_as_value expr var_env acc =
  match expr with
  | Reference r -> (
    let (_, ref_ty) = type_reference r var_env in
    match ref_ty with
    | T_Int -> compile_reference r var_env (FetchFull :: FetchFull :: acc)
    | T_Bool -> compile_reference r var_env (FetchFull :: FetchByte :: acc)
    | T_Char -> compile_reference r var_env (FetchFull :: FetchByte :: acc)
    | T_Array _ -> compile_reference r var_env (FetchFull :: FetchFull :: acc)
    | T_Struct _ -> compile_reference r var_env (FetchFull :: FetchFull :: acc)
    | T_Null -> compile_reference r var_env acc
    | T_Generic _ -> raise_error "Generic variables not yet supported, 1"
  )
  | _ -> compile_assignable_expr expr var_env acc

and compile_value val_expr var_env acc =
  match val_expr with
  | Bool b -> PlaceBool(b) :: acc
  | Int i -> PlaceInt(i) :: acc
  | Char c -> PlaceChar(c) :: acc
  | ArraySize refer -> (
    let (_, ref_ty) = type_reference refer var_env in
    match ref_ty with
    | T_Array _ -> compile_reference refer var_env  (FetchFull :: SizeOf :: acc)
    | _ -> raise_error "Array size only makes sense for arrays"
  )
  | GetInput ty -> ( match type_index ty with
    | -1 -> raise_error "Unsupported GetInput variant"
    | x -> GetInput(x) :: acc
  )
  | Lookup refer -> (
    let (_, ref_ty) = type_reference refer var_env in
    match ref_ty with
    | T_Int -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Bool -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Char -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Array ty -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Struct(n, _) -> compile_assignable_expr_as_value (Reference refer) var_env acc
    | T_Null -> raise_error ("Direct null pointer dereferencing")
    | T_Generic _ -> raise_error "Cannot make a value lookup on a generic variable"
  )
  | NewArray (arr_ty, size_expr) -> (
    let (_, s_ty) = type_assignable_expr size_expr var_env in
    match s_ty with
    | T_Int -> compile_assignable_expr_as_value (optimize_assignable_expr size_expr var_env) var_env (DeclareStruct :: IncrRef :: acc)
    | _ -> raise_error ("Init array with non-int size")
  )
  | ArrayLiteral elements -> (
    let (lo,ty) = type_array_literal (List.map (fun e -> type_assignable_expr e var_env) elements) in
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
        let (ha_lock, ha_ty) = type_assignable_expr ha var_env in
        if not (type_equal field_ty ha_ty) then raise_error ("Struct argument type mismatch")
        else if ha_lock && (not field_lock) then raise_error "Cannot give a locked variable as a parameter that is not locked"
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
      | (_,_) -> raise_error ("Struct argument count mismatch")
    in
    match lookup_struct name var_env.structs with
    | Some (typ_vars, params) -> (
      if (List.length typ_vars > 0) then ( (* Generic *)
        if (List.length typ_args = 0) then (
          raise_error "Generic inference is not supported yet"
        )
        else if (List.length typ_args) = (List.length typ_vars) then (
          PlaceInt(List.length params) :: DeclareStruct :: (aux (List.rev args) (List.rev (replace_generics params typ_vars typ_args var_env.structs)) ((List.length params)-1) acc)
        )
        else raise_error ("Amount of type arguments does not match required amount") 
      )
      else ( (* Not generic *)
        PlaceInt(List.length params) :: DeclareStruct :: (aux (List.rev args) (List.rev params) ((List.length params)-1) acc)
      ) 
    )
    | None -> raise_error ("No such struct: " ^ name)
  )
  | Binary_op (op, e1, e2) -> (
      let (_, t1) = type_assignable_expr e1 var_env in
      let (_, t2) = type_assignable_expr e2 var_env in
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
    let (_, t) = type_assignable_expr e var_env in
    match (op, t, e) with
    | ("!", T_Bool, _) -> compile_assignable_expr_as_value e var_env (BoolNot :: acc)
    | _ -> raise_error "Unknown unary operator, or type mismatch"
  )

let compile_arguments args var_env acc =
  let rec aux ars acc =
    match ars with
    | ([]) -> acc
    | (((plock, pty, pname),eh)::t) -> (
        let (expr_lock, expr_ty) = type_assignable_expr eh var_env in
        if not (type_equal pty expr_ty) then raise_error ("Type mismatch on call: expected " ^ (type_string pty) ^ ", got " ^ (type_string expr_ty)) 
        else if expr_lock && (not plock) then raise_error "Cannot give a locked variable as a parameter that is not locked"
        else let opteh = optimize_assignable_expr eh var_env in
        match opteh with
        | Value _ -> ( match expr_ty with
          | T_Int -> aux t (DeclareFull :: IncrRef :: CloneFull :: (compile_assignable_expr_as_value opteh var_env (AssignFull :: acc)))
          | T_Bool -> aux t (DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr_as_value opteh var_env (AssignByte :: acc)))
          | T_Char -> aux t (DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr_as_value opteh var_env (AssignByte :: acc)))
          | T_Array _ -> aux t (compile_assignable_expr_as_value opteh var_env (IncrRef :: acc))
          | T_Struct _ -> aux t (compile_assignable_expr_as_value opteh var_env (IncrRef :: acc))
          | T_Null -> aux t (compile_assignable_expr_as_value opteh var_env (acc))
          | T_Generic _ -> raise_error "Generic variables not yet supported, in call arguments"
        )
        | Reference r -> ( match r with
          | VarRef _ -> aux t (compile_reference r var_env (IncrRef :: acc)) 
          | StructRef _ -> aux t (compile_reference r var_env (FetchFull :: IncrRef :: acc)) 
          | ArrayRef _ -> aux t (compile_reference r var_env (FetchFull :: IncrRef :: acc)) 
          | Null -> aux t (compile_reference r var_env acc) 
        )
      )
  in
  aux (List.rev args) acc

let rec compile_assignment target assign var_env acc =
  let (target_lock, target_type) = type_reference target var_env in
  let (assign_lock, assign_type) = type_assignable_expr assign var_env in 
  if target_lock then raise_error "Assignment to a locked variable"
  else if assign_lock then raise_error "Cannot assign a locked variable, to another variable"
  else if not (type_equal target_type assign_type) then raise_error ("Type mismatch in assignment, expected '"^(type_string target_type)^"' but got '" ^(type_string assign_type)^ "'")
  else match (target, assign) with
  | (Null, _) -> raise_error "Assignment to null"
  | (VarRef name, Value v) -> ( match assign_type with 
    | T_Int ->  compile_reference target var_env (FetchFull :: (compile_value v var_env (AssignFull :: acc)))
    | T_Bool -> compile_reference target var_env (FetchFull :: (compile_value v var_env (AssignByte :: acc)))
    | T_Char -> compile_reference target var_env (FetchFull :: (compile_value v var_env (AssignByte :: acc)))
    | T_Array _ -> compile_reference target var_env (compile_value v var_env (IncrRef :: RefAssign :: acc))
    | T_Struct _ -> compile_reference target var_env (compile_value v var_env (IncrRef :: RefAssign :: acc))
    | T_Null -> compile_reference target var_env (compile_value v var_env (IncrRef :: RefAssign :: acc))
    | T_Generic _ -> raise_error "Generic variables not yet supported, 2"
  )
  | (VarRef name, Reference re) -> ( match assign_type with 
    | T_Int -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Bool -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Char -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Array _ -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Struct _ -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
    | T_Null -> compile_reference target var_env (compile_reference re var_env (RefAssign :: acc))
    | T_Generic _ -> compile_reference target var_env (compile_reference re var_env (FetchFull :: IncrRef :: RefAssign :: acc))
  )
  | (StructRef(refer, field), Value v) -> ( match type_reference refer var_env with
    | (_,T_Struct (str_name, typ_args)) -> ( match lookup_struct str_name var_env.structs with
      | None -> raise_error ("Could not find struct: " ^ str_name)
      | Some (typ_vars, fields) -> ( match struct_field field fields with
        | (field_lock,field_ty,index) -> ( match assign_type with
          | T_Int -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: FetchFull :: (compile_value v var_env (AssignFull :: acc)))
          | T_Bool -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))
          | T_Char -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))
          | T_Array _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_value v var_env (IncrRef :: FieldAssign :: acc)))
          | T_Struct _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_value v var_env (IncrRef :: FieldAssign :: acc)))
          | T_Null  -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_value v var_env (FieldAssign :: acc)))
          | T_Generic _ -> raise_error "Assignment to unresolved generic field"
        )
      )
    )
    | (_,_) -> raise_error "Struct assignment to non-struct" 
  )
  | (StructRef(refer, field), Reference re) -> ( match type_reference refer var_env with
    | (_,T_Struct (str_name, typ_args)) -> ( match lookup_struct str_name var_env.structs with
      | None -> raise_error ("Could not find struct: " ^ str_name)
      | Some (typ_vars, fields) -> ( match struct_field field fields with
        | (field_lock, field_ty, index) -> ( match assign_type with
          | T_Int -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Bool -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Char -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Array _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Struct _ -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc)))
          | T_Null  -> compile_reference refer var_env (FetchFull :: PlaceInt(index) :: (compile_reference re var_env (FieldAssign :: acc)))
          | T_Generic _ -> raise_error "Assignment to unresolved generic field"
        )
      )
    )
    | (_,_) -> raise_error "Struct assignment to non-struct" 
  )
  | (ArrayRef(refer, index), Value v) -> ( match type_reference refer var_env with
    | (_,T_Array arr_ty) -> ( match type_assignable_expr index var_env with
      | (_,T_Int) -> ( match assign_type with
        | T_Int -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: FetchFull :: (compile_value v var_env (AssignFull :: acc)))))
        | T_Bool -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))))
        | T_Char -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (FieldFetch :: FetchFull :: (compile_value v var_env (AssignByte :: acc)))))
        | T_Array _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_value v var_env (IncrRef :: FieldAssign :: acc))))
        | T_Struct _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_value v var_env (IncrRef :: FieldAssign :: acc))))
        | T_Null -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_value v var_env (FieldAssign :: acc))))
        | T_Generic _ -> raise_error "Declaring generic arrays is not supported yet"
      )
      | (_,_) -> raise_error "Array index must be of type 'int'"
    )
    | (_,_) -> raise_error "Array assignment to non-array" 
  )
  | (ArrayRef(refer, index), Reference re) -> ( match type_reference refer var_env with
    | (_,T_Array arr_ty) -> ( match type_assignable_expr index var_env with 
      | (_,T_Int) -> ( match assign_type with
        | T_Int -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Bool -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Char -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Array _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Struct _ -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FetchFull :: IncrRef :: FieldAssign :: acc))))
        | T_Null -> compile_reference refer var_env (FetchFull :: (compile_assignable_expr_as_value index var_env (compile_reference re var_env (FieldAssign :: acc))))
        | T_Generic _ -> raise_error "Declaring generic arrays is not supported yet"
      )
      | (_,_) -> raise_error "Array index must be of type 'int'"
    )
    | (_,_) -> raise_error "Array assignment to non-array" 
  )

let compile_unassignable_expr expr env break continue cleanup acc =
  match expr with
  | Assign (target, aexpr) -> compile_assignment target (optimize_assignable_expr aexpr env.var_env) env.var_env acc
  | Call (n, typ_args, aexprs) -> (
    match lookup_routine n env.routine_env with
    | None -> raise_error ("Call to undefined routine: " ^ n)
    | Some (tvs,ps) -> (
      if List.length tvs != List.length typ_args then raise_error (n ^ "(...) requires " ^ (Int.to_string (List.length tvs)) ^ " type arguments, but was given " ^  (Int.to_string (List.length typ_args)))
      else if List.length ps != List.length aexprs then raise_error (n ^ "(...) requires " ^ (Int.to_string (List.length ps)) ^ " arguments, but was given " ^  (Int.to_string (List.length aexprs)))
      else (compile_arguments (List.combine (replace_generics ps tvs typ_args env.var_env.structs) aexprs) env.var_env (PlaceInt(List.length ps) :: Call(n) :: acc))
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
        let (_, expr_ty) = type_assignable_expr h env.var_env in
        let opte = optimize_assignable_expr h env.var_env in
        match expr_ty with
        | T_Bool -> aux t (compile_assignable_expr_as_value opte env.var_env (PrintBool :: acc))
        | T_Int -> aux t (compile_assignable_expr_as_value opte env.var_env (PrintInt :: acc))
        | T_Char -> aux t (compile_assignable_expr_as_value opte env.var_env (PrintChar :: acc))
        | _ -> compile_assignable_expr_as_value opte env.var_env acc ; aux t (PlaceBool(false) :: PrintBool :: acc) (* This is not as intended! *)
      )
    in
    aux (List.rev exprs) acc
  )

let rec compile_declaration dec var_env acc =
  match dec with
  | TypeDeclaration (l, ty, n) -> (
    if localvar_exists n var_env.locals then raise_error ("Duplicate variable name: " ^ n)
    else match ty with
    | T_Int -> DeclareFull :: IncrRef :: CloneFull :: PlaceInt(0) :: AssignFull :: acc
    | T_Bool -> DeclareByte :: IncrRef :: CloneFull :: PlaceBool(false) :: AssignByte :: acc
    | T_Char -> DeclareByte :: IncrRef :: CloneFull :: PlaceChar('0') :: AssignByte :: acc
    | T_Array _ -> PlaceInt(0) :: acc
    | T_Struct _ -> PlaceInt(0) :: acc
    | T_Null -> raise_error "Cannot declare the 'null' type"
    | T_Generic _ -> raise_error "Declaring variables of generic type is not supported yet"
  )
  | AssignDeclaration (l, ty, n, expr) -> (
    if localvar_exists n var_env.locals then raise_error ("Duplicate variable name: " ^ n)
    else let (expr_lock, expr_ty) = type_assignable_expr expr var_env in 
    if expr_lock && (not l) then raise_error "Cannot assign a locked variable to a non-locked variable"
    else if not (type_equal ty expr_ty) then raise_error ("Type mismatch on declaration: expected '" ^ (type_string ty) ^ "', got '" ^ (type_string expr_ty) ^ "'") 
    else let opte = optimize_assignable_expr expr var_env in
    match expr with
    | Reference _ -> compile_assignable_expr opte var_env (IncrRef :: acc)
    | Value _ -> (
      match ty with
      | T_Int -> DeclareFull :: IncrRef :: CloneFull :: (compile_assignable_expr opte var_env (AssignFull :: acc))
      | T_Bool -> DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr opte var_env (AssignByte :: acc))
      | T_Char -> DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr opte var_env (AssignByte :: acc))
      | T_Array _ -> compile_assignable_expr opte var_env (IncrRef :: acc)
      | T_Struct _ -> compile_assignable_expr opte var_env (IncrRef :: acc)
      | T_Null -> compile_assignable_expr opte var_env acc
      | T_Generic _ -> raise_error "Declaring variables of generic type is not supported yet"
    )
  )
  | VarDeclaration (l, n, expr) -> (
    if localvar_exists n var_env.locals then raise_error ("Duplicate variable name: " ^ n)
    else let (expr_lock, expr_ty) = type_assignable_expr expr var_env in
    if expr_lock && (not l) then raise_error "Cannot assign a locked variable to a non-locked variable"
    else let opte = optimize_assignable_expr expr var_env in
    match expr with
    | Reference _ -> compile_assignable_expr opte var_env (IncrRef :: acc)
    | Value _ -> (
      match expr_ty with
      | T_Int -> DeclareFull :: IncrRef :: CloneFull :: (compile_assignable_expr opte var_env (AssignFull :: acc))
      | T_Bool -> DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr opte var_env (AssignByte :: acc))
      | T_Char -> DeclareByte :: IncrRef :: CloneFull :: (compile_assignable_expr opte var_env (AssignByte :: acc))
      | T_Array _ -> compile_assignable_expr opte var_env (IncrRef :: acc)
      | T_Struct _ -> compile_assignable_expr opte var_env (IncrRef :: acc)
      | T_Null -> raise_error "Cannot infere a type from 'null'"
      | T_Generic _ -> raise_error "Declaring variables of generic type is not supported yet"
    )
  )

let update_locals env dec =
  match dec with
  | TypeDeclaration (lock, ty, name) -> ({ env with var_env = ({ env.var_env with locals = (lock, ty, name)::env.var_env.locals }) })
  | AssignDeclaration (lock, ty, name, _) -> ({ env with var_env = ({ env.var_env with locals = (lock, ty, name)::env.var_env.locals }) })
  | VarDeclaration (lock, name, expr) -> (
    let (lo, ty) = type_assignable_expr expr env.var_env in
    ({ env with var_env = ({ env.var_env with locals = (lock, ty, name)::env.var_env.locals }) })
  )

let rec compile_sod_list sod_list env break continue cleanup acc =
  match sod_list with
  | [] -> acc
  | h::t -> (
    match h with
    | Statement (stmt, offset) -> (try (
      compile_stmt stmt env break continue cleanup (compile_sod_list t env break continue cleanup (acc))
    ) with
      | Error msg -> raise_offset_error msg offset
      (* | Offset_error (msg, offset) -> raise_offset_error msg offset *)
      | e -> raise e
    )
    | Declaration (dec, offset) -> (try (
      compile_declaration dec env.var_env (compile_sod_list t (update_locals env dec) break continue (cleanup+1) acc)
    ) with
      | Error msg -> raise_offset_error msg offset
      | Offset_error (msg, offset) -> raise_offset_error msg offset
      | e -> raise e
    )
  )

and compile_stmt stmt env break continue cleanup acc =
  match stmt with
  | If (expr, s1, s2) -> (
    let label_true = new_label () in
    let label_stop = new_label () in
    let (_, t) = type_assignable_expr expr env.var_env in
    if t != T_Bool then raise_error "Condition not of type 'bool'"
    else compile_assignable_expr_as_value expr env.var_env (IfTrue(label_true) :: (compile_stmt s2 env break continue cleanup (GoTo(label_stop) :: CLabel(label_true) :: (compile_stmt s1 env break continue cleanup (CLabel(label_stop) :: acc)))))
  )
  | While (expr, s) -> (
    let label_cond = new_label () in
    let label_start = new_label () in
    let label_stop = new_label () in
    let (_, t) = type_assignable_expr expr env.var_env in
    if t != T_Bool then raise_error "Condition not of type 'bool'"
    else GoTo(label_cond) :: CLabel(label_start) :: (compile_stmt s env (Some label_stop) (Some label_cond) 0 (CLabel(label_cond) :: (compile_assignable_expr_as_value expr env.var_env (IfTrue(label_start) :: CLabel(label_stop) :: acc))))
  )
  | Block (sod_list) -> (
    let decs = count_decl sod_list in
    if decs = 0 then compile_sod_list sod_list env break continue cleanup acc
    else compile_sod_list sod_list env break continue cleanup (addFreeVars decs acc)
  )
  | Expression (expr) -> compile_unassignable_expr expr env break continue cleanup acc

let compile_globalvars globvars structs acc =
  compile_sod_list (List.map (fun (_,_,_,_,dec) -> Declaration (dec, 0)) globvars) ({ var_env = ({ locals = []; globals = globvars; structs = structs; }); routine_env = []; }) None None 0 acc

let compile topdecs =
  let globvars = (order_dep_globvars (get_globvar_dependencies (get_globvars topdecs))) in
  let routines = get_routines topdecs in
  let structs = get_structs topdecs in
  let rec aux tds acc =
    match tds with
    | [] -> acc
    | h::t -> match h with
      | Routine (accmod, n, typ_vars, params, stmt) -> 
        aux t ((routine_head accmod n params)::(compile_stmt stmt ({ var_env = ({ locals = (List.rev params); globals = globvars; structs = structs;}); routine_env = routines; }) None None 0 (addStop(acc))))
      | _ -> aux t acc
  in
  match topdecs with
  | Topdecs tds -> Program(structs, (gather_globvar_info globvars), ProgramRep.translate(compile_globalvars globvars structs (ToStart :: (aux tds []))))