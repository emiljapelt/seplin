open ProgramRep
open Exceptions
open Absyn
open Helpers

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
  
let rec type_assignable_expr expr var_env =
  match expr with
  | Reference ref_expr -> type_reference ref_expr var_env
  | Value val_expr -> type_value val_expr var_env

and type_reference ref_expr var_env =
  match ref_expr with
  | VariableAccess name -> (var_locked name var_env, var_type name var_env)
  | StructAccess (refer, field) -> (
    let (lock, ty) =  type_reference refer var_env in
    match ty with 
    | T_Struct (str_name, typ_args) -> (match lookup_struct str_name var_env.structs with
      | Some (typ_vars, fields) -> (
        let resolved_fields = replace_generics fields typ_vars typ_args var_env.structs in
        let (field_lock, field_ty,_) = struct_field field resolved_fields in
        (lock || field_lock, field_ty)
      )
      | None -> raise_error ("No such struct: " ^ str_name)
    )
    | _ -> raise_error ("Field access on non-struct variable")
  )
  | ArrayAccess (refer, expr) -> (
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
  | ValueOf (refer) -> type_reference refer var_env
  | NewArray (ty, _) -> (false, T_Array(ty))
  | ArrayLiteral elements -> (match type_array_literal (List.map (fun e -> type_assignable_expr e var_env) elements) with (lo,ety) -> (lo, T_Array(ety)))
  | NewStruct (name, typ_args, args) -> ( match lookup_struct name var_env.structs with
    | Some (typ_vars, params) -> (
      if (List.length typ_vars > 0) then ( (* Generic *)
        if (typ_args = []) then (
          let typ_args = infere_generics typ_vars params args var_env in
          (false, T_Struct(name, typ_args))
        )
        else if (List.length typ_args) = (List.length typ_vars) then (
          (false, T_Struct(name, typ_args))
        )
        else raise_error ("Amount of type arguments does not match required amount") 
      )
      else (false, T_Struct(name, typ_args)) (* Not generic *)
    )
    | None -> raise_error ("No such struct: " ^ name)
  )
  | StructLiteral _ -> raise_error "Cannot infere a type from a struct literal"

  and resolve_generic c typ_vars typ_args = 
    let rec aux lst = 
      match lst with
      | [] -> failwith "Could not resolve generic index"
      | (v,a)::t -> if v = c then a else aux t
    in
    aux (List.combine typ_vars typ_args)
  
  and replace_generics lst typ_vars typ_args structs = 
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
    
  and infere_generic c param_tys arg_tys =
    let rec aux pt et =
      match (pt, et ) with
      | (_, T_Null) -> None
      | (T_Generic g, _) -> if g = c then Some(et) else None
      | (T_Array(sub_t), T_Array(sub_et)) -> aux sub_t sub_et
      | (T_Struct(name_t, param1), T_Struct(name_et, param2)) when name_t = name_et -> infere_generic c param1 param2
      | _ -> None (* raise_error "Parameter/Argument structure mismatch in generic inference" *)
    in match (param_tys, arg_tys) with
    | (param_t::tp, arg_t::ta) -> ( match aux param_t arg_t with
      | None -> infere_generic c tp ta
      | Some(t) -> Some(t)
    )
    | _ -> None
  
  and infere_generics typ_vars params args var_env =
    let param_tys = List.map (fun p -> match p with (_,t,_) -> t ) params in
    let arg_tys = List.map (fun a -> match type_assignable_expr a var_env with (_,t) -> t) args in
    List.map (fun tv -> (
      match infere_generic tv param_tys arg_tys with
      | Some(t) -> t
      | None -> raise_error "Could not infere a type for all type variables"
    )) typ_vars

let elements_unique lst =
  let rec aux l seen =
    match l with
    | [] -> true
    | h::t -> if List.mem h seen then false else aux t (h::seen)
  in
  aux lst []

let parameters_check typ_vars structs params =
  let rec check p_ty =
    match p_ty with
    | T_Int
    | T_Bool
    | T_Char -> true
    | T_Null -> false
    | T_Array(sub_ty) -> check sub_ty
    | T_Generic(c) -> if List.mem c typ_vars then true else false
    | T_Struct(name, field_typs) -> if Helpers.struct_exists name structs then List.fold_right (fun field_ty acc -> (check field_ty) && acc) field_typs true else false
  in
  List.fold_right (fun (_,ty,_) acc -> (check ty) && acc) params true

let rec well_defined_type typ var_env =
  match typ with
  | T_Struct(name,typ_args) -> (
    match lookup_struct name var_env.structs with
    | None -> false
    | Some(typ_vars,_) -> (List.length typ_args == List.length typ_vars) && (List.fold_right (fun e acc -> (well_defined_type e var_env) && acc ) typ_args true)
  )
  | T_Array sub -> if well_defined_type sub var_env then true else false
  | T_Generic c -> if List.mem c var_env.typ_vars then true else false
  | _ -> true

let rec check_topdecs topdecs structs =
  let rec aux tds =
    match tds with
    | [] -> ()
    | Routine(_,name,typ_vars,params,_)::t -> (
      if not(elements_unique typ_vars) then raise_error ("Non-unique type variables in routine definition: " ^ name)
      else if not(parameters_check typ_vars structs params) then raise_error ("illegal parameters in routine: " ^ name)
      else aux t
    )
    | Struct(name,typ_vars,params)::t -> ( 
      if not(elements_unique typ_vars) then raise_error ("Non-unique type variables in struct definition: " ^ name)
      else if not(parameters_check typ_vars structs params) then raise_error ("illegal parameters in struct: " ^ name)
      else aux t
    )
    | h::t -> aux t
  in
  match topdecs with
  | Topdecs(tds) -> aux tds

let check_structs structs =
  let rec aux strs seen =
    match strs with
    | [] -> ()
    | (name, typ_vars, fields)::t -> if List.mem name seen then failwith ("Duplicate struct name: " ^name) else aux t (name::seen) 
  in
  aux structs []

let rec check_struct_literal struct_fields exprs var_env =
  let rec aux pairs =
    match pairs with
    | [] -> true 
    | ((lock,T_Struct(name,typ_args),_),Value(StructLiteral(literal_fields)))::t -> (
      match lookup_struct name var_env.structs with
      | None -> false
      | Some(tvs,ps) -> (
        let replaced = replace_generics ps tvs typ_args var_env.structs in
        check_struct_literal replaced literal_fields var_env
      )
    )
    | ((lock,T_Null,_),e)::t -> false
    | ((lock,typ,_),e)::t -> (
      let (expr_lock, expr_typ) = type_assignable_expr e var_env in
      if not(type_equal typ expr_typ) then false else
      if expr_lock && not(lock) then false else
      aux t
    )
  in 
  aux (List.combine struct_fields exprs)

let assignment_type_check target assign var_env =
  match assign with
  | Value(StructLiteral(exprs)) -> (
    let (target_lock, target_type) = type_reference target var_env in
    if target_lock then raise_error "Assignment to a locked variable"
    else match target_type with
    | T_Struct(name, typ_args) -> ( match lookup_struct name var_env.structs with
      | Some(typ_vars, params) -> (
        if not(check_struct_literal (replace_generics params typ_vars typ_args var_env.structs) exprs var_env) then raise_error "Structure mismatch in assignment"
        else target_type
      )
      | None -> raise_error ("No such struct: " ^ name)
    )
    | _ -> raise_error ("Struct literal assignment to a variable of type: " ^ type_string target_type)
  )
  | _ -> (
    let (target_lock, target_type) = type_reference target var_env in
    let (assign_lock, assign_type) = type_assignable_expr assign var_env in
    if target_lock then raise_error "Assignment to a locked variable"
    else if assign_lock then raise_error "Cannot assign a locked variable, to another variable"
    else if not (type_equal target_type assign_type) then raise_error ("Type mismatch in assignment, expected '"^(type_string target_type)^"' but got '" ^(type_string assign_type)^ "'")
    else assign_type
  )
  
let declaration_type_check name lock typ expr var_env = 
    if localvar_exists name var_env.locals then raise_error ("Duplicate variable name: " ^ name)
    else match expr with
    | Value(StructLiteral(exprs)) -> ( match typ with
      | Some(T_Struct(n,tas)) -> ( match lookup_struct n var_env.structs with
        | Some(tvs,ps) ->  (
          let typ = if Option.is_some typ then (if well_defined_type (Option.get typ) var_env then Option.get typ else raise_error "Not a well defined type") else raise_error "Struct literals cannot be infered to a type" in
          if not(check_struct_literal (replace_generics ps tvs tas var_env.structs) exprs var_env) then raise_error ("Could not match struct literal with: '" ^ type_string typ ^ "'")
          else typ
        )
        | None -> raise_error ("No such struct: " ^ n)
      )
      | _ -> raise_error "Struct literals must be assigned to variables with a explict struct type"
    )
    | _ -> (
      let (expr_lock, expr_ty) = type_assignable_expr expr var_env in
      if (Option.is_none typ) && (expr_ty = T_Null) then raise_error "Cannot infere a type from 'null'" else
      let typ = if Option.is_some typ then (if well_defined_type (Option.get typ) var_env then Option.get typ else raise_error "Not a well defined type") else expr_ty in
      if expr_lock && (not lock) then raise_error "Cannot use a locked variable as a non-locked variable"
      else if not (type_equal typ expr_ty) then raise_error ("Type mismatch: expected '" ^ (type_string typ) ^ "', got '" ^ (type_string expr_ty) ^ "'")
      else typ
    )

let argument_type_check name lock typ expr var_env = 
  match expr with
  | Value(StructLiteral(exprs)) -> ( match typ with
    | Some(T_Struct(n,tas)) -> ( match lookup_struct n var_env.structs with
      | Some(tvs,ps) ->  (
        let typ = if Option.is_some typ then (if well_defined_type (Option.get typ) var_env then Option.get typ else raise_error "Not a well defined type") else raise_error "Struct literals cannot be infered to a type" in
        if not(check_struct_literal (replace_generics ps tvs tas var_env.structs) exprs var_env) then raise_error ("Could not match struct literal with: '" ^ type_string typ ^ "'")
        else typ
      )
      | None -> raise_error ("No such struct: " ^ n)
    )
    | _ -> raise_error "Struct literals must be assigned to variables with a explict struct type"
  )
  | _ -> (
    let (expr_lock, expr_ty) = type_assignable_expr expr var_env in
    if (Option.is_none typ) && (expr_ty = T_Null) then raise_error "Cannot infere a type from 'null'" else
    let typ = if Option.is_some typ then (if well_defined_type (Option.get typ) var_env then Option.get typ else raise_error "Not a well defined type") else expr_ty in
    if expr_lock && (not lock) then raise_error "Cannot use a locked variable as a non-locked variable"
    else if not (type_equal typ expr_ty) then raise_error ("Type mismatch: expected '" ^ (type_string typ) ^ "', got '" ^ (type_string expr_ty) ^ "'")
    else typ
  )