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
  | VarRef name -> (var_locked name var_env, var_type name var_env)
  | StructRef (refer, field) -> (
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
  | ValueOf (refer) -> type_reference refer var_env
  | NewArray (ty, _) -> (false, T_Array(ty))
  | ArrayLiteral elements -> (match type_array_literal (List.map (fun e -> type_assignable_expr e var_env) elements) with (lo,ety) -> (lo, T_Array(ety)))
  | NewStruct (name, typ_args, args) -> ( match lookup_struct name var_env.structs with
    | Some (typ_vars, params) -> (
      if (List.length typ_vars > 0) then ( (* Generic *)
        if (List.length typ_args = 0) then (
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
      | _ -> raise_error "Parameter/Argument structure mismatch in generic inference"
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