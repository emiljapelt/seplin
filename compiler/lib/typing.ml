open ProgramRep
open Exceptions
open Absyn
open Helpers

let rec type_string t =
  match t with
  | T_Bool -> "bool"
  | T_Int -> "int"
  | T_Char -> "char"
  | T_Array arr_ty -> (type_string (Option.get arr_ty))^"[]"
  | T_Struct(n,typ_args) -> n ^ "<" ^ (String.concat ","  (List.map (fun e -> (type_string (Option.get e))) typ_args)) ^ ">"
  | T_Null -> "null"
  | T_Generic c -> String.make 1 c
  | T_Routine ts -> "(" ^ (String.concat ","  (List.map (fun e -> type_string(snd e)) ts)) ^ ")"

let rec type_equal type1 type2 =
  let aux t1 t2 =
    match (t1, t2) with
    | (T_Int, T_Int) -> true
    | (T_Bool, T_Bool) -> true
    | (T_Char, T_Char) -> true
    | (T_Array at1, T_Array at2) -> type_equal (Option.get at1) (Option.get at2)
    | (T_Struct(n1,ta1), T_Struct(n2, ta2)) when n1 = n2 && (List.length ta1) = (List.length ta2) -> true && (List.fold_right (fun e acc -> (type_equal (Option.get (fst e)) (Option.get (snd e))) && acc) (List.combine ta1 ta2) true)
    | (T_Null, _) -> true
    | (T_Generic c1, T_Generic c2) -> c1 = c2
    | (T_Routine types1, T_Routine types2) -> if List.length types1 = List.length types2 then List.fold_right (fun ((_,t1),(_,t2)) acc -> type_equal t1 t2 && acc) (List.combine types1 types2) true else false
    | _ -> false
  in aux type1 type2 || aux type2 type1

let rec first_non_null typs =
  match typs with
  | [] -> (Open, Error "No non-null type in array")
  | (_,Ok T_Null)::t -> first_non_null t
  | h::_ -> h

let type_array_literal lst =
  let rec aux_type ty vmod li =
    match li with
    | [] -> (vmod, Ok(ty))
    | (expr_vmod,Ok h)::t -> (
      if not (type_equal h ty) then raise_failure "Array literal containing expressions of non-equatable types"
      else aux_type ty (strictest_mod vmod expr_vmod) t
      )
    | (_,_)::_ -> vmod, Error "awww"
  in 
  let (vm,res) = first_non_null lst in
    if Result.is_ok res 
    then (let ty = Result.get_ok res in aux_type ty vm lst) 
    else (vm,res)

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

let rec translate_operational_type op_typ =
  match op_typ with
  | NOp_T t -> t
  | BinOp_T(op, ot1, ot2) -> ( match op, (translate_operational_type ot1), (translate_operational_type ot2) with
    | "&&", T_Bool, T_Bool -> T_Bool
    | "||", T_Bool, T_Bool -> T_Bool
    | "=", _, T_Null -> T_Bool
    | "=", T_Null, _ -> T_Bool
    | "=", T_Bool, T_Bool -> T_Bool
    | "=", T_Char, T_Char -> T_Bool
    | "=", T_Int, T_Int -> T_Bool
    | "!=", _, T_Null -> T_Bool
    | "!=", T_Null, _ -> T_Bool
    | "!=", T_Bool, T_Bool -> T_Bool
    | "!=", T_Char, T_Char -> T_Bool
    | "!=", T_Int, T_Int -> T_Bool
    | "<=", T_Int, T_Int -> T_Bool
    | "<", T_Int, T_Int -> T_Bool
    | ">=", T_Int, T_Int -> T_Bool
    | ">", T_Int, T_Int -> T_Bool
    | "+", T_Int, T_Int -> T_Int
    | "-", T_Int, T_Int -> T_Int
    | "*", T_Int, T_Int -> T_Int
    | _,t1,t2 -> raise_failure ("Unknown binary operation: '"^type_string t1^" "^op^" "^type_string t2^"'")
  )
  | UnOp_T(op, ot) -> ( match op, (translate_operational_type ot) with
    | "!", T_Bool -> T_Bool
    | _,t1 -> raise_failure ("Unknown unary operation: '"^op^" "^type_string t1^"'")
  )

let has_generic ts =
  List.fold_left (fun acc t -> match t with
  | T_Generic _ -> true
  | _ -> acc
  ) false ts

let routine_signature_equal (params1 : (var_mod * typ * string) list) params2 : bool =
  let rec aux params =
    match params with
    | [] -> true
    | ((vm1,t1,_),(vm2,t2,_))::t -> if (type_equal t1 t2) && (vm1 = vm2) then aux t else false
  in
  if (List.length params1 == List.length params2) then aux (List.combine params1 params2) else false
  
let rec type_expr expr env contexts : var_mod * (op_typ, string) result =
  match expr with
  | Reference ref_expr -> ( match type_reference ref_expr env contexts with 
    | (vm,Ok(t)) -> (vm, Ok(NOp_T t))
    | (vm,Error m) -> (vm,Error m)
  )
  | Value val_expr -> type_value val_expr env contexts

and type_inner_reference iref env contexts : var_mod * (typ, string) result =
  match iref with
  | Access name -> (var_modifier name env, var_type name env)
  | StructAccess (refer, field) -> (
    let (vmod, typ_res) = type_inner_reference refer env contexts in
    match typ_res with
    | Error _ -> (vmod, typ_res)
    | Ok ty -> (
      match ty with 
      | T_Struct (str_name, typ_args) -> (match lookup_struct str_name env.var_env.structs with
      | Some (typ_vars, fields) -> (
        let resolved_fields = replace_generics (List.map (fun (a,b,_) -> (a,b)) fields) typ_vars typ_args in
        let (field_mod, field_ty,_) = struct_field field (List.map (fun ((a,b),c) -> (a,b,c)) (List.combine resolved_fields (List.map (fun (_,_,n) -> n) fields))) in
        ((if vmod = Open then field_mod else Const), Ok field_ty)
        )
        | None -> raise_failure ("No such struct '" ^ str_name ^ "'")
        )
        | _ -> raise_failure ("Field access of non-struct value")
    )
  )
  | ArrayAccess (refer, index) -> (
    let (vm,typ_res) = type_expr index env contexts in
    match typ_res with
    | Error m -> (vm,Error m)
    | Ok ot -> (
      match translate_operational_type ot with
      | T_Int -> (
        let (vmod, typ_res) =  type_inner_reference refer env contexts in
        match typ_res with
        | Error _ -> (vmod, typ_res)
        | Ok ty -> ( match ty with 
          | T_Array array_typ -> (vmod, Ok (Option.get array_typ))
          | _ -> raise_failure ("Array access of non-array value")
        )
      )
      | _ -> raise_failure ("Array indexed with non-integer value")
    )
  )

and type_reference ref_expr env contexts : var_mod * (typ, string) result =
  match ref_expr with
  | Null -> (Open, Ok T_Null)
  | LocalContext(ref) -> type_inner_reference ref env contexts
  | OtherContext(cn,ref) -> ( match lookup_context cn env.file_refs contexts with
    | None -> failwith ("No such context: " ^ cn)
    | Some(env) -> type_inner_reference ref env contexts
  )

and type_value val_expr env contexts : var_mod * (op_typ, string) result =
  match val_expr with
  | Binary_op (op, expr1, expr2) -> (
    let (_, ty1) = type_expr expr1 env contexts in
    let (_, ty2) = type_expr expr2 env contexts in
    match ty1, ty2 with
    | Ok(ot1), Ok(ot2) -> (Open, Ok(BinOp_T(op, ot1, ot2)))
    | Error m, _
    | _, Error m -> (Open,Error m)
    (*match (op, ty1, ty2) with
    | ("&&", Ok T_Bool, Ok T_Bool) ->  (Open, Ok T_Bool)
    | ("||", Ok T_Bool, Ok T_Bool) -> (Open, Ok T_Bool)
    | ("=", Ok _, Ok T_Null) -> (Open, Ok T_Bool)
    | ("=", Ok T_Null, Ok _) -> (Open, Ok T_Bool)
    | ("=", Ok T_Bool, Ok T_Bool) -> (Open, Ok T_Bool)
    | ("=", Ok T_Char, Ok T_Char) -> (Open, Ok T_Bool)
    | ("=", Ok T_Int, Ok T_Int) -> (Open, Ok T_Bool)
    | ("!=", Ok _, Ok T_Null) -> (Open, Ok T_Bool)
    | ("!=", Ok T_Null, Ok _) -> (Open, Ok T_Bool)
    | ("!=", Ok T_Bool, Ok T_Bool) -> (Open, Ok T_Bool)
    | ("!=", Ok T_Char, Ok T_Char) -> (Open, Ok T_Bool)
    | ("!=", Ok T_Int, Ok T_Int) -> (Open, Ok T_Bool)
    | ("<=", Ok T_Int, Ok T_Int) -> (Open, Ok T_Bool) 
    | ("<", Ok T_Int, Ok T_Int) -> (Open, Ok T_Bool)
    | (">=", Ok T_Int, Ok T_Int) -> (Open, Ok T_Bool)
    | (">", Ok T_Int, Ok T_Int) -> (Open, Ok T_Bool)
    | ("+", Ok T_Int, Ok T_Int) -> (Open, Ok T_Int)
    | ("-", Ok T_Int, Ok T_Int) -> (Open, Ok T_Int)
    | ("*", Ok T_Int, Ok T_Int) -> (Open, Ok T_Int)
    | _ -> raise_failure "Unknown binary operator, or type mismatch"*)
  )
  | Unary_op (op, expr) -> (
    let (_, ty) = type_expr expr env contexts in
    match ty with
    | Ok ot -> (Open, Ok(UnOp_T(op, ot)))
    | Error m -> (Open, Error m)
  )
  | ArraySize (refer) ->  (
    let (_, ty) = type_inner_reference refer env contexts in
    match ty with
    | Ok(T_Array _) -> (Open, Ok(NOp_T T_Int))
    | _ -> raise_failure "Array size of non-array value"
  )
  | GetInput ty -> (Open, Ok(NOp_T ty))
  | Bool _ -> (Open, Ok(NOp_T T_Bool))
  | Int _ -> (Open, Ok(NOp_T T_Int))
  | Char _ -> (Open, Ok(NOp_T T_Char))
  | ValueOf (refer) -> (
    let (vmod, typ_res) = type_inner_reference refer env contexts in
    match typ_res with
    | Ok t -> (vmod, Ok(NOp_T t))
    | Error m -> (vmod, Error m)
  )
  | NewArray (ty, _) -> (Open, Ok(NOp_T (T_Array(Some ty))))
  | ArrayLiteral elements -> ( match type_array_literal (List.map (fun e -> let (vm,ot) = type_expr e env contexts in if Result.is_ok ot then (vm,Ok (translate_operational_type (Result.get_ok ot))) else (vm,Error (Result.get_error ot))) elements) with 
    | (vmod, Ok ety) -> (vmod, Ok(NOp_T(T_Array(Some ety))))
    | (_, Error m) -> raise_failure m
  )
  | NewStruct (name, typ_args, args) -> ( match lookup_struct name env.var_env.structs with
    | Some (typ_vars, params) -> (
      if (List.length typ_vars > 0) then ( (* Generic *)
        let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
        (Open, Ok(NOp_T(T_Struct(name, typ_args))))
      )
      else (Open, Ok(NOp_T(T_Struct(name, typ_args)))) (* Not generic *)
    )
    | None -> raise_failure ("No such struct '" ^ name ^ "'")
  )
  | StructLiteral _ ->  (Open, Ok(NOp_T(T_Struct("",[])))) (* failwith "fuck"  raise_failure "Cannot infere a type from a struct literal" *)

  and replace_generic c typ_vars typ_args = 
    let rec aux lst = 
      match lst with
      | [] -> failwith "Could not resolve generic index"
      | (v,a)::t -> if v = c then a else aux t
    in
    aux (try List.combine typ_vars typ_args with | _ -> raise_failure "meme") 
  
  and replace_generics lst typ_vars typ_args : (var_mod * typ) list = 
    let rec replace element = 
      match element with
      | T_Generic(c) -> replace_generic c typ_vars typ_args
      | T_Array(sub) -> Some (T_Array((replace (Option.get sub))))
      | T_Struct(str_name, ta) -> Some (T_Struct(str_name, List.map (fun e -> replace (Option.get e)) ta))
      | T_Routine(ts) -> Some (T_Routine (List.map (fun (vm,t) -> (vm,Option.get (replace t))) ts))
      | e -> Some e
    in
    let aux element =
      match element with
      | (vmod, ty) -> (vmod, replace ty)
    in
    List.map (fun e -> match aux e with (a,t) -> (a,Option.get t)) lst
  
  and is_fully_defined_type typ_opt var_env =
    match typ_opt with
    | None -> false
    | Some(T_Array sub_t) -> is_fully_defined_type sub_t var_env
    | Some(T_Generic c) -> List.mem c var_env.typ_vars
    | Some(T_Struct(name, typ_args)) -> ( match lookup_struct name var_env.structs with
      | None -> false
      | Some(tvs,_) -> (List.length tvs = List.length typ_args) && List.fold_left (fun acc ta -> (is_fully_defined_type ta var_env) && acc) true typ_args 
    )
    | _ -> true

  and dig_into_struct typ typ_vars_args_map param_arg_map var_env acc =
    match typ_vars_args_map with
    | [] -> acc
    | (c,Some(ta))::t when type_equal ta typ -> dig_into_struct typ t param_arg_map var_env (find_related_args (T_Generic c) param_arg_map var_env acc)
    | _::t -> dig_into_struct typ t param_arg_map var_env acc

  and find_related_args typ param_arg_map (env : environment) acc =
    match param_arg_map with
    | [] -> acc
    | ((_,p_typ), expr)::t when type_equal typ p_typ -> find_related_args typ t env (expr::acc)
    | (((_,T_Struct(name,typ_args)), expr)::t) -> find_related_args typ t env ( match lookup_struct name env.var_env.structs with
      | None -> raise_failure ("No such struct: " ^ name)
      | Some(tvs,params) -> ( match expr with
        | Value(StructLiteral(exprs)) -> dig_into_struct typ (List.combine tvs typ_args) (List.combine (List.map (fun (a,b,_) -> (a,b)) params) exprs) env acc
        | _ -> find_related_args typ t env acc
      )
    )
    | _::t -> find_related_args typ t env acc

  and get_first_type exprs env contexts =
    match exprs with
    | [] -> raise_failure "Could not infer type from context"
    | h::t -> try (
      let (_,typ_res) = type_expr h env contexts in
      match typ_res with
      | Error m -> Error m
      | Ok ot -> if (translate_operational_type ot) = T_Null then get_first_type t env contexts else typ_res
    ) with | _ -> get_first_type t env contexts

  and fix_type_args typ_vars typ_args =
    if typ_args = [] then List.init (List.length typ_vars) (fun _ -> None)
    else if List.length typ_args = List.length typ_vars then typ_args
    else raise_failure ("Expected " ^(string_of_int (List.length typ_vars))^ " type arguments, but was given " ^(string_of_int (List.length typ_args))) 

  and resolve_type_args typ_vars typ_args (params : (var_mod * typ) list) args (env : environment) contexts =
    let typ_args = fix_type_args typ_vars typ_args in
    let rec aux tvas acc =
      match tvas with
      | [] -> List.rev acc
      | (c,typ_arg)::t -> ( match is_fully_defined_type typ_arg env.var_env with
        | true -> aux t (typ_arg::acc)
        | false -> ( match typ_arg with
          | None -> aux t (Some(translate_operational_type(Result.get_ok (get_first_type (find_related_args (T_Generic c) (List.combine params args) env []) env contexts)))::acc)
          | Some(T_Struct(name,tas)) -> ( match lookup_struct name env.var_env.structs with
            | None -> raise_failure ("No such struct: " ^ name)
            | Some(tvs,ps) -> (
              let rec infer_from_related related_exprs =
                match related_exprs with
                | [] -> raise_failure "Could not infer a type from context"
                | Value(StructLiteral(exprs))::t -> (try (Some(T_Struct(name, resolve_type_args tvs tas (List.map (fun (a,b,_) -> (a,b)) ps) exprs env contexts))) with | _ -> infer_from_related t)
                | Value(NewStruct(name,tas,exprs))::t -> ( match lookup_struct name env.var_env.structs with
                  | Some(tvs,ps) -> (try (Some(T_Struct(name, resolve_type_args tvs tas (List.map (fun (a,b,_) -> (a,b)) ps) exprs env contexts))) with | _ -> infer_from_related t)
                  | None -> raise_failure ("No such struct: " ^ name)
                )
                | Reference(Null)::t -> infer_from_related t
                | _::t -> infer_from_related t
              in
              aux t ((infer_from_related (find_related_args (T_Generic(c)) (List.combine params args) env []))::acc)
            )
          )
          | _ -> raise_failure "This should not happen 1"
        )
      )
    in
    aux (List.combine typ_vars typ_args) []

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
    | T_Array(sub_ty) -> check (Option.get sub_ty)
    | T_Generic(c) -> if List.mem c typ_vars then true else false
    | T_Struct(name, typ_args) -> ( match lookup_struct name structs with
      | Some(tvs, _) -> List.length tvs = List.length typ_args && List.fold_left (fun acc field_ty -> (check (Option.get field_ty)) && acc) true typ_args
      | None -> false
    )
    | T_Routine(types) -> List.fold_left (fun acc (_,typ) -> (check typ) && acc) true types
  in
  List.fold_left (fun acc (_,ty,_) -> (check ty) && acc) true params

let rec well_defined_type typ_opt var_env =
  match typ_opt with
  | None -> false
  | Some(T_Array sub_t) -> well_defined_type sub_t var_env
  | Some(T_Generic c) -> List.mem c var_env.typ_vars
  | Some(T_Struct(name, typ_args)) -> ( match lookup_struct name var_env.structs with
    | None -> false
    | Some(tvs,_) -> (List.length tvs = List.length typ_args) && List.fold_left (fun acc ta -> (well_defined_type ta var_env) && acc) true typ_args 
  )
  | Some(T_Routine(ts)) -> List.fold_left (fun acc (_,t) -> well_defined_type (Some t) var_env && acc) true ts
  | _ -> true

let check_topdecs file structs =
  let rec aux tds =
    match tds with
    | [] -> ()
    | Routine(_,name,typ_vars,params,_)::t -> (
      if not(elements_unique typ_vars) then raise_failure ("Non-unique type variables in routine definition '" ^ name ^ "'")
      else if not(parameters_check typ_vars structs params) then raise_failure ("illegal parameters in defenition of routine '" ^ name ^ "'")
      else aux t
    )
    | Struct(name,typ_vars,params)::t -> ( 
      if not(elements_unique typ_vars) then raise_failure ("Non-unique type variables in struct definition '" ^ name ^ "'")
      else if not(parameters_check typ_vars structs params) then raise_failure ("illegal parameters in defenition of struct '" ^ name ^ "'")
      else aux t
    )
    | _::t -> aux t
  in
  match file with
  | File(tds) -> aux tds

let check_structs structs =
  let rec aux strs seen =
    match strs with
    | [] -> ()
    | (name, _, _)::t -> if List.mem name seen then raise_failure ("Duplicate struct name '" ^ name ^ "'") else aux t (name::seen) 
  in
  aux structs []

let check_struct_literal struct_fields expr (env : environment) contexts =
  let rec aux (pairs : ((var_mod * typ) * expression) list) =
    match pairs with
    | [] -> true 
    | ((_,T_Struct(name,typ_args)), Value(StructLiteral(exprs)))::t -> (
      match lookup_struct name env.var_env.structs with
      | None -> false
      | Some(tvs,ps) -> (
        let replaced = replace_generics (List.map (fun (a,b,_) -> (a,b)) ps) tvs typ_args in
        aux (List.combine replaced exprs)
      ) && aux t
    )
    | ((_,T_Null),_)::_ -> false
    | ((vmod,typ),e)::t -> (
      let (expr_vmod, expr_typ_res) = type_expr e env contexts in
      match expr_typ_res with
      | Ok(expr_typ) -> (
        if not(type_equal typ (translate_operational_type expr_typ)) then false else
        if vmod = Open && (expr_vmod = Stable || expr_vmod = Const) then false else
        aux t
      )
      | _ -> false
    )
  in match expr with
  | Value(StructLiteral(exprs)) -> (try aux (List.combine struct_fields exprs) with | _ -> false)
  | Reference(Null) -> true
  | _ -> false

let match_typ_arg ts =
  match ts with
  | None, Some t -> Some t
  | Some t, None -> Some t
  | Some(t1), Some(t2) when type_equal t1 t2 -> Some t1
  | _ -> raise_failure "Type match failed" 

let resolve_type typ_opt expr (env : environment) contexts : typ =
  match typ_opt with
  | Some(typ) -> ( match typ with
    | T_Generic c -> raise_failure ((String.make 1 c) ^ " is not defined in this scope")
    | T_Array(_) -> raise_failure "not implemented"
    | T_Struct(name,typ_args) -> (match lookup_struct name env.var_env.structs with
      | None -> raise_failure ("No such struct: " ^ name)
      | Some(typ_vars,params) -> ( match expr with
        | Reference(Null) -> raise_failure "Cannot infer from null"
        | Reference(ref) -> (Result.get_ok (snd(type_reference ref env contexts)))
        | Value(StructLiteral(exprs)) -> (
            let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) exprs env contexts in
            let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars typ_args in
            if not(check_struct_literal params (Value (StructLiteral exprs)) env contexts) then raise_failure ("Could not match struct literal with '" ^ type_string (T_Struct(name,typ_args)) ^ "'")
            else (T_Struct(name,typ_args))
        )
        | Value(NewStruct(expr_name,tas,args)) -> (
            if not(expr_name = name) then raise_failure "mismatched struct names" else
            let expr_typ_args = resolve_type_args typ_vars tas (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
            let typ_args = List.map (fun x -> (fst x)) (List.combine typ_args expr_typ_args) in
            (T_Struct(name,typ_args))
        )
        | _ -> raise_failure "cannot infer"
      )
    )
    | _ -> raise_failure "well-definedness checker has failed"
  )
  | None -> let (_, t) = type_expr expr env contexts in translate_operational_type (Result.get_ok t)

let declaration_checks target_vmod target_typ expr_vmod expr_typ =
    if target_vmod = Open && expr_vmod != Open then raise_failure "Cannot assign a protected variable to an open variable"
    else if not (type_equal target_typ (translate_operational_type expr_typ)) then raise_failure ("Type mismatch: expected '" ^ (type_string target_typ) ^ "', got '" ^ (type_string (translate_operational_type expr_typ)) ^ "'")
    else Ok(expr_typ)

let argument_checks target_vmod target_typ expr_vmod expr_typ =
    if target_vmod = Open && (expr_vmod != Open) then raise_failure "Cannot use a protected variable as an open variable"
    else if target_vmod = Stable && (expr_vmod = Const) then raise_failure "Cannot use a constant variable as a stable parameter"
    else if not (type_equal target_typ (translate_operational_type expr_typ)) then raise_failure ("Type mismatch: expected '" ^ (type_string target_typ) ^ "', got '" ^ (type_string (translate_operational_type expr_typ)) ^ "'")
    else Ok(expr_typ)

let rec type_check checks vmod typ expr (env : environment) contexts : (op_typ, string) result =
  match typ, expr with
  | None, Value(StructLiteral(_)) -> Error "Cannot infer a type from a struct literal"
  (*| None, Value(ArrayLiteral []) -> Error "Cannot infer a type from the empty array"*)
  | None, Reference(ref) -> ( 
    let ref_env = ( match ref with
      | LocalContext (Access _) -> env
      | OtherContext (other,Access _) -> ( match lookup_context other env.file_refs contexts with
        | None -> raise_failure ("Unknown context alias: "^other)
        | Some(e) -> e
        )
      | _ -> env
    )
    in match ref with
    | LocalContext (Access n) 
    | OtherContext (_,Access n) when (name_type n ref_env = RoutineName) -> ( match lookup_routine n ref_env.routine_env with
      | Some(_,_,_,[],ps,_)  -> Ok(NOp_T(T_Routine(List.map (fun (a,b,_) -> (a,b)) ps)))
      | None -> raise_failure "Should not happen"
      | _ -> Error "Cannot infere type variables for routine"
    )
    | _ -> (
      let (vm,res) = type_expr expr env contexts in 
      match res with
      | Ok(expr_typ) -> checks vmod (translate_operational_type expr_typ) vm expr_typ
      | Error m -> Error m
    )
  )
  | None, Value(ArrayLiteral elems) -> (
    let elem_types = List.map (fun elem -> type_check checks vmod None elem env contexts) elems in
    let t = List.fold_left (fun acc t -> match acc with
      | Error _ -> if t = Ok(NOp_T T_Null) then Error "Only nulls in array literal" else t
      | Ok(a) -> if Result.is_error t then acc else if type_equal (translate_operational_type a) (translate_operational_type(Result.get_ok t)) then acc else Error ("Array literal contains unequatable types")
    ) (Error "Cannot infer a type from the empty array") elem_types in
    if Result.is_ok t then type_check checks vmod (Some(T_Array(Some(translate_operational_type(Result.get_ok t))))) expr env contexts else t
    (*if Result.is_ok t then (List.iter (fun elem -> let _ = type_check checks vmod (Some (Result.get_ok t)) elem env contexts in ()) elems ; Ok(T_Array(Some (Result.get_ok t)))) else t*)
  )
  | Some(T_Array(Some st)), Value(ArrayLiteral []) -> Ok(NOp_T(T_Array(Some(st))))
  (*| None, Value(ArrayLiteral elems) -> ( match type_value (ArrayLiteral elems) env contexts with
    | (_,T_Array(Some T_Null)) -> raise_failure "'null' array type"
    | (vm, T_Array(Some(T_Routine rst))) -> let rt = List.fold_left (fun acc e -> let et = type_check checks vm (Some(T_Routine rst)) e env contexts in if acc = None then Some(et) else acc) None elems in T_Array(rt)
    | (vm,T_Array(st)) -> List.iter (fun e -> let _ = type_check checks vm st e env contexts in ()) elems ; T_Array(st)
    | _ -> raise_failure "Should not happen"
  )*)
  | Some(T_Array(Some(T_Routine rst))), Value(ArrayLiteral elems) -> (
    let ress = List.map (fun elem -> type_check checks vmod (Some(T_Routine rst)) elem env contexts) elems in
    List.fold_left (fun acc res -> if Result.is_error acc then acc else if Result.is_error res then res else acc) (Ok(NOp_T(T_Array(Some(T_Routine rst))))) ress
    (*Ok(T_Array(Some(T_Routine rst))))*)
  )
  | Some(T_Array(Some st)), Value(ArrayLiteral elems) -> (
    List.iter (fun elem -> let _ = type_check checks vmod (Some st) elem env contexts in ()) elems ; Ok(NOp_T(T_Array(Some st)))
  )
  (*| Some(T_Array(Some(T_Routine rst))), Value(ArrayLiteral elems) -> (
    let rt = List.fold_left (fun acc e -> let et = type_check checks Open (Some(T_Routine rst)) e env contexts in if acc = None then Some(et) else acc) None elems in T_Array(rt)
  )*)
  | Some(T_Routine params), Reference(ref) -> ( 
    let ref_env = ( match ref with
      | LocalContext (Access _) -> env
      | OtherContext (other,Access _) -> ( match lookup_context other env.file_refs contexts with
        | None -> raise_failure ("Unknown context alias: "^other)
        | Some(e) -> e
        )
      | _ -> env
    )
    in match ref with
    | LocalContext (Access n)
    | OtherContext (_,Access n) when (name_type n ref_env = RoutineName) -> ( match lookup_routine n ref_env.routine_env with
      | None -> raise_failure "Should not happen"
      | Some(_,_,_,[],ps,_)  -> if List.fold_left2 (fun acc (vm1,t1) (vm2,t2,_) -> (vm1 = vm2 && (type_equal t1 t2)) && acc) true params ps then Ok(NOp_T(T_Routine params)) else Error "Type mismatch"
      | Some(_,_,_,_,ps,_) -> (let rec aux pairs resolved acc = match pairs with
        | [] -> acc
        | ((vm1,t1), (vm2,T_Generic c,_))::t -> ( if vm1 != vm2 then raise_failure "" else match List.find_opt (fun (tv,_) -> tv = c) resolved with
          | None -> aux t ((c,t1)::resolved) ((vm1,t1)::acc)
          | Some(_,ty) -> if type_equal ty t1 then aux t resolved ((vm1,t1)::acc) else raise_failure ""
        )
        | ((vm1,t1), (vm2,t2,_))::t -> if type_equal t1 t2 && vm1 = vm2 then aux t resolved ((vm1,t1)::acc) else raise_failure ""
      in try (
        let inf_typ = T_Routine (aux (List.combine params ps) [] []) in
        if well_defined_type (Some inf_typ) env.var_env then Ok(NOp_T inf_typ) else Error ""
      ) with _ -> Error "Could not coerce generic routine to the required type" )
    )
    | Null -> Ok(NOp_T(T_Routine params))
    | _ -> (
      let (vm,res) = type_expr expr env contexts in 
      match res with
      | Ok(expr_typ) -> checks vmod (T_Routine params) vm expr_typ
      | Error m -> Error m
    )
  )
  | Some(T_Struct(name,typ_args)), Value(StructLiteral(exprs)) -> ( match lookup_struct name env.var_env.structs with
    | Some(typ_vars,params) ->  (
      let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) exprs env contexts in
      let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars typ_args in
      if not(check_struct_literal params (Value (StructLiteral exprs)) env contexts) then raise_failure ("Could not match struct literal with '" ^ type_string (T_Struct(name,typ_args)) ^ "'")
      else Ok(NOp_T(T_Struct(name,typ_args)))
    )
    | None -> raise_failure ("No such struct '" ^ name ^ "'")
  )
  | Some(T_Struct(name,typ_args)), Value(NewStruct(expr_name,tas,args)) -> ( match lookup_struct name env.var_env.structs with
    | Some(typ_vars,params) ->  (
      if not(expr_name = name) then raise_failure "mismatched struct names" else
      let matched_typ_args = List.map (fun ts -> match_typ_arg ts) (List.combine (fix_type_args typ_vars typ_args) (fix_type_args typ_vars tas)) in
      let resolved_typ_args = resolve_type_args typ_vars matched_typ_args (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
      let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars resolved_typ_args in
      if not(List.fold_left (fun acc ((_,param_typ),arg) -> acc && type_equal param_typ (translate_operational_type(Result.get_ok (snd(type_expr arg env contexts))))) true (List.combine params args)) then raise_failure ("Struct argument type mismatch") else
      Ok(NOp_T(T_Struct(name,resolved_typ_args)))
    )
    | None -> raise_failure ("No such struct '" ^ name ^ "'")
  )
  | None, Value(NewStruct(name,typ_args,args)) -> ( match lookup_struct name env.var_env.structs with
    | Some(typ_vars,params) ->  (
      let resolved_typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
      let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars resolved_typ_args in
      if not(List.fold_left (fun acc ((_,param_typ),arg) -> acc && type_equal param_typ (translate_operational_type(Result.get_ok (snd(type_expr arg env contexts))))) true (List.combine params args)) then raise_failure ("Struct argument type mismatch") else
      Ok(NOp_T(T_Struct(name,resolved_typ_args)))
    )
    | None -> raise_failure ("No such struct '" ^ name ^ "'")
  )
  | None, expr -> (
    let (vm,res) = type_expr expr env contexts in 
    match res with
    | Ok(expr_typ) -> checks vmod (translate_operational_type expr_typ) vm expr_typ
    | Error m -> Error m
  )
  | Some(target_typ), expr -> (
    let (vm, typ_res) = type_expr expr env contexts in
    match typ_res with
    | Ok(expr_typ) -> checks vmod target_typ vm expr_typ
    | Error m -> Error m
  )
  (*| _ -> raise_failure "big sad"checks vmod typ expr env contexts*)


let argument_type_check = type_check argument_checks
let declaration_type_check = type_check declaration_checks

let assignment_type_check target assign (env : environment) contexts : (op_typ, string) result =
  let (target_vmod, target_type_res) = type_reference target env contexts in
  match target_type_res with
  | Error m -> Error m
  | Ok target_type -> (
  let (assign_vmod, assign_type) = match assign with
    | Value(StructLiteral(exprs)) -> ( match target_type with
      | T_Struct(name, typ_args) -> ( match lookup_struct name env.var_env.structs with
        | Some(typ_vars, params) -> (
          let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) exprs env contexts in
          if not(check_struct_literal (replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars typ_args) (Value(StructLiteral exprs)) env contexts) then raise_failure "Structure mismatch in assignment"
          else (Open, (NOp_T(T_Struct(name, typ_args))))
        )
        | None -> raise_failure ("No such struct '" ^ name ^ "'")
      )
      | _ -> raise_failure ("Struct literal assignment to a variable of type '" ^ type_string target_type ^ "'")
      
    )
    | _ -> (
      let (assign_vmod, assign_type_res) = type_expr assign env contexts in
      match assign_type_res with
      | Ok assign_type -> (
        if not (type_equal target_type (translate_operational_type assign_type)) then raise_failure ("Type mismatch in assignment, expected '"^(type_string target_type)^"' but got '" ^(type_string (translate_operational_type assign_type))^ "'") 
        else (assign_vmod, assign_type)
      )
      | Error m -> raise_failure m
    )
    in
    match target_vmod with
    | Open -> (
      if assign_vmod != Open then raise_failure "Assignment of protected variable, to non-protected variable"
      else Ok assign_type
    )
    | Stable -> ( match assign with
      | Value _ -> raise_failure "Attempt to overwrite stable data"
      | Reference _ -> Ok assign_type
    )
    | Const -> raise_failure "Assignment to a protected variable"
  )


(* Might be useful for infering struct arrays
let rec meme typ_vars typ_args params array_exprs var_env =
  match array_exprs with
  | [] -> raise_failure "Fuck dig"
  | Value(StructLiteral(exprs))::t -> ( try (
    let typ_args = resolve_type_args typ_vars typ_args params exprs var_env in
    let params = replace_generics params typ_vars typ_args in
    if not(List.fold_left (fun acc array_expr -> check_struct_literal params array_expr var_env && acc) true array_exprs) then raise_failure ("?1")
    else typ_args
  ) with _ -> meme typ_vars typ_args params t var_env)
  | _::t -> meme typ_vars typ_args params t var_env   
*)