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
  | T_Routine ts -> "r(" ^ (String.concat ","  (List.map (fun e -> type_string(snd e)) ts)) ^ ")"

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

let type_array_literal lst =
  let rec aux_type ty vmod li =
    match li with
    | [] -> (vmod,ty)
    | (expr_vmod,h)::t -> (
      if not (type_equal h ty) then raise_error "Array literal containing expressions of differing types"
      else aux_type ty (strictest_mod vmod expr_vmod) t
    )
  in
  match lst with 
  | [] -> (Open, T_Null)
  | (vmod,h)::t -> aux_type h vmod t

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

let routine_signature_equal (params1 : (var_mod * typ * string) list) params2 : bool =
  let rec aux params =
    match params with
    | [] -> true
    | ((vm1,t1,_),(vm2,t2,_))::t -> if (type_equal t1 t2) && (vm1 = vm2) then aux t else false
  in
  if (List.length params1 == List.length params2) then aux (List.combine params1 params2) else false
  
let rec type_expr expr var_env =
  match expr with
  | Reference ref_expr -> type_reference ref_expr var_env
  | Value val_expr -> type_value val_expr var_env

and type_inner_reference iref env contexts =
  match iref with
  | Access name -> (var_modifier name env, var_type name env)
  | StructAccess (refer, field) -> (
    let (vmod, ty) =  type_inner_reference refer env contexts in
    match ty with 
    | T_Struct (str_name, typ_args) -> (match lookup_struct str_name env.var_env.structs with
      | Some (typ_vars, fields) -> (
        let resolved_fields = replace_generics (List.map (fun (a,b,_) -> (a,b)) fields) typ_vars typ_args in
        let (field_mod, field_ty,_) = struct_field field (List.map (fun ((a,b),c) -> (a,b,c)) (List.combine resolved_fields (List.map (fun (_,_,n) -> n) fields))) in
        ((if vmod = Open then field_mod else Const), field_ty)
      )
      | None -> raise_error ("No such struct '" ^ str_name ^ "'")
    )
    | _ -> raise_error ("Field access of non-struct value")
  )
  | ArrayAccess (refer, index) -> (
    if snd (type_expr index env contexts) != T_Int then raise_error ("Array indexed with non-integer value")
    else let (vmod, ty) =  type_inner_reference refer env contexts in
    match ty with 
    | T_Array array_typ -> (vmod, Option.get array_typ)
    | _ -> raise_error ("Array access of non-array value")
  )

and type_reference ref_expr env contexts =
  match ref_expr with
  | Null -> (Open, T_Null)
  | LocalContext(ref) -> type_inner_reference ref env contexts
  | OtherContext(cn,ref) -> ( match lookup_context cn env.file_refs contexts with
    | None -> failwith ("No such context: " ^ cn)
    | Some(env) -> type_inner_reference ref env contexts
  )

and type_value val_expr env contexts =
  match val_expr with
  | Binary_op (op, expr1, expr2) -> (
    let (_, ty1) = type_expr expr1 env contexts in
    let (_, ty2) = type_expr expr2 env contexts in
    match (op, ty1, ty2) with
    | ("&&", T_Bool, T_Bool) ->  (Open, T_Bool)
    | ("||", T_Bool, T_Bool) -> (Open, T_Bool)
    | ("=", _, T_Null) -> (Open, T_Bool)
    | ("=", T_Null, _) -> (Open, T_Bool)
    | ("=", T_Bool, T_Bool) -> (Open, T_Bool)
    | ("=", T_Char, T_Char) -> (Open, T_Bool)
    | ("=", T_Int, T_Int) -> (Open, T_Bool)
    | ("!=", _, T_Null) -> (Open, T_Bool)
    | ("!=", T_Null, _) -> (Open, T_Bool)
    | ("!=", T_Bool, T_Bool) -> (Open, T_Bool)
    | ("!=", T_Char, T_Char) -> (Open, T_Bool)
    | ("!=", T_Int, T_Int) -> (Open, T_Bool)
    | ("<=", T_Int, T_Int) -> (Open, T_Bool) 
    | ("<", T_Int, T_Int) -> (Open, T_Bool)
    | (">=", T_Int, T_Int) -> (Open, T_Bool)
    | (">", T_Int, T_Int) -> (Open, T_Bool)
    | ("+", T_Int, T_Int) -> (Open, T_Int)
    | ("-", T_Int, T_Int) -> (Open, T_Int)
    | ("*", T_Int, T_Int) -> (Open, T_Int)
    | _ -> raise_error "Unknown binary operator, or type mismatch"
  )
  | Unary_op (op, expr) -> (
    let (_, ty) = type_expr expr env contexts in
    match (op, ty) with
    | ("!", T_Bool) -> (Open, T_Bool)
    | _ -> raise_error "Unknown unary operator, or type mismatch"
  )
  | ArraySize (refer) ->  (
    let (_, ty) = type_inner_reference refer env contexts in
    match ty with
    | T_Array _ -> (Open, T_Int)
    | _ -> raise_error "Array size of non-array value"
  )
  | GetInput ty -> (Open, ty)
  | Bool _ -> (Open, T_Bool)
  | Int _ -> (Open, T_Int)
  | Char _ -> (Open, T_Char)
  | ValueOf (refer) -> type_inner_reference refer env contexts
  | NewArray (ty, _) -> (Open, T_Array(Some ty))
  | ArrayLiteral elements -> (match type_array_literal (List.map (fun e -> type_expr e env contexts) elements) with (vmod,ety) -> (vmod, T_Array(Some ety)))
  | NewStruct (name, typ_args, args) -> ( match lookup_struct name env.var_env.structs with
    | Some (typ_vars, params) -> (
      if (List.length typ_vars > 0) then ( (* Generic *)
        let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
        (Open, T_Struct(name, typ_args))
      )
      else (Open, T_Struct(name, typ_args)) (* Not generic *)
    )
    | None -> raise_error ("No such struct '" ^ name ^ "'")
  )
  | StructLiteral _ ->  (Open, T_Struct("",[])) (* failwith "fuck"  raise_error "Cannot infere a type from a struct literal" *)

  and replace_generic c typ_vars typ_args = 
    let rec aux lst = 
      match lst with
      | [] -> failwith "Could not resolve generic index"
      | (v,a)::t -> if v = c then a else aux t
    in
    aux (try List.combine typ_vars typ_args with | _ -> raise_error "meme") 
  
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
      | None -> raise_error ("No such struct: " ^ name)
      | Some(tvs,params) -> ( match expr with
        | Value(StructLiteral(exprs)) -> dig_into_struct typ (List.combine tvs typ_args) (List.combine (List.map (fun (a,b,_) -> (a,b)) params) exprs) env acc
        | _ -> find_related_args typ t env acc
      )
    )
    | _::t -> find_related_args typ t env acc

  and get_first_type exprs env contexts =
    match exprs with
    | [] -> raise_error "Could not infer type from context"
    | h::t -> try (
      let (_,typ) = type_expr h env contexts in
      if typ = T_Null then get_first_type t env contexts else typ
    ) with | _ -> get_first_type t env contexts

  and fix_type_args typ_vars typ_args =
    if typ_args = [] then List.init (List.length typ_vars) (fun _ -> None)
    else if List.length typ_args = List.length typ_vars then typ_args
    else raise_error ("Expected " ^(string_of_int (List.length typ_vars))^ " type arguments, but was given " ^(string_of_int (List.length typ_args))) 

  and resolve_type_args typ_vars typ_args (params : (var_mod * typ) list) args (env : environment) contexts =
    let typ_args = fix_type_args typ_vars typ_args in
    let rec aux tvas acc =
      match tvas with
      | [] -> List.rev acc
      | (c,typ_arg)::t -> ( match is_fully_defined_type typ_arg env.var_env with
        | true -> aux t (typ_arg::acc)
        | false -> ( match typ_arg with
          | None -> aux t (Some(get_first_type (find_related_args (T_Generic c) (List.combine params args) env []) env contexts)::acc)
          | Some(T_Struct(name,tas)) -> ( match lookup_struct name env.var_env.structs with
            | None -> raise_error ("No such struct: " ^ name)
            | Some(tvs,ps) -> (
              let rec infer_from_related related_exprs =
                match related_exprs with
                | [] -> raise_error "Could not infer a type from context"
                | Value(StructLiteral(exprs))::t -> (try (Some(T_Struct(name, resolve_type_args tvs tas (List.map (fun (a,b,_) -> (a,b)) ps) exprs env contexts))) with | _ -> infer_from_related t)
                | Value(NewStruct(name,tas,exprs))::t -> ( match lookup_struct name env.var_env.structs with
                  | Some(tvs,ps) -> (try (Some(T_Struct(name, resolve_type_args tvs tas (List.map (fun (a,b,_) -> (a,b)) ps) exprs env contexts))) with | _ -> infer_from_related t)
                  | None -> raise_error ("No such struct: " ^ name)
                )
                | Reference(Null)::t -> infer_from_related t
                | _::t -> infer_from_related t
              in
              aux t ((infer_from_related (find_related_args (T_Generic(c)) (List.combine params args) env []))::acc)
            )
          )
          | _ -> raise_error "This should not happen 1"
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
      if not(elements_unique typ_vars) then raise_error ("Non-unique type variables in routine definition '" ^ name ^ "'")
      else if not(parameters_check typ_vars structs params) then raise_error ("illegal parameters in defenition of routine '" ^ name ^ "'")
      else aux t
    )
    | Struct(name,typ_vars,params)::t -> ( 
      if not(elements_unique typ_vars) then raise_error ("Non-unique type variables in struct definition '" ^ name ^ "'")
      else if not(parameters_check typ_vars structs params) then raise_error ("illegal parameters in defenition of struct '" ^ name ^ "'")
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
    | (name, _, _)::t -> if List.mem name seen then raise_error ("Duplicate struct name '" ^ name ^ "'") else aux t (name::seen) 
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
    | ((_,T_Null),_)::_ -> Printf.printf "f\n"; false
    | ((vmod,typ),e)::t -> (
      let (expr_vmod, expr_typ) = type_expr e env contexts in
      if not(type_equal typ expr_typ) then false else
      if vmod = Open && (expr_vmod = Stable || expr_vmod = Const) then false else
      aux t
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
  | _ -> raise_error "Type match failed" 

let resolve_type typ_opt expr (env : environment) contexts : typ =
  match typ_opt with
  | Some(typ) -> ( match typ with
    | T_Generic c -> raise_error ((String.make 1 c) ^ " is not defined in this scope")
    | T_Array(_) -> raise_error "not implemented"
    | T_Struct(name,typ_args) -> (match lookup_struct name env.var_env.structs with
      | None -> raise_error ("No such struct: " ^ name)
      | Some(typ_vars,params) -> ( match expr with
        | Reference(Null) -> raise_error "Cannot infer from null"
        | Reference(ref) -> snd (type_reference ref env contexts)
        | Value(StructLiteral(exprs)) -> (
            let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) exprs env contexts in
            let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars typ_args in
            if not(check_struct_literal params (Value (StructLiteral exprs)) env contexts) then raise_error ("Could not match struct literal with '" ^ type_string (T_Struct(name,typ_args)) ^ "'")
            else (T_Struct(name,typ_args))
        )
        | Value(NewStruct(expr_name,tas,args)) -> (
            if not(expr_name = name) then raise_error "mismatched struct names" else
            let expr_typ_args = resolve_type_args typ_vars tas (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
            let typ_args = List.map (fun x -> (fst x)) (List.combine typ_args expr_typ_args) in
            (T_Struct(name,typ_args))
        )
        | _ -> raise_error "cannot infer"
      )
    )
    | _ -> raise_error "well-definedness checker has failed"
  )
  | None -> let (_, t) = type_expr expr env contexts in t

let declaration_checks vmod typ expr env contexts =
  let (expr_vmod, expr_ty) = type_expr expr env contexts in
  if (Option.is_none typ) && (expr_ty = T_Null) then raise_error "Cannot infere a type from 'null'" else
  let typ = if Option.is_some typ then (if well_defined_type typ env.var_env then Option.get typ else raise_error "Not a well defined type") else expr_ty in
  if vmod = Open && expr_vmod != Open then raise_error "Cannot assign a protected variable to an open variable"
  else if not (type_equal typ expr_ty) then raise_error ("Type mismatch: expected '" ^ (type_string typ) ^ "', got '" ^ (type_string expr_ty) ^ "'")
  else typ

let argument_checks vmod typ expr env contexts =
  let typ = Option.get typ in
  let (expr_vmod, expr_ty) = type_expr expr env contexts in
  if vmod = Open && (expr_vmod != Open) then raise_error "Cannot use a protected variable as an open variable"
  else if vmod = Stable && (expr_vmod = Const) then raise_error "Cannot use a constant variable as a stable parameter"
  else if not (type_equal typ expr_ty) then raise_error ("Type mismatch: expected '" ^ (type_string typ) ^ "', got '" ^ (type_string expr_ty) ^ "'")
  else typ

let type_check checks vmod typ expr (env : environment) contexts =
  match typ, expr with
  | None, Value(StructLiteral(_)) -> raise_error "Cannot infer a type from a struct literal"
  | Some(T_Struct(name,typ_args)), Value(StructLiteral(exprs)) -> ( match lookup_struct name env.var_env.structs with
    | Some(typ_vars,params) ->  (
      let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) exprs env contexts in
      let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars typ_args in
      if not(check_struct_literal params (Value (StructLiteral exprs)) env contexts) then raise_error ("Could not match struct literal with '" ^ type_string (T_Struct(name,typ_args)) ^ "'")
      else (T_Struct(name,typ_args))
    )
    | None -> raise_error ("No such struct '" ^ name ^ "'")
  )
  | Some(T_Struct(name,typ_args)), Value(NewStruct(expr_name,tas,args)) -> ( match lookup_struct name env.var_env.structs with
    | Some(typ_vars,params) ->  (
      if not(expr_name = name) then raise_error "mismatched struct names" else
      let matched_typ_args = List.map (fun ts -> match_typ_arg ts) (List.combine (fix_type_args typ_vars typ_args) (fix_type_args typ_vars tas)) in
      let resolved_typ_args = resolve_type_args typ_vars matched_typ_args (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
      let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars resolved_typ_args in
      if not(List.fold_left (fun acc ((_,param_typ),arg) -> acc && type_equal param_typ (snd(type_expr arg env contexts))) true (List.combine params args)) then raise_error ("Struct argument type mismatch") else
      (T_Struct(name,resolved_typ_args))
    )
    | None -> raise_error ("No such struct '" ^ name ^ "'")
  )
  | None, Value(NewStruct(name,typ_args,args)) -> ( match lookup_struct name env.var_env.structs with
    | Some(typ_vars,params) ->  (
      let resolved_typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) args env contexts in
      let params = replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars resolved_typ_args in
      if not(List.fold_left (fun acc ((_,param_typ),arg) -> acc && type_equal param_typ (snd(type_expr arg env contexts))) true (List.combine params args)) then raise_error ("Struct argument type mismatch") else
      (T_Struct(name,resolved_typ_args))
    )
    | None -> raise_error ("No such struct '" ^ name ^ "'")
  )
  | _ -> checks vmod typ expr env contexts


let argument_type_check = type_check argument_checks
let declaration_type_check = type_check declaration_checks

let assignment_type_check target assign (env : environment) contexts =
  let (target_vmod, target_type) = type_reference target env contexts in
  let (assign_vmod, assign_type) = match assign with
  | Value(StructLiteral(exprs)) -> ( match target_type with
    | T_Struct(name, typ_args) -> ( match lookup_struct name env.var_env.structs with
      | Some(typ_vars, params) -> (
        let typ_args = resolve_type_args typ_vars typ_args (List.map (fun (a,b,_) -> (a,b)) params) exprs env contexts in
        if not(check_struct_literal (replace_generics (List.map (fun (a,b,_) -> (a,b)) params) typ_vars typ_args) (Value(StructLiteral exprs)) env contexts) then raise_error "Structure mismatch in assignment"
        else (Open, T_Struct(name, typ_args))
      )
      | None -> raise_error ("No such struct '" ^ name ^ "'")
    )
    | _ -> raise_error ("Struct literal assignment to a variable of type '" ^ type_string target_type ^ "'")
  )
  | _ -> (
    let (assign_vmod, assign_type) = type_expr assign env contexts in
    if not (type_equal target_type assign_type) then raise_error ("Type mismatch in assignment, expected '"^(type_string target_type)^"' but got '" ^(type_string assign_type)^ "'") 
    else (assign_vmod, assign_type)
  )
  in
  match target_vmod with
  | Open -> (
    if assign_vmod != Open then raise_error "Assignment of protected variable, to non-protected variable"
    else assign_type
  )
  | Stable -> ( match assign with
    | Value _ -> raise_error "Attempt to overwrite stable data"
    | Reference _ -> assign_type
  )
  | Const -> raise_error "Assignment to a protected variable"


(* Might be useful for infering struct arrays
let rec meme typ_vars typ_args params array_exprs var_env =
  match array_exprs with
  | [] -> raise_error "Fuck dig"
  | Value(StructLiteral(exprs))::t -> ( try (
    let typ_args = resolve_type_args typ_vars typ_args params exprs var_env in
    let params = replace_generics params typ_vars typ_args in
    if not(List.fold_left (fun acc array_expr -> check_struct_literal params array_expr var_env && acc) true array_exprs) then raise_error ("?1")
    else typ_args
  ) with _ -> meme typ_vars typ_args params t var_env)
  | _::t -> meme typ_vars typ_args params t var_env   
*)