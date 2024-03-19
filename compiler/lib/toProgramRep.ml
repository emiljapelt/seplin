open Absyn
open ProgramRep
open Exceptions
open Typing
open Optimize
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

let rec ternary_is_reference expr1 expr2 =
  match expr1, expr2 with
  | Reference _, Reference _ -> true
  | Ternary(_,t1e1,t1e2), Ternary(_,t2e1,t2e2) -> ternary_is_reference t1e1 t1e2 && ternary_is_reference t2e1 t2e2
  | Ternary(_,e1,e2), Reference _ -> ternary_is_reference e1 e2
  | Reference _ , Ternary(_,e1,e2) -> ternary_is_reference e1 e2
  | _,_ -> false

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

(*    list of: string * char list * (bool * typ * string) list   *)
(*let get_structs file =
  let rec aux topdecs acc =
    match topdecs with
    | [] -> acc
    | h::t -> (
      match h with
      | Struct (name, typ_vars, params) -> (
        if struct_exists name acc then raise_failure ("Duplicate struct name: " ^ name)
        else aux t ((name, typ_vars, params)::acc)
        )
      | _ -> aux t acc
    )
  in match file with
  | File (tds) -> aux tds []*)


(*** Global variable handling ***)
(*    Compute the list of variable dependencies for each global variable    *)
let get_globvar_dependencies gvs =
  let rec dependencies_from_assignable expr acc =
    match expr with
    | Reference Null -> acc
    | Reference OtherContext _ -> raise_failure ("Global variables cannot depend on other contexts")
    | Reference LocalContext Access name -> name::acc
    | Reference LocalContext ArrayAccess(refer,_) -> dependencies_from_assignable (Reference(LocalContext refer)) acc
    | Reference LocalContext StructAccess(refer,_) -> dependencies_from_assignable (Reference(LocalContext refer)) acc
    | Value Binary_op (_, expr1, expr2) -> dependencies_from_assignable expr1 (dependencies_from_assignable expr2 acc)
    | Value Unary_op (_, expr1) -> dependencies_from_assignable expr1 acc
    | Value ArraySize (refer) -> dependencies_from_assignable (Reference(LocalContext refer)) acc
    | Value Bool _ -> acc
    | Value Int _ -> acc
    | Value Char _ -> acc
    | Value GetInput _ -> acc
    | Value NewArray (_,expr1) -> dependencies_from_assignable expr1 acc
    | Value ArrayLiteral exprs -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
    | Value NewStruct (_,_,exprs) -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
    | Value StructLiteral (exprs) -> List.fold_right (fun e a -> dependencies_from_assignable e a) exprs []
    | Value AnonRoutine _ -> acc
    | Ternary (cond, expr1, expr2) -> dependencies_from_assignable cond (dependencies_from_assignable expr1 (dependencies_from_assignable expr2 acc))
  in
  let dependencies_from_declaration dec =
    match dec with
    | TypeDeclaration _ -> []
    | AssignDeclaration (_,_,_,expr) -> dependencies_from_assignable expr []
  in
  List.map (fun (accmod,name,context_name,varmod,ty,dec) -> ((accmod,name,context_name,varmod,ty,dec), dependencies_from_declaration dec)) gvs

let extract_name t =
  match t with
  | (_,f,_,_,_,_,_) -> f

(*    Compute an ordering of the global variables, according to their dependencies    *)
let order_dep_globvars dep_gvs =
  let rec aux dep_globvars count prev_count remain acc =
    match dep_globvars with
    | [] when remain = [] -> acc
    | [] when count = prev_count -> raise_failure "Could not resolve a global variable order, there might be a circular dependency"
    | [] -> aux remain count count [] acc
    | h::t -> ( match h with
      | ((accmod,name,context,varmod,ty,dec), deps) -> (
        if List.for_all (fun dep -> List.exists (fun a -> dep = extract_name a) acc) deps then aux t (count+1) prev_count remain ((accmod,name,context,count,varmod,ty,dec)::acc)
        else aux t count prev_count (h::remain) acc
      )
    )
  in
  List.rev (aux dep_gvs 0 0 [] [])

let gather_globvar_info gvs =
  List.map (fun (accmod,name,_,idx,varmod,ty,_) -> (accmod, varmod, ty, idx, name)) gvs


(*** Compiling functions ***)

let fetch_var_index (name: string) globvars localvars = 
  match lookup_localvar name localvars with
  | Some (lc,_,_) -> BPFetch(lc)
  | None -> 
    match lookup_globvar name globvars with
    | Some (_,gc,_,_) -> StackFetch(gc)
    | None -> raise_failure ("No such variable '" ^ name ^ "'")

let rec compile_expr expr (op_typ: op_typ) var_env contexts acc =
  match expr with
  | Reference ref_expr -> compile_reference ref_expr var_env contexts acc
  | Value val_expr -> compile_value val_expr op_typ var_env contexts acc
  | Ternary(cond,exp1,exp2) -> (
    match op_typ with
    | TernaryOp_T(op_typ_cond, op_typ1, op_typ2) -> (
      let label_true = Helpers.new_label () in
      let label_end = Helpers.new_label () in
      compile_expr_as_value cond op_typ_cond var_env contexts (IfTrue(label_true) :: compile_expr_as_value exp2 op_typ2 var_env contexts (GoTo(label_end) :: CLabel(label_true) :: compile_expr_as_value exp1 op_typ1 var_env contexts (CLabel(label_end) :: acc)))
    )
    | _ -> raise_failure "Failed compilation of ternary"
  )

and compile_inner_reference iref env contexts acc = 
  match iref with
  | Access name -> (fetch_var_index name env.var_env.globals env.var_env.locals) :: RefFetch :: acc
  | StructAccess (refer, field) -> ( 
    let (_, ref_ty) = Typing.type_inner_reference refer env contexts in
    match ref_ty with
    | Ok T_Struct (name, _) -> (
      match lookup_struct name env.var_env.structs with
      | Some (_, params) -> (
        let (_, _, index) = struct_field field params in
        compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: FieldFetch :: acc)
      )
      | None -> raise_failure ("No such struct '" ^ name ^ "'")
    )
    | Ok _ -> raise_failure ("Struct field lookup on non-struct reference")
    | Error m -> raise_failure m
  )
  | ArrayAccess (refer, index) -> (
    match type_expr index env contexts with
    | _, Ok op_typ -> ( match translate_operational_type op_typ with
      | T_Int -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index op_typ env contexts (FieldFetch :: acc)))
      | _ -> raise_failure "Array index with non-int type"
    )
    | _, Error msg -> raise_failure msg
  )

and compile_reference ref_expr (env : environment) (contexts : context list) acc =
  match ref_expr with
  | Null -> PlaceFull(C_Int 0) :: acc
  | LocalContext ref -> compile_inner_reference ref env contexts acc
  | OtherContext(cn,ref) -> ( match lookup_context cn env.file_refs contexts with
    | None -> raise_failure ("No such environment: " ^cn)
    | Some(env) -> compile_inner_reference ref env contexts acc
  )

and compile_expr_as_value expr (op_typ: op_typ) (env : environment) contexts acc =
  match expr with
  | Reference r -> (
    match translate_operational_type op_typ with
    | T_Int -> compile_reference r env contexts (FetchFull :: FetchFull :: acc)
    | T_Bool -> compile_reference r env contexts (FetchFull :: FetchByte :: acc)
    | T_Char -> compile_reference r env contexts (FetchFull :: FetchByte :: acc)
    | T_Array _ -> compile_reference r env contexts (FetchFull :: acc)
    | T_Struct _ -> compile_reference r env contexts (FetchFull :: acc)
    | T_Generic _ -> compile_reference r env contexts (FetchFull :: acc)
    | T_Null -> compile_reference r env contexts acc
    | T_Routine _ -> compile_reference r env contexts (FetchFull :: FetchFull :: acc)
  )
  | _ -> compile_expr expr op_typ env contexts acc

and compile_structure_arg arg (op_typ:op_typ) idx env contexts acc =
  let optha = optimize_expr arg env in
  match optha with
  | Value(AnonRoutine _) -> (
    CloneFull :: PlaceFull(C_Int idx) :: DeclareFull :: IncrRef :: CloneFull :: compile_expr optha op_typ env contexts (AssignFull :: FieldAssign :: acc)
  )
  | Value _ -> (
    match translate_operational_type op_typ with
    | T_Int -> (CloneFull :: PlaceFull(C_Int idx) :: DeclareFull :: IncrRef :: CloneFull :: compile_expr optha op_typ env contexts (AssignFull :: FieldAssign :: acc))
    | T_Char -> (CloneFull :: PlaceFull(C_Int idx) :: DeclareFull :: IncrRef :: CloneFull :: compile_expr optha op_typ env contexts (AssignByte :: FieldAssign :: acc))
    | T_Bool -> (CloneFull :: PlaceFull(C_Int idx) :: DeclareFull :: IncrRef :: CloneFull :: compile_expr optha op_typ env contexts (AssignByte :: FieldAssign :: acc))
    | _ -> (CloneFull :: PlaceFull(C_Int idx) :: compile_expr optha op_typ env contexts (IncrRef :: FieldAssign :: acc))
  )
  | Reference Null -> (CloneFull :: PlaceFull(C_Int idx) :: compile_expr optha op_typ env contexts (FieldAssign :: acc))
  | Reference _ -> (CloneFull :: PlaceFull(C_Int idx) :: compile_expr optha op_typ env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))
  | Ternary(cond,expr1,expr2) -> ( let cond = optimize_expr cond env in 
      match cond with
      | (Value(Bool true)) -> compile_structure_arg expr1 op_typ idx env contexts acc
      | (Value(Bool false)) -> compile_structure_arg expr2 op_typ idx env contexts acc
      | _ -> ( match type_expr cond env contexts with
        | (_, Error msg) -> raise_failure msg
        | (_, Ok ot) -> ( match translate_operational_type ot with
          | T_Bool -> (
            let label_true = new_label () in
            let label_end = new_label () in
            compile_expr_as_value cond (ot) env contexts ((IfTrue label_true) :: (compile_structure_arg expr2 op_typ idx env contexts ((GoTo label_end) :: (CLabel label_true) :: (compile_structure_arg expr1 op_typ idx env contexts ((CLabel label_end) :: acc)))))
            )
          | _ -> raise_failure "Not bool"
        )
      )
    )

and compile_structure args typs var_env contexts acc =
  PlaceFull(C_Int (List.length args)) :: DeclareStruct :: (List.fold_left (fun acc (typ, (arg, c)) -> compile_structure_arg arg typ c var_env contexts acc) acc (List.combine typs (List.mapi (fun i a -> (a,i)) args)))

and compile_value val_expr (op_typ: op_typ) env contexts acc =
  match val_expr with
  | Bool b -> PlaceByte(C_Bool b) :: acc
  | Int i -> PlaceFull(C_Int i) :: acc
  | Char c -> PlaceByte(C_Char c) :: acc
  | ArraySize refer -> compile_inner_reference refer env contexts (FetchFull :: SizeOf :: acc)
  | GetInput ty -> GetInput(type_input_index ty) :: acc
  | NewArray (_, size_expr) -> (
    compile_expr_as_value (optimize_expr size_expr env) (NOp_T T_Int) env contexts (DeclareStruct :: IncrRef :: acc)
  )
  | ArrayLiteral exprs -> ( match translate_operational_type op_typ with
    | T_Array st -> (
      let typs = List.map (fun _ -> NOp_T (Option.get st)) exprs in
      compile_structure exprs typs env contexts acc 
    )
    | _ -> raise_failure "Not an array type"
  )
  | NewStruct (_, _, exprs)
  | StructLiteral (exprs) -> ( match translate_operational_type op_typ with
    | T_Struct(name,typ_args) -> ( match lookup_struct name env.var_env.structs with
      | Some(tvs,ps) -> ( match replace_generics (List.map (fun (a,b,_) -> (a,b)) ps) tvs typ_args with
        | Ok typs -> compile_structure exprs (List.map (fun (_,t) -> NOp_T t) typs) env contexts acc 
        | Error m -> raise_failure (m^"what is going on")
      )
       (* let typs = List.map (fun t -> match t with Ok(_,t) -> NOp_T t | _ -> raise_failure ":(") () in
        compile_structure exprs typs var_env contexts acc *)
      | None -> raise_failure ("No such struct: "^name)
    )
    | _ -> raise_failure "Not a struct type"
  )
  | Binary_op (op, e1, e2) -> ( match op, e1, e2 with
    | "=", Reference r, Reference Null -> compile_reference r env contexts (FetchFull :: PlaceFull(C_Int 0) :: FullEq :: acc)
    | "=", Reference Null, Reference r -> compile_reference r env contexts (FetchFull :: PlaceFull(C_Int 0) :: FullEq :: acc)
    | "!=", Reference r, Reference Null -> compile_reference r env contexts (FetchFull :: PlaceFull(C_Int 0) :: FullEq :: BoolNot :: acc)
    | "!=", Reference Null, Reference r -> compile_reference r env contexts (FetchFull :: PlaceFull(C_Int 0) :: FullEq :: BoolNot :: acc)
    | _ -> (
      match op_typ with
      | BinOp_T(op, ot1, ot2) -> ( match op, translate_operational_type ot1, translate_operational_type ot2 with
        | "&&", T_Bool, T_Bool -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts(BoolAnd :: acc))
        | "||", T_Bool, T_Bool -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts(BoolOr :: acc))
        | "=", T_Bool, T_Bool -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (BoolEq :: acc))
        | "=", T_Char, T_Char -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (ByteEq :: acc))
        | "=", T_Int, T_Int -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (FullEq :: acc))
        | "!=", T_Bool, T_Bool -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (BoolEq :: BoolNot :: acc))
        | "!=", T_Char, T_Char -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (ByteEq :: BoolNot :: acc))
        | "!=", T_Int, T_Int -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (FullEq :: BoolNot :: acc))
        | "<=", T_Int, T_Int -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (IntLt :: BoolNot :: acc))
        | "<", T_Int, T_Int -> compile_expr_as_value e2 ot2 env contexts (compile_expr_as_value e1 ot1 env contexts (IntLt :: acc))
        | ">=", T_Int, T_Int -> compile_expr_as_value e2 ot2 env contexts (compile_expr_as_value e1 ot1 env contexts (IntLt :: BoolNot :: acc))
        | ">", T_Int, T_Int -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (IntLt :: acc))
        | "+", T_Int, T_Int -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (IntAdd :: acc))
        | "-", T_Int, T_Int -> compile_expr_as_value e2 ot2 env contexts (compile_expr_as_value e1 ot1 env contexts (IntSub :: acc))
        | "*", T_Int, T_Int -> compile_expr_as_value e1 ot1 env contexts (compile_expr_as_value e2 ot2 env contexts (IntMul :: acc))
        | _ -> raise_failure "Unknown binary operation"
      )
      | _ -> raise_failure "Not a binary operation"
    )
  )
  | Unary_op (_, e) -> (
    match op_typ with
    | UnOp_T(op, ot) -> ( match op, translate_operational_type ot with 
      | "!", T_Bool -> compile_expr_as_value e ot env contexts (BoolNot :: acc)
      | "$", _ -> if e = Reference(Null) then raise_failure "Direct null dereference" else compile_expr_as_value e ot env contexts acc
      | _ -> raise_failure "Unknown unary operation"
    )
    | _ -> raise_failure "Not a unary operation"
  )
  | AnonRoutine(tvs,params,stmt) -> (
    if not(elements_unique tvs) then raise_failure ("Non-unique type variables in anonymous routine")
    else if not(parameters_check tvs env.var_env.structs (List.map (fun (a,b,_) -> (a,b)) params)) then raise_failure ("illegal parameters in anonymous routine")
    else let label = new_label () in
    let skip = new_label () in
    CPlaceLabel label :: GoTo skip :: CLabel label :: compile_stmt stmt {env with var_env = ({env.var_env with locals = List.rev params; typ_vars = tvs}) } contexts None None 0 (addStop(CLabel skip :: acc))
  )

and compile_argument arg (env : environment) contexts acc =
  match arg with ((pmod, pty),eh) -> (
      let opteh = optimize_expr eh env.var_env in
      let typ_res = argument_type_check pmod (Some pty) opteh env contexts in
      let op_typ = if Result.is_ok typ_res then Result.get_ok typ_res else raise_failure (Result.get_error typ_res) in
      let typ = translate_operational_type op_typ in
      match opteh with
      | Value _ -> ( match typ with
        | T_Int -> DeclareFull :: IncrRef :: CloneFull :: (compile_expr_as_value opteh op_typ env contexts (AssignFull :: acc))
        | T_Bool -> DeclareByte :: IncrRef :: CloneFull :: (compile_expr_as_value opteh op_typ env contexts (AssignByte :: acc))
        | T_Char -> DeclareByte :: IncrRef :: CloneFull :: (compile_expr_as_value opteh op_typ env contexts (AssignByte :: acc))
        | T_Array _ -> compile_expr_as_value opteh op_typ env contexts (IncrRef :: acc)
        | T_Struct _ -> compile_expr_as_value opteh op_typ env contexts (IncrRef :: acc)
        | T_Null -> compile_expr_as_value opteh op_typ env contexts (acc)
        | T_Generic _ -> compile_expr_as_value opteh op_typ env contexts (IncrRef :: acc)
        | T_Routine _ -> DeclareFull :: IncrRef :: CloneFull :: (compile_expr_as_value opteh op_typ env contexts (AssignFull :: acc))
      )
      | Reference LocalContext ref -> ( match ref with
        | Access _ -> compile_inner_reference ref env contexts ((*FetchFull ::*) IncrRef :: acc)
        | StructAccess _ -> compile_inner_reference ref env contexts (FetchFull :: IncrRef :: acc)
        | ArrayAccess _ -> compile_inner_reference ref env contexts (FetchFull :: IncrRef :: acc)
      )
      | Reference OtherContext (cn,ref) -> ( match lookup_context cn env.file_refs contexts with
        | None -> raise_failure ("No such context:"^cn)
        | Some(env) -> ( match ref with
          | Access _ -> compile_inner_reference ref env contexts ((*FetchFull ::*) IncrRef :: acc)
          | StructAccess _ -> compile_inner_reference ref env contexts (FetchFull :: IncrRef :: acc)
          | ArrayAccess _ -> compile_inner_reference ref env contexts (FetchFull :: IncrRef :: acc)
        )
        )
      | Reference Null -> compile_reference Null env contexts acc
      | Ternary(cond,expr1,expr2) -> ( let cond = optimize_expr cond env in 
        match cond with
        | (Value(Bool true)) -> compile_argument ((pmod, pty),expr1) env contexts acc
        | (Value(Bool false)) -> compile_argument ((pmod, pty),expr2) env contexts acc
        | _ -> ( match type_expr cond env contexts with
          | (_, Error msg) -> raise_failure msg
          | (_, Ok ot) -> ( match translate_operational_type ot with
            | T_Bool -> (
              let label_true = new_label () in
              let label_end = new_label () in
              compile_expr_as_value cond ot env contexts ((IfTrue label_true) :: (compile_argument ((pmod, pty),expr2) env contexts ((GoTo label_end) :: (CLabel label_true) :: (compile_argument ((pmod, pty),expr1) env contexts ((CLabel label_end) :: acc)))))
              )
            | _ -> raise_failure "Not bool"
          )
        )
    )
    )

and compile_arguments args (env : environment) contexts acc =
  let rec aux ars acc =
    match ars with
    | [] -> acc
    | h::t -> aux t (compile_argument h env contexts acc)
  in
  aux (List.rev args) acc

and compile_assignment target assign (env : environment) contexts acc =
  let assign = optimize_expr assign env in
  let assign_type_res = Typing.assignment_type_check target assign env contexts in
  match assign_type_res with
  | Error m -> raise_failure m
  | Ok assign_type -> (
    let target_ref = match target with
      | Null -> raise_failure "Assignment to null"
      | LocalContext(r) -> r
      | OtherContext(cn,r) -> ( match lookup_context cn env.file_refs contexts with
        | None -> raise_failure ("No such context: " ^ cn)
        | Some _ -> r
      )
    in
    match target_ref, assign with
    | (_, Ternary(cond,expr1,expr2)) -> ( let cond = optimize_expr cond env in 
      match cond with
      | (Value(Bool true)) -> compile_assignment target expr1 env contexts acc
      | (Value(Bool false)) -> compile_assignment target expr2 env contexts acc
      | _ -> ( match type_expr cond env contexts with
        | (_, Error msg) -> raise_failure msg
        | (_, Ok ot) -> ( match translate_operational_type ot with
          | T_Bool -> (
            let label_true = new_label () in
            let label_end = new_label () in
            compile_expr_as_value cond (ot) env contexts ((IfTrue label_true) :: (compile_assignment target expr2 env contexts ((GoTo label_end) :: (CLabel label_true) :: (compile_assignment target expr1 env contexts ((CLabel label_end) :: acc)))))
            )
          | _ -> raise_failure "Not bool"
        )
      )
    )
    | (Access _, Value v) -> ( match translate_operational_type assign_type with 
      | T_Int ->  compile_reference target env contexts (FetchFull :: (compile_value v assign_type env contexts (AssignFull :: acc)))
      | T_Bool -> compile_reference target env contexts (FetchFull :: (compile_value v assign_type  env contexts (AssignByte :: acc)))
      | T_Char -> compile_reference target env contexts (FetchFull :: (compile_value v assign_type  env contexts (AssignByte :: acc)))
      | T_Array _ -> compile_reference target env contexts (compile_value v assign_type  env contexts (IncrRef :: RefAssign :: acc))
      | T_Struct _ -> compile_reference target env contexts (compile_value v assign_type  env contexts (IncrRef :: RefAssign :: acc))
      | T_Null -> compile_reference target env contexts (compile_value v assign_type  env contexts (RefAssign :: acc))
      | T_Generic _ -> compile_reference target env contexts (compile_value v assign_type  env contexts (IncrRef :: RefAssign :: acc))
      | T_Routine _ -> compile_reference target env contexts (FetchFull::(compile_value v assign_type env contexts (AssignFull :: acc))
      )
    )
    | (Access _, Reference re) -> ( match translate_operational_type assign_type with 
      | T_Int -> compile_reference target env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: RefAssign :: acc))
      | T_Bool -> compile_reference target env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: RefAssign :: acc))
      | T_Char -> compile_reference target env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: RefAssign :: acc))
      | T_Array _ -> compile_reference target env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: RefAssign :: acc))
      | T_Struct _ -> compile_reference target env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: RefAssign :: acc))
      | T_Null -> compile_reference target env contexts (compile_reference re env contexts (RefAssign :: acc))
      | T_Generic _ -> compile_reference target env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: RefAssign :: acc))
      | T_Routine _ -> compile_reference target env contexts (FetchFull :: compile_reference re env contexts (FetchFull:: FetchFull :: IncrRef :: RefAssign :: acc))
    )
    | (StructAccess(refer, field), Value v) -> ( match Typing.type_inner_reference refer env contexts with
      | (_,Ok T_Struct (str_name, _)) -> ( match lookup_struct str_name env.var_env.structs with
        | None -> raise_failure ("Could not find struct '" ^ str_name ^ "'")
        | Some (_, fields) -> ( match struct_field field fields with
          | (_,_,index) -> ( match translate_operational_type assign_type with
            | T_Int -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: FieldFetch :: FetchFull :: (compile_value v assign_type  env contexts (AssignFull :: acc)))
            | T_Bool -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: FieldFetch :: FetchFull :: (compile_value v assign_type  env contexts (AssignByte :: acc)))
            | T_Char -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: FieldFetch :: FetchFull :: (compile_value v assign_type  env contexts (AssignByte :: acc)))
            | T_Array _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_value v assign_type  env contexts (IncrRef :: FieldAssign :: acc)))
            | T_Struct _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_value v assign_type  env contexts (IncrRef :: FieldAssign :: acc)))
            | T_Null  -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_value v assign_type  env contexts (FieldAssign :: acc)))
            | T_Generic _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_value v assign_type  env contexts (IncrRef :: FieldAssign :: acc)))
            | T_Routine _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: FieldFetch :: FetchFull :: (compile_value v assign_type  env contexts (AssignFull :: acc)))
          )
        )
      )
      | (_, Ok t) -> raise_failure ("Struct field assignment to variable of type '" ^ Typing.type_string t ^ "'") 
      | (_, Error m) -> raise_failure m
    )
    | (StructAccess(refer, field), Reference re) -> ( match Typing.type_inner_reference refer env contexts with
      | (_,Ok T_Struct (str_name, _)) -> ( match lookup_struct str_name env.var_env.structs with
        | None -> raise_failure ("Could not find struct '" ^ str_name ^ "'")
        | Some (_, fields) -> ( match struct_field field fields with
          | (_, _, index) -> ( match translate_operational_type assign_type with
            | T_Int -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc)))
            | T_Bool -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc)))
            | T_Char -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc)))
            | T_Array _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc)))
            | T_Struct _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc)))
            | T_Null  -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FieldAssign :: acc)))
            | T_Generic _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc)))
            | T_Routine _ -> compile_inner_reference refer env contexts (FetchFull :: PlaceFull(C_Int index) :: (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc)))
          )
        )
      )
      | (_,Ok t) -> raise_failure ("Struct field assignment to variable of type '" ^ Typing.type_string t ^ "'") 
      | (_, Error m) -> raise_failure m
    )
    | (ArrayAccess(refer, index), Value v) -> ( match Typing.type_inner_reference refer env contexts with
      | (_,Ok T_Array _) -> ( 
        let index = optimize_expr index env in
        let (_,index_type_res) = Typing.type_expr index env contexts in
        match index_type_res with
        | Ok index_ot -> ( match translate_operational_type index_ot with 
          | T_Int -> ( match translate_operational_type assign_type with
            | T_Int -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (FieldFetch :: FetchFull :: (compile_value v assign_type env contexts (AssignFull :: acc)))))
            | T_Bool -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (FieldFetch :: FetchFull :: (compile_value v assign_type  env contexts (AssignByte :: acc)))))
            | T_Char -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (FieldFetch :: FetchFull :: (compile_value v assign_type  env contexts (AssignByte :: acc)))))
            | T_Array _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_value v assign_type  env contexts (IncrRef :: FieldAssign :: acc))))
            | T_Struct _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_value v assign_type  env contexts (IncrRef :: FieldAssign :: acc))))
            | T_Null -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_value v assign_type  env contexts (FieldAssign :: acc))))
            | T_Generic _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_value v assign_type  env contexts (IncrRef :: FieldAssign :: acc))))
            | T_Routine _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (FieldFetch :: FetchFull :: (compile_value v assign_type env contexts (AssignFull :: acc)))))
          )
          | _ -> raise_failure "Array index must be of type 'int'"
        )
        | Error m -> raise_failure m
        )
      | (_,Ok t) -> raise_failure ("Array assignment to variable of type '" ^ Typing.type_string t ^ "'") 
      | (_,Error m) -> raise_failure m
    )
    | (ArrayAccess(refer, index), Reference re) -> ( match Typing.type_inner_reference refer env contexts with
      | (_,Ok T_Array _) -> ( 
        let index = optimize_expr index env in
        let (_,index_type_res) = Typing.type_expr index env contexts in
        match index_type_res with
        | Ok index_ot -> ( match translate_operational_type index_ot with
          | T_Int -> ( match translate_operational_type assign_type with
            | T_Int -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))))
            | T_Bool -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))))
            | T_Char -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))))
            | T_Array _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))))
            | T_Struct _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))))
            | T_Null -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FieldAssign :: acc))))
            | T_Generic _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))))
            | T_Routine _ -> compile_inner_reference refer env contexts (FetchFull :: (compile_expr_as_value index index_ot env contexts (compile_reference re env contexts (FetchFull :: IncrRef :: FieldAssign :: acc))))
          )
          | _ -> raise_failure "Array index must be of type 'int'"
        )
        | Error m -> raise_failure m
        )
      | (_,Ok t) -> raise_failure ("Array assignment to variable of type '" ^ Typing.type_string t ^ "'") 
      | (_, Error m) -> raise_failure m
    )
  )

and compile_declaration dec env contexts =
  match dec with
  | TypeDeclaration (vmod, typ, name) -> (
    if localvar_exists name env.var_env.locals then raise_failure ("Duplicate variable name '" ^ name ^ "'")
    else if not(well_defined_type (Some typ) env.var_env) then raise_failure "Ill defined type"
    else 
    ( Helpers.update_locals env vmod typ name,
      match typ with
      | T_Int -> fun a -> DeclareFull :: IncrRef :: CloneFull :: PlaceFull(C_Int 0) :: AssignFull :: a
      | T_Bool -> fun a -> DeclareByte :: IncrRef :: CloneFull :: PlaceByte(C_Bool false) :: AssignByte :: a
      | T_Char -> fun a -> DeclareByte :: IncrRef :: CloneFull :: PlaceByte(C_Char '0') :: AssignByte :: a
      | T_Array _
      | T_Struct _
      | T_Routine _ 
      | T_Generic _ -> fun a -> PlaceFull(C_Int 0) :: a
      | T_Null -> raise_failure "Cannot declare the 'null' type"
    )
  )
  | AssignDeclaration (vmod, typ, name, expr) -> (
    if localvar_exists name env.var_env.locals then raise_failure ("Duplicate variable name '" ^ name ^ "'") ;
    let opt_expr = optimize_expr expr env.var_env in
    let typ_res = declaration_type_check vmod typ opt_expr env contexts in
    let o_typ = if Result.is_ok typ_res then Result.get_ok typ_res else raise_failure (Result.get_error typ_res) in
    let typ = translate_operational_type o_typ in
    ( update_locals env vmod typ name,
      match opt_expr with
      | Reference(LocalContext(Access _)) -> fun a -> compile_expr opt_expr o_typ env contexts (FetchFull :: IncrRef :: a)
      | Reference(OtherContext(_, Access _)) -> fun a -> compile_expr opt_expr o_typ env contexts (FetchFull :: IncrRef :: a)
      | Reference _ -> fun a -> compile_expr opt_expr o_typ env contexts (IncrRef :: a)
      | Value v -> (
        match typ with
        | T_Int -> fun a -> DeclareFull :: IncrRef :: CloneFull :: (compile_expr opt_expr o_typ env contexts (AssignFull :: a))
        | T_Bool -> fun a -> DeclareByte :: IncrRef :: CloneFull :: (compile_expr opt_expr o_typ env contexts (AssignByte :: a))
        | T_Char -> fun a -> DeclareByte :: IncrRef :: CloneFull :: (compile_expr opt_expr o_typ env contexts (AssignByte :: a))
        | T_Array _ -> fun a -> compile_expr opt_expr o_typ env contexts (IncrRef :: a)
        | T_Struct _ -> fun a -> compile_expr opt_expr o_typ env contexts (IncrRef :: a)
        | T_Generic _ -> fun a -> compile_expr opt_expr o_typ env contexts (IncrRef :: a)
        | T_Null -> fun a -> compile_expr opt_expr o_typ env contexts a
        | T_Routine _ -> fun a -> DeclareFull :: IncrRef :: CloneFull :: (compile_value v o_typ env contexts (AssignFull :: a))
      )
      | Ternary(cond,expr1,expr2) -> (
        let cond = optimize_expr cond env in
        match cond with
        | Value(Bool true) -> let (_,f) = compile_declaration (AssignDeclaration(vmod,Some typ,name,expr1)) env contexts in f
        | Value(Bool false) -> let (_,f) = compile_declaration (AssignDeclaration(vmod,Some typ,name,expr2)) env contexts in f
        | _ -> ( match type_expr cond env contexts with
            | (_, Error msg) -> raise_failure msg
            | (_, Ok ot) -> ( match translate_operational_type ot with
              | T_Bool -> (
                let label_true = new_label () in
                let label_end = new_label () in
                let (_,f_true) = compile_declaration (AssignDeclaration(vmod,Some typ,name,expr1)) env contexts in
                let (_,f_false) = compile_declaration (AssignDeclaration(vmod,Some typ,name,expr2)) env contexts in
                fun a -> compile_expr_as_value cond ot env contexts ((IfTrue label_true) :: (f_false ((GoTo label_end) :: (CLabel label_true) :: (f_true ((CLabel label_end) :: a)))))
                )
              | _ -> raise_failure "Not bool"
            )
        )
      )
    )
  )

and compile_sod_list sod_list env contexts break continue cleanup acc =
  match sod_list with
  | [] -> acc
  | h::t -> (
    match h with
    | Statement(stmt, line) -> ( try (
        compile_stmt stmt env contexts break continue cleanup (compile_sod_list t env contexts break continue cleanup (acc))
      ) with
      | Failure(_,line_opt,expl) when Option.is_none line_opt -> raise (Failure(None, Some(line), expl))
      | e -> raise e
    )
    | Declaration(dec, line) -> ( try (
        let (new_env, f) = compile_declaration dec env contexts in
        f (compile_sod_list t new_env contexts break continue (cleanup+1) acc)
      ) with
      | Failure(_,line_opt,expl) when Option.is_none line_opt -> raise (Failure(None, Some(line), expl))
      | e -> raise e
    )
  )

and compile_stmt stmt env contexts break continue cleanup acc =
  match stmt with
  | If (expr, s1, s2) -> (
    let label_true = Helpers.new_label () in
    let label_stop = Helpers.new_label () in
    let opt_expr = optimize_expr expr env in
    let (_, t) = Typing.type_expr opt_expr env contexts in
    match t with
    | Ok ot -> ( match translate_operational_type ot with 
      | T_Bool -> ( match opt_expr with 
        | Value(Bool true) -> (compile_stmt s1 env contexts break continue cleanup acc)
        | Value(Bool false) -> (compile_stmt s2 env contexts break continue cleanup acc)
        | _ -> compile_expr_as_value expr ot env contexts (IfTrue(label_true) :: (compile_stmt s2 env contexts break continue cleanup (GoTo(label_stop) :: CLabel(label_true) :: (compile_stmt s1 env contexts break continue cleanup (CLabel(label_stop) :: acc)))))
      )
      | _ -> raise_failure "Condition not of type 'bool'"
    )
    | Error m -> raise_failure m
  )
  | While (expr, s) -> (
    let label_cond = Helpers.new_label () in
    let label_start = Helpers.new_label () in
    let label_stop = Helpers.new_label () in
    let opt_expr = optimize_expr expr env in
    let (_, t) = Typing.type_expr opt_expr env contexts in
    match t with
    | Ok ot -> ( match translate_operational_type ot with
      | T_Bool -> ( match opt_expr with 
        | Value(Bool true) -> CLabel(label_start) :: (compile_stmt s env contexts (Some label_stop) (Some label_start) 0 (CLabel(label_cond) :: (GoTo(label_start) :: CLabel(label_stop) :: acc)))
        | Value(Bool false) -> acc
        | _ -> GoTo(label_cond) :: CLabel(label_start) :: (compile_stmt s env contexts (Some label_stop) (Some label_cond) 0 (CLabel(label_cond) :: (compile_expr_as_value opt_expr ot env contexts (IfTrue(label_start) :: CLabel(label_stop) :: acc))))
      )
      | _ -> raise_failure "Condition not of type 'bool'"
    )
    | Error m -> raise_failure m
  )
  | Block (sod_list) -> (
    let decs = count_decl sod_list in
    if decs = 0 then compile_sod_list sod_list env contexts break continue cleanup acc
    else compile_sod_list sod_list env contexts break continue cleanup (addFreeVars decs acc)
  )
  | Assign (target, aexpr) -> compile_assignment target (optimize_expr aexpr env.var_env) env contexts acc
  | Call (ref, typ_args, args) -> ( 
    let (typ_vars,params,call_f,env) = match ref with
    | Null -> raise_failure ("Call to 'null'")
    | OtherContext (cn,Access n) -> ( match lookup_context cn env.file_refs contexts with
      | None -> raise_failure ("No such context: "^cn)
      | Some(cenv) -> match lookup_globvar n cenv.var_env.globals with
        | None -> raise_failure ("No such routine '" ^n^ "' in context '" ^cn^ "'" )
        | Some(Internal,_,_,_) -> raise_failure ("Internal access of other context")
        | Some(_,_,Some T_Routine(tvs,ps),_) -> (tvs, ps, (fun acc -> compile_expr_as_value (Reference ref) (NOp_T(T_Routine(tvs,ps))) env contexts (Call :: acc)),env)
        | Some _ -> raise_failure (n ^ " is not a routine in " ^ cn)
    )
    | LocalContext(Access n) -> ( 
      if (localvar_exists n env.var_env.locals) || (globvar_exists n env.var_env.globals) then match type_inner_reference (Access n) env contexts with
        | (_, Ok T_Routine(tvs, ts)) -> (tvs, ts, (fun acc -> compile_expr_as_value (Reference ref) (NOp_T(T_Routine(tvs,ts))) env contexts (Call :: acc)), env)
        | _ -> raise_failure "Call to non-routine value"
      else raise_failure ("No such routine '" ^n^ "' in context '" ^env.context_name^ "'" )
    )
    | LocalContext(access) -> ( match type_inner_reference access env contexts with
      | (_, Ok T_Routine(tvs,ts)) -> (tvs, ts, (fun acc -> compile_inner_reference access env contexts (RefFetch :: FetchFull :: FetchFull :: Call :: acc)), env)
      | _ -> raise_failure "Call to non-routine value"
    )
    | _ -> raise_failure "Illegal call"
    in
    if List.length params != List.length args then raise_failure ("Call requires " ^ (Int.to_string (List.length params)) ^ " arguments, but was given " ^  (Int.to_string (List.length args)))
    else if typ_vars = [] then compile_arguments (List.combine params args) env contexts (PlaceFull(C_Int (List.length params)) :: call_f acc) 
    else (
      let typ_args = resolve_type_args typ_vars typ_args params args env contexts in
      match replace_generics params typ_vars typ_args with
      | Ok typs -> compile_arguments (List.combine typs args) env contexts (PlaceFull(C_Int (List.length params)) :: call_f acc)
      | Error m -> raise_failure (m^" i dont know")
    )
  )
  | Stop -> addStop(acc)
  | Halt -> addHalt(acc)
  | Break -> (
    match break with
    | Some name when cleanup = 0 -> GoTo(name) :: acc
    | Some name -> addFreeVars cleanup (GoTo(name) :: acc)
    | None -> raise_failure "No loop to break out of"
  )
  | Continue -> (
    match continue with
    | Some name when cleanup = 0 -> GoTo(name) :: acc
    | Some name -> addFreeVars cleanup (GoTo(name) :: acc)
    | None -> raise_failure "No loop to continue in"
  )
  | Print exprs -> (
    let rec aux es acc =
      match es with
      | [] -> acc
      | h::t -> (
        let opt_h = optimize_expr h env.var_env in
        let (_, expr_ty) = Typing.type_expr opt_h env contexts in
        match expr_ty with
        | Ok ot -> (match translate_operational_type ot with
          | T_Bool -> aux t (compile_expr_as_value opt_h ot env contexts (PrintBool :: acc))
          | T_Int -> aux t (compile_expr_as_value opt_h ot env contexts (PrintInt :: acc))
          | T_Char -> aux t (compile_expr_as_value opt_h ot env contexts (PrintChar :: acc))
          | T_Array (Some T_Char) -> (
            let data_handle = new_label () in
            let index_handle = new_label () in
            let code =
            Block[
              Declaration(AssignDeclaration(Stable,Some(T_Array(Some T_Char)),data_handle,h),0);
              Declaration(AssignDeclaration(Open,Some T_Int,index_handle,Value(Int 0)),0);
              Statement(
                While(Value(Binary_op("<",Reference(LocalContext(Access index_handle)), Value(ArraySize(Access data_handle)))),
                  Block[
                    Statement(Print [Reference(LocalContext(ArrayAccess(Access data_handle, Reference(LocalContext(Access index_handle)))))],0);
                    Statement(Assign(LocalContext(Access index_handle),Value(Binary_op("+",Reference(LocalContext(Access index_handle)),Value(Int 1)))), 0)
                  ]
                ),
                0
              );
            ]
            in
            aux t (compile_stmt code env contexts break continue cleanup acc)
          )
          | _ -> aux t (compile_expr opt_h (NOp_T T_Null) env contexts (PrintInt :: acc))
        )
        | Error m -> raise_failure m
      )
    in
    aux (List.rev exprs) acc
  )

let set_globalvar_typ name context_name new_typ contexts =
  List.map (fun (Context(cn,env) as context) -> 
    if context_name = cn then (
      let updated = List.map ( fun (accmod,n,cn,idx,vm,old_ty,dec) ->
        if name = n then (accmod,n,cn,idx,vm,Some new_typ,dec)
        else (accmod,n,cn,idx,vm,old_ty,dec)
      ) env.var_env.globals in
      Context(cn, { env with var_env = { env.var_env with globals = updated}})
    )
    else context
  ) contexts

let type_globalvars globals contexts =
  let rec aux globals contexts = match globals with
    | [] -> contexts
    | (_,name,context_name,_,vmod,Some ty,AssignDeclaration(_,typ,_,expr))::t -> (
    try (
      match List.find_opt (fun c -> match c with Context(name,_) -> name = context_name) contexts with
      | None -> raise_failure "Failed context lookup"
      | Some(Context(_,env)) -> (
        let contexts = if not(well_defined_type (Some ty) env.var_env) then 
          let typ_res = declaration_type_check vmod typ expr env contexts in
          let o_typ = if Result.is_ok typ_res then Result.get_ok typ_res else raise_failure (Result.get_error typ_res) in
          let typ = translate_operational_type o_typ in
          set_globalvar_typ name context_name typ contexts
        else contexts 
        in
        aux t contexts 
      )
    )
    with
    | Failure(_,line_opt,expl_opt) -> raise (Failure(Some context_name,line_opt,expl_opt))
  )
  | (_,name,context_name,_,_,_,TypeDeclaration(_,typ,_))::t -> (
    try (
      match List.find_opt (fun c -> match c with Context(name,_) -> name = context_name) contexts with
      | None -> raise_failure "Failed context lookup"
      | Some(Context(_,_)) -> (
        let contexts = set_globalvar_typ name context_name typ contexts in
        aux t contexts
      )
    )
    with
    | Failure(_,line_opt,expl_opt) -> raise (Failure(Some context_name,line_opt,expl_opt))
  )
  | (_,name,context_name,_,vmod,None,AssignDeclaration(_,typ,_,expr))::t -> (
    try (
      match List.find_opt (fun c -> match c with Context(name,_) -> name = context_name) contexts with
      | None -> raise_failure "Failed context lookup"
      | Some(Context(_,env)) -> (
        let typ_res = declaration_type_check vmod typ expr env contexts in
        let o_typ = if Result.is_ok typ_res then Result.get_ok typ_res else raise_failure (Result.get_error typ_res) in
        let typ = translate_operational_type o_typ in
        let contexts = set_globalvar_typ name context_name typ contexts in
        aux t contexts
      )
    )
    with
    | Failure(_,line_opt,expl_opt) -> raise (Failure(Some context_name,line_opt,expl_opt)) 
  )
  in
  aux globals contexts


let rec compile_globalvars globvars structs contexts acc =
  match globvars with
  | [] -> (acc,contexts)
  | (_,_,context_name,_,_,_,dec)::t -> (
    try (
      match List.find_opt (fun c -> match c with Context(name,_) -> name = context_name) contexts with
      | None -> raise_failure "Failed context lookup"
      | Some(Context(_,env)) -> (
        let (_,f) = compile_declaration dec ({ context_name = context_name; var_env = ({ locals = []; globals = env.var_env.globals; structs = structs; typ_vars = env.var_env.typ_vars}); file_refs = env.file_refs }) contexts in
        compile_globalvars t structs contexts (f acc)
      )
    )
    with
    | Failure(_,line_opt,expl_opt) -> raise (Failure(Some context_name,line_opt,expl_opt))
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
  let rec get_context_environment path topdecs file_refs globals structs =
    match topdecs with
    | [] -> (complete_path base_path path, globals, structs, file_refs)
    | (FileReference(alias, ref_path))::t -> (
      if StringMap.mem alias file_refs then raise_failure ("Duplicate context alias '" ^ alias ^ "'") else
      let ref_path = complete_path path ref_path in
      get_context_environment path t (StringMap.add alias ref_path file_refs) globals structs
    )
    | (Struct(name, typ_vars, fields))::t -> get_context_environment path t file_refs globals (StringMap.add name (typ_vars, fields) structs)
    | (GlobalDeclaration(accmod,declaration))::t -> ( match declaration with
      | TypeDeclaration(vmod, typ, name) -> 
        get_context_environment path t file_refs ((accmod,name,(complete_path base_path path),vmod,Some typ,declaration)::globals) structs
      | AssignDeclaration(vmod, None, name, Value AnonRoutine(tvs,args,_)) -> 
        get_context_environment path t file_refs ((accmod,name,(complete_path base_path path),vmod,Some(T_Routine(tvs, List.map (fun (vm,t,_) -> (vm,t)) args)),declaration)::globals) structs
      | AssignDeclaration(vmod, typ_opt, name, _) -> 
        get_context_environment path t file_refs ((accmod,name,(complete_path base_path path),vmod,typ_opt,declaration)::globals) structs
    )
  in
  let rec get_contexts path parse acc =
    let path = complete_path base_path path in
    let file = parse path in
    let context_env = get_context_environment path (match file with File(t) -> t) StringMap.empty [] StringMap.empty in
    let (_,_,_,file_refs) = context_env in
    StringMap.fold (fun _ ref_path acc -> 
      if List.exists (fun (_,(p,_,_,_)) -> p = ref_path) acc then acc
      else get_contexts ref_path parse acc
    ) file_refs ((file,context_env)::acc)
  in
  get_contexts base_path parse []

let merge_contexts contexts =
  let rec aux cs topdecs globals structs =
    match cs with
    | [] -> (File(topdecs), globals, structs)
    | (File(tds),(_,c_globals,c_structs,_))::t -> (
      aux t 
        (List.rev_append topdecs tds) 
        (List.rev_append globals c_globals) 
        (StringMap.merge (fun _ o n -> match o, n with
          | Some _, _ -> o
          | _, Some _ -> n
          | _,_ -> None
        ) structs c_structs)
    )
  in
  aux contexts [][] StringMap.empty

let create_contexts globals context_infos : context list =
  let get_globalvar_info var_name context_name = 
    match List.find_opt (fun (_,n,cn,_,_,_,_) -> n = var_name && cn = context_name) globals with
    | None -> raise_failure "Failed global variable lookup"
    | Some((accmod,n,cn,idx,vmod,typ,dec)) -> (accmod,n,cn,idx,vmod,typ,dec)
  in
  let rec aux c_infos acc =
    match c_infos with
    | [] -> acc
    | (_,(context_name, globs, structs, file_refs))::t -> (
      aux t (Context(context_name, ({ context_name = context_name; var_env = { locals = []; globals = (List.map (fun (_,n,cn,_,_,_) -> get_globalvar_info n cn) globs); structs = structs; typ_vars = []}; file_refs = file_refs }))::acc)
    )
  in
  aux context_infos []

let compile path parse =
  let path = (compress_path (total_path path)) in
  try (
    let context_infos = gather_context_infos path parse in
    let (topdecs,globals,structs) = merge_contexts context_infos in
    let globals_ordered = (order_dep_globvars (get_globvar_dependencies globals)) in
    let contexts = create_contexts globals_ordered context_infos in
    let contexts = type_globalvars globals_ordered contexts in

    let () = check_topdecs topdecs structs in
    let () = check_structs structs in

    let (program, contexts) = compile_globalvars (List.rev globals_ordered) structs contexts [Start] in
    let global_var_info = gather_globvar_info (match (List.find (fun c -> match c with Context(cn,_) -> cn = path) contexts) with Context(_,env) -> env.var_env.globals) in
    let struct_info = StringMap.to_list structs |> List.map (fun (n,(tv,ps)) -> (n,tv,ps)) in
    Program(struct_info, global_var_info, program)
  )
  with 
  | Failure _ as f -> raise f
  | _ -> raise (Failure(Some path, None, "Parser error"))