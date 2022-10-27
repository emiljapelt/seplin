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

let type_string t =
  match t with
  | T_Bool -> "'bool'"
  | T_Int -> "'int'"

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

let fetch_globvar_expr (name: string) globvars =
  let rec aux li =
    match li with
    | [] -> compile_error ("No such global variable: " ^ name)
    | (n,_,_,_,expr)::t -> if n = name then expr else aux t
  in
  aux globvars

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

let fetch_var_val (name: string) globvars localvars = 
  let t = match lookup_localvar name localvars with
    | Some (_,lt,_) -> lt
    | None -> 
      match lookup_globvar name globvars with
      | Some (_,gt,_) -> gt
      | None -> compile_error ("No such variable " ^ name)
  in
  match t with
  | T_Int -> (t, (fetch_var_index name globvars localvars) :: [FetchInt])
  | T_Bool -> (t, (fetch_var_index name globvars localvars) :: [FetchBool])

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


let default_value t =
  match t with
  | T_Int -> Int 0
  | T_Bool -> Bool false

(*    list of: string * int * bool * typ * assignable_expression    *)
let get_globvars (tds : topdecs) = 
  let rec aux topdecs acc count =
    match topdecs with
    | [] -> acc
    | h::t -> (
       match h with
      | Global (locked, ty, name) -> (
        if globvar_exists name acc then compile_error ("Duplicate global variable name: " ^ name)
        else aux t ((name, count, locked, ty, (default_value ty))::acc) (count+1)
        )
      | GlobalAssign (locked, ty, name, a_expr) -> (
        if globvar_exists name acc then compile_error ("Duplicate global variable name: " ^ name)
        else aux t ((name, count, locked, ty, a_expr)::acc) (count+1)
        )
      | _ -> aux t acc count
    )
  in match tds with 
  | Topdecs l -> aux l [] 0

(*    list of: string * access_mod * (typ * string) list * statement    *)
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



let rec compile_assignable_expr expr globvars localvars =
  match expr with
  | Bool b -> (T_Bool, [PlaceBool(b)])
  | Int i -> (T_Int, [PlaceInt(i)])
  | Lookup n -> fetch_var_val n globvars localvars
  | Binary_op (op, e1, e2) -> (
      let (t1, ins1) = compile_assignable_expr e1 globvars localvars in
      let (t2, ins2) = compile_assignable_expr e2 globvars localvars in
      match (op, t1, t2, e1, e2) with
      | ("&", T_Bool, T_Bool, Bool true, _) ->  (T_Bool, ins2)
      | ("&", T_Bool, T_Bool, _, Bool true) ->  (T_Bool, ins1)
      | ("&", T_Bool, T_Bool, Bool false, _) ->  (T_Bool, [PlaceBool(false)])
      | ("&", T_Bool, T_Bool, _, Bool false) ->  (T_Bool, [PlaceBool(false)])
      | ("&", T_Bool, T_Bool, _, _) ->  (T_Bool, ins1 @ ins2 @ [BoolAnd])
      | ("|", T_Bool, T_Bool, Bool true, _) -> (T_Bool, [PlaceBool(true)])
      | ("|", T_Bool, T_Bool, _, Bool true) -> (T_Bool, [PlaceBool(true)])
      | ("|", T_Bool, T_Bool, Bool false, _) -> (T_Bool, ins2)
      | ("|", T_Bool, T_Bool, _, Bool false) -> (T_Bool, ins1)
      | ("|", T_Bool, T_Bool, _, _) -> (T_Bool, ins1 @ ins2 @ [BoolOr])
      | ("=", T_Bool, T_Bool, _, _) -> (T_Bool, ins1 @ ins2 @ [BoolEq])
      | ("!=", T_Bool, T_Bool, _, _) -> (T_Bool, ins1 @ ins2 @ [BoolEq] @ [BoolNot])
      | ("=", T_Int, T_Int, _, _) -> (T_Bool, ins1 @ ins2 @ [IntEq])
      | ("!=", T_Int, T_Int, _, _) -> (T_Bool, ins1 @ ins2 @ [IntEq] @ [BoolNot])
      | ("<=", T_Int, T_Int, _, _) -> (T_Bool, ins1 @ ins2 @ [IntLt] @ [BoolNot]) 
      | ("<", T_Int, T_Int, _, _) -> (T_Bool, ins2 @ ins1 @ [IntLt])
      | (">=", T_Int, T_Int, _, _) -> (T_Bool, ins2 @ ins1 @ [IntLt] @ [BoolNot])
      | (">", T_Int, T_Int, _, _) -> (T_Bool, ins1 @ ins2 @ [IntLt])
      | ("+", T_Int, T_Int, Int 0, _) -> (T_Int, ins2)
      | ("+", T_Int, T_Int, _, Int 0) -> (T_Int, ins1)
      | ("+", T_Int, T_Int, _, _) -> (T_Int, ins1 @ ins2 @ [IntAdd])
      | ("-", T_Int, T_Int, _, Int 0) -> (T_Int, ins1)
      | ("-", T_Int, T_Int, _, _) -> (T_Int, ins2 @ ins1 @ [IntSub])
      | ("*", T_Int, T_Int, Int 0, _) -> (T_Int, [PlaceInt(0)])
      | ("*", T_Int, T_Int, _, Int 0) -> (T_Int, [PlaceInt(0)])
      | ("*", T_Int, T_Int, Int 1, _) -> (T_Int, ins2)
      | ("*", T_Int, T_Int, _, Int 1) -> (T_Int, ins1)
      | ("*", T_Int, T_Int, _, _) -> (T_Int, ins1 @ ins2 @ [IntMul])
      | _ -> compile_error "Unknown binary operator, or type mismatch"
    )
  | Unary_op (op, e) -> (
    let (t, ins) = compile_assignable_expr e globvars localvars in
    match (op, t) with
    | ("!", T_Bool) -> (T_Bool, ins @ [BoolNot])
    | _ -> compile_error "Unknown unary operator, or type mismatch"
  )

let compile_arguments params exprs globvars localvars =
  let rec aux ps es acc =
    match (ps, es) with
    | ([],[]) -> acc
    | ((plock, pty, pname)::pt,eh::et) -> (
        let (ety, ins) = compile_assignable_expr eh globvars localvars in
        if pty != ety then compile_error ("Type mismatch on assignment: expected " ^ (type_string pty) ^ ", got " ^ (type_string ety)) 
        else match eh with
        | Lookup n -> (
          match (plock, var_locked n globvars localvars) with
          | (false, true) -> compile_error "Cannot give a locked variable as a parameter that is not locked"
          | _ -> aux pt et ((fetch_var_index n globvars localvars) :: acc)
        )
        | _ -> (
          match ety with
          | T_Int -> aux pt et (DeclareInt :: CloneFull :: ins @ (AssignInt :: acc))
          | T_Bool -> aux pt et (DeclareBool :: CloneFull :: ins @ (AssignBool :: acc))
        )
      )
    | _ -> compile_error "Insufficient arguments in call"
  in
  aux (List.rev params) (List.rev exprs) []


let compile_unassignable_expr expr globvars localvars routines break continue cleanup =
  match expr with
  | Assign (name, aexpr) -> (
    let (ty, ins) = compile_assignable_expr aexpr globvars localvars in
    let get = match lookup_localvar name localvars with
    | Some(cl,tl,ll) -> (
        if ll then compile_error ("Cannot assign to locked variable: " ^ name)
        else if tl != ty then compile_error ("Type mismatch on assignment: expected " ^ (type_string tl) ^ ", got " ^ (type_string ty)) 
        else BPFetch(cl)
      )
    | None -> (
      match lookup_globvar name globvars with
      | Some(cg,tg,lg) -> (
        if lg then compile_error ("Cannot assign to locked variable: " ^ name) 
        else if tg != ty then compile_error ("Type mismatch on assignment: expected " ^ (type_string tg) ^ ", got " ^ (type_string ty))  
        else StackFetch(cg)
      )
      | None -> compile_error ("No such variable: " ^ name)
    )
    in match ty with
    | T_Bool -> get :: ins @ [AssignBool]
    | T_Int -> get :: ins @ [AssignInt]
  )
  | Call (n, aexprs) -> (
    match lookup_routine n routines with
    | None -> compile_error ("No such routine: " ^ n)
    | Some (ps) when (List.length ps) = (List.length aexprs) -> (
      (compile_arguments ps aexprs globvars localvars) @ (PlaceInt(List.length ps) :: [Call(n)])
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
    | None -> compile_error "No loosp to continue in"
  )
  | Print expr -> (
    let (t, ins) = compile_assignable_expr expr globvars localvars in
    match t with
    | T_Bool -> ins @ [PrintBool]
    | T_Int -> ins @ [PrintInt]
  )

let rec compile_declaration dec globvars localvars =
  match dec with
  | TypeDeclaration (l, ty, n) -> (
    if localvar_exists n localvars then compile_error ("Duplicate variable name: " ^ n)
    else match ty with
    | T_Int -> (DeclareInt :: CloneFull :: PlaceInt(0) :: [AssignInt], (l,ty,n)::localvars)
    | T_Bool -> (DeclareBool :: CloneFull :: PlaceBool(false) :: [AssignBool], (l,ty,n)::localvars)
  )
  | AssignDeclaration (l, ty, n, expr) -> (
    if localvar_exists n localvars then compile_error ("Duplicate variable name: " ^ n)
    else let (expr_ty, ins) = compile_assignable_expr expr globvars localvars in
    match ty with
    | T_Int when expr_ty = T_Int -> (DeclareInt :: [CloneFull] @ ins @ [AssignInt], (l,ty,n)::localvars)
    | T_Bool when expr_ty = T_Bool -> (DeclareBool :: [CloneFull] @ ins @ [AssignBool], (l,ty,n)::localvars)
    | _ -> compile_error ("Type mismatch on declaration: expected " ^ (type_string ty) ^ ", got " ^ (type_string expr_ty)) 
  )
  | VarDeclaration (l, n, expr) -> (
    if localvar_exists n localvars then compile_error ("Duplicate variable name: " ^ n)
    else let (ty, ins) = compile_assignable_expr expr globvars localvars in
    match ty with
    | T_Int -> (DeclareInt :: [CloneFull] @ ins @ [AssignInt], (l,ty,n)::localvars)
    | T_Bool -> (DeclareBool :: [CloneFull] @ ins @ [AssignBool], (l,ty,n)::localvars)
  )

let rec compile_sod_list sod_list globvars localvars routines break continue cleanup =
  match sod_list with
  | [] -> []
  | h::t -> (
    match h with
    | Statement s -> compile_stmt s globvars localvars routines break continue cleanup @ compile_sod_list t globvars localvars routines break continue cleanup
    | Declaration dec -> (
      let (dec_ins, new_localvars) = compile_declaration dec globvars localvars in
      dec_ins @ compile_sod_list t globvars new_localvars routines break continue (cleanup+1)
    )
  )

and compile_stmt stmt globvars localvars routines break continue cleanup =
  match stmt with
  | If (e, s1, s2) -> (
    let label_true = new_label () in
    let label_stop = new_label () in
    let (t, ins) = compile_assignable_expr e globvars localvars in
    if t != T_Bool then compile_error "Conditional requires 'bool'"
    else ins @ [IfTrue(label_true)] @ (compile_stmt s2 globvars localvars routines break continue cleanup) @ [GoTo(label_stop)] @ [CLabel(label_true)] @ (compile_stmt s1 globvars localvars routines break continue cleanup) @ [CLabel(label_stop)]
  )
  | While (e, s) -> (
    let label_cond = new_label () in
    let label_start = new_label () in
    let label_stop = new_label () in
    let (t, ins) = compile_assignable_expr e globvars localvars in
    if t != T_Bool then compile_error "Conditional requires 'bool'"
    else (GoTo(label_cond)) :: CLabel(label_start) :: (compile_stmt s globvars localvars routines (Some label_stop) (Some label_cond) 0) @ [CLabel(label_cond)] @ ins @ (IfTrue(label_start) :: [CLabel(label_stop)])
  )
  | For (dec, con, modi, s) -> (
    let label_cond = new_label () in
    let label_start = new_label () in
    let label_modi = new_label () in
    let label_stop = new_label () in
    let (dec_ins, new_localvars) = compile_declaration dec globvars localvars in
    let (con_t, con_ins) = compile_assignable_expr con globvars new_localvars in
    let modi_ins = compile_unassignable_expr modi globvars new_localvars routines None None cleanup in
    if con_t != T_Bool then compile_error "Conditional requires 'bool'"
    else dec_ins @ (GoTo(label_cond) :: CLabel(label_start) :: compile_stmt s globvars new_localvars routines (Some label_stop) (Some label_modi) 0) @ (CLabel(label_modi) :: modi_ins) @ [CLabel(label_cond)] @ con_ins @ (IfTrue(label_start) :: CLabel(label_stop) :: [FreeVar])
  )
  | Block (sod_list) -> (
    let decs = count_decl sod_list in
    let block_ins = compile_sod_list sod_list globvars localvars routines break continue cleanup in 
    if decs = 0 then block_ins else block_ins @ [FreeVars(count_decl sod_list)]
  )
  | Expression (expr) -> compile_unassignable_expr expr globvars localvars routines break continue cleanup

let rec evaluate_globvar used_vars expr globvars = 
  match expr with
  | Bool b -> Bool b
  | Int i -> Int i
  | Lookup n -> (
    if List.for_all (fun var -> n != var) used_vars then evaluate_globvar (n::used_vars) (fetch_globvar_expr n globvars) globvars
    else compile_error "Cyclic referencing detected in global variables"
    )
  | Binary_op (op, e1, e2) -> (
      let v1 = evaluate_globvar used_vars e1 globvars in
      let v2 = evaluate_globvar used_vars e2 globvars in
      match (op, v1, v2) with
      | ("&", Bool b1, Bool b2) -> Bool (b1 && b2)
      | ("|", Bool b1, Bool b2) -> Bool (b1 || b2)
      | ("=", Bool b1, Bool b2) -> Bool (b1 = b2)
      | ("!=", Bool b1, Bool b2) -> Bool (b1 != b2)
      | ("=", Int i1, Int i2) -> Bool (i1 = i2)
      | ("!=", Int i1, Int i2) -> Bool (i1 != i2)
      | ("<=", Int i1, Int i2) -> Bool (i1 <= i2)
      | ("<", Int i1, Int i2) -> Bool (i1 < i2)
      | (">=", Int i1, Int i2) -> Bool (i1 >= i2)
      | (">", Int i1, Int i2) -> Bool (i1 > i2)
      | ("+", Int i1, Int i2) -> Int (i1 + i2)
      | ("-", Int i1, Int i2) -> Int (i1 - i2)
      | ("*", Int i1, Int i2) -> Int (i1 * i2)
      | _ -> compile_error "Unknown binary operator, or type mismatch"
    )
  | Unary_op (op, e) -> (
    let v = evaluate_globvar used_vars e globvars in
    match (op, v) with
    | ("!", Bool b) -> Bool (not b)
    | _ -> compile_error "Unknown unary operator, or type mismatch"
  )

let compile_globvars lst =
  let rec aux l acc = 
    match l with
    | [] -> acc
    | (n,_,l,ty,expr)::t -> (
      let v = evaluate_globvar [n] expr lst in
      match (ty, v) with
      | (T_Bool, Bool b) -> aux t ((G_Bool(b))::acc)
      | (T_Int, Int i) ->  aux t ((G_Int(i))::acc)
      | _ -> compile_error ("Type mismatch in global variable: " ^ n)
    )
  in
  aux lst []

let compile topdecs =
  let globvars = get_globvars topdecs in
  let routines = get_routines topdecs in
  let rec aux tds acc =
    match tds with
    | [] -> acc
    | h::t -> match h with
      | Routine (accmod, n, params, stmt) -> 
        aux t ((routine_head accmod n params)::(compile_stmt stmt globvars (List.rev params) routines None None 0) @ [CStop] @ acc)
      | _ -> aux t acc
  in
  match topdecs with
  | Topdecs tds -> Program(compile_globvars globvars, ProgramRep.translate(aux tds []))