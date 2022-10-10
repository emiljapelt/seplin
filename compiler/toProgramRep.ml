open Absyn
open ProgramRep

(*    list of: string * int * bool * typ * assignable_expression    *)
let get_globvars (tds : topdecs) = 
  let rec aux topdecs acc count =
    match topdecs with
    | [] -> acc
    | h::t -> (
      match h with
      | GlobalVar (locked, ty, name, a_expr) -> aux t ((name, count, locked, ty, a_expr)::acc) (count+1)
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
      | Routine (accmod, name, params, stmt) -> aux t ((name, params)::acc)
      | _ -> aux t acc
    )
  in match tds with
  | Topdecs l -> aux l []



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
    | h::t -> (
      match h with
      | Declaration _ -> aux t (c+1)
      | _ -> aux t c
    )
  in
  aux stmt_dec_list 0

let type_string t =
  match t with
  | T_Bool -> "'bool'"
  | T_Int -> "'int'"

let routine_head accmod name params =
  match accmod with
  | Internal -> Label(name)
  | External -> EntryPoint(name, List.map (fun (l,t,n) -> t) params)

type label_generator = { mutable next : int }

let lg = ( {next = 0;} )

let new_label () =
  let number = lg.next in
  let () = lg.next <- lg.next+1 in
  Int.to_string number

let fetch_var_index (name : string) globvars localvars = 
  match lookup_localvar name localvars with
  | Some (lc,_,_) -> IntInstruction(37, lc)
  | None -> 
    match lookup_globvar name globvars with
    | Some (gc,_,_) -> IntInstruction(36, gc)
    | None -> failwith ("No such variable " ^ name)

let fetch_var_val (name : string) globvars localvars = 
  let t = match lookup_localvar name localvars with
    | Some (_,lt,_) -> lt
    | None -> 
      match lookup_globvar name globvars with
      | Some (_,gt,_) -> gt
      | None -> failwith ("No such variable " ^ name)
  in
  match t with
  | T_Int -> (t, (fetch_var_index name globvars localvars) :: [Instruction(12)])
  | T_Bool -> (t, (fetch_var_index name globvars localvars) :: [Instruction(11)])

let var_locked (name : string) globvars localvars = 
  match lookup_localvar name localvars with
    | Some (_,_,ll) -> ll
    | None -> 
      match lookup_globvar name globvars with
      | Some (_,_,gl) -> gl
      | None -> failwith ("No such variable " ^ name)
  

let rec compile_assignable_expr expr globvars localvars routines =
  match expr with
  | Bool b -> (T_Bool, [BoolInstruction(5, b)])
  | Int i -> (T_Int, [IntInstruction(6, i)])
  | Lookup n -> fetch_var_val n globvars localvars
  | Binary_op (op, e1, e2) -> (
      let (t1, ins1) = compile_assignable_expr e1 globvars localvars routines in
      let (t2, ins2) = compile_assignable_expr e2 globvars localvars routines in
      match (op, t1, t2) with
      | ("&", T_Bool, T_Bool) ->  (T_Bool, ins1 @ ins2 @ [Instruction(24)])
      | ("|", T_Bool, T_Bool) -> (T_Bool, ins1 @ ins2 @ [Instruction(25)])
      | ("=", T_Bool, T_Bool) -> (T_Bool, ins1 @ ins2 @ [Instruction(22)])
      | ("!=", T_Bool, T_Bool) -> (T_Bool, ins1 @ ins2 @ [Instruction(22)] @ [Instruction(23)])
      | ("=", T_Int, T_Int) -> (T_Bool, ins1 @ ins2 @ [Instruction(20)])
      | ("!=", T_Int, T_Int) -> (T_Bool, ins1 @ ins2 @ [Instruction(20)] @ [Instruction(23)])
      | ("<=", T_Int, T_Int) -> (T_Bool, ins1 @ ins2 @ [Instruction(21)] @ [Instruction(23)]) 
      | ("<", T_Int, T_Int) -> (T_Bool, ins2 @ ins1 @ [Instruction(21)])
      | (">=", T_Int, T_Int) -> (T_Bool, ins2 @ ins1 @ [Instruction(21)] @ [Instruction(23)])
      | (">", T_Int, T_Int) -> (T_Bool, ins1 @ ins2 @ [Instruction(21)])
      | ("+", T_Int, T_Int) -> (T_Int, ins1 @ ins2 @ [Instruction(17)])
      | ("-", T_Int, T_Int) -> (T_Int, ins2 @ ins1 @ [Instruction(19)])
      | ("*", T_Int, T_Int) -> (T_Int, ins1 @ ins2 @ [Instruction(18)])
      | _ -> failwith "Unknown binary operator, or type mismatch"
    )
  | Unary_op (op, e) -> (
    let (t, ins) = compile_assignable_expr e globvars localvars routines in
    match (op, t) with
    | ("!", T_Bool) -> (T_Bool, ins @ [Instruction(23)])
    | _ -> failwith "Unknown unary operator, or type mismatch"
  )

let compile_arguments params exprs globvars localvars routines =
  let rec aux ps es acc =
    match (ps, es) with
    | ([],[]) -> acc
    | ((pl,pty,pn)::pt,eh::et) -> (
      let (ety, ins) = compile_assignable_expr eh globvars localvars routines in
      if pty != ety then failwith ("Type mismatch on assignment: expected " ^ (type_string pty) ^ ", got " ^ (type_string ety)) 
      else match eh with
      | Lookup n -> (
        match (pl, var_locked n globvars localvars) with
        | (false, true) -> failwith "Cannot give a locked variable as a parameter that is not locked"
        | _ -> aux pt et ((fetch_var_index n globvars localvars) :: acc)
      )
      | _ -> (
        match ety with
        | T_Int -> aux pt et (Instruction(14) :: Instruction(7) :: ins @ (Instruction(16) :: acc))
        | T_Bool -> aux pt et (Instruction(13) :: Instruction(7) :: ins @ (Instruction(15) :: acc))
      )
    )
    | _ -> failwith "Insufficient arguments in call"
  in
  aux params exprs []


let compile_unassignable_expr expr globvars localvars routines =
  match expr with
  | Assign (name, aexpr) -> (
    let (ty, ins) = compile_assignable_expr aexpr globvars localvars routines in
    let get = match lookup_localvar name localvars with
    | Some(cl,tl,ll) -> (
        if ll then failwith ("Cannot assign to locked variable: " ^ name)
        else if tl != ty then failwith ("Type mismatch on assignment: expected " ^ (type_string tl) ^ ", got " ^ (type_string ty)) 
        else IntInstruction(37, cl)
      )
    | None -> (
      match lookup_globvar name globvars with
      | Some(cg,tg,lg) -> (
        if lg then failwith ("Cannot assign to locked variable: " ^ name) 
        else if tg != ty then failwith ("Type mismatch on assignment: expected " ^ (type_string tg) ^ ", got " ^ (type_string ty))  
        else IntInstruction(36, cg)
      )
      | None -> failwith ("No such variable " ^ name)
    )
    in match ty with
    | T_Bool -> get :: ins @ [Instruction(15)]
    | T_Int -> get :: ins @ [Instruction(16)]
  )
  | Call (n, aexprs) -> (
    match lookup_routine n routines with
    | None -> failwith ("No such routine: " ^ n)
    | Some (ps) when (List.length ps) = (List.length aexprs) -> (
      (compile_arguments ps aexprs globvars localvars routines) @ (IntInstruction(6, List.length ps) :: [LabelInstruction(2, n)])
    )
    | Some (ps) -> failwith (n ^ " requires " ^ (Int.to_string (List.length ps)) ^ " arguments, but was given " ^  (Int.to_string (List.length aexprs)))
  )
  | Stop -> [Instruction(1)]
  | Halt -> [Instruction(0)]
  | Print expr -> (
    let (t, ins) = compile_assignable_expr expr globvars localvars routines in
    match t with
    | T_Bool -> ins @ [Instruction(35)]
    | T_Int -> ins @ [Instruction(34)]
  )

let rec compile_sod_list sod_list globvars localvars routines =
  match sod_list with
  | [] -> []
  | h::t -> (
    match h with
    | Statement s -> compile_stmt s globvars localvars routines @ compile_sod_list t globvars localvars routines
    | Declaration (l, ty, n, expr) -> (
      let (expr_ty, ins) = compile_assignable_expr expr globvars localvars routines in
      match ty with
      | T_Int when expr_ty = T_Int -> Instruction(14) :: [Instruction(7)] @ ins @ [Instruction(16)] @ compile_sod_list t globvars ((l,ty,n)::localvars) routines
      | T_Bool when expr_ty = T_Bool -> Instruction(13) :: [Instruction(7)] @ ins @ [Instruction(15)] @ compile_sod_list t globvars ((l,ty,n)::localvars) routines
      | _ -> failwith ("Type mismatch on declaration: expected " ^ (type_string ty) ^ ", got " ^ (type_string expr_ty)) 
    )
  )

and compile_stmt stmt globvars localvars routines =
  match stmt with
  | If (e, s1, s2) -> (
    let label_name1 = new_label () in
    let label_name2 = new_label () in
    let (t, ins) = compile_assignable_expr e globvars localvars routines in
    if t != T_Bool then failwith "Conditional requires 'bool'"
    else ins @ [LabelInstruction(4, label_name1)] @ (compile_stmt s2 globvars localvars routines) @ [LabelInstruction(3,label_name2)] @ [Label(label_name1)] @ (compile_stmt s1 globvars localvars routines) @ [Label(label_name2)]
  )
  | While (e, s) -> (
    let label_cond = new_label () in
    let label_start = new_label () in
    let (t, ins) = compile_assignable_expr e globvars localvars routines in
    if t != T_Bool then failwith "Conditional requires 'bool'"
    else (LabelInstruction(3, label_cond)) :: Label(label_start) :: (compile_stmt s globvars localvars routines) @ [Label(label_cond)] @ ins @ [LabelInstruction(4, label_start)]
  )
  | Block (sod_list) -> (
    (compile_sod_list sod_list globvars localvars routines) @ [IntInstruction(32, (count_decl sod_list))]
  )
  | Expression (expr) -> compile_unassignable_expr expr globvars localvars routines



let compile_globvars lst =
  let rec aux l acc = 
    match l with
    | [] -> acc
    | (_,_,l,ty,a_expr)::t -> (
      match (ty, a_expr) with
      | (T_Bool, Bool b) -> aux t ((G_Bool(l, b))::acc)
      | (T_Int, Int i) ->  aux t ((G_Int(l, i))::acc)
      | _ -> failwith "Global variables are lacking in features, sorry :("
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
        aux t ((routine_head accmod n params)::(compile_stmt stmt globvars params routines) @ [Instruction(1)] @ acc)
      | _ -> aux t acc
  in
  match topdecs with
  | Topdecs tds -> Program(compile_globvars globvars, aux tds [])