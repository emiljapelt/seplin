open Exceptions
open ProgramRep
open Absyn

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

(*** Lookup ***)
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