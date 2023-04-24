open Exceptions
open ProgramRep
open Absyn

(*** Types ***)
type variable_environment = { 
  locals: (var_mod * typ * string) list; (* modifier, type, name *)
  globals: (string * string * int * var_mod * typ * declaration) list; (* name, stack_index, modifier, type, expression *)
  structs: (string * char list * (var_mod * typ * string) list) list; (* name, type_vars, parameters(modifier, type, name) *)
  typ_vars: char list;
}

type environment = { 
  context_name: string;
  var_env: variable_environment;
  routine_env: (access_mod * string * string * char list * (var_mod * typ * string) list * statement) list; (* name, type_vars, parameters(modifier, type, name) *)
  file_refs: (string * string) list
}

type label_generator = { mutable next : int }

type context =
  | Context of string * environment

(* Labels *)
let lg = ( {next = 0;} )

let new_label () =
  let number = lg.next in
  let () = lg.next <- lg.next+1 in
  Int.to_string number
  

(* Lookup *)
let rec lookup f l =
  match l with
  | [] -> None
  | h::t -> ( match f h with
    | None -> lookup f t
    | a -> a
  )

let lookup_i f l =
  let rec aux l i =
    match l with
    | [] -> None
    | h::t -> ( match f i h with
      | None -> aux t (i-1)
      | a -> a
    )
  in
  aux l ((List.length l)-1)

let lookup_routine (name: string) routines =
  lookup (fun (accmod,n,cn,tvs,ps,stmt) -> if n = name then Some(accmod,n,cn,tvs,ps,stmt) else None) routines

let lookup_struct (name: string) structs =
  lookup (fun (n,tvs,ps) -> if n = name then Some(tvs,ps) else None) structs

let lookup_globvar (name: string) globvars =
  lookup (fun (n,_,cnt,vmod,ty,_) -> if n = name then Some(cnt,ty,vmod) else None) globvars

let lookup_localvar (name: string) localvars =
  lookup_i (fun i (vmod,ty,n) -> if n = name then Some(i,ty,vmod) else None) localvars

let struct_field field params =
  let rec aux ps c =
    match ps with
    | [] -> raise_error ("No such field '" ^ field ^ "'")
    | (vmod,ty,n)::t -> if n = field then (vmod,ty,c) else aux t (c+1)
  in
  aux params 0

let strictest_mod m1 m2 =
  if m1 = Const || m2 = Const then Const
  else if m1 = Stable || m2 = Stable then Stable
  else Open

let var_modifier (name: string) var_env = 
  match lookup_localvar name var_env.locals with
    | Some (_,_,l_vmod) -> l_vmod
    | None -> 
      match lookup_globvar name var_env.globals with
      | Some (_,_,g_vmod) -> g_vmod
      | None -> raise_error ("No such variable '" ^ name ^ "'")

let var_type (name: string) var_env = 
  match lookup_localvar name var_env.locals with
  | Some (_,lty,_) -> lty
  | None -> 
    match lookup_globvar name var_env.globals with
    | Some (_,gty,_) -> gty
    | None -> raise_error ("No such variable '" ^ name ^ "'")

let globvar_exists (name: string) globvars =
  Option.is_some (lookup_globvar name globvars)
  
let localvar_exists (name: string) localvars =
  Option.is_some (lookup_localvar name localvars)

let routine_exists (name: string) routines =
  Option.is_some (lookup_routine name routines)

let struct_exists (name: string) structs =
  Option.is_some (lookup_struct name structs)