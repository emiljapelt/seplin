open Exceptions
open ProgramRep
open Absyn

module StringMap = Map.Make(String)

(*** Types ***)
type variable_environment = { 
  locals: (var_mod * typ * string) list; (* modifier, type, name *) (* Make into StringMap *)
  globals: (access_mod * string * string * int * var_mod * typ * declaration) list; (* Make into StringMap *)
  structs: (char list * (var_mod * typ * string) list) StringMap.t; (* name, type_vars, parameters(modifier, type, name) *)
  typ_vars: char list;
}

type environment = { 
  context_name: string;
  var_env: variable_environment;
  file_refs: string StringMap.t;
}

type context =
| Context of string * environment

type label_generator = { mutable next : int }

let update_locals env (vmod : var_mod) typ name =
  ({ env with var_env = ({ env.var_env with locals = (vmod, typ, name)::env.var_env.locals }) })

(* Labels *)
let lg = ( {next = 0;} )

let new_label () =
  let number = lg.next in
  let () = lg.next <- lg.next+1 in
  Int.to_string number
  
let empty_env () = { context_name = "" ; var_env = { locals = [] ; globals = [] ; structs = StringMap.empty ; typ_vars = [] } ; file_refs = StringMap.empty }

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

let lookup_context (name: string) file_refs contexts =
  match StringMap.find_opt name file_refs with
  | None -> None
  | Some(cn) -> lookup (fun (c) -> match c with Context(n,e) -> if cn = n then Some e else None) contexts

let lookup_routine (name: string) routines =
  lookup (fun (accmod,n,cn,tvs,ps,stmt) -> if n = name then Some(accmod,n,cn,tvs,ps,stmt) else None) routines

let lookup_struct (name: string) structs =
  StringMap.find_opt name structs

let lookup_globvar (name: string) globvars =
  lookup (fun (accmod,n,_,cnt,vmod,ty,_) -> if n = name then Some(accmod,cnt,ty,vmod) else None) globvars

let lookup_localvar (name: string) localvars =
  lookup_i (fun i (vmod,ty,n) -> if n = name then Some(i,ty,vmod) else None) localvars

let struct_field field params =
  let rec aux ps c =
    match ps with
    | [] -> raise_failure ("No such field '" ^ field ^ "'")
    | (vmod,ty,n)::t -> if n = field then (vmod,ty,c) else aux t (c+1)
  in
  aux params 0

let strictest_mod m1 m2 =
  if m1 = Const || m2 = Const then Const
  else if m1 = Stable || m2 = Stable then Stable
  else Open

let access_modifier (name: string) env =
  match lookup_localvar name env.var_env.locals with
    | Some (_,_,_) -> Internal
    | None -> 
      match lookup_globvar name env.var_env.globals with
      | Some (g_amod,_,_,_) -> g_amod
      | None -> raise_failure ("No such variable '" ^ name ^ "'")

let var_modifier (name: string) env = 
  match lookup_localvar name env.var_env.locals with
    | Some (_,_,l_vmod) -> l_vmod
    | None -> 
      match lookup_globvar name env.var_env.globals with
      | Some (_,_,_,g_vmod) -> g_vmod
      | None -> raise_failure ("No such variable '" ^ name ^ "'")

let var_type (name: string) env : (typ, string) result = 
  match lookup_localvar name env.var_env.locals with
  | Some (_,lty,_) -> Ok lty
  | None -> 
    match lookup_globvar name env.var_env.globals with
    | Some (_,_,gty,_) -> Ok gty
    | None -> raise_failure ("No such variable '" ^ name ^ "'")

let globvar_exists (name: string) globvars =
  Option.is_some (lookup_globvar name globvars)
  
let localvar_exists (name: string) localvars =
  Option.is_some (lookup_localvar name localvars)

let routine_exists (name: string) routines =
  Option.is_some (lookup_routine name routines)

let struct_exists (name: string) structs =
  Option.is_some (lookup_struct name structs)
