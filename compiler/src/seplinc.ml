open Seplinclib.AssemblyWriter
open Seplinclib.ToProgramRep
open Str
open Seplinclib.Exceptions

let () = Printexc.record_backtrace true

type input_type =
| SEP
| SEA

type compilation_strategy =
| CompileToSeplinVM
| TranspileToC

let resolve_input input =
  try (
    if not (Sys.file_exists input) then (Printf.printf "%s\n" input; raise_failure "Input file does not exist")
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.sep$|}) input 0 then (input, SEP)
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.sea$|}) input 0 then (input, SEA)
    else raise_failure "Invalid input file extension"
  ) with
  | ex -> raise ex

let file_extention comp_strat = match comp_strat with
    | CompileToSeplinVM -> ".sec"
    | TranspileToC -> ".c"

let resolve_output input output comp_strat =
  let extension = file_extention comp_strat in
  try (
    if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*$|}) output 0 then  (* Directory *) (
      output ^ List.hd (String.split_on_char '.' (List.hd (List.rev (String.split_on_char '/' input)))) ^ extension
    )
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+$|}) output 0 then (* File with extension*) (
      output
    )
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+$|}) output 0 then (* File without extension *) (
      output ^ extension
    )
    else raise_failure "Invalid output destination"
  ) with
  | ex -> raise ex


let resolve_arguments () : ((string * input_type) * string * compilation_strategy) =
  let arguments = Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1) in
  let (flags,args) = Array.fold_left (fun (flags,acc) arg -> if String.starts_with ~prefix:"-" arg then (arg::flags,acc) else (flags,arg::acc)) ([],[]) arguments in
  let args = List.rev args in
  let comp_strat = if List.mem "-t" flags then TranspileToC else CompileToSeplinVM in
  match args with
  | [input] -> (resolve_input input, ((String.sub input 0 ((String.length input) - 4)) ^ (file_extention comp_strat)), comp_strat)
  | [input;output] -> (resolve_input input, resolve_output input output comp_strat, comp_strat)
  | _ -> raise_failure "Wrong number of arguments"


let print_line ls l =
  Printf.printf "%i | %s\n" (l+1) (List.nth ls l)

let read_file path =
  let file = open_in path in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  content
    
let () = try (
  let ((input, in_type), output, comp_strat) = resolve_arguments () in
  let program = match in_type with
    | SEA -> Seplinclib.AssemblyParser.main (Seplinclib.AssemblyLexer.start input) (Lexing.from_string (read_file input)) 
    | SEP -> compile input (fun file -> Seplinclib.Parser.main (Seplinclib.Lexer.start file) (Lexing.from_string (read_file file)))
  in match comp_strat with
    | CompileToSeplinVM -> write program output
    | TranspileToC -> Printf.fprintf (open_out output) "%s" (Seplinclib.Transpile.transpile_to_c program)
) with 
| Failure(file_opt, line_opt, expl) -> (
  Printf.printf "%s" expl ;
  if Option.is_some file_opt then (
    Printf.printf " in:\n%s" (Option.get file_opt) ;
    if Option.is_some line_opt then (
      Printf.printf ", line %i: \n" (Option.get line_opt) ;
      let line = Option.get line_opt in
      let lines = String.split_on_char '\n' (read_file (Option.get file_opt)) in
      let printer =  print_line lines in match line with
      | 1 -> printer 0 ; printer 1
      | n when n = (List.length lines)-1 -> printer (n-2) ; printer (n-1)
      | _ ->  printer (line-2) ; printer (line-1) ; printer line
    )
    else Printf.printf "\n" ;
  )
  else Printf.printf "\n" ;
)