open AssemblyWriter
open ToProgramRep
open Str
open Exceptions

type input_type =
| IX
| IXA

let resolve_input () =
  try (
    let input = Sys.argv.(1) in
    if not (Sys.file_exists input) then compile_error "Input file does not exist"
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.ix$|}) input 0 then (input, IX)
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.ixa$|}) input 0 then (input, IXA)
    else compile_error "Invalid input file extension"
  ) with
  | Invalid_argument _ -> compile_error "No file given to compile"
  | ex -> raise ex

let (input, in_type) = resolve_input ()

let resolve_output i =
  try (
    let output = Sys.argv.(2) in
    if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*$|}) output 0 then  (* Directory *) (
      output ^ List.hd (String.split_on_char '.' (List.hd (List.rev (String.split_on_char '/' i)))) ^ ".ixc"
    )
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.[a-zA-Z0-9_-]+$|}) output 0 then (* File with extension*) (
      output
    )
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+$|}) output 0 then (* File without extension *) (
      output ^ ".ixc"
    )
    else compile_error "Invalid output destination"
  ) with
  | Invalid_argument _ -> "./" ^ List.hd (String.split_on_char '.' (List.hd (List.rev (String.split_on_char '/' i)))) ^ ".ixc"
  | ex -> raise ex

let output = resolve_output input

let print_line ls l =
  Printf.printf "%i | %s\n" (l+1) (List.nth ls l)

let compile get_program =
  let file = open_in input in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  try (
    AssemblyWriter.write (get_program content) output
  ) with
  | Syntax_error (msg, l) -> (
    let () = Printf.printf "%s, on line %i:\n" msg (l+1) in
    let lines = String.split_on_char '\n' content in
    let printer =  print_line lines in match l with
    | 1 -> printer 0 ; printer 1
    | n when n = (List.length lines)-1 -> printer (n-1) ; printer (n)
    | _ ->  printer (l-1) ; printer l ; printer (l+1) 
  )
  | Compile_error msg -> Printf.printf "%s\n" msg

let () = match in_type with
  | IX -> compile (fun content -> ToProgramRep.compile(Parser.main Lexer.lex (Lexing.from_string content)))
  | IXA -> compile (fun content -> AssemblyParser.main AssemblyLexer.lex (Lexing.from_string content))