open Inexclib.AssemblyWriter
open Inexclib.ToProgramRep
open Str
open Inexclib.Exceptions


type input_type =
| IX
| IXA

let resolve_input () =
  try (
    let input = Sys.argv.(1) in
    if not (Sys.file_exists input) then raise_error "Input file does not exist"
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.ix$|}) input 0 then (input, IX)
    else if Str.string_match (regexp {|^\(\.\.?\)?\/\(\([a-zA-Z0-9_-]+\|\(\.\.?\)\)\/\)*[a-zA-Z0-9_-]+\.ixa$|}) input 0 then (input, IXA)
    else raise_error "Invalid input file extension"
  ) with
  | Invalid_argument _ -> raise_error "No file given to compile"
  | ex -> raise ex

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
    else raise_error "Invalid output destination"
  ) with
  | Invalid_argument _ -> "./" ^ List.hd (String.split_on_char '.' (List.hd (List.rev (String.split_on_char '/' i)))) ^ ".ixc"
  | ex -> raise ex

let print_line ls l =
  Printf.printf "%i | %s\n" (l+1) (List.nth ls l)

let read_file path =
  let file = open_in path in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  content
    
let (input, in_type) = resolve_input ()
let output = resolve_output input
let () = try (
  match in_type with
  | IX -> write (compile input (fun file -> Inexclib.Parser.main (Inexclib.Lexer.start file) (Lexing.from_string (read_file file)))) output
  | IXA -> write (Inexclib.AssemblyParser.main (Inexclib.AssemblyLexer.start input) (Lexing.from_string (read_file input))) output
) with
| Line_error (msg, file, line) -> (
  let lines = String.split_on_char '\n' (read_file file) in
  let () = Printf.printf "Error in %s:\n%s, on line %i:\n" file msg line in
  let printer =  print_line lines in match line with
  | 1 -> printer 0 ; printer 1
  | n when n = (List.length lines)-1 -> printer (n-2) ; printer (n-1)
  | _ ->  printer (line-2) ; printer (line-1) ; printer line
)
| Error msg -> Printf.printf "%s\n" msg