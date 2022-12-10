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

let offset_to_line o ls =
  let rec aux offset lines acc = 
    match lines with
    | [] -> acc
    | h::t -> if offset - (String.length h) < 0 then acc else aux (offset - (String.length h)) t (acc+1)
  in
  aux o ls 0


let (input, in_type) = resolve_input ()
let output = resolve_output input
let file = open_in input
let content = really_input_string (file) (in_channel_length file)
let () = close_in_noerr file
let () = try (
  match in_type with
  | IX -> AssemblyWriter.write (ToProgramRep.compile(Parser.main Lexer.lex (Lexing.from_string content))) output
  | IXA -> AssemblyWriter.write (AssemblyParser.main AssemblyLexer.lex (Lexing.from_string content)) output
) with
| Offset_error (msg, offset) -> (
  let lines = String.split_on_char '\n' content in
  let line_num = offset_to_line offset lines in
  let () = Printf.printf "%s, on line %i:\n" msg (line_num+1) in
  let printer =  print_line lines in match line_num with
  | 1 -> printer 0 ; printer 1
  | n when n = (List.length lines)-1 -> printer (n-1) ; printer (n)
  | _ ->  printer (line_num-1) ; printer line_num ; printer (line_num+1) 
)
| Error msg -> Printf.printf "%s\n" msg