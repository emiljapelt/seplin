open AssemblyWriter
open ToProgramRep
open Str
open Exceptions

let input = Sys.argv.(1)
let output = Sys.argv.(2)

let print_line ls l =
  Printf.printf "%i |%s\n" l (List.nth ls (l-1))

let compileAssembly () = 
  let file = open_in input in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  try (
    let program = AssemblyParser.main AssemblyLexer.lex (Lexing.from_string content) in
    AssemblyWriter.write program output
  ) with
  | Syntax_error (msg, l) -> (
    let () = Printf.printf "%s on line %i:\n" msg l in
    let lines = String.split_on_char '\n' content in
    let printer =  print_line lines in match l with
    | 1 -> printer 0 ; printer 1
    | n when n = List.length lines -> printer (n-1) ; printer (n-2)
    | _ ->  printer (l-1) ; printer l ; printer (l+1) 
  )
  | Compile_error msg -> Printf.printf "%s\n" msg

let compile () =
  let file = open_in input in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  try (
    let program = ToProgramRep.compile(Parser.main Lexer.lex (Lexing.from_string content)) in
    AssemblyWriter.write program output
  ) with
  | Syntax_error (msg, l) -> (
    let () = Printf.printf "%s on line %i:\n" msg l in
    let lines = String.split_on_char '\n' content in
    let printer =  print_line lines in match l with
    | 1 -> printer 0 ; printer 1
    | n when n = List.length lines -> printer (n-1) ; printer (n-2)
    | _ ->  printer (l-1) ; printer l ; printer (l+1) 
  )
  | Compile_error msg -> Printf.printf "%s\n" msg

let () = 
  if Str.string_match (regexp {|.+\.ixa$|}) input 0 then compileAssembly ()
  else if Str.string_match (regexp {|.+\.ix$|}) input 0 then compile ()
  else Printf.printf "File type not supported"