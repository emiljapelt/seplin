open AssemblyWriter
open ToProgramRep
open Str
open Exceptions

let input = Sys.argv.(1)
let output = Sys.argv.(2)

let compileAssembly () = 
  let file = open_in input in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  let program = AssemblyParser.main AssemblyLexer.lex (Lexing.from_string content) in
  AssemblyWriter.write program output

let compile () =
  let file = open_in input in
  let content = really_input_string (file) (in_channel_length file) in
  let () = close_in_noerr file in
  try (
    let program = ToProgramRep.compile(Parser.main Lexer.lex (Lexing.from_string content)) in
    AssemblyWriter.write program output
  ) with
  | Syntax_error (msg, l) -> Printf.printf "%s\n%i | %s\n" (msg ^ " on line " ^ (string_of_int l)) l (List.nth (String.split_on_char '\n' content) (l-1))
  | Compile_error msg -> Printf.printf "%s\n" msg

let () = 
  if Str.string_match (regexp {|.+\.ixa$|}) input 0 then compileAssembly ()
  else if Str.string_match (regexp {|.+\.ix$|}) input 0 then compile ()
  else Printf.printf "File type not supported"