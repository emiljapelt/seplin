open AssemblyWriter
open ToProgramRep
open Str

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
  let program = ToProgramRep.compile(Parser.main Lexer.lex (Lexing.from_string content)) in
  AssemblyWriter.write program output

let () = 
  if Str.string_match (regexp {|.+\.ixa$|}) input 0 then compileAssembly ()
  else if Str.string_match (regexp {|.+\.ix$|}) input 0 then compile ()
  else failwith "File type not supported"