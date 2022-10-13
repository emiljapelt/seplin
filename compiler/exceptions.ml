exception Compile_error of string

let compile_error msg = raise (Compile_error msg)

exception Syntax_error of string * int

let syntax_error msg l = raise (Syntax_error (msg, !l))