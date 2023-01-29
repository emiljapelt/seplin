exception Error of string

let raise_error msg = raise (Error msg)

exception Line_error of string * string * int

let raise_line_error msg file line = raise (Line_error (msg, file, line))