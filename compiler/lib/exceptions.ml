

exception Error of string option * int option * string  (* file, line, explanation *)

let raise_error msg = raise (Error (None, None, msg))