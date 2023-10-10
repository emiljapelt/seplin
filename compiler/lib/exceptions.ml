

exception Failure of string option * int option * string  (* file, line, explanation *)

let raise_failure msg = raise (Failure (None, None, msg))