exception Error of string

let raise_error msg = raise (Error msg)

exception Offset_error of string * int

let raise_offset_error msg offset = raise (Offset_error (msg, offset))