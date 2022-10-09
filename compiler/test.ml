
open Absyn
open Program

let get_glob_vars p acc =
  match p with
  | [] -> acc
  | h::t -> match h with
    | Constant (l, t, n, expr) -> match t with
      | INT_T -> get_glob_vars t ()::acc
      | BOOL_T -> get_glob_vars t ()::acc
    | _ -> get_glob_vars t acc

let text = ("
locked int ten := 10;
external routine main() {
  int var := 0;
  stop;
}
")

let result = Parser.main Lexer.lex (Lexing.from_string text)
let () = print_ast result