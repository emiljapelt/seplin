open Printf
open ProgramRep

(* HELPERS *)

let write_word f w =
  for i = 0 to 7 do
    fprintf f "%c" (Char.chr (Int64.to_int (Int64.logand 255L (Int64.shift_right w (i * 8)))))
  done

let count_entry_points program =
  let rec aux p acc =
    match p with
    | [] -> acc
    | h::t -> (
      match h with
      | EntryPoint _ -> aux t (acc+1)
      | _ -> aux t acc 
    )
  in
  aux program 0

let rec retrieve_labels program c acc =
    match program with
    | [] -> acc
    | h::t -> (
      match h with
      | Label (s) -> retrieve_labels t c ((s, c)::acc)
      | EntryPoint (n, _) -> retrieve_labels t c ((n, c)::acc)
      | IntInstruction _ -> retrieve_labels t (c+9) acc
      | BoolInstruction _ -> retrieve_labels t (c+2) acc
      | LabelInstruction _ -> retrieve_labels t (c+9) acc
      | _ -> retrieve_labels t (c+1) acc
    )

let retrieve_global_vars program =
  match program with
  | Program (gvs, _) -> gvs

let retrieve_program_parts program =
  match program with
  | Program (_, p) -> p

let rec write_entry_point f name addr ts =
  fprintf f "%s%c" name '\x00';
  write_word f (Int64.of_int addr);
  fprintf f "%c" (Char.chr (List.length ts));
  let rec print_ts types = 
    match types with
    | [] -> ()
    | h::t -> 
      match h with
      | T_Int -> fprintf f "\x02" ; print_ts t
      | T_Bool -> fprintf f "\x01" ; print_ts t
  in
  print_ts ts

let rec write_entry_points f eps addr =
  match eps with
  | [] -> ()
  | h::t -> match h with
    | Label _ -> write_entry_points f t addr
    | EntryPoint (n, ts) -> write_entry_point f n addr ts ; write_entry_points f t addr
    | IntInstruction _ -> write_entry_points f t (addr+9)
    | BoolInstruction _ -> write_entry_points f t (addr+2)
    | LabelInstruction _ -> write_entry_points f t (addr+9)
    | _ -> write_entry_points f t (addr+1)

let rec write_global_vars f gvs =
  match gvs with
  | [] -> ()
  | h::t -> match h with
    | G_Int (lock, v) -> (
      if lock then fprintf f "%c" '\x82' else fprintf f "\x02" ;
      write_word f (Int64.of_int v) ;
      write_global_vars f t
    )
    | G_Bool (lock, v) -> (
      if lock then fprintf f "\x81" else fprintf f "\x01" ;
      if v then fprintf f "%c" '\x01' else fprintf f "%c" '\x00' ;
      write_global_vars f t
    )

let rec find_label l labels =
  match labels with
  | [] -> None
  | (n,a)::t -> if (String.equal n l) then Some a else find_label l t

let rec write_program_parts f pp labels =
  match pp with
  | [] -> ()
  | h::t -> match h with
    | Instruction i -> (
      fprintf f "%c" (Char.chr i);
      write_program_parts f t labels
    )
    | IntInstruction (i, v) -> (
      fprintf f "%c" (Char.chr i);
      write_word f (Int64.of_int v);
      write_program_parts f t labels
    )
    | BoolInstruction (i, v) -> (
      fprintf f "%c" (Char.chr i);
      if v then fprintf f "\x01" else fprintf f "\x00";
      write_program_parts f t labels
    )
    | LabelInstruction (i, l) -> (
      fprintf f "%c" (Char.chr i);
      match find_label l labels with
      | None -> failwith (String.concat " " ["Undefined label:"; l])
      | Some a -> write_word f (Int64.of_int a);
      write_program_parts f t labels
    )
    | _ -> write_program_parts f t labels

let write program dest =
  let program_parts = retrieve_program_parts program in
  let global_vars = retrieve_global_vars program in
  let labels = retrieve_labels program_parts 0 [] in
  let output = open_out dest in
  let () = write_word output (Int64.of_int (count_entry_points program_parts))  in
  let () = write_entry_points output program_parts 0 in
  let () = write_word output (Int64.of_int (List.length global_vars)) in
  let () = write_global_vars output global_vars in
  let () = write_program_parts output program_parts labels in
  close_out output