open Printf
open ProgramRep
open Exceptions

(* HELPERS *)

let write_word file w =
  for i = 0 to 7 do
    fprintf file "%c" (Char.chr (Int64.to_int (Int64.logand 255L (Int64.shift_right w (i * 8)))))
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
      | CharInstruction _ -> retrieve_labels t (c+2) acc
      | LabelInstruction _ -> retrieve_labels t (c+9) acc
      | _ -> retrieve_labels t (c+1) acc
    )

let retrieve_structs program =
  match program with
  | Program (strs,_,_) -> strs

let retrieve_global_vars program =
  match program with
  | Program (_,gvs,_) -> gvs

let retrieve_program_parts program =
  match program with
  | Program (_,_,p) -> p

let get_index list prop =
  let rec aux l acc =
    match l with
    | [] -> None
    | h::t -> if prop(h) then Some acc else aux t (acc+1)
  in
  aux list 0


(* Writers *)

let rec write_type_info file lock ty structs =
  if lock then fprintf file "\x01" else fprintf file "\x00" ;
  match ty with
  | T_Int -> fprintf file "\x00" ; fprintf file "%c" (Char.chr (ProgramRep.type_index T_Int))
  | T_Bool -> fprintf file "\x00" ; fprintf file "%c" (Char.chr (ProgramRep.type_index T_Bool))
  | T_Char -> fprintf file "\x00" ; fprintf file "%c" (Char.chr (ProgramRep.type_index T_Char))
  | T_Array arr_ty -> fprintf file "\x01" ; write_type_info file false arr_ty structs
  | T_Struct str_name -> (
    match get_index structs (fun (name, fields) -> str_name = name) with
    | None -> failwith "struct not found while writing binary"
    | Some i -> fprintf file "\x02" ; write_word file (Int64.of_int i)
  )
  | T_Null -> failwith ("writing null type")



let rec write_entry_point_info file name addr args structs =
  fprintf file "%s%c" name '\x00';
  write_word file (Int64.of_int addr);
  fprintf file "%c" (Char.chr (List.length args));
  let rec print_args a = 
    match a with
    | [] -> ()
    | (lock,ty)::t -> write_type_info file lock ty structs
  in
  print_args args

let rec write_entry_points file pps structs =
  write_word file (Int64.of_int (count_entry_points pps)) ;
  let rec aux parts addr =
    match parts with
    | [] -> ()
    | h::t -> match h with
      | Label _ -> aux t addr
      | EntryPoint (name, args) -> write_entry_point_info file name addr args structs ; aux t addr
      | IntInstruction _ -> aux t (addr+9)
      | BoolInstruction _ -> aux t (addr+2)
      | CharInstruction _ -> aux t (addr+2)
      | LabelInstruction _ -> aux t (addr+9)
      | _ -> aux t (addr+1)
  in
  aux pps 0



let rec write_global_var_info file name lock ty structs =
  fprintf file "%s%c" name '\x00' ; write_type_info file lock ty structs

let rec write_global_vars file gvs structs =
  write_word file (Int64.of_int (List.length gvs)) ;
  match gvs with
  | [] -> ()
  | (lock,ty,name)::t -> write_global_var_info file name lock ty structs



let rec write_struct_info file name fields structs =
  fprintf file "%s%c" name '\x00'; fprintf file "%c" (Char.chr (List.length structs)) ;
  let rec aux fs =
    match fs with
    | [] -> ()
    | (lock,ty,_)::t -> write_type_info file lock ty structs
  in
  aux fields

let write_structs file structs =
  write_word file (Int64.of_int (List.length structs)) ;
  let rec aux strs =
    match strs with
    | [] -> ()
    | (name,fields)::t -> write_struct_info file name fields structs ; aux t
  in
  aux structs






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
    | CharInstruction (i, c) -> (
      fprintf f "%c" (Char.chr i);
      fprintf f "%c" (c);
      write_program_parts f t labels
    )
    | LabelInstruction (i, l) -> (
      fprintf f "%c" (Char.chr i);
      match find_label l labels with
      | None -> raise_error ("Undefined label: " ^ l)
      | Some a -> write_word f (Int64.of_int a);
      write_program_parts f t labels
    )
    | _ -> write_program_parts f t labels

let write program dest =
  let structs = retrieve_structs program in
  let program_parts = retrieve_program_parts program in
  let global_vars = retrieve_global_vars program in
  let labels = retrieve_labels program_parts 0 [] in
  let output = open_out dest in
  let () = write_structs output structs in
  let () = write_global_vars output global_vars structs in
  let () = write_entry_points output program_parts structs in
  let () = write_program_parts output program_parts labels in
  close_out output