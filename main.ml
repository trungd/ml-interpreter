open Syntax
open Eval
open Printf

let rec print_result ls = List.iter 
  (function (id, v) -> printf "val %s = " id; pp_val v; print_newline ()) ls
  
let err msg = Printf.printf "Error: %s\n" msg

let contains_semisemi s = let flag = ref false in
  for i = 0 to String.length(s) - 2 do
    if s.[i] = ';' && s.[i + 1] = ';' then flag := true else ();
  done; !flag

let rec get_instr instr = 
  let s = String.trim(read_line()) in 
  let new_instr = instr ^ " " ^ s in
  if s = "" then get_instr instr
  else if contains_semisemi s then new_instr
  else get_instr new_instr

let rec read_eval_print env =
  print_string("# ");
  flush stdout;
  let instr = get_instr "" in
  let decl = Parser.toplevel Lexer.main (Lexing.from_string instr) in
    (* Ex 3.2.2 *)
    try let (ls, newenv) =  eval_decl env decl in
      print_result ls;
      read_eval_print newenv
    with
    | End_of_file -> print_endline ""
    | Error msg -> err msg; read_eval_print env
    (**)

(* Ex 3.2.1 *)
let initial_env = Environment.append Environment.empty
  [("i", IntV 1); ("ii", IntV 2); ("iii", IntV 3); ("iv", IntV 4); ("v", IntV 5); ("x", IntV 10)]


let _ = let env = initial_env in
  print_endline "      miniml by Dang Viet Trung";
  print_endline "";
  read_eval_print env
