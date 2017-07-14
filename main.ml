open Syntax
open Eval
open Printf
open Typing

let rec print_result id_val_ls ty_ls = (match id_val_ls with
| [] -> ()
| (id, v)::rest1 -> (match ty_ls with
    | [] -> ()
    | ty::rest2 ->
      printf "val %s : " id; 
      pp_ty ty; printf " = "; pp_val v; 
      print_newline (); 
      print_result rest1 rest2
  )
)
  
let err msg = Printf.printf "%s\n" msg

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

let rec read_eval_print env tyenv =
  print_string("# ");
  flush stdout;
  let instr = get_instr "" in
  let decl = Parser.toplevel Lexer.main (Lexing.from_string instr) in
    (* Ex 3.2.2 *)
    try 
      let (newtyenv, ty_ls) = ty_decl tyenv decl in
      let (id_val_ls, newenv) =  eval_decl env decl in
      print_result id_val_ls ty_ls;
      read_eval_print newenv newtyenv
    with
    | End_of_file -> print_endline ""
    | Error msg -> err msg; read_eval_print env tyenv
    (**)

(* Ex 3.2.1 *)
let initial_env = Environment.append Environment.empty
  [("i", IntV 1); ("ii", IntV 2); ("iii", IntV 3); ("iv", IntV 4); ("v", IntV 5); ("x", IntV 10)]

let initial_tyenv = Environment.append Environment.empty
  [("i", TyInt); ("ii", TyInt); ("iii", TyInt); ("iv", TyInt); ("v", TyInt); ("x", TyInt)]

let _ = print_endline "      miniml by Dang Viet Trung";
  print_endline "";
  read_eval_print initial_env initial_tyenv