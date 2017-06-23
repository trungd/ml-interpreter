open Syntax

let print_env env = 
  print_endline (string_of_int (Environment.length env))