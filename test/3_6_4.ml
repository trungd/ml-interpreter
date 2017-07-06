match [1] with 
  [] -> [] 
  | [x] -> 1000 
  | head::tail -> tail;;

match [] with
  [] -> 1000;;