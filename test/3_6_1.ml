let is_empty = fun ls -> match ls with [] -> true | ls::rest -> false;;
is_empty [1; 2; 3];;
is_empty [];;