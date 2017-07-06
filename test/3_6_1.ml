let is_empty = fun ls -> match ls with [] -> true | ls::rest -> false;;
is_empty [1; 2; 3];;
is_empty [];;
match 5::3::[] with [] -> 1 | head :: tail -> head * 2;;