let f x: bool = 5;;
let f (x: (int -> int)) = fun y -> x;;

let x: int = 0 in 0;;
let f: int = fun x -> x;;
let f (x: int) = x;;

let rec f x = x in f f;;