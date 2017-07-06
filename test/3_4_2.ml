let f a b = a + b;;

let threetimes = fun f -> fun x -> f (f x x) (f x x) in threetimes (+) 5;;