let f1 = fun x y z -> x + y + z in f1 2 5 6;;
let f3 = let f2 = fun x y z -> x + y + z in f2 4 5;;
f3 6;;