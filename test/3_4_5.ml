(* using fun *)
let a = 3 in
let p = fun x -> x + a in
let a = 5 in
a * p 2;;

(* using dfun *)
let a = 3 in
let p = dfun x -> x + a in
let a = 5 in
a * p 2;;