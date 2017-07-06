let makemult maker x =
  (if x < 1 then 0 else 4) + maker (maker (x - 1));;

let times4 = fun x -> makemult makemult x in
  times4 5;;