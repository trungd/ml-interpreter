Compile

```
make
```

Input a file:

```
./miniml < <file_path>
```

or run in interactive mode

```
./miniml
```

# Available Functionality

- Declaration with arithmetic & logic calculations: +, -, *, &&, ||

```(ocaml)
# let y = let x = 2 in x * 2;;
val y : int = 4

# let x = 1 let y = x + 1;;
val x : int 1
val y : int 2

# let x = 1 in let x = 100 and y = x in x + y;;
val - : int = 101
```

- Comment (* like this *)

- Function & (Mutual) Recursive Function

```(ocaml)
# fun x -> fun y -> y x;;
val - : 'a -> ('a -> 'b) -> 'b = <fun>

# let rec fact n = if n = 0 then 1 else n * (fact (n + -1)) in fact 5;;
val - : int = 120

# let rec is_odd n = if n = 1 then true else is_even (n + -1) and is_even = if n = 0 then true else is_odd (n + -1);; is_even 99;;
val - : false

# fun x -> fun y -> fun z -> let b = x y z in if b then z y else y;;
val - : ('a -> ('a -> 'a) -> bool) -> 'a -> ('a -> 'a) -> 'a = <fun>
```

- List Manipulation (::, match)

```(ocaml)
# let l = 1::2::3::[];;
val l : int list = [1; 2; 3];;

# let l = [1; 2; 3];;
val l : int list = [1; 2; 3];;

let rec length l = match l with [] -> 0 | hd::tl -> 1 + length tl in length [1; 2; 3];;
val - : int 3
```

- Type Inference (Type Scheme supported)

All the results above will be returned with type.

```(ocaml)
# let f x = x in if f true then f 1 else f 0;;
val - : int = 1
```
