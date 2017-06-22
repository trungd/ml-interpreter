(* ML interpreter / type reconstruction *)
type id = string
type idlist = id list

type binOp = Plus | Mult | Lt | Gt | Eq | And | Or

type exp =
  | Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | LetRecExp of id * id * exp * exp
  | FunExp of idlist * exp
  | AppExp of exp * exp list
  | OpFunExp of binOp

type program = 
  | Exp of exp
  | Decl of id * exp  
  | RecDecl of id * id * exp
