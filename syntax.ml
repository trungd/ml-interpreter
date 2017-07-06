(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Minus | Mult | Lt | Gt | Eq | And | Or

type matchPattern =
| EmptyList
| SingleElementList of id
| ListHeadTail of id * id

type exp =
| Var of id
| ILit of int
| BLit of bool
| BinOp of binOp * exp * exp
| IfExp of exp * exp * exp
| LetExp of (id * exp) list * exp
| LetRecExp of (id * id * exp) list * exp
| FunExp of id list * exp
| DFunExp of id * exp
| AppExp of exp * exp list
| OpFunExp of binOp
| MatchExp of exp * (matchPattern * exp) list
| ListExp of exp list
| AppendExp of exp * exp

type program = 
| Exp of exp
| Decl of id * exp
| Decls of (id * exp) list
| AndDecls of (id * exp) list
| RecDecls of (id * id * exp) list

exception Error of string
let err s = raise (Error s)