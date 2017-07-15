(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Minus | Mult | Lt | Gt | Eq | And | Or | Cons

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

type tyvar = int

type ty =
| TyInt
| TyBool
| TyList of ty
| TyVar of tyvar
| TyFun of ty * ty

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)

let cvar = ref (Char.chr 96)
let cvar_ls = ref []

let rec pp_ty = function
| TyInt -> print_string "int"
| TyBool -> print_string "bool"
| TyList ty -> pp_ty ty; print_string " list"
| TyVar tyvar -> let (id, c) =
    try List.find (fun (id, c) -> id = tyvar) !cvar_ls 
    with Not_found -> 
      cvar := Char.chr (Char.code !cvar + 1);
      cvar_ls := (tyvar, !cvar)::!cvar_ls; 
      (tyvar, !cvar) in
  print_string ("'" ^ (Char.escaped c))
| TyFun (ty1, ty2) -> match ty1 with 
  | TyFun (_, _) -> print_string "("; pp_ty ty1; print_string ")"; print_string " -> "; pp_ty ty2
  | _ -> pp_ty ty1; print_string " -> "; pp_ty ty2
  

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; v
  in body

exception Error of string
let err s = raise (Error s)