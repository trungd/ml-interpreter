(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Minus | Mult | Lt | Gt | Eq | And | Or | Cons

type matchPattern =
| EmptyList
| SingleElementList of id
| ListHeadTail of id * id

type tyvar = 
| TyVarId of int 
| TyVarName of string

type ty =
| TyNone
| TyInt
| TyBool
| TyList of ty
| TyVar of tyvar
| TyFun of ty * ty

type exp =
| Var of id
| ILit of int
| BLit of bool
| BinOp of binOp * exp * exp
| IfExp of exp * exp * exp
| LetExp of ((id * ty) * exp) list * exp
| LetRecExp of ((id * ty) * (id * ty) * exp) list * exp
| FunExp of (id * ty) list * ty * exp
| DFunExp of id * exp
| AppExp of exp * exp list
| OpFunExp of binOp
| MatchExp of exp * (matchPattern * exp) list
| ListExp of exp list
| AppendExp of exp * exp

type program = 
| Exp of exp
| Decl of (id * ty) * exp
| Decls of ((id * ty) * exp) list
| AndDecls of ((id * ty) * exp) list
| RecDecls of ((id * ty) * (id * ty) * exp) list

type tysc = TyScheme of tyvar list * ty

let tysc_of_ty ty = TyScheme ([], ty)

let cvar_count = ref 0
let cvar_ls = ref []
let current_ty = ref TyNone

let type_str cvar_num = 
  if cvar_num < 27 then "'" ^ (Char.escaped (Char.chr (96 + cvar_num)))
  else "'" ^ (Char.escaped (Char.chr (96 + cvar_num - 26))) ^ "1"

(* check if type_str exists in user specified types *)
let rec check_valid_cvar type_str = function
| TyVar (TyVarName tvname) -> type_str != tvname
| TyFun (ty1, ty2) -> (check_valid_cvar type_str ty1) && (check_valid_cvar type_str ty2)
| _ -> true

let rec next_cvar () = (
  if check_valid_cvar (type_str (!cvar_count + 1)) !current_ty then cvar_count := !cvar_count + 1
  else next_cvar ()
)

let string_of_ty ty = current_ty := ty; let rec string_of_ty' ty = (match ty with
| TyNone -> "none"
| TyInt -> "int"
| TyBool -> "bool"
| TyList ty -> (string_of_ty' ty) ^ " list"
| TyVar (TyVarName tvname) -> tvname
| TyVar (TyVarId tyvar) -> let (id, c) =
    try List.find (fun (id, c) -> id = tyvar) !cvar_ls 
    with Not_found -> 
      next_cvar ();
      cvar_ls := (tyvar, !cvar_count)::!cvar_ls; 
      (tyvar, !cvar_count) in
  (type_str c)
| TyFun (ty1, ty2) -> match ty1 with 
  | TyFun (_, _) -> let sty1 = string_of_ty' ty1 in let sty2 = string_of_ty' ty2 in "(" ^ sty1 ^ ") -> " ^ sty2
  | _ -> let sty1 = string_of_ty' ty1 in let sty2 = string_of_ty' ty2 in sty1 ^ " -> " ^ sty2) in
string_of_ty' ty

let pp_ty ty = print_string (string_of_ty ty)
  
let rec freevar_ty = function
| TyVar tv -> MySet.singleton tv
| TyFun (ty1, ty2) -> MySet.union (freevar_ty ty1) (freevar_ty ty2)
| _ -> MySet.empty

let rec freevar_tysc = function TyScheme (vars, ty) ->
  let freevars = freevar_ty ty in
  MySet.diff freevars (MySet.from_list vars)

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
    counter := v + 1; TyVarId v
  in body

exception Error of string
let err s = raise (Error s)