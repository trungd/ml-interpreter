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

let type_str cvar_num = 
  if cvar_num < 27 then "'" ^ (Char.escaped (Char.chr (96 + cvar_num)))
  else "'" ^ (Char.escaped (Char.chr (96 + cvar_num - 26))) ^ "1"

(* check if type_str exists in user specified types *)
let rec check_valid_cvar type_str = function
| TyVar (TyVarName tvname) -> type_str == tvname
| TyFun (ty1, ty2) -> (check_valid_cvar type_str ty1) && (check_valid_cvar type_str ty2)
| _ -> true

let rec next_cvar ty = (
  if check_valid_cvar (type_str (!cvar_count + 1)) ty then cvar_count := !cvar_count + 1
  else next_cvar ty
)

let pp_ty ty = let rec pp_ty' ty org_ty = (match ty with
| TyInt -> print_string "int"
| TyBool -> print_string "bool"
| TyList ty -> pp_ty' ty org_ty; print_string " list"
| TyVar (TyVarName tvname) -> print_string tvname
| TyVar (TyVarId tyvar) -> let (id, c) =
    try List.find (fun (id, c) -> id = tyvar) !cvar_ls 
    with Not_found -> 
      next_cvar org_ty;
      cvar_ls := (tyvar, !cvar_count)::!cvar_ls; 
      (tyvar, !cvar_count) in
  print_string (type_str c)
| TyFun (ty1, ty2) -> match ty1 with 
  | TyFun (_, _) -> print_string "("; pp_ty' ty1 org_ty; print_string ")"; print_string " -> "; pp_ty' ty2 org_ty
  | _ -> pp_ty' ty1 org_ty; print_string " -> "; pp_ty' ty2 org_ty) in
pp_ty' ty ty
  
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