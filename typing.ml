open Syntax

exception Error of string
let err s = raise (Error s)

(* Type Environement *)
type tyenv = ty Environment.t

type subst = (tyvar * ty) list

let print_ty_ls s = 
  print_string ("ty " ^ string_of_int (List.length s) ^ "\n"); 
  List.iter (function (ty1, ty2) -> print_string "("; pp_ty ty1; print_string "; "; pp_ty ty2; print_string ")";) s;
  print_endline ""

let print_subst s = 
  print_string ("subst " ^ string_of_int (List.length s) ^ "\n"); 
  List.iter (function (tv, ty) -> print_string "("; pp_ty (TyVar tv); print_string "; "; pp_ty ty; print_string ")";) s;
  print_endline ""

let print_tv_ty tv ty = print_string "("; pp_ty (TyVar tv); print_string "; "; pp_ty ty; print_string ")\n"

let rec subst_type subst t = match subst with
| [] -> t
| (tv, ty)::rest -> (match t with 
  | TyInt -> TyInt
  | TyBool ->  TyBool
  | TyFun (ty1, ty2) -> 
      TyFun (subst_type subst ty1, subst_type subst ty2)
  | TyVar tv2 -> 
      if tv2 = tv then subst_type rest ty
      else subst_type rest t
  | _ -> err ".")

let rec subst_types subst types = List.map (fun (ty1, ty2) -> (subst_type subst ty1, subst_type subst ty2)) types

let rec eqs_of_subst subst = match subst with
| [] -> []
| (tv, ty)::rest -> (TyVar tv, ty)::(eqs_of_subst rest)

let rec check_in_ftv tv = function
| TyVar tv' -> tv = tv'
| TyFun (ty1, ty2) -> (check_in_ftv tv ty1) || (check_in_ftv tv ty2)
| _ -> false

let rec unify l = match l with
| [] -> []
| (t1, t2)::tail -> 
    if t1 = t2 then unify tail
    else (match (t1, t2) with
    | (TyFun  (t11, t12), TyFun (t21, t22)) -> unify ((t11, t21)::(t12, t22)::tail)
    | (TyVar tv, ty) -> 
        if (check_in_ftv tv ty) then err "型があってませんよ"
        else (tv, ty) :: (unify (subst_types [(tv, t2)] tail))
    | (ty, TyVar tv) -> 
        if (check_in_ftv tv ty) then err "型があってませんよ2"
        else (tv, ty) :: (unify (subst_types [(tv, t2)] tail))
    | _ -> err "型があってませんよ3")

let ty_prim op ty1 ty2 = match op with
| Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
| Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
| And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
| Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
| Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
| Gt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
| Eq -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
| _ -> err "Not Implemented 3"

let rec ty_exp tyenv = function
| Var x ->
    (try ([], Environment.lookup x tyenv) with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
| ILit _ -> ([], TyInt)
| BLit _ -> ([], TyBool)
| BinOp (op, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
    let s = unify eqs in (s, subst_type s ty)
| IfExp (exp1, exp2, exp3) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (s3, ty3) = ty_exp tyenv exp3 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ [(ty1, TyBool); (ty2, ty3)] in
    let s = unify eqs in (s, subst_type s ty2)
| LetExp (id_exp_ls, exp_ret) -> (match List.hd id_exp_ls with (id, exp) ->
    let (s1, ty1) = ty_exp tyenv exp in
    let (s_ret, ty_ret) = ty_exp (Environment.extend id ty1 tyenv) exp_ret in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s_ret) in
    let s = unify eqs in (s, subst_type s ty_ret))
| FunExp (id_ls, exp) ->
    let domty = TyVar (fresh_tyvar ()) in
    let s, ranty =
      ty_exp (Environment.extend (List.hd id_ls) domty tyenv) exp in
      (s, TyFun (subst_type s domty, ranty))
| AppExp (expfun, ls_exp) -> 
    let sfun, tyfun = ty_exp tyenv expfun in
    let tyret = ref tyfun in
    let eqs = ref [] in
    let eqs' = ref [] in
    let f exp = let (se, tye) = ty_exp tyenv exp in
      match !tyret with 
      | TyFun (ty1', ty2') ->
        eqs := !eqs @ (eqs_of_subst se) @ [(tye, ty1')];
        tyret := ty2';
      | TyVar tv -> let tv_ret = TyVar (fresh_tyvar()) in 
        eqs := !eqs @ [(TyVar tv, TyFun(tye, tv_ret))];
        eqs' := !eqs' @ [(TyVar tv, TyFun(tye, tv_ret))];
        tyret := tv_ret
      | _ -> err "Cannot be applied"
    in
    List.iter f ls_exp;
    let s = unify !eqs in (unify !eqs', subst_type s !tyret)
| _ -> err ("Not Implemented 1")

let ty_decl tyenv decl = cvar := (Char.chr 96); cvar_ls := []; 
match decl with
| Exp e -> let (s, ty) = ty_exp tyenv e in (tyenv, [ty])
| Decls id_exp_ls -> 
    let newtyenv = ref tyenv in
    let f = function(id, exp) -> 
      let (env', ty) = ty_exp tyenv exp in
      newtyenv := Environment.extend id ty !newtyenv; ty in
    let tyexps = List.map f id_exp_ls in
    (!newtyenv, tyexps)
| AndDecls id_exp_ls -> 
    let newtyenv = ref tyenv in
    let f = function(id, exp) -> 
      let (env', ty) = ty_exp tyenv exp in
      newtyenv := Environment.extend id ty tyenv; ty in
    let tyexps = List.map f id_exp_ls in
    (!newtyenv, tyexps)