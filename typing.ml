open Syntax

exception Error of string
let err s = raise (Error s)
let debug_mode = false

(* Type Environement *)
type tyenv = tysc Environment.t

let rec freevar_tyenv tyenv = 
  let tysc_ls = (List.map (fun (id, tysc) -> tysc) (Environment.get_list tyenv)) in
  MySet.bigunion(
    MySet.map (fun tysc -> freevar_tysc tysc) (MySet.from_list tysc_ls)
  )

type subst = (tyvar * ty) list

let log_ty_ls s = if debug_mode then
  (print_string ("ty " ^ string_of_int (List.length s) ^ "\n"); 
  List.iter (function (ty1, ty2) -> print_string "("; pp_ty ty1; print_string "; "; pp_ty ty2; print_string ")";) s;
  print_endline "") else ()

let log_ty ty = if debug_mode then (pp_ty ty; print_endline "") else ()

let log_subst s = if debug_mode then
  (print_string ("subst " ^ string_of_int (List.length s) ^ "\n"); 
  List.iter (function (tv, ty) -> print_string "("; pp_ty (TyVar tv); print_string "; "; pp_ty ty; print_string ")";) s;
  print_endline "") else ()

let log s = if debug_mode then print_endline s else ()

let log_tv_ty tv ty = print_string "("; pp_ty (TyVar tv); print_string "; "; pp_ty ty; print_string ")\n"

let rec subst_type subst t = match subst with
| [] -> t
| (tv, ty)::rest -> (match t with
  | TyInt -> TyInt
  | TyBool -> TyBool
  | TyList ty -> TyList (subst_type subst ty)
  | TyFun (ty1, ty2) -> 
      TyFun (subst_type subst ty1, subst_type subst ty2)
  | TyVar tv2 -> 
      if tv2 = tv then subst_type rest ty
      else subst_type rest t)

let closure ty tyenv subst = 
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv = MySet.bigunion(
    MySet.map (fun id -> freevar_ty (subst_type subst (TyVar id))) fv_tyenv'
  ) in 
  let ids = MySet.diff (freevar_ty ty) fv_tyenv in 
  TyScheme (MySet.to_list ids, ty)

let rec subst_types subst types = List.map (fun (ty1, ty2) -> (subst_type subst ty1, subst_type subst ty2)) types

let rec eqs_of_subst subst = match subst with
| [] -> []
| (tv, ty)::rest -> (TyVar tv, ty)::(eqs_of_subst rest)

let rec check_in_ftv tv = function
| TyVar tv' -> tv = tv'
| TyFun (ty1, ty2) -> (check_in_ftv tv ty1) || (check_in_ftv tv ty2)
| TyList ty -> check_in_ftv tv ty
| _ -> false

let rec unify l = match l with
| [] -> []
| (t1, t2)::tail -> 
    if t1 = t2 then unify tail
    else (match (t1, t2) with
    | (TyFun  (t11, t12), TyFun (t21, t22)) -> unify ((t11, t21)::(t12, t22)::tail)
    | (TyList ty1, TyList ty2) -> unify ((ty1, ty2)::tail)
    | (TyVar tv, ty) -> 
        if (check_in_ftv tv ty) then err "型があってませんよ1"
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
    (try let TyScheme (vars, ty) = Environment.lookup x tyenv in
      let s = List.map (fun id -> (id, TyVar (fresh_tyvar ()))) vars in
      ([], subst_type s ty)
    with
      Environment.Not_bound -> err ("Variable not bound: " ^ x))
| ILit _ -> ([], TyInt)
| BLit _ -> ([], TyBool)
| ListExp exps -> 
    if (List.length exps > 0) then let (s, ty) = ty_exp tyenv (List.hd exps) in ([], TyList ty)
    else ([], TyList (TyVar (fresh_tyvar())))
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
| LetExp (id_exp_ls, exp_ret) -> (match List.hd id_exp_ls with ((id, id_ty), exp) ->
    let (s1, ty1) = ty_exp tyenv exp in
    let newenv = Environment.extend id (closure ty1 tyenv s1) tyenv in
    let sret, tyret = ty_exp newenv exp_ret in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst sret) in
    let s = unify eqs in (s, subst_type s tyret))
| LetRecExp (ls, ret_exp) -> log "LetRecExp";
    let ((fun_id, fun_ty), (param_id, param_ty), exp) = List.hd ls in
    let param_ty = TyVar (fresh_tyvar ()) in
    let ret_tv = fresh_tyvar () in let ret_ty = TyVar ret_tv in
    let fun_ty = TyFun (param_ty, ret_ty) in
    log ("fun " ^ fun_id ^ ": ");
    log_ty fun_ty;
    (* extend param *)
    let newenv_param = Environment.extend param_id (tysc_of_ty param_ty) tyenv in
    let newenv = Environment.extend fun_id (tysc_of_ty fun_ty) newenv_param in
    let (se, tye) = ty_exp newenv exp in
    let se' = unify (eqs_of_subst se @ [(ret_ty, tye)]) in
    let fun_ty' = subst_type se' (TyFun(param_ty, tye)) in
    
    log ("fun " ^ fun_id ^ ": ");
    log_ty (TyFun(param_ty, tye));
    log_subst (se @ [(ret_tv, tye)]);
    log_ty (subst_type (se @ [(ret_tv, tye)]) (TyFun(param_ty, tye)));
    
    (* extend param and id *)
    let newenv_id = Environment.extend fun_id (closure fun_ty' tyenv se) tyenv in
    let sret, tyret = ty_exp newenv_id ret_exp in
    let eqs = (eqs_of_subst sret) @ (eqs_of_subst se) @ [(ret_ty, tye)] in
    log_ty_ls eqs;
    let s = unify eqs in
    log "/LetRecExp";
    (s, subst_type s tyret)
| FunExp (param_id_ls, ty, exp) -> log "FunExp";
    let (param_id, param_type) = List.hd param_id_ls in
    let param_ty = (match param_type with TyNone -> TyVar (fresh_tyvar ()) | ty -> ty) in
    let s_ret, ty_ret = ty_exp (Environment.extend param_id (tysc_of_ty param_ty) tyenv) exp in
    let eqs = (eqs_of_subst s_ret) @ (if ty != TyNone then [(ty, ty_ret)] else []) in
    let s = unify eqs in
    log ("fun " ^ param_id ^ " -> ...: ");
    log "/FunExp";
    (s, (subst_type s (TyFun (param_ty, ty_ret))))
| AppExp (expfun, ls_exp) -> log "AppExp";
    let sfun, tyfun = ty_exp tyenv expfun in
    let tyret = ref tyfun in
    let eqs = ref (eqs_of_subst sfun) in
    let f exp = let (se, tye) = ty_exp tyenv exp in
      match !tyret with 
      | TyFun (ty1', ty2') ->
        eqs := !eqs @ (eqs_of_subst se) @ [(tye, ty1')];
        tyret := ty2';
      | TyVar tv -> let ty_ret = TyVar (fresh_tyvar()) in 
        eqs := !eqs @ (eqs_of_subst se) @ [(TyVar tv, TyFun (tye, ty_ret))];
        tyret := ty_ret
      | _ -> err "型があってませんよ"
    in
    List.iter f ls_exp;
    log_ty_ls !eqs;
    log "/AppExp";
    let s = unify !eqs in (unify !eqs, subst_type s !tyret)
| AppendExp (e1, e2) ->
    let s1, ty1 = ty_exp tyenv e1 in
    let s2, ty2 = ty_exp tyenv e2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ [(ty2, TyList ty1)] in
    let s = unify eqs in
    (s, subst_type s (TyList ty1))
| MatchExp (exp, match_pattern_list) ->
    let se, tye = ty_exp tyenv exp in
    let lsty = (match tye with TyList lsty -> lsty | _ -> TyVar (fresh_tyvar ())) in
    let (pattern0, exp0) = List.hd (match_pattern_list) in
    let se0, tye0 = ty_exp tyenv exp0 in
    let eqs = ref [(tye, TyList lsty)] in
    let f = function (pattern, match_exp) -> let newenv = 
      (match pattern with
        | EmptyList -> tyenv
        | ListHeadTail (head_id, tail_id) -> Environment.extend tail_id (tysc_of_ty tye) (Environment.extend head_id (tysc_of_ty lsty) tyenv)) 
    in
    let sme, tyme = ty_exp newenv match_exp in
    eqs := !eqs @ (eqs_of_subst sme) @ [(tyme, tye0)];
    in
    List.iter f (List.tl match_pattern_list);
    let s = unify !eqs in (s, subst_type s tye0)
| _ -> err ("Not Implemented 1")

let ty_decl tyenv decl = cvar_count := 0; cvar_ls := []; 
match decl with
| Exp e -> let (_, ty) = ty_exp tyenv e in (tyenv, [ty])
| Decls id_exp_ls -> 
    let newtyenv = ref tyenv in
    let f = function((id, ty), exp) -> 
      let (env', ty) = ty_exp tyenv exp in
      newtyenv := Environment.extend id (TyScheme (MySet.to_list (freevar_ty ty), ty)) !newtyenv; ty in
    let tyexps = List.map f id_exp_ls in
    (!newtyenv, tyexps)
| AndDecls id_exp_ls -> 
    let newtyenv = ref tyenv in
    let f = function((id, ty), exp) -> 
      let (env', ty) = ty_exp tyenv exp in
      newtyenv := Environment.extend id (TyScheme (MySet.to_list (freevar_ty ty), ty)) !newtyenv; ty in
    let tyexps = List.map f id_exp_ls in
    (!newtyenv, tyexps)
| RecDecls id_exp_ls -> let ((fun_id, fun_ty), (param_id, param_ty), exp) = List.hd id_exp_ls in
    let (s, ty) = ty_exp tyenv (LetRecExp (id_exp_ls, Var fun_id)) in
    let newenv = Environment.extend fun_id (closure ty tyenv s) tyenv in
    (newenv, [ty])