open Syntax

type exval = 
    IntV of int
  | BoolV of bool
  | ProcV of idlist * exp * dnval Environment.t ref
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
  | IntV i -> string_of_int i
  | BoolV b -> string_of_bool b
  | ProcV (ids, exp, dnval) -> "<fun>"

let rec string_of_exp = function
  | Var id -> id
  | ILit i -> string_of_int i
  | BLit b -> string_of_bool b

let pp_val v = print_string (string_of_exval v)

let rec apply_prim op arg1 arg2 = match op, arg1, arg2 with
  | Plus, IntV i1, IntV i2 -> IntV (i1 + i2)
  | Plus, _, _ -> err ("Both arguments must be integer: +" ^ string_of_exval arg1)
  | Minus, IntV i1, IntV i2 -> IntV (i1 - i2)
  | Minus, _, _ -> err ("Both arguments must be integer: -" ^ string_of_exval arg1)
  | Mult, IntV i1, IntV i2 -> IntV (i1 * i2)
  | Mult, _, _ -> err ("Both arguments must be integer: *")
  | Lt, IntV i1, IntV i2 -> BoolV (i1 < i2)
  | Lt, _, _ -> err ("Both arguments must be integer: <")
  | Gt, IntV i1, IntV i2 -> BoolV (i1 > i2)
  | Gt, _, _ -> err ("Both arguments must be integer: >")
  | Eq, IntV i1, IntV i2 -> BoolV (i1 = i2)
  | Eq, _, _ -> err ("Both arguments must be integer: =")
  (* Ex 3.2.3: implement && and || *)
  | And, BoolV b1, BoolV b2 -> BoolV (b1 && b2)
  | And, _, _ -> err ("Both arguments must be boolean: &&")
  | Or, BoolV b1, BoolV b2 -> BoolV (b1 || b2)
  | Or, _, _ -> err ("Both arguments must be boolean: ||")
  (* /Ex 3.2.3 *)

let rec eval_exp env = function
  | Var x -> 
      (try Environment.lookup x env with 
        Environment.Not_bound -> err ("Variable not bound: " ^ x))
  | ILit i -> IntV i
  | BLit b -> BoolV b
  | BinOp (op, exp1, exp2) -> 
      let arg1 = eval_exp env exp1 in
      let arg2 = eval_exp env exp2 in
      apply_prim op arg1 arg2
  | IfExp (exp1, exp2, exp3) ->
      let test = eval_exp env exp1 in
        (match test with
          | BoolV true -> eval_exp env exp2 
          | BoolV false -> eval_exp env exp3
          | _ -> err ("Test expression must be boolean: if"))
  | LetExp(ls, exp2) ->
      let rec extend_env ls newenv = match ls with
        | [] -> newenv
        | it::rest -> (match it with (id, exp) -> 
            let v = eval_exp env exp
            in extend_env rest (Environment.extend id v newenv))
      in eval_exp (extend_env ls env) exp2
  | FunExp (ids, exp) -> ProcV(ids, exp, ref env)
  | AppExp (expfun, explist) ->
      let funval = eval_exp env expfun in (match funval with
        | ProcV (idlist, body, env') -> 
            let rec add_to_env ids exps newenv =
              match exps with
                | [] -> (newenv, ids, exps)
                | exp::exps_rest ->
                    (match ids with
                      | [] -> (newenv, ids, exps)
                      | id::ids_rest ->
                          let arg = eval_exp env exp
                          in add_to_env ids_rest exps_rest (Environment.extend id arg newenv)
                    )
            in let (newenv, ids, exps) = add_to_env idlist explist !env' 
              in
                if (List.length ids) > 0 then ProcV(ids, body, ref newenv)
                else if (List.length exps) > 0 then
                  let ret_exval = eval_exp newenv body
                  in (match ret_exval with
                      (* TODO: review env2' + newenv *)
                      | ProcV(idlist2, body2, env2') -> 
                          eval_exp newenv (AppExp (FunExp (idlist2, body2), exps))
                    )
                else eval_exp newenv body
        | _ -> err ("None-function value is applied: " ^ string_of_exp(expfun))
      )
      
  (* Ex 3.4.2 *)
  | OpFunExp (op) ->
      ProcV (["x"], FunExp(["y"], BinOp(op, Var("x"), Var("y"))), ref env)
  (**)
  | LetRecExp (id, para, exp1, exp2) ->
      let dummyenv = ref Environment.empty in
        let newenv = Environment.extend id (ProcV ([para], exp1, dummyenv)) env in
          dummyenv := newenv;
          eval_exp newenv exp2

let eval_decl env = function
  | Exp exp -> let value = eval_exp env exp in ([("-", value)], env)
  | Decls id_val_ls (* Ex 3.3.2: enable multiple declaration *) ->
      let newenv = ref env in
      (* Add id and value to the extended newenv *)
      let rec decl_ls ls = match ls with
        | [] -> []
        | (id, exp)::rest -> 
            let value = eval_exp env exp (* All expressions are calculated under original env *)
            in 
              newenv := Environment.extend id value !newenv;
              (id, value)::decl_ls rest 
      in (decl_ls id_val_ls, !newenv)
  | RecDecl (id, param, exp) -> let value = ProcV ([param], exp, ref env)
      in ([(id, value)], Environment.extend id value env)