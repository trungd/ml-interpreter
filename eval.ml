open Syntax

type exval = 
| IntV of int
| BoolV of bool
| ProcV of id list * exp * dnval Environment.t ref
| DProcV of id * exp * dnval Environment.t ref
| ListV of exval list
and dnval = exval

exception Error of string

let err s = raise (Error s)

(* pretty printing *)
let rec string_of_exval = function
| IntV i -> string_of_int i
| BoolV b -> string_of_bool b
| ProcV (ids, exp, dnval) -> "<fun>"
| ListV (ls) -> let rec string_of_list ls ret = match ls with
  | [] -> ret ^ "]"
  | [x] -> ret ^ (string_of_exval x) ^ "]"
  | item::rest -> string_of_list rest (ret ^ (string_of_exval item) ^ "; ")
  in string_of_list ls "["

let rec string_of_exp = function
  | Var id -> id
  | ILit i -> string_of_int i
  | BLit b -> string_of_bool b


let debug str env = 
    Printf.printf "%s (size = %d): " str (Environment.length env);
    List.iter (fun (id, v) -> print_string(id ^ "=" ^ (string_of_exval v) ^ " ")) (Environment.get_list env);
    print_endline ""

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
    if op = And && arg1 = BoolV false then BoolV false else
    if op = Or && arg1 = BoolV true then BoolV true else
    let arg2 = eval_exp env exp2 in
    apply_prim op arg1 arg2
| IfExp (exp1, exp2, exp3) ->
    let test = eval_exp env exp1 in
      (match test with
      | BoolV true -> eval_exp env exp2 
      | BoolV false -> eval_exp env exp3
      | _ -> err ("Test expression must be boolean: if"))
| LetExp(ls, ret_exp) ->
    (* Allow multiple 'let' with 'and' connected *)
    (* Extend env with list of (id, exp) with calculation under original env *)
    let _env_ = let f = fun newenv id_exp -> 
      match id_exp with (id, exp) -> Environment.extend id (eval_exp env exp) newenv in 
    List.fold_left f env ls in
    eval_exp _env_ ret_exp
| FunExp (ids, exp) -> ProcV(ids, exp, ref env)
| DFunExp (id, exp) -> DProcV(id, exp, ref env)
| AppExp (expfun, ls_exp) -> apply env (eval_exp env expfun) ls_exp
(* Ex 3.4.2 *)
| OpFunExp (op) ->
    ProcV (["x"; "y"], BinOp(op, Var("x"), Var("y")), ref env)
(**)
| LetRecExp (ls, ret_exp) ->  
    (* Mutual recursion *)
    let rec extend_env ls newenv dummyenv_ls = match ls with
      | [] -> 
          (* Update environment of all Proc *)
          List.map (fun dummyenv -> (dummyenv := newenv;)) dummyenv_ls;
          newenv
      | it::rest -> (match it with (id, param, exp) ->
        let dummyenv = ref Environment.empty in
          let newenv' = Environment.extend id (ProcV ([param], exp, dummyenv)) newenv in
            dummyenv := newenv';
            extend_env rest newenv' (dummyenv::dummyenv_ls)
      )
    in eval_exp (extend_env ls env []) ret_exp
| ListExp (exps) ->
    let rec eval_list exps values = match exps with
      | [] -> values
      | exp :: rest -> eval_list rest (values @ [eval_exp env exp])
    in ListV (eval_list exps [])
| AppendExp (e1, e2) ->
    let v1 = eval_exp env e1 in
    let v2 = eval_exp env e2 in 
    (match v2 with 
    | ListV ls -> ListV (v1::ls)
    | _ -> err ("There must be a list after ::"))
| MatchExp (exp, match_pattern_list) ->
    let value = eval_exp env exp
    in match_exp env value match_pattern_list

and extend_env env eval_env ids exps = 
  match exps with
  | [] -> (ids, exps)
  | exp::exps_rest -> match ids with
      | [] -> (ids, exps)
      | id::ids_rest -> let arg = eval_exp eval_env exp in
          env := Environment.extend id arg !env;
          extend_env env eval_env ids_rest exps_rest

and apply env valfun ls_exp =
  match valfun with
  | ProcV (ls_id, body, env') -> 
      let _env_ = env' in
      let (ids, exps) = extend_env _env_ env ls_id ls_exp in
      (* Apply function *)
      if (List.length ids) > 0 then
        (* More parameters than applied values -> return new function *)
        ProcV(ids, body, _env_)
      else if (List.length exps) > 0 then
        (* More applied values than parameters -> continue applying *)
        let ret_exval = eval_exp !_env_ body in 
        apply env ret_exval exps
      else eval_exp !_env_ body
  | DProcV (id, body, env') ->
      let _env_ = update_env !env' env in
      let exp = List.hd ls_exp in
      _env_ := Environment.extend id (eval_exp env exp) !_env_;
      eval_exp !_env_ body
  | _ -> err ("None-function value is applied.")

and extend_env2 env1 env2 =
  let newenv = ref env1 in
  let ls = Environment.get_list env2 in
  let f (id, v) = newenv := Environment.extend id v !newenv; in
  List.iter f ls;
  newenv

(* if variable in env found in env2, update with value from env2 *)
and update_env env1 env2 = let ls = Environment.get_list env1 in
  let newenv = ref env1 in
  let f (id, v) = try let value = Environment.lookup id env2
    in newenv := Environment.update !newenv id value;
    with Environment.Not_bound -> newenv := Environment.extend id v !newenv; in
  List.iter f ls;
  newenv

(* Find a matching with value *)
and match_exp env value = function
| [] -> err "No matching pattern found."
| mp::mp_rest -> match mp with (pattern, ret_exp) ->
    (match pattern with
    (* [] *)
    | EmptyList -> (match value with ListV ls -> 
        match ls with
        | [] -> eval_exp env ret_exp
        | _ -> match_exp env value mp_rest)
    (* [x] *)
    | SingleElementList(id) -> (match value with ListV ls -> 
        match ls with
        | [x] -> let newenv = Environment.extend id x env in
            eval_exp newenv ret_exp
        | _ -> match_exp env value mp_rest)
    (* item::rest *)
    | ListHeadTail (head_id, tail_id) -> 
        (* TODO: This only asserts id1 <> id2 on evaluation. Match inside function will still be valid *)
        if head_id = tail_id then err("Variable " ^ head_id ^ " is bound several times in this matching") else
        (match value with 
        | ListV ls -> 
            (match ls with item::rest ->
              let newenv' = Environment.extend head_id item env in
              let newenv = Environment.extend tail_id (ListV rest) newenv' in
              eval_exp newenv ret_exp)
        | _ -> match_exp env value mp_rest)
    | _ -> match_exp env value mp_rest)

let eval_decl env = function
| Exp exp -> let value = eval_exp env exp in ([("-", value)], env)
| Decls id_exp_ls (* Ex 3.3.2: Multiple declaration *) ->
    let newenv = ref env in
    (* Add id and value to the extended newenv *)
    let rec decl_ls = function
    | [] -> []
    | (id, exp)::rest -> 
        (* All expressions are calculated under new env *)
        let value = eval_exp !newenv exp in 
        newenv := Environment.extend id value !newenv;
        (id, value)::decl_ls rest
    in
    (decl_ls id_exp_ls, !newenv)
| AndDecls id_exp_ls ->
    let _env_ = ref env in
    let f = fun id_exp id_val_ls -> 
      (match id_exp with (id, exp) ->
        let value = eval_exp env exp in
        _env_ := Environment.extend id value !_env_;
        (id, value) :: id_val_ls) in 
    (List.fold_right f id_exp_ls [], !_env_)
| RecDecls id_exp_ls -> 
  let id_val_ls = ref [] in
  (* Mutual recursion *)
  let rec extend_env ls newenv dummyenv_ls = match ls with
  | [] -> 
    (* Update environment of all Proc *)
    List.map (fun dummyenv -> (dummyenv := newenv;)) dummyenv_ls;
    newenv
  | it::rest -> (match it with (id, param, exp) ->
    let dummyenv = ref Environment.empty in
    let proc = ProcV ([param], exp, dummyenv) in
      let newenv' = Environment.extend id proc newenv in
        dummyenv := newenv';
        id_val_ls := (id, proc) :: !id_val_ls;
        extend_env rest newenv' (dummyenv::dummyenv_ls)
    ) in 
    let newenv = extend_env id_exp_ls env [] in
    (!id_val_ls, newenv)