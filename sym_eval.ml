open Lang

exception DivisionByZero
exception SyntaxError
exception NotImplemented

let rec sym_eval_aux : (sym_value * path_cond) list -> (sym_value -> path_cond -> (sym_value * path_cond) list) -> (sym_value * path_cond) list
= fun l f ->
  match l with
  | [] -> []
  | (v, pi)::tl -> (f v pi)@(sym_eval_aux tl f)

let rec sym_eval : exp -> sym_env -> path_cond -> (sym_value * path_cond) list
= fun e env pi ->
  match e with
  | CONST n -> [(Int n, pi)]
  | VAR x -> [(find env x, pi)]
  | ADD (e1, e2) ->
    let l1 = sym_eval e1 env pi in
    sym_eval_aux l1 (
      fun v1 pi ->
        let l2 = sym_eval e2 env pi in
        sym_eval_aux l2 (
          fun v2 pi ->
          match v1, v2 with
          | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
          | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
          | Int n1, Int n2 -> [(Int (n1 + n2), pi)]
          | _ -> [(SExp (SADD, v1, v2), pi)]
        )
    )
  | SUB (e1, e2) ->
    let l1 = sym_eval e1 env pi in
    sym_eval_aux l1 (
      fun v1 pi ->
        let l2 = sym_eval e2 env pi in
        sym_eval_aux l2 (
          fun v2 pi ->
          match v1, v2 with
          | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
          | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
          | Int n1, Int n2 -> [(Int (n1 - n2), pi)]
          | _ -> [(SExp (SSUB, v1, v2), pi)]
        )
    )
  | MUL (e1, e2) ->
    let l1 = sym_eval e1 env pi in
    sym_eval_aux l1 (
      fun v1 pi ->
        let l2 = sym_eval e2 env pi in
        sym_eval_aux l2 (
          fun v2 pi ->
          match v1, v2 with
            | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
            | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
            | Int n1, Int n2 -> [(Int (n1 * n2), pi)]
            | _ -> [(SExp (SMUL, v1, v2), pi)]
        )
    )
  | DIV (e1, e2) ->
    let l1 = sym_eval e1 env pi in
    sym_eval_aux l1 (
      fun v1 pi ->
        let l2 = sym_eval e2 env pi in
        sym_eval_aux l2 (
          fun v2 pi ->
          match v1, v2 with
          | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
          | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
          | _, Int 0 -> raise DivisionByZero
          | Int n1, Int n2 -> [(Int (n1 / n2), pi)]
          | _ -> [(SExp (SDIV, v1, v2), AND(pi, NOTEQ(v2, Int 0)))]
        )
    )
  | ISZERO e -> 
    let l = sym_eval e env pi in
    sym_eval_aux l (
      fun v pi ->
      match v with
      | Int 0 -> [(Bool true, pi)]
      | Int _ -> [(Bool false, pi)]
      | SInt _ | SExp _ -> [(Bool true, AND(pi, EQUAL(v, Int 0))); (Bool false, AND(pi, NOTEQ(v, Int 0)))]
      | _ -> raise SyntaxError
    )
  | READ -> [(SInt (new_sym ()), pi)]
  | IF (cond, e1, e2) ->
    let l = sym_eval cond env pi in
    sym_eval_aux l (
      fun b pi ->
      match b with
      | Bool b -> if b then sym_eval e1 env pi else sym_eval e2 env pi
      | SBool _ -> (sym_eval e1 env (AND (pi, EQUAL (b, Bool true))))@(sym_eval e2 env (AND (pi, EQUAL (b, Bool false))))
      | _ -> raise SyntaxError
    )
  | LET (x, e1, e2) ->
    let l = sym_eval e1 env pi in
    sym_eval_aux l (
      fun v pi -> sym_eval e2 (append env (x, v)) pi
    )
  | LETREC (f, x, e1, e2) -> (* TODO *)
    let func = FunRec(f, x, e1, env, recursive_cnt) in
    sym_eval e2 (append env (f, func)) pi
  | PROC (x, e) -> [(Fun (x, e, env), pi)]
  | CALL (e1, e2) ->
    let l = sym_eval e1 env pi in
    sym_eval_aux l (
      fun func pi ->
      match func with
      | Fun (x, body, denv) ->
        let l = sym_eval e2 env pi in
        sym_eval_aux l (
          fun v pi -> sym_eval body (append denv (x, v)) pi
        )
      | FunRec (f, x, body, denv, cnt) ->
        if cnt > 0
        then
          let func = FunRec (f, x, body, denv, cnt - 1) in
          let l = sym_eval e2 env pi in
          sym_eval_aux l (
            fun v pi -> sym_eval body (append (append denv (f, func)) (x, v)) pi
          )
        else
          let l = sym_eval e2 env pi in
          sym_eval_aux l (
            fun v pi -> [(EoR f, pi)]
          )
      | SFun (id, t1, t2) -> raise NotImplemented (* TODO *)
      | EoR _ -> [(func, pi)]
      | _ -> raise SyntaxError
    )

let rec find_sym_var : sym_env-> var -> sym_env =
  fun senv x ->
  match senv with
  | [] -> (x, SVar (new_sym()))::senv
  | (y, v):: tl -> if y = x then senv else find_sym_var tl x

(* environment generator *)
let rec gen_senv : (var * typ) list -> sym_env -> sym_env
= fun args r ->
  match args with
  | [] -> r  
  | (x, tp)::tl ->
    let r = begin
      match tp with
      | TyInt -> append r (x, SInt (new_sym ()))
      | TyBool -> append r (x, SBool (new_sym ()))
      | TyFun (t1, t2) -> append r (x, SFun (new_sym (), t1, t2))
      | TyVar t -> find_sym_var r x
    end in
    gen_senv tl r
  
let solve : (sym_value * path_cond) list -> (sym_value * path_cond) list -> bool
= fun t1 t2 -> false (* TODO *)