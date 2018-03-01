type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp             
  | PROC of var * exp
  | CALL of exp * exp
and var = string

type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar
and tyvar = string

type arithmetic_op = SADD | SSUB | SMUL | SDIV
and sym_value =
  (* value *)
  | Int of int
  | Bool of bool
  | Fun of var * exp * sym_env
  | FunRec of var * var * exp * sym_env
  (* symbol *)
  | SInt of id
  | SBool of id
  | SVar of id (* ? *)
  | SFun of id * typ * typ
  (* arithmetic expression *)
  | SExp of arithmetic_op * sym_value * sym_value
and id = int
and sym_env = (var * sym_value) list

let sym_cnt = ref 0
let init_sym_cnt () = sym_cnt := 0
let new_sym () = sym_cnt := !sym_cnt + 1; !sym_cnt

let empty_env = []
let rec find env x =
  match env with
  | [] -> raise (Failure "Env is Empty")
  | (y, v)::tl -> if y = x then v else find tl x
let append env (x, v) = (x, v)::env

type path_exp =
  (* boolean exp *)
  | TRUE
  | FALSE
  (* boolean op *)
  | AND of path_exp * path_exp
  | OR of path_exp * path_exp
  | NOT of path_exp
  (* symbolic equation *)
  | EQUAL of sym_value * sym_value
  | NOTEQ of sym_value * sym_value
and path_cond = path_exp

let default_path_cond = TRUE

exception DivisionByZero
exception SyntaxError
exception NotImplemented

let rec sym_eval : exp -> sym_env -> path_cond -> (sym_value * path_cond)
= fun e env pi ->
  match e with
  | CONST n -> (Int n, pi)
  | VAR x -> (find env x, pi)
  | ADD (e1, e2) ->
    let (v1, pi) = sym_eval e1 env pi in
    let (v2, pi) = sym_eval e2 env pi in
    begin
      match v1, v2 with
      | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
      | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
      | Int n1, Int n2 -> (Int (n1 + n2), pi)
      | _ -> (SExp (SADD, v1, v2), pi)
    end
  | SUB (e1, e2) ->
    let (v1, pi) = sym_eval e1 env pi in
    let (v2, pi) = sym_eval e2 env pi in
    begin
      match v1, v2 with
      | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
      | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
      | Int n1, Int n2 -> (Int (n1 - n2), pi)
      | _ -> (SExp (SSUB, v1, v2), pi)
    end
  | MUL (e1, e2) ->
    let (v1, pi) = sym_eval e1 env pi in
    let (v2, pi) = sym_eval e2 env pi in
    begin
      match v1, v2 with
      | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
      | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
      | Int n1, Int n2 -> (Int (n1 * n2), pi)
      | _ -> (SExp (SMUL, v1, v2), pi)
    end
  | DIV (e1, e2) ->
    let (v1, pi) = sym_eval e1 env pi in
    let (v2, pi) = sym_eval e2 env pi in
    begin
      match v1, v2 with
      | Bool _, _ | Fun _, _ | FunRec _, _ | SBool _, _ | SVar _, _ | SFun _, _ -> raise SyntaxError
      | _, Bool _ | _, Fun _ | _, FunRec _ | _, SBool _ | _, SVar _ | _, SFun _ -> raise SyntaxError
      | _, Int 0 -> raise DivisionByZero
      | Int n1, Int n2 -> (Int (n1 / n2), pi)
      | _ -> (SExp (SDIV, v1, v2), AND(pi, NOTEQ(v2, Int 0)))
    end
  | ISZERO e -> 
    let (v, pi) = sym_eval e env pi in
    begin
      match v with
      | Int 0 -> (Bool true, AND(pi, EQUAL(v, Int 0)))
      | Int _ -> (Bool false, AND(pi, NOTEQ(v, Int 0)))
      | SInt n -> raise NotImplemented (*TODO*)
      | SExp (saop, sv1, sv2) -> raise NotImplemented (*TODO*) 
      | _ -> raise SyntaxError
    end
  | READ -> (SInt (new_sym ()), pi)
  | IF (cond, e1, e2) ->
    let (b, pi) = sym_eval cond env pi in
    begin
      match b with
      | Bool b -> if b then sym_eval e1 env pi else sym_eval e2 env pi
      | SBool x -> raise NotImplemented (* TODO *)
      | _ -> raise SyntaxError
    end
  | LET (x, e1, e2) ->
    let (v, pi) = sym_eval e1 env pi in
    sym_eval e2 (append env (x, v)) pi
  | LETREC (f, x, e1, e2) ->
    let func = FunRec(f, x, e1, env) in
    sym_eval e2 (append env (f, func)) pi
  | PROC (x, e) -> (Fun (x, e, env), pi)
  | CALL (e1, e2) ->
    let (func, pi) = sym_eval e1 env pi in
    begin
      match func with
      | Fun (x, body, denv) ->
        let (v, pi) = sym_eval e2 env pi in
        sym_eval body (append denv (x, v)) pi
      | FunRec (f, x, body, denv) ->
        let (v, pi) = sym_eval e2 env pi in
        sym_eval body (append (append denv (f, func)) (x, v)) pi
      | SFun (id, t1, t2) -> raise NotImplemented (* TODO *)
      | _ -> raise SyntaxError
    end

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
  
let solve : (sym_value * path_cond) -> (sym_value * path_cond) -> bool
= fun t1 t2 -> false (* TODO *)

let prog_equal : exp -> exp -> bool
= fun p1 p2 ->
  init_sym_cnt ();
  let r1 = sym_eval p1 empty_env default_path_cond in
  let r2 = sym_eval p2 empty_env default_path_cond in
  solve r1 r2

let fun_equal : exp -> (var * typ) list -> exp -> (var * typ) list -> bool
= fun f1 args1 f2 args2 ->
  let env1 = init_sym_cnt (); gen_senv args1 empty_env in
  let r1 = sym_eval f1 env1 default_path_cond in
  let env2 = init_sym_cnt (); gen_senv args2 empty_env in
  let r2 = sym_eval f2 env2 default_path_cond in
  solve r1 r2
  