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
  | SVar of id
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

exception NotImplemented

let sym_eval : exp -> sym_env -> path_cond -> (sym_value * path_cond)
= fun e env pi ->
  match e with
  | CONST n -> (Int n, pi)
  | VAR x -> (find env x, pi)
  | ADD (e1, e2) -> raise NotImplemented
  | SUB (eq, e2) -> raise NotImplemented
  | MUL (eq, e2) -> raise NotImplemented
  | DIV (eq, e2) -> raise NotImplemented
  | ISZERO e -> raise NotImplemented
  | READ -> (SInt (new_sym ()), pi)
  | IF (cond, e1, e2) -> raise NotImplemented
  | LET (x, e1, e2) -> raise NotImplemented
  | LETREC (f, x, e1, e2) -> raise NotImplemented
  | PROC (x, e) -> raise NotImplemented
  | CALL (e1, e2) -> raise NotImplemented


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
  
let equal : exp -> exp -> typ list -> bool
= fun p1 p2 typs -> raise NotImplemented
