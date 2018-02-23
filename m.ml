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

type equation_op = Add | Sub | Mul | Div
and sym_value =
  (* value *)
  | Int of int
  | Bool of bool
  | Fun of var * exp * sym_env
  | FunRec of var * var * exp * sym_env
  (* symbol *)
  | SInt of id
  | SBool of id
  | SFun of id * typ * typ
  (* equation *)
  | SEqu of equation_op * sym_value * sym_value
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
      | TyVar t -> raise NotImplemented
    end in
    gen_senv tl r
  
let equal : exp -> exp -> typ list -> bool
= fun p1 p2 typs -> raise NotImplemented

(*
exception TypeError

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_string (string_of_type ty1 ^ " = " ^ string_of_type ty2 ^ "\n")) eqns;
  print_endline ""

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool 
    | TyFun (t1,t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyVar x -> 
      try find x subst
      with _ -> typ

  (* add a binding (tv,ty) to the subsutition and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
=fun tenv e ty ->
match e with 
| CONST n -> [(ty, TyInt)]
| VAR x -> [(ty, (TEnv.find tenv x))]
| ADD (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
| SUB (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
| MUL (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
| DIV (e1, e2) -> (ty, TyInt)::(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
| ISZERO e -> (ty, TyBool)::(gen_equations tenv e TyInt)
| READ -> [(ty, TyInt)]
| IF (e1, e2, e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
| LET (x, e1, e2) -> let a = fresh_tyvar () in (gen_equations tenv e1 a)@(gen_equations (TEnv.extend (x, a) tenv) e2 ty)
| LETREC (f, x, e1, e2) -> let a1 = fresh_tyvar () in let a2 = fresh_tyvar () in
                        (gen_equations (TEnv.extend (f, TyFun(a2, a1))(TEnv.extend (x,a2) tenv)) e1 a1)@(gen_equations (TEnv.extend (f,TyFun(a2,a1)) tenv) e2 ty)
| PROC (x, e) -> let a1 = fresh_tyvar () in let a2 = fresh_tyvar () in 
                (ty, TyFun (a1, a2))::(gen_equations (TEnv.extend (x, a1) tenv) e a2)
| CALL (e1, e2) -> let a = fresh_tyvar () in (gen_equations tenv e1 (TyFun(a, ty)))@(gen_equations tenv e2 a)
| _ -> (raise TypeError)


let rec solve : typ_eqn -> Subst.t
=fun eqns ->
let rec unify : typ -> typ -> Subst.t -> Subst.t
= fun ty1 ty2 s ->
match (ty1, ty2, s) with
| (TyInt, TyInt, s) -> s
| (TyBool, TyBool, s) -> s
| (TyVar a1, TyVar a2, s) -> if a1 = a2 then s
                            else Subst.extend a1 (TyVar a2) s
| ((TyVar a), t, s) -> let rec occurrence : typ -> typ -> bool 
                        = fun (TyVar a) t ->
                        match t with
                        | TyVar b -> a = b
                        | TyFun (b1, b2) -> (occurrence (TyVar a) b1) || (occurrence (TyVar a) b2)
                        | _ -> false 
                        in
                      if occurrence (TyVar a) t then (raise TypeError)
                      else Subst.extend a t s
| (t, (TyVar a), s) -> unify (TyVar a) t s
| (TyFun (t1, t2), TyFun (t1_1, t2_1), s) -> let s_1 = unify t1 t1_1 s in
                                            unify (Subst.apply t2 s_1) (Subst.apply t2_1 s_1) s_1
| (_ ,_ ,_) -> (raise TypeError)
in
let rec unifyall : typ_eqn -> Subst.t -> Subst.t
= fun typeqn s ->
match typeqn with
| [] -> s
| (t1,t2)::u -> let s_1 = unify (Subst.apply t1 s) (Subst.apply t2 s) s in
                unifyall u s_1
in
unifyall eqns Subst.empty

(* typeof: Do not modify this function *)
let rec typeof : exp -> typ 
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let _ = print_endline "= Equations = ";
          print_typ_eqns eqns in
  try 
    let subst = solve eqns in
    let ty = Subst.apply new_tv subst in
     print_endline "= Substitution = ";
      Subst.print subst;
      print_endline "";
      print_endline ("Type of the given program: " ^ string_of_type ty);
      print_endline "";
      ty
  with TypeError -> (print_endline "The program does not have type. Rejected."); exit (1)
*)
