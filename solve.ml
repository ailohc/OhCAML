open Lang
open Z3_translator
open Z3
open Z3.Expr
open Z3.Solver

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

let sat_check : path_cond -> bool
= fun pi ->
  let ctx = new_ctx () in
  let expr = path2expr_aux ctx pi in
  let solver = mk_solver ctx None in
  let _ = Z3.Solver.add solver [expr] in
  match (check solver []) with
  | UNSATISFIABLE -> false
  | UNKNOWN -> false
  | SATISFIABLE -> true
(*
let get_assertion : Expr.expr -> Expr.expr list 
= fun exp -> 
  let ctx = new_ctx () in
  let solver = mk_solver ctx None in
  let _ = Z3.Solver.add solver [exp] in
  Z3.Solver.get_assertions solver

let condition_assertions exp =
    let assertions = get_assertion exp in List.map expr2path assertions*)

let solve : (sym_value * path_cond) list -> (sym_value * path_cond) list -> bool
= fun t1 t2 -> raise (Failure "solve: Not Implemented") (* TODO *)
