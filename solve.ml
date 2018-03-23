open Lang
open Z3_translator
open Z3.Solver
open Z3.Expr
open Z3.Proof
open Z3enums

exception CannotCompare

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

let rec path_equal_check : path_cond -> path_cond -> bool
  = fun p1 p2 ->
  let ctx = new_ctx () in
  let expr1 = path2expr_aux ctx p1 in
  let expr2 = path2expr_aux ctx p2 in
  let expr1 = simplify expr1 None in
  let expr2 = simplify expr2 None in
  Z3.Expr.equal expr1 expr2

let rec sym_val_check : sym_value -> sym_value -> bool
    = fun s1 s2 -> if sat_check (EQUAL (s1, s2)) then true else false

let rec solve_aux : (sym_value * path_cond) -> (sym_value * path_cond) list -> bool
= fun v1 v2_list ->
  match v2_list with
  | [] -> false
  | (s2, p2)::tl -> 
    match v1 with
    | (s1, p1) -> if path_equal_check p1 p2 then begin sym_val_check s1 s2 end else begin solve_aux v1 tl end
    | _ -> raise CannotCompare

let rec solve : (sym_value * path_cond) list -> (sym_value * path_cond) list -> bool
= fun v1_list v2_list ->
  match v1_list with
  | [] -> true
  | (s1, p1)::tl -> (solve_aux (s1, p1) v2_list) && (solve tl v2_list)