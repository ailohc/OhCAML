open Lang
open Util
open Z3
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

let rec solve : context -> solver -> (sym_value * path_cond) list -> (sym_value * path_cond) list -> bool
= fun ctx solver l1 l2 ->
  let r = 
    match l1 with
    | [] -> raise (Failure "NotRunnable")
    | hd::tl -> let (v, _) = hd in Z3_translator.mk_const ctx "return" (
        if is_int v then Z3_translator.int_sort ctx
        else Z3_translator.bool_sort ctx
      )
  in
  let e1 = fold (
    fun tup rst ->
    let (v, pi) = tup in
    let eq_value = Z3_translator.eq ctx r (val2expr_aux ctx v) in
    let exp_pi = path2expr_aux ctx pi in
    let exp = Z3_translator.and_b ctx eq_value exp_pi in
    Z3_translator.or_b ctx exp rst
  ) l1 (Z3_translator.const_b ctx false) in
  let e2 = fold (
    fun tup rst ->
    let (v, pi) = tup in
    let eq_value = Z3_translator.eq ctx r (val2expr_aux ctx v) in
    let exp_pi = path2expr_aux ctx pi in
    let exp = Z3_translator.and_b ctx eq_value exp_pi in
    Z3_translator.or_b ctx exp rst
  ) l2 (Z3_translator.const_b ctx false)  in
  let expr = Z3_translator.neq ctx e1 e2 in
  let _ = Z3.Solver.add solver [expr] in
  match (check solver []) with
  | UNSATISFIABLE -> true
  | _ -> false

let gen_counter_ex : solver -> (sym_value * sym_value) list
= fun solver ->
  let rec mk_eqs : 'a list -> 'b list -> ('a * 'b) list -> ('a * 'b) list
  = fun a_list b_list r ->
    match a_list, b_list with
    | [], [] -> r
    | h1::t1, h2::t2 -> mk_eqs t1 t2 (r@[(h1, h2)])
    | _ -> []
  in
  let m = Solver.get_model solver in
  if m = None then []
  else let Some m = m in
  let l = Model.get_const_decls m in
  let r = map (fun decl -> let t = Model.get_const_interp m decl in let Some t = t in t) l in
  let l = map funcdecl2val l in
  let r = map expr2val r in
  mk_eqs l r []
  