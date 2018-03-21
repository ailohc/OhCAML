open Lang
open Z3_translator
open Z3.Solver

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

(*let rec compare_list = []; 

let rec list_solve_aux : sym_value -> sym_value -> bool list
= fun s1 s2 -> if s1 = s2 then true::compare_list else false::compare_list

let rec list_solve : (sym_value * path_cond) list -> (sym_value * path_cond) list -> bool list
= fun lst1 lst2 -> 
  match lst1 with
  | (s1, p1)::t1 -> 
    match lst2 with
    | (s2, p2)::t2 -> if p1 = p2 then list_solve_aux s1 s2; list_solve t1 t2 else list_solve lst1 t2
    | _ -> raise CannotCompare *)

let rec solve_aux : (sym_value * path_cond) -> (sym_value * path_cond) list -> bool
= fun v1 v2_list ->
  match v2_list with
  | [] -> false
  | hd::tl -> if v1 = hd then true else solve_aux v1 tl

let rec solve : (sym_value * path_cond) list -> (sym_value * path_cond) list -> bool
= fun v1_list v2_list ->
  match v1_list with
  | [] -> true (**)
  | hd::tl -> (solve_aux hd v2_list) && (solve tl v2_list)
