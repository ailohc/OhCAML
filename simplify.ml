open Lang
open Z3.Expr
open Z3_translator

let simplify_val : sym_value -> sym_value
= fun v ->
  match v with
  | Error _ | EoR _ -> v
  | _ -> let expr = val2expr v in let expr = simplify expr None in expr2val expr

let simplify_path : path_exp -> path_exp
= fun p -> let expr = path2expr p in let expr = simplify expr None in expr2path expr

let rec list_simplify : (sym_value * path_cond) list -> (sym_value * path_cond) list
= fun lst ->
    match lst with
    | [] -> []
    | (s, p)::tl -> (simplify_val (s), simplify_path (p))::(list_simplify tl)
