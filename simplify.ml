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

let make_list : sym_value -> path_exp -> (sym_value * path_exp) list -> (sym_value * path_exp) list 
= fun sv pv lst -> [(sv , pv)]@lst
