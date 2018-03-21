open Lang
open Solve
open Z3.Expr
open Z3_translator

let simplify_val v =
  match v with
  | Error _ | EoR _ -> v
  | _ -> let expr = val2expr v in let expr = simplify expr None in expr2val expr

let simplify_path : path_exp -> path_exp
= fun p -> let expr = path2expr p in let expr = simplify expr None in let assertions = get_assertion expr in expr2path expr


