open Lang
open Z3.Expr
open Z3_translator

let simplify_val : sym_value -> sym_value
= fun v ->
  let expr = val2expr v in
  let expr = simplify expr None in
  print_endline (to_string expr); expr2val expr