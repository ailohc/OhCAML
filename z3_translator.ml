open Lang
open Z3
open Z3.Solver
open Z3.Expr
open Z3.Arithmetic.Integer
open Z3.Boolean

(* context *)
let new_ctx () = mk_context []

(* sort *)
let int_sort ctx = Z3.Arithmetic.Integer.mk_sort ctx
let bool_sort ctx = Z3.Boolean.mk_sort ctx

(* var *)
let mk_symbol ctx str = Symbol.mk_string ctx str
let mk_const ctx str sort = Z3.Expr.mk_const_s ctx str sort
let const_n ctx n = Z3.Expr.mk_numeral_int ctx n (int_sort ctx)
let const_b ctx b = Z3.Boolean.mk_val ctx b

(* aop *)
let add ctx expr1 expr2 = Z3.Arithmetic.mk_add ctx [expr1; expr2]
let sub ctx expr1 expr2 = Z3.Arithmetic.mk_sub ctx [expr1; expr2]
let mul ctx expr1 expr2 = Z3.Arithmetic.mk_mul ctx [expr1; expr2]
let div ctx expr1 expr2 = Z3.Arithmetic.mk_div ctx expr1 expr2
let minus ctx expr = Z3.Arithmetic.mk_unary_minus ctx expr

(* bop *)
let and_b ctx expr1 expr2 = Z3.Boolean.mk_and ctx [expr1; expr2]
let or_b ctx expr1 expr2 = Z3.Boolean.mk_or ctx [expr1; expr2]
let lt ctx expr1 expr2 = Z3.Arithmetic.mk_lt ctx expr1 expr2
let gt ctx expr1 expr2 = Z3.Arithmetic.mk_gt ctx expr1 expr2
let le ctx expr1 expr2 = Z3.Arithmetic.mk_le ctx expr1 expr2
let ge ctx expr1 expr2 = Z3.Arithmetic.mk_ge ctx expr1 expr2
let not_b ctx expr = Z3.Boolean.mk_not ctx expr
let eq ctx expr1 expr2 = Z3.Boolean.mk_eq ctx expr1 expr2
let neq ctx expr1 expr2 = (not_b ctx (eq ctx expr1 expr2))

exception NotComputableValue

let rec val2expr_aux : context -> sym_value -> Expr.expr
= fun ctx v ->
  match v with
  | Int n -> const_n ctx n
  | Bool b -> const_b ctx b
  | SInt id -> mk_const ctx ("alpha" ^ string_of_int id) (int_sort ctx)
  | SBool id -> mk_const ctx ("beta" ^ string_of_int id) (bool_sort ctx)
  | SExp (aop, v1, v2) ->
    begin
      match aop with
      | SADD -> add ctx (val2expr_aux ctx v) (val2expr_aux ctx v)
      | SSUB -> sub ctx (val2expr_aux ctx v) (val2expr_aux ctx v)
      | SMUL -> mul ctx (val2expr_aux ctx v) (val2expr_aux ctx v)
      | SDIV -> div ctx (val2expr_aux ctx v) (val2expr_aux ctx v)
    end
  | SMinus v1 -> minus ctx (val2expr_aux ctx v1)
  | Fun _ | FunRec _ | SVar _ | SFun _ | SFunApp _ | EoR _ | Error _ -> raise NotComputableValue

let val2expr : sym_value -> Expr.expr
= fun v -> val2expr_aux (new_ctx ()) v

let expr2val : Expr.expr -> sym_value
= fun expr -> raise (Failure "expr2val: Not Implemented")