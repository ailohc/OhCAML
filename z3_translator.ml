open Util
open Lang
open Z3
open Z3enums

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
  | SInt id -> mk_const ctx ("alpha_" ^ string_of_int id) (int_sort ctx)
  | SBool id -> mk_const ctx ("beta_" ^ string_of_int id) (bool_sort ctx)
  | SExp (aop, v1, v2) ->
    begin
      match aop with
      | SADD -> add ctx (val2expr_aux ctx v1) (val2expr_aux ctx v2)
      | SSUB -> sub ctx (val2expr_aux ctx v1) (val2expr_aux ctx v2)
      | SMUL -> mul ctx (val2expr_aux ctx v1) (val2expr_aux ctx v2)
      | SDIV -> div ctx (val2expr_aux ctx v1) (val2expr_aux ctx v2)
    end
  | SMinus v1 -> minus ctx (val2expr_aux ctx v1)
  | Sum l ->
    begin
      match l with
      | [] -> val2expr_aux ctx (Int 0)
      | hd::tl -> add ctx (val2expr_aux ctx hd) (val2expr_aux ctx (Sum tl))
    end
  | Product l ->
    begin
      match l with
      | [] -> val2expr_aux ctx (Int 1)
      | hd::tl -> mul ctx (val2expr_aux ctx hd) (val2expr_aux ctx (Product tl))
    end
  | Fun _ | FunRec _ | SVar _ | SFun _ | SFunApp _ | EoR _ | Error _ -> raise NotComputableValue

let val2expr : sym_value -> Expr.expr
= fun v -> val2expr_aux (new_ctx ()) v

let rec expr2val : Expr.expr -> sym_value
= fun expr -> 
  let op = FuncDecl.get_decl_kind (Expr.get_func_decl expr) in
  match op with
  | OP_ANUM -> (* int *)
    let str = Expr.to_string expr in
    let str = if Str.string_match (Str.regexp "(- ") str 0 then Str.replace_first (Str.regexp "(- ") "-" (Str.replace_first (Str.regexp ")") "" str) else str in
    let n = int_of_string (str) in Int n
  | OP_TRUE -> Bool true
  | OP_FALSE -> Bool false
  | OP_UNINTERPRETED -> (* symbol *)
    begin
      let str = Symbol.get_string (FuncDecl.get_name (Expr.get_func_decl expr)) in
      let l = Str.split (Str.regexp "_") str in
      match l with
      | [hd; tl] ->
        if hd = "alpha" then SInt (int_of_string tl)
        else if tl = "beta" then SBool (int_of_string tl)
        else raise (Failure "SHOULD NOT COME HERE")
      | _ -> raise (Failure "SHOULD NOT COME HERE")
    end
  | OP_ADD -> (* sum *)
    begin
      let n = Expr.get_num_args expr in
      if n = 2 then
      begin
        let [hd; tl] = Expr.get_args expr in
        SExp (SADD, expr2val hd, expr2val tl)
      end
      else if n > 2 then
      begin
        let l = Expr.get_args expr in
        Sum (map expr2val l)
      end
      else (* Expr.get_num_args < 2 *) raise (Failure "SHOULD NOT COME HERE")
    end
  | OP_MUL -> (* product *)
    begin
      let n = Expr.get_num_args expr in
      if n = 2 then
      begin
        let [hd; tl] = Expr.get_args expr in
        SExp (SMUL, expr2val hd, expr2val tl)
      end
      else if n > 2 then
      begin
        let l = Expr.get_args expr in
        Product (map expr2val l)
      end
      else (* Expr.get_num_args < 2 *) raise (Failure "SHOULD NOT COME HERE")
    end
  | OP_IDIV -> (* div *) let [hd; tl] = Expr.get_args expr in SExp (SDIV, expr2val hd, expr2val tl)
  | _ -> raise (Failure "expr2val: Not Implemented")

let rec path2expr_aux : context -> path_exp -> Expr.expr
= fun ctx p ->
  match p with
  | TRUE -> const_b ctx true
  | FALSE -> const_b ctx false
  | AND (p1, p2) -> and_b ctx (path2expr_aux ctx p1) (path2expr_aux ctx p2)
  | OR (p1, p2) -> or_b ctx (path2expr_aux ctx p1) (path2expr_aux ctx p2)
  | NOT p -> not_b ctx (path2expr_aux ctx p)
  | EQUAL (p1, p2) -> eq ctx (val2expr_aux ctx p1) (val2expr_aux ctx p2)
  | NOTEQ (p1, p2) -> neq ctx (val2expr_aux ctx p1) (val2expr_aux ctx p2)
  | LESSTHAN (p1, p2) -> lt ctx (val2expr_aux ctx p1) (val2expr_aux ctx p2)
  | LESSEQ (p1, p2) -> le ctx (val2expr_aux ctx p1) (val2expr_aux ctx p2)
  | GREATTHAN (p1, p2) -> gt ctx (val2expr_aux ctx p1) (val2expr_aux ctx p2)
  | GREATEQ (p1, p2) -> ge ctx (val2expr_aux ctx p1) (val2expr_aux ctx p2)
  | PATHEQ (p1, p2) -> eq ctx (path2expr_aux ctx p1) (path2expr_aux ctx p2)
  | _ -> raise NotComputableValue

let path2expr : path_exp -> Expr.expr
= fun p -> path2expr_aux (new_ctx ()) p

let rec expr2path : Expr.expr -> path_exp
= fun expr ->
  let op = FuncDecl.get_decl_kind (Expr.get_func_decl expr) in
  match op with
  | OP_TRUE -> TRUE
  | OP_FALSE -> FALSE
  | OP_AND -> (* and *)
    begin
      let n = Expr.get_num_args expr in
      if n = 2 then
      begin
        let [hd; tl] = Expr.get_args expr in AND (expr2path hd, expr2path tl)
      end
      else if n > 2 then
      begin
        let l = Expr.get_args expr in ANDL (map expr2path l)
      end
      else (* Expr.get_num_args < 2 *) raise (Failure "SHOULD NOT COME HERE")
    end
  | OP_OR -> (* or *)
    begin
      let n = Expr.get_num_args expr in
      if n = 2 then
        let [hd; tl] = Expr.get_args expr in OR (expr2path hd, expr2path tl)
      else if n > 2 then
        let l = Expr.get_args expr in ANDL (map expr2path l)
      else (* Expr.get_num_args <  2 *) raise (Failure "SHOULD NOT COME HERE")
    end
  | OP_EQ -> (* equal *) let [hd; tl] = Expr.get_args expr in EQUAL (expr2val hd, expr2val tl)
  | OP_NOT -> let [e] = Expr.get_args expr in NOT (expr2path e)
  | OP_LE -> let [hd; tl] = Expr.get_args expr in LESSEQ (expr2val hd, expr2val tl)
  | OP_GE -> let [hd; tl] = Expr.get_args expr in GREATEQ (expr2val hd, expr2val tl)
  | OP_LT -> let [hd; tl] = Expr.get_args expr in LESSTHAN (expr2val hd, expr2val tl)
  | OP_GT -> let [hd; tl] = Expr.get_args expr in GREATTHAN (expr2val hd, expr2val tl)
  | _ -> raise (Failure "expr2path: Not Implemented")

