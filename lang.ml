(* expression *)
type program = exp
and exp = 
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

(* type *)
type typ = TyInt | TyBool | TyFun of typ * typ | TyVar of tyvar
and tyvar = string

let rec typ2str t =
  match t with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1, t2) -> typ2str t1 ^ " -> " ^ typ2str t2
  | TyVar x -> x

(* value *)
type sym_value =
  (* value *)
  | Int of int
  | Bool of bool
  | Fun of var * exp * sym_env
  | FunRec of var * var * exp * sym_env * int
  (* symbol *)
  | SInt of id
  | SBool of id
  | SVar of id (* ? *)
  | SFun of id * typ * typ
  (* arithmetic expression *)
  | SExp of arithmetic_op * sym_value * sym_value
  | SMinus of sym_value
  (* end of recursive *)
  | EoR of var
and arithmetic_op = SADD | SSUB | SMUL | SDIV
and id = int
and sym_env = (var * sym_value) list

let recursive_cnt = 5

let sym_cnt = ref 0
let init_sym_cnt () = sym_cnt := 0
let new_sym () = sym_cnt := !sym_cnt + 1; !sym_cnt

let empty_env = []
let rec find env x =
  match env with
  | [] -> raise (Failure "Env is Empty")
  | (y, v)::tl -> if y = x then v else find tl x
let append env (x, v) = (x, v)::env

let rec fix_point : 'a -> ('a -> 'a) -> 'a
= fun x f-> let next = f x in if x = next then x else fix_point next f

let rec simplify_val_aux : sym_value -> sym_value
= fun v ->
  match v with
  | Int _ | Bool _ | Fun _ | FunRec _ | SInt _ | SBool _ | SVar _ | SFun _ | EoR _ -> v
  | SExp (aop, v1, v2) ->
    let v1, v2 = simplify_val_aux v1, simplify_val_aux v2 in
    begin
      match aop with
      | SADD -> 
        begin
          match v1, v2 with
          | EoR f, _ | _, EoR f -> EoR f
          | _ -> v (* TODO *)
        end
      | SSUB -> SExp (SADD, v1, SMinus v2)
      | SMUL ->
        begin
          match v1, v2 with
          | EoR f, _ | _, EoR f -> EoR f
          | _ -> v (* TODO *)
        end
      | SDIV ->
        begin
          match v1, v2 with
          | EoR f, _ | _, EoR f -> EoR f
          | _ -> v (* TODO *)
        end
      end
  | SMinus v ->
    begin
      match v with
      | Int n -> Int (-n)
      | Bool _ | Fun _ | FunRec _ | SBool _ | SVar _ | SFun _ -> raise (Failure "Not Integer Value") (* Should not reach heer *)
      | SInt _ -> SMinus v
      | EoR _ -> v
      | SExp _ -> SMinus (simplify_val_aux v)
      | SMinus v -> v
    end

let simplify_val : sym_value -> sym_value
= fun v -> fix_point v simplify_val_aux

let rec value2str : sym_value -> string
= fun v ->
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Fun _ -> "fun" (* TODO *)
  | FunRec _ -> "fun" (* TODO *)
  | SInt id -> "alpha" ^ string_of_int id
  | SBool id -> "beta" ^ string_of_int id
  | SVar id -> "var" ^ string_of_int id
  | SFun (id, t1, t2) -> "sym_fun" ^ string_of_int id ^ " (" ^ typ2str t1 ^ " -> " ^ typ2str t2 ^ ")"
  | SExp (aop, v1, v2) ->
    begin
      match aop with
      | SADD -> "(" ^ value2str v1 ^ " + " ^ value2str v2 ^ ")"
      | SSUB -> "(" ^ value2str v1 ^ " - " ^ value2str v2 ^ ")"
      | SMUL -> "(" ^ value2str v1 ^ " * " ^ value2str v2 ^ ")"
      | SDIV -> "(" ^ value2str v1 ^ " / " ^ value2str v2 ^ ")"
    end
  | SMinus v -> "(-" ^ value2str v ^ ")"
  | EoR f -> "Can't eval: fun " ^ f ^ " called more than " ^ string_of_int recursive_cnt ^ " times recursively"

type path_exp =
  (* boolean exp *)
  | TRUE
  | FALSE
  (* boolean op *)
  | AND of path_exp * path_exp
  | OR of path_exp * path_exp
  | NOT of path_exp
  (* symbolic equation *)
  | EQUAL of sym_value * sym_value
  | NOTEQ of sym_value * sym_value
and path_cond = path_exp

let default_path_cond = TRUE

let rec simplify_cond : path_exp -> path_exp
= fun pi ->
  match pi with
  | TRUE -> TRUE
  | FALSE -> FALSE
  | AND (e1, e2) ->
    let e1, e2 = simplify_cond e1, simplify_cond e2 in
    begin
      match e1, e2 with
      | TRUE, _ -> e2
      | FALSE, _ -> FALSE
      | _ -> AND (e1, e2)
    end
  | OR (e1, e2) ->
    let e1, e2 = simplify_cond e1, simplify_cond e2 in
    begin
      match e1, e2 with
      | TRUE, _ -> TRUE
      | FALSE, _ -> e2
      | _ -> OR (e1, e2)
    end
  | NOT e -> NOT (simplify_cond e)
  | EQUAL (v1, v2) -> EQUAL (simplify_val v1, simplify_val v2)
  | NOTEQ (v1, v2) -> NOTEQ (simplify_val v1, simplify_val v2)

let rec cond2str : path_exp -> string
= fun pi ->
  match pi with
  | TRUE -> "true"
  | FALSE -> "false"
  | AND (e1, e2) -> "(" ^ cond2str e1 ^ " and " ^ cond2str e2 ^ ")"
  | OR (e1, e2) -> "(" ^ cond2str e1 ^ " or " ^ cond2str e2 ^ ")"
  | NOT e -> "!(" ^ cond2str e ^ ")"
  | EQUAL (v1, v2) -> "(" ^ value2str v1 ^ " = " ^ value2str v2 ^ ")"
  | NOTEQ (v1, v2) -> "(" ^ value2str v1 ^ " != " ^ value2str v2 ^ ")"
