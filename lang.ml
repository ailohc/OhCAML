open Util

(* expression *)
type program = exp
and exp = 
  | CONST of int
  | TRUE
  | FALSE
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp
  | LT of exp * exp
  | LE of exp * exp
  | GT of exp * exp
  | GE of exp * exp
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

let rec replace_typ src from des =
  match from with
  | TyVar x ->
    begin
      match src with
      | TyInt | TyBool -> src
      | TyFun (t1, t2) -> TyFun (replace_typ t1 from des, replace_typ t2 from des)
      | TyVar y -> if x = y then des else src
    end
  | _ -> raise (Failure "Not Type Variable")

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
  | SFunApp of id * sym_value * typ
  (* error *)
  | EoR of var (* end of recursive *)
  | Error of string
  (* simplify *)
  | Sum of sym_value list
  | Product of sym_value list
and arithmetic_op = SADD | SSUB | SMUL | SDIV
and id = int
and sym_env = (var * sym_value) list

let recursive_cnt = 4

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
  | SFunApp (id, v, t) -> "Sym_fun" ^ string_of_int id ^ "(" ^ value2str v ^ ")"
  | EoR f -> "Can't eval: fun " ^ f ^ " called more than " ^ string_of_int recursive_cnt ^ " times recursively"
  | Error s -> "Error: " ^ s
  | Sum l -> "(" ^ fold l (fun v1 s2 -> value2str v1 ^ (if s2 = ")" then "" else " + ") ^ s2) ")"
  | Product l -> "(" ^ fold l (fun v1 s2 -> value2str v1 ^ (if s2 = ")" then "" else " * ") ^ s2) ")"

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
  | LESSTHAN of sym_value * sym_value
  | LESSEQ of sym_value * sym_value
  | GREATTHAN of sym_value * sym_value
  | GREATEQ of sym_value * sym_value
  (* simplify *)
  | ANDL of path_exp list
  | ORL of path_exp list
and path_cond = path_exp

let default_path_cond = TRUE

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
  | LESSTHAN (v1, v2) -> "(" ^ value2str v1 ^ " < " ^ value2str v2 ^ ")"
  | LESSEQ (v1, v2) -> "(" ^ value2str v1 ^ " <= " ^ value2str v2 ^ ")"
  | GREATTHAN (v1, v2) -> "(" ^ value2str v1 ^ " > " ^ value2str v2 ^ ")"
  | GREATEQ (v1, v2) -> "(" ^ value2str v1 ^ " >= " ^ value2str v2 ^ ")"
  | ANDL l -> "(" ^ fold l (fun v1 s2 -> cond2str v1 ^ (if s2 = ")" then "" else " and ") ^ s2) ")"
  | ORL l -> "(" ^ fold l (fun v1 s2 -> cond2str v1 ^ (if s2 = ")" then "" else " or ") ^ s2) ")"
