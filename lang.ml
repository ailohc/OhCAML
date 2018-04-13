open Util

exception StackOverflow of string

type id = string
type typ =
  | TUnit
  | TInt
  | TBool
  | TString
  | TBase of id (* user defined *)
  | TList of typ
  | TTuple of typ list
  | TCtor of typ * typ list (* tbase x, tl *)
  | TArr of typ * typ (* fun variable *)
  | TVar of id (* type variable *)
  | TExn

type ctor = id * typ list

type pat =
  | PUnit
  | PInt of int
  | PBool of bool
  | PVar of id
  | PList of pat list
  | PTuple of pat list
  | PCtor of id * pat list
  | PCons of pat list
  | PUnder
  | Pats of pat list

type let_bind =
  | BindUnder (* let _ = ... in x *)
  | BindOne of id (* let x = ... in x *)
  | BindTuple of let_bind list (* let x, y = (..., ...) in x, y *)

type arg =
  | ArgUnder of typ
  | ArgOne of id * typ
  | ArgTuple of arg list

type decl = 
  | DException of ctor
  | Equn of id * typ
  | DData of id * ctor list
  | DLet of binding
and exp =
  (* const *)
  | EUnit
  | Const of int
  | TRUE
  | FALSE
  | EList of exp list
  | String of id
  | EVar of id
  | Ector of id * exp list
  | ETuple of exp list
  (* aop *)
  | ADD of exp * exp    (* a1 + a2 *)
  | SUB of exp * exp    (* a1 - a2 *)
  | MUL of exp * exp    (* a1 * a2 *)
  | DIV of exp * exp    (* a1 / a2 *)
  | MOD of exp * exp    (* a1 % a2 *)
  | MINUS of exp
  (* bop *)
  | NOT of exp            (* not b1 *)
  | OR of exp * exp       (* b1 || b2 *)
  | AND of exp * exp      (* b1 && b2 *)
  | LESS of exp * exp     (* a1 < a2 *)
  | LARGER of exp * exp   (* a1 > a2 *)
  | EQUAL of exp * exp    (* a1 == a2 *)
  | NOTEQ of exp * exp    (* a1 <> a2 or a1 != a2 *)
  | LESSEQ of exp * exp   (* a1 <= a2 *)
  | LARGEREQ of exp * exp (* a1 >= a2 *)
  (* lop *)
  | AT of exp * exp
  | DOUBLECOLON of exp * exp
  | STRCON of exp * exp
  (* else *)
  | EApp of exp * exp     (* e1 e2 *)
  | EFun of arg * exp     (* fun (x:tl) -> e *)
  | ELet of let_bind * bool * arg list * typ * exp * exp  (* let [rec] (x1:t1) ... (xn:tn) : t = e1 in e2 *)
  | EBlock of bool * binding list * exp (* let x1 = e1 and x2 = e2 and ... xn = en in e' | let rec f1 x1 = e1 and f2 x2 = e2 ... fn xn = en in e' *)
  | EMatch of exp * branch list
  | IF of exp * exp * exp
  (* exception *)
  | Raise of exp
and branch = pat * exp
and binding = (let_bind * bool * arg list * typ * exp)  (* f [rec] x1, x2, ..., xn : t = e *)

type prog = decl list

(* semantics *)
type sym_value =
  (* value *)
  | Unit
  | Int of int
  | String of string
  | Bool of bool
  | List of sym_value list
  | Tuple of sym_value list
  | Ctor of id * sym_value list
  | Fun of arg * exp * env
  | FunRec of id * arg * exp * env
  | Block of id * (id * sym_value) list
  (* symbol *)
  | SInt of id
  | SBool of id
  (* expression *)
  | SAdd of sym_value * sym_value
  | SSub of sym_value * sym_value
  | SMul of sym_value * sym_value
  | SDiv of sym_value * sym_value
  | SMod of sym_value * sym_value
  | SMinus of sym_value
  (* simplify *)
  | SUM of sym_value list
  | PROD of sym_value list
and env = (id, sym_value) BatMap.t
and components = exp BatSet.t

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

exception EExcept of sym_value

let empty_env = BatMap.empty
let lookup_env = BatMap.find
let update_env = BatMap.add

(* generate a fresh type variable *)
let tvar_num = ref 0
let fresh_tvar () = (tvar_num := !tvar_num + 1; (TVar ("#" ^ string_of_int !tvar_num)))

let rec appify : exp -> exp list -> exp
= fun exp exp_list ->
  match exp_list with
  | [] -> exp
  | hd::tl -> appify (EApp (exp, hd)) tl

let rec let_to_exp : let_bind -> exp
= fun x ->
  match x with
  | BindOne x -> EVar x
  | BindTuple xs -> ETuple (map let_to_exp xs)
  | _ -> raise (Failure "Wild-card _ is not valid")

let default_path_cond = TRUE

let rec typ2str : typ -> string
= fun t ->
  match t with
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TBase id -> raise (Failure "not implement")
  | TList typ -> typ2str typ ^ " list"
  | TTuple typ_list -> string_of_strlist "(" " * " ")" (map typ2str typ_list)
  | TCtor (typ, typ_list) -> raise (Failure "not implement")
  | TArr (typ1, typ2) -> raise (Failure "not implement")
  | TVar id -> raise (Failure "not implement")
  | TExn -> raise (Failure "not implement")

let rec val2str : sym_value -> string
= fun v ->
  match v with
  | Unit -> "()"
  | Int n -> string_of_int n
  | String str -> "\"" ^ str ^ "\""
  | Bool b -> string_of_bool b
  | List l -> string_of_strlist "[" "; " "]" (map val2str l)
  | Tuple l -> string_of_strlist "(" ", " ")" (map val2str l)
  | Ctor (id, l) -> raise (Failure "not implemented")
  | Fun (arg, _, _) | FunRec (_, arg, _, _) -> raise (Failure "not implemented")
  | Block _ -> raise (Failure "not implemented")
  | SInt id -> "alpha " ^ id
  | SBool id -> "beta " ^ id
  | SAdd (v1, v2) -> "(" ^ val2str v1 ^ " + " ^ val2str v2 ^ ")"
  | SSub (v1, v2) -> "(" ^ val2str v1 ^ " - " ^ val2str v2 ^ ")"
  | SMul (v1, v2) -> "(" ^ val2str v1 ^ " * " ^ val2str v2 ^ ")"
  | SDiv (v1, v2) -> "(" ^ val2str v1 ^ " / " ^ val2str v2 ^ ")"
  | SMod (v1, v2) -> "(" ^ val2str v1 ^ " % " ^ val2str v2 ^ ")"
  | SMinus v -> "-" ^ val2str v
  | SUM l -> string_of_strlist "(" " + " ")" (map val2str l)
  | PROD l -> string_of_strlist "(" " * " ")" (map val2str l)

let rec cond2str : path_exp -> string
= fun pi ->
  match pi with
  | TRUE -> "true"
  | FALSE -> "false"
  | AND (e1, e2) -> "(" ^ cond2str e1 ^ " and " ^ cond2str e2 ^ ")"
  | OR (e1, e2) -> "(" ^ cond2str e1 ^ " or " ^ cond2str e2 ^ ")"
  | NOT e -> "!" ^ cond2str e
  | EQUAL (v1, v2) -> "(" ^ val2str v1 ^ " = " ^ val2str v2 ^ ")"
  | NOTEQ (v1, v2) -> "(" ^ val2str v1 ^ " != " ^ val2str v2 ^ ")"
  | LESSTHAN (v1, v2) -> "(" ^ val2str v1 ^ " < " ^ val2str v2 ^ ")"
  | LESSEQ (v1, v2) -> "(" ^ val2str v1 ^ " <= " ^ val2str v2 ^ ")"
  | GREATTHAN (v1, v2) -> "(" ^ val2str v1 ^ " > " ^ val2str v2 ^ ")"
  | GREATEQ (v1, v2) -> "(" ^ val2str v1 ^ " >= " ^ val2str v2 ^ ")"
  | ANDL l -> string_of_strlist "(" " and " ")" (map cond2str l)
  | ORL l -> string_of_strlist "(" " or " ")" (map cond2str l)

