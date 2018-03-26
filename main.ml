open Lang
open Options
open Sym_eval
open Solve
open Simplify

(* equality comparison between programs *)
let prog_equal : exp -> exp -> bool -> unit
= fun p1 p2 counter ->
  let _ = init_sym_cnt () in
  let r1 = sym_eval p1 empty_env default_path_cond in
  let _ = init_sym_cnt () in
  let r2 = sym_eval p2 empty_env default_path_cond in
  let to_solve1 = list_simplify r1 in
  let to_solve2 = list_simplify r2 in
  let ctx = Z3_translator.new_ctx () in
  let solver = Z3.Solver.mk_solver ctx None in
  match solve ctx solver to_solve1 to_solve2 with
  | true -> print_endline ("two programs are equivalent")
  | false ->
    begin
        print_endline ("two programs are not equivalent");
        if not counter then ()
        else
        print_endline ("different results come when");
        let ctr_ex = gen_counter_ex solver in
        match ctr_ex with
        | [] -> print_endline ("Can't find counter example")
        | hd::tl ->
            let rec print_aux : (sym_value * sym_value) list -> unit
            = fun l ->
                match l with
                | [] -> print_newline ()
                | (v, x)::tl -> print_endline (value2str v ^ " = " ^ value2str x); print_aux tl
            in print_aux ctr_ex
    end

(* equality comparison between functions *)
let fun_equal : exp -> (var * typ) list -> exp -> (var * typ) list -> bool
= fun f1 args1 f2 args2 ->
  let env1 = init_sym_cnt (); gen_senv args1 empty_env in
  let r1 = sym_eval f1 env1 default_path_cond in
  let env2 = init_sym_cnt (); gen_senv args2 empty_env in
  let r2 = sym_eval f2 env2 default_path_cond in
  let ctx = Z3_translator.new_ctx () in
  let solver = Z3.Solver.mk_solver ctx None in
  solve ctx solver r1 r2

(* simple symbolic eval *)
let run : program -> unit
= fun pgm ->
    let rec print_aux : (sym_value * path_cond) list -> int -> unit
    = fun l cnt -> 
        match l with 
        | [] -> print_newline ()
        | (v, pi)::tl ->
            print_endline ("<" ^ string_of_int cnt ^ ">");
            print_endline ("path condition: " ^ cond2str (simplify_path (pi)));
            print_endline ("value: " ^ value2str (simplify_val (v)));
            print_newline ();
            print_aux tl (cnt + 1)
    in
    let r = sym_eval pgm empty_env default_path_cond in
    print_aux r 1

let usage_msg = "'main.native -h' for help"

let main () =
    let _ = Arg.parse options (fun s -> ()) usage_msg in
    if !opt_help then begin
        print_endline ("OhCAML: OhCAML is Checking Assistant for ML");
        print_endline ("Usage: main.native <options> <file>"); print_newline ();
        print_endline ("<option description>");
        print_endline ("    --help              help");
        print_endline ("    -h"); print_newline ();
        print_endline ("    --run <file>        print result of symbolic execution");
        print_endline ("    -r <file>"); print_newline ();
        print_endline ("    --criteria <file>   compare with 'target' file");
        print_endline ("    -c <file>"); print_newline ();
        print_endline ("    --target <file>     compare with 'criteria' file");
        print_endline ("    -t <file>"); print_newline ();
        print_endline ("    --example           make counter example that make different output");
        print_endline ("    -e")
    end else
    let pgm =
        if !opt_run = "" then None
        else Some (
            let file_channel = open_in !opt_run in
            let lexbuf = Lexing.from_channel file_channel in
            Parser.program Lexer.start lexbuf
        ) in
    let criteria = 
        if !opt_cri_filename = "" then None
        else Some (
            let file_channel = open_in !opt_cri_filename in
            let lexbuf = Lexing.from_channel file_channel in
            Parser.program Lexer.start lexbuf
        ) in
    let target =
        if !opt_trg_filename = "" then None
        else Some (
            let file_channel = open_in !opt_trg_filename in
            let lexbuf = Lexing.from_channel file_channel in
            Parser.program Lexer.start lexbuf
        ) in
    match pgm, criteria, target with
    | Some e, None, None -> run e
    | None, Some e1, Some e2 -> prog_equal e1 e2 !opt_gen_counter_example
    | _ -> print_endline ("Please check the arguments are correct"); print_endline (usage_msg)

let _ = main ()