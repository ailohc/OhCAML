open Lang
open Sym_eval
open Solve

(* equality comparison between programs *)
let prog_equal : exp -> exp -> bool
= fun p1 p2 ->
  init_sym_cnt ();
  let r1 = sym_eval p1 empty_env default_path_cond in
  let r2 = sym_eval p2 empty_env default_path_cond in
  solve r1 r2

(* equality comparison between functions *)
let fun_equal : exp -> (var * typ) list -> exp -> (var * typ) list -> bool
= fun f1 args1 f2 args2 ->
  let env1 = init_sym_cnt (); gen_senv args1 empty_env in
  let r1 = sym_eval f1 env1 default_path_cond in
  let env2 = init_sym_cnt (); gen_senv args2 empty_env in
  let r2 = sym_eval f2 env2 default_path_cond in
  solve r1 r2

(* simple symbolic eval *)
let run : program -> unit
= fun pgm ->
    let rec print_aux : (sym_value * path_cond) list -> int -> unit
    = fun l cnt ->
        match l with
        | [] -> print_newline ()
        | (v, pi)::tl ->
            print_endline ("<" ^ string_of_int cnt ^ ">");
            print_endline ("value: " ^ value2str (simplify_val v));
            print_endline ("path condition: " ^ cond2str (simplify_cond pi));
            print_newline ();
            print_aux tl (cnt + 1)
    in
    let r = sym_eval pgm empty_env default_path_cond in
    print_aux r 1

let main () =
    let print_code = ref false in
    let src = ref "" in
    let spec = [("-pp", Arg.Set print_code, "pretty print the input program")] in
    let usage = "Usage: run <options> <file>" in
    let _ = Arg.parse spec
                (fun
                   x ->
                     if Sys.file_exists x then src := x
                     else raise (Arg.Bad (x ^ ": No files given")))
                usage
    in
    
	if !src = "" then Arg.usage spec usage
    else
    	let file_channel = open_in !src in
    	let lexbuf = Lexing.from_channel file_channel in
    	let pgm = Parser.program Lexer.start lexbuf in
		try
            run pgm
        with e ->
            match e with
            | Lexer.LexicalError -> print_endline (!src ^ ": Lexical Error")
            | DivisionByZero -> print_endline (!src ^ ": / by Zero")
            | SyntaxError -> print_endline (!src ^ ": Syntax Error")

let _ = main ()
