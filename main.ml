open M

(* simple symbolic eval *)
let run : program -> sym_value
= fun pgm -> (fun (v, _) -> v) (sym_eval pgm empty_env default_path_cond) 

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
      print_endline (value2str (run pgm))
		with Lexer.LexicalError -> print_endline (!src ^ ": Lexical Error")

let _ = main ()
