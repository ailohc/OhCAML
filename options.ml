let opt_trg_filename = ref ""
let opt_cri_filename = ref ""
let opt_run = ref ""
let opt_gen_counter_example = ref false
let opt_help = ref false

let options =
  [
    ("--target", Arg.String (fun fname -> opt_trg_filename := fname), " target file");
    ("-t", Arg.String (fun fname -> opt_trg_filename := fname), " target file");
    ("--criteria", Arg.String (fun fname -> opt_cri_filename := fname), " criteria file");
    ("-c", Arg.String (fun fname -> opt_cri_filename := fname), " criteria file");
    ("--run", Arg.String (fun fname -> opt_run := fname), " run symbolic exectuion");
    ("-r", Arg.String (fun fname -> opt_run := fname), " run symbolic exectuion");
    ("--example", Arg.Set opt_gen_counter_example, " counter example generation");
    ("-e", Arg.Set opt_gen_counter_example, " counter example generation");
    ("--help", Arg.Set opt_help, " help");
    ("-h", Arg.Set opt_help, " help")
  ]
  |> Arg.align

