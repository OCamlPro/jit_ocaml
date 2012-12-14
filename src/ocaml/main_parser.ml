let file = ref ""
let test_flag = ref false
let args = [("-t", Arg.Unit (fun () -> test_flag := true), ": run tests")]
let usage = "Usage: ./main -t [fichier]  (stdin par default)"

let run_test file =
  let ch = if file = "" then stdin else open_in file in
  try
    let lexbuf = Lexing.from_channel ch in
    let ast = Parser.program Lexer.token lexbuf in
    begin
      match ast with
        | ScilabAst.Exp exp -> print_endline (ScilabAstPrinter.to_string exp)
        | _ -> ()
    end;
    flush stdout
  with Not_found -> 
    Printf.printf "catch exn\n"

let run_tests dirname = 
  let files = Sys.readdir dirname in
  Printf.printf "# tests to run : %i\n\n" (Array.length files);
  Array.iter (fun file ->
    let file = Filename.concat dirname file in
    Printf.printf "Testing %s :\n" file;
    if Filename.check_suffix file ".sci" then 
      run_test file
  ) files

let run_good_tests () =
  let dirname = "../../test/good/" in
  run_tests dirname 

let run_my_tests () =
  let dirname = "test/" in
  run_tests dirname

let _ =
  Arg.parse args (fun s -> file := s) usage;
  if !test_flag 
  then 
    begin 
      run_my_tests (); 
      run_good_tests ()   
    end
  else run_test !file
(* print_program stdout p; *)
(* fprintf stdout "\n\n"; *)
(* fprintf stdout "Type du programme : %s.\n" (Typage.type_program p); *)
(* fprintf stdout "Valeur calculée : %a.\n" print_value (Eval.eval_program p) *)
    
(* with *)
(*   | LexingError e -> print_lexing_error stderr e *)
(*   | Parser.Error -> print_parsing_error stderr lexbuf *)
(*   | TypingError e -> print_typing_error stderr e *)
(*   | EvalError e -> print_eval_error stderr e *)










