let file = ref ""
let args = []
let usage = "Usage: ./main [fichier]  (stdin par default)"

let _ =
  Arg.parse args (fun s -> file := s) usage;
  let ch = if !file = "" then stdin else open_in !file in
  let lexbuf = Lexing.from_channel ch in
  
  (* try *)
  while true do
    let ast = Parser.program Lexer.token lexbuf in
    begin
      match ast with
        | ScilabAst.Exp exp -> print_endline (ScilabAstPrinter.to_string exp)
        | _ -> ()
    end;
    flush stdout
  done
(* print_program stdout p; *)
(* fprintf stdout "\n\n"; *)
(* fprintf stdout "Type du programme : %s.\n" (Typage.type_program p); *)
(* fprintf stdout "Valeur calculée : %a.\n" print_value (Eval.eval_program p) *)
    
(* with *)
(*   | LexingError e -> print_lexing_error stderr e *)
(*   | Parser.Error -> print_parsing_error stderr lexbuf *)
(*   | TypingError e -> print_typing_error stderr e *)
(*   | EvalError e -> print_eval_error stderr e *)
