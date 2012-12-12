
let _ =
  Printf.fprintf stderr "HELLO !\n%!"

external jit_ocaml_register_callback_ml :
  (string -> string) -> unit = "jit_ocaml_register_callback_c"

let _ =
  jit_ocaml_register_callback_ml
    (fun s ->
      Printf.fprintf stderr "jit_ocaml_register_callback_ml\n%!";
      try
        let ast = ScilabString2Ast.ast_of_buffer s in
        print_string (ScilabAstPrinter.to_string ast);
        print_newline ();
        "retour"
      with e ->
        Printf.fprintf stderr "jit_ocaml_register_callback_ml: exception %S\n%!"
          (Printexc.to_string e);
        "exception"
    )

let main () = ()
