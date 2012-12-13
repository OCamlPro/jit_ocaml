(*
 *  Scilab ( http://www.scilab.org/ ) - This file is part of Scilab
 *  Copyright (C) 2012-2012 - OCAMLPRO INRIA - Fabrice LE FESSANT
 *
 *  This file must be used under the terms of the CeCILL.
 *  This source file is licensed as described in the file COPYING, which
 *  you should have received as part of this distribution.  The terms
 *  are also available at
 *  http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt
 *
 *)

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
