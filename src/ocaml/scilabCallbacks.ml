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
    (fun s_c ->
      Printf.fprintf stderr "jit_ocaml_register_callback_ml\n%!";
      try
        let ast = ScilabString2Ast.ast_of_string s_c in
        Printf.fprintf stderr "1\n%!";
        let s1 = ScilabString2Ast.copy_string s_c in
        Printf.fprintf stderr "2\n%!";
        let s2 = ScilabAst2String.string_of_ast ast in
        Printf.fprintf stderr "3\n%!";
        if s1 <> s2 then begin
          let len1 = String.length s1 in
          let len2 = String.length s2 in
          Printf.fprintf stderr "len1 = %d <> len2 = %d\n%!" len1 len2;
          for i = 4 to (min len1 len2)-1 do
            if s1.[i] <> s2.[i] then begin
              Printf.fprintf stderr "diff at %d\n%!" i;
              exit 2
            end
          done

        end;

        print_string (ScilabAstPrinter.to_string ast);
        print_newline ();
        "retour"
      with e ->
        Printf.fprintf stderr "jit_ocaml_register_callback_ml: exception %S\n%!"
          (Printexc.to_string e);
        "exception"
    )

let main () = ()
