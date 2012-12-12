
(* the first parameter is *not* expected to be an OCaml string, but a
   C string. *)
val ast_of_buffer : string -> ScilabAst.exp
