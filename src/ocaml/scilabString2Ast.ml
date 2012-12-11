open ScilabAst

let _ =
  Printf.fprintf stderr "HELLO !\n%!"

external jit_ocaml_register_callback_ml :
  (string -> string) -> unit = "jit_ocaml_register_callback_c"

let get_uint8 s pos =
  int_of_char (String.unsafe_get s pos), pos+1

let get_bool s pos =
  let n, pos = get_uint8 s pos in
  (n <> 0), pos

let get_uint32 s pos =
  let c0, pos = get_uint8 s pos in
  let c1, pos = get_uint8 s pos in
  let c2, pos = get_uint8 s pos in
  let c3, pos = get_uint8 s pos in
  c0 + ((c1 + ((c2 + (c3 lsl 8)) lsl 8)) lsl 8), pos

let get_ast s pos =
  let code, pos = get_uint8 s pos in
  let first_line, pos = get_uint32 s pos in
  let first_column, pos = get_uint32 s pos in
  let last_line, pos = get_uint32 s pos in
  let last_column, pos = get_uint32 s pos in
  let is_verbose, pos = get_bool s pos in
  let is_break, pos = get_bool s pos in
  let is_breakable, pos = get_bool s pos in
  let is_return, pos = get_bool s pos in
  let is_returnable, pos = get_bool s pos in
  let is_continue, pos = get_bool s pos in
  let is_continuable, pos = get_bool s pos in

  let loc = {
    first_line; first_column; last_line; last_column
  } in
  let exp_info = {
    is_verbose;
    is_break; is_breakable;
    is_return; is_returnable;
    is_continue; is_continuable;
  } in
  code, exp_info, loc, pos

let mkexp exp_desc exp_info exp_location =
  { exp_desc; exp_location; exp_info }


let get_wstring s pos =
  let size, pos = get_uint32 s pos in
  let wstring = String.create size in
  String.unsafe_blit s pos wstring 0 size;
  let pos = pos + size in
  wstring, pos

let rec get_exp s pos =
  let code, info, loc, pos = get_ast s pos in
  match code with
  | 1 ->
    let items, pos = get_exps s pos in
    mkexp (SeqExp items) info loc, pos
  | 2 ->
    let stringExp_value, pos = get_wstring s pos in
    mkexp (ConstExp (StringExp {
      stringExp_value;
      stringExp_bigString = ();
    }
    )) info loc, pos
  | 3 ->
    let commentExp_comment, pos = get_wstring s pos in
    mkexp (ConstExp (CommentExp {
      commentExp_comment;
    }
    )) info loc, pos
  | 35 ->
    let callExp_name, pos = get_exp s pos in
    let callExp_args, pos = get_exps s pos in
    let callExp_args = Array.of_list callExp_args in
    mkexp (CallExp {
      callExp_name; callExp_args
    }) info loc, pos
  | _ -> failwith (Printf.sprintf "ast_of_buffer: code %d unknown" code)

and get_exps s pos =
  let nitems, pos = get_uint32 s pos in
  let rec iter nitems s pos ritems =
    if nitems > 0 then
      let item, pos = get_exp s pos in
      iter (nitems-1) s pos (item :: ritems)
    else (List.rev ritems, pos)
  in
  iter nitems s pos []

let ast_of_buffer s =
  let pos = 0 in
  let buflen, pos = get_uint32 s pos in
  Printf.fprintf stderr "buflen : %d\n%!" buflen;
  let ast, pos = get_exp s pos in
  ast

let _ =
  jit_ocaml_register_callback_ml
    (fun s ->
      Printf.fprintf stderr "jit_ocaml_register_callback_ml\n%!";
      try
        let ast = ast_of_buffer s in
        print_string (ScilabAstPrinter.to_string ast);
        print_newline ();
        "retour"
      with e ->
        Printf.fprintf stderr "jit_ocaml_register_callback_ml: exception %S\n%!"
          (Printexc.to_string e);
        "exception"
    )
