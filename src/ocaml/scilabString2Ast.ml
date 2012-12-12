open ScilabAst

external jit_ocaml_get_double : string -> int -> float = "jit_ocaml_get_double_c"

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

let get_uint32_32 s pos =
  let c0, pos = get_uint8 s pos in
  let c1, pos = get_uint8 s pos in
  let c2, pos = get_uint8 s pos in
  let c3, pos = get_uint8 s pos in
  Int32.add (Int32.of_int c0)
    (Int32.shift_left (Int32.of_int ((c1 + ((c2 + (c3 lsl 8)) lsl 8)))) 8), pos

let get_location s pos =
  let first_line, pos = get_uint32 s pos in
  let first_column, pos = get_uint32 s pos in
  let last_line, pos = get_uint32 s pos in
  let last_column, pos = get_uint32 s pos in
  let loc = {
    first_line; first_column; last_line; last_column
  } in
  loc, pos

let get_ast s pos =
  let code, pos = get_uint8 s pos in

  let loc, pos = get_location s pos in

  let is_verbose, pos = get_bool s pos in
  let is_break, pos = get_bool s pos in
  let is_breakable, pos = get_bool s pos in
  let is_return, pos = get_bool s pos in
  let is_returnable, pos = get_bool s pos in
  let is_continue, pos = get_bool s pos in
  let is_continuable, pos = get_bool s pos in

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

let get_OpExp_Kind s pos =
  let code, pos = get_uint8 s pos in
  let kind =
    match code with
    | 1 -> OpExp_invalid_kind
    | 2 -> OpExp_bool_kind
    | 3 -> OpExp_string_kind
    | 4 -> OpExp_integer_kind
    | 5 -> OpExp_float_kind
    | 6 -> OpExp_double_kind
    | 7 -> OpExp_float_complex_kind
    | 8 -> OpExp_double_complex_kind

    | 9 -> OpExp_bool_matrix_kind
    | 10 -> OpExp_string_matrix_kind
    | 11 -> OpExp_integer_matrix_kind
    | 12 -> OpExp_float_matrix_kind
    | 13 -> OpExp_double_matrix_kind
    | 14 -> OpExp_float_complex_matrix_kind
    | 15 -> OpExp_double_complex_matrix_kind
    | 16 -> OpExp_matrix_kind
    | _ -> failwith (Printf.sprintf "get_OpExp_Kind : unknown code %d" code)
  in
  kind, pos

let get_OpExp_Oper s pos =
  let code, pos = get_uint8 s pos in
  let oper =
    match code with
    | 1 -> OpExp_plus
    | 2 -> OpExp_minus
    | 3 -> OpExp_times
    | 4 -> OpExp_rdivide
    | 5 -> OpExp_ldivide
    | 6 -> OpExp_power

    | 7 -> OpExp_dottimes
    | 8 -> OpExp_dotrdivide
    | 9 -> OpExp_dotldivide
    | 10 -> OpExp_dotpower

    | 11 -> OpExp_krontimes
    | 12 -> OpExp_kronrdivide
    | 13 -> OpExp_kronldivide

    | 14 -> OpExp_controltimes
    | 15 -> OpExp_controlrdivide
    | 16 -> OpExp_controlldivide

    | 17 -> OpExp_eq
    | 18 -> OpExp_ne
    | 19 -> OpExp_lt
    | 20 -> OpExp_le
    | 21 -> OpExp_gt
    | 22 -> OpExp_ge

    | 23 -> OpExp_unaryMinus
    | _ -> failwith (Printf.sprintf "get_OpExp_Oper : unknown code %d" code)
  in
  oper, pos

let get_LogicalOpExp_Oper s pos =
  let code, pos = get_uint8 s pos in
  let oper =
    match code with

    | 24 -> OpLogicalExp_logicalAnd
    | 25 -> OpLogicalExp_logicalOr
    | 26 -> OpLogicalExp_logicalShortCutAnd
    | 27 -> OpLogicalExp_logicalShortCutOr
    | _ -> failwith (Printf.sprintf "get_LogicalOpExp_Oper : unknown code %d" code)
  in
  oper, pos

let get_IntExp_Prec s pos =
  let code, pos = get_uint8 s pos in
  let prec =
    match code with

    | 1 -> IntExp_8
    | 2 -> IntExp_16
    | 3 -> IntExp_32
    | 4 -> IntExp_64
    | _ -> failwith (Printf.sprintf "get_IntExp_Oper : unknown code %d" code)
  in
  prec, pos

let get_IfExp_Kind s pos =
  let code, pos = get_uint8 s pos in
  let kind =
    match code with
    | 1 -> IfExp_invalid_kind
    | 2 -> IfExp_instruction_kind
    | 3 -> IfExp_expression_kind
    | 4 -> IfExp_untyped_kind
    | _ -> failwith (Printf.sprintf "get_IfExp_Oper : unknown code %d" code)
  in
  kind, pos

let get_double s pos =
  let d = jit_ocaml_get_double s pos in
  let pos = pos + 8 in
  d, pos

let dummy_info = {
  is_verbose = false;
  is_break = false;
  is_breakable = false;
  is_return = false;
  is_returnable = false;
  is_continue = false;
  is_continuable = false;
}
let dummy_loc = {
  first_line = 0;
  first_column = 0;
  last_line = 0;
  last_column = 0;
}

let dummyExp = mkexp (SeqExp []) dummy_info dummy_loc

let warning = ref None
let return_dummyExp = ref false
let rec get_exp s pos =
  if !return_dummyExp then dummyExp, pos else
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

  | 4 ->
    let intExp_prec, pos = get_IntExp_Prec s pos in
    let intExp_value, pos = get_uint32_32 s pos in
    mkexp (ConstExp (IntExp {
      intExp_value;
      intExp_prec;
    })) info loc, pos

  | 5 ->
    let floatExp_value, pos = get_double s pos in
    mkexp (ConstExp (FloatExp {
      floatExp_value;
    })) info loc, pos

  | 6 ->
    let doubleExp_value, pos = get_double s pos in
    mkexp (ConstExp (DoubleExp {
      doubleExp_value;
      doubleExp_bigDouble = ();
    })) info loc, pos

  | 7 ->
    let boolExp_value, pos = get_uint8 s pos in
    let boolExp_value = boolExp_value <> 0 in
    mkexp (ConstExp (BoolExp {
      boolExp_value;
      boolExp_bigBool = ();
    })) info loc, pos

  | 8 ->
    mkexp (ConstExp NilExp) info loc, pos

  | 9 ->
    let symbol, pos = get_wstring s pos in
    mkexp (Var {
      var_location = loc;
      var_desc = SimpleVar symbol;
    }) info loc, pos

  | 10 ->
    mkexp (Var {
      var_location = loc;
      var_desc = ColonVar;
    }) info loc, pos

  | 11 ->
    mkexp (Var {
      var_location = loc;
      var_desc = DollarVar;
    }) info loc, pos

  | 12 ->
    let vars, pos = get_vars s pos in
    mkexp (Var { var_desc = ArrayListVar vars;
                 var_location = loc }) info loc, pos

  | 14 ->
    let ifExp_kind, pos = get_IfExp_Kind s pos in
    let has_else, pos = get_uint8 s pos in
    let ifExp_test, pos = get_exp s pos in
    let ifExp_then, pos = get_exp s pos in
    let ifExp_else, pos =
      if has_else <> 0 then
        let ifExp_else, pos = get_exp s pos in
        Some ifExp_else, pos
      else
        None, pos
    in
    mkexp (ControlExp (IfExp {
      ifExp_kind; ifExp_test; ifExp_then; ifExp_else
    })) info loc, pos

  | 29 ->
    let functionDec_symbol, pos = get_wstring s pos in
    let functionDec_body, pos = get_exp s pos in
    let args_loc, pos = get_location s pos in
    let args, pos = get_vars s pos in
    let functionDec_args = {
      arrayListVar_location = args_loc;
      arrayListVar_vars = args;
    } in
    let returns_loc, pos = get_location s pos in
    let returns, pos = get_vars s pos in
    let functionDec_returns = {
      arrayListVar_location = returns_loc;
      arrayListVar_vars = returns;
    } in
    mkexp (Dec (FunctionDec {
      functionDec_symbol; functionDec_location = loc;
      functionDec_body; functionDec_args; functionDec_returns;
    })) info loc, pos

  | 31 ->
    let assignExp_left_exp, pos = get_exp s pos in
    let assignExp_right_exp, pos = get_exp s pos in
    mkexp (AssignExp {
      assignExp_left_exp; assignExp_right_exp
    }) info loc, pos

  | 32 ->
    let opExp_kind, pos = get_OpExp_Kind s pos in
    let opExp_oper, pos = get_OpExp_Oper s pos in
    let opExp_left, pos = get_exp s pos in
    let opExp_right, pos =get_exp s pos in
    let opExp_right = Some opExp_right in (* TODO, why an option ? *)
    mkexp (MathExp (OpExp (opExp_oper,
      {
        opExp_right; opExp_left; opExp_kind
      }))) info loc, pos

  | 33 ->
    let opExp_kind, pos = get_OpExp_Kind s pos in
    let opExp_oper, pos = get_LogicalOpExp_Oper s pos in
    let opExp_left, pos = get_exp s pos in
    let opExp_right, pos =get_exp s pos in
    let opExp_right = Some opExp_right in (* TODO, why an option ? *)
    mkexp (MathExp (LogicalOpExp (opExp_oper,
      {
        opExp_right; opExp_left; opExp_kind
      }))) info loc, pos

  | 34 ->
    let matrixExp_lines, pos = get_matrixLines s pos in
    mkexp (MathExp (MatrixExp { matrixExp_lines })) info loc, pos

  | 35 ->
    let callExp_name, pos = get_exp s pos in
    let callExp_args, pos = get_exps s pos in
    let callExp_args = Array.of_list callExp_args in
    mkexp (CallExp {
      callExp_name; callExp_args
    }) info loc, pos

  | _ ->
    warning := Some (
      Printf.sprintf  "ast_of_buffer: code %d unknown" code);
    return_dummyExp := true;
    dummyExp, pos

and get_exps s pos =
  if !return_dummyExp then [], pos else
  let nitems, pos = get_uint32 s pos in
  let rec iter nitems s pos ritems =
    if nitems > 0 then
      let item, pos = get_exp s pos in
      iter (nitems-1) s pos (item :: ritems)
    else (List.rev ritems, pos)
  in
  iter nitems s pos []

and get_matrixLines s pos =
  if !return_dummyExp then [||], pos else
  let nitems, pos = get_uint32 s pos in
  let rec iter nitems s pos ritems =
    if nitems > 0 then
      let loc, pos = get_location s pos in
      let exps, pos = get_exps s pos in
      let item = {
        matrixLineExp_location = loc;
        matrixLineExp_columns = Array.of_list exps;
      } in
      iter (nitems-1) s pos (item :: ritems)
    else (Array.of_list (List.rev ritems), pos)
  in
  iter nitems s pos []

and get_vars s pos =
  let exps, pos = get_exps s pos in
  let vars = if !return_dummyExp then [||] else
      Array.map (fun exp ->
        match exp.exp_desc with
          Var var -> var
        | _ -> assert false
      ) (Array.of_list exps)
  in
  vars, pos

let ast_of_buffer s =
  return_dummyExp := false;
  warning := None;
  let pos = 0 in
  let buflen, pos = get_uint32 s pos in
  Printf.fprintf stderr "buflen : %d\n%!" buflen;
  let ast, pos = get_exp s pos in
  begin
    match !warning with
      None -> ()
    | Some s ->
      Printf.fprintf stderr "Warning: %s\n%!" s
  end;
  ast
