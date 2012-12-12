open ScilabAst

let string_of_opExp_Kind  = function
  OpExp_invalid_kind -> "OpExp_invalid_kind"
| OpExp_bool_kind -> "OpExp_bool_kind"
| OpExp_string_kind -> "OpExp_string_kind"
| OpExp_integer_kind -> "OpExp_integer_kind"
| OpExp_float_kind -> "OpExp_float_kind"
| OpExp_double_kind  -> "OpExp_double_kind"
| OpExp_float_complex_kind  -> "OpExp_float_complex_kind"
| OpExp_double_complex_kind  -> "OpExp_double_complex_kind"
| OpExp_bool_matrix_kind  -> "OpExp_bool_matrix_kind"
| OpExp_string_matrix_kind  -> "OpExp_string_matrix_kind"
| OpExp_integer_matrix_kind  -> "OpExp_integer_matrix_kind"
| OpExp_float_matrix_kind  -> "OpExp_float_matrix_kind"
| OpExp_double_matrix_kind  -> "OpExp_double_matrix_kind"
| OpExp_float_complex_matrix_kind  -> "OpExp_float_complex_matrix_kind"
| OpExp_double_complex_matrix_kind  -> "OpExp_double_complex_matrix_kind"
| OpExp_matrix_kind  -> "OpExp_matrix_kind"

let string_of_opExp_Oper = function
  | OpExp_plus -> "OpExp_plus"
  | OpExp_minus -> "OpExp_minus"
  | OpExp_times -> "OpExp_times"
  | OpExp_rdivide -> "OpExp_rdivide"
  | OpExp_ldivide -> "OpExp_ldivide"
  | OpExp_power -> "OpExp_power"
  | OpExp_unaryMinus -> "OpExp_unaryMinus"
  | OpExp_dottimes -> "OpExp_dottimes"
  | OpExp_dotrdivide -> "OpExp_dotrdivide"
  | OpExp_dotldivide -> "OpExp_dotldivide"
  | OpExp_dotpower -> "OpExp_dotpower"
  | OpExp_krontimes -> "OpExp_krontimes"
  | OpExp_kronrdivide -> "OpExp_kronrdivide"
  | OpExp_kronldivide -> "OpExp_kronldivide"
  | OpExp_controltimes -> "OpExp_controltimes"
  | OpExp_controlrdivide -> "OpExp_controlrdivide"
  | OpExp_controlldivide -> "OpExp_controlldivide"
  | OpExp_eq -> "OpExp_eq"
  | OpExp_ne -> "OpExp_ne"
  | OpExp_lt -> "OpExp_lt"
  | OpExp_le -> "OpExp_le"
  | OpExp_gt -> "OpExp_gt"
  | OpExp_ge -> "OpExp_ge"

let string_of_logicalOpExp_Oper = function
  | OpLogicalExp_logicalAnd -> "OpLogicalExp_logicalAnd"
  | OpLogicalExp_logicalOr -> "OpLogicalExp_logicalOr"
  | OpLogicalExp_logicalShortCutAnd -> "OpLogicalExp_logicalShortCutAnd"
  | OpLogicalExp_logicalShortCutOr -> "OpLogicalExp_logicalShortCutOr"

let rec print_exp buf indent ast =
  match ast.exp_desc with

  |  SeqExp list ->
    Printf.bprintf buf "%sSeqExp\n" indent;
    let indent2 = indent ^ "  " in
    List.iter (fun exp ->
      print_exp buf indent2 exp;
    ) list

  | ConstExp exp ->
    begin
      match exp with
      | StringExp { stringExp_value } ->
        Printf.bprintf buf "%sStringExp %S\n" indent stringExp_value
      | CommentExp { commentExp_comment } ->
        Printf.bprintf buf "%sCommentExp %S\n" indent commentExp_comment
      | BoolExp { boolExp_value } ->
        Printf.bprintf buf "%sBoolExp %b\n" indent boolExp_value
      | DoubleExp { doubleExp_value } ->
        Printf.bprintf buf "%sDoubleExp %f\n" indent doubleExp_value
      | FloatExp  { floatExp_value } ->
        Printf.bprintf buf "%sFloatExp %f\n" indent floatExp_value
      | IntExp { intExp_value; intExp_prec } ->
        Printf.bprintf buf "%sIntExp%d %Ld\n" indent
          (match intExp_prec with
            IntExp_8 -> 8
          | IntExp_16 -> 16
          | IntExp_32 -> 32
          | IntExp_64 -> 64)
          intExp_value
      | NilExp ->
        Printf.bprintf buf "%sNilExp\n" indent
    end

  | CallExp { callExp_name; callExp_args } ->
    Printf.bprintf buf "%sCallExp\n" indent;
    Printf.bprintf buf "%s  name:\n" indent;
    let indent2 = indent ^ "    " in
    print_exp buf indent2 callExp_name;
    Printf.bprintf buf "%s  args:\n" indent;
    Array.iter (fun exp ->
      print_exp buf indent2 exp;
    ) callExp_args

  | AssignExp { assignExp_left_exp; assignExp_right_exp } ->
    Printf.bprintf buf "%sAssignExp\n" indent;
    let indent2 = indent ^ "    " in
    Printf.bprintf buf "%s  left_exp:\n" indent;
    print_exp buf indent2 assignExp_left_exp;
    Printf.bprintf buf "%s  right_exp:\n" indent;
    print_exp buf indent2 assignExp_right_exp;

  | ControlExp controlExp ->
    begin
      match controlExp with

      | BreakExp ->
        Printf.bprintf buf "%sBreakExp\n" indent

      | ContinueExp ->
        Printf.bprintf buf "%sContinueExp\n" indent

      | ForExp { forExp_vardec; forExp_body } ->
        Printf.bprintf buf "%sForExp\n" indent;
        let indent2 = indent ^ "    " in
        Printf.bprintf buf "%s  vardec:\n" indent;
        print_varDec buf indent2 forExp_vardec;
        Printf.bprintf buf "%s  body:\n" indent;
        print_exp buf indent2 forExp_body;

      | IfExp { ifExp_test; ifExp_then; ifExp_else; ifExp_kind } ->
        Printf.bprintf buf "%sIfExp %s\n" indent
          (match ifExp_kind with
            IfExp_invalid_kind -> "invalid_kind"
          | IfExp_instruction_kind -> "instruction_kind"
          | IfExp_expression_kind -> "expression_kind");
        let indent2 = indent ^ "    " in
        Printf.bprintf buf "%s  test:\n" indent;
        print_exp buf indent2 ifExp_test;
        Printf.bprintf buf "%s  then:\n" indent;
        print_exp buf indent2 ifExp_then;
        begin match ifExp_else with
          None -> ()
        | Some ifExp_else ->
          Printf.bprintf buf "%s  then:\n" indent;
          print_exp buf indent2 ifExp_else
        end

      | ReturnExp { returnExp_exp; returnExp_is_global } ->
        Printf.bprintf buf "%sReturnExp %s\n" indent
          (if returnExp_is_global then "(global)" else "");
        let indent2 = indent ^ "  " in
        print_exp buf indent2 returnExp_exp

      | SelectExp { selectExp_selectme;
                    selectExp_cases; selectExp_default } ->
        Printf.bprintf buf "%sSelectExp\n" indent;
        let indent2 = indent ^ "    " in
        let indent3 = indent2 ^ "  " in
        Printf.bprintf buf "%s  selectme:\n" indent;
        print_exp buf indent2 selectExp_selectme;
        Printf.bprintf buf "%s  cases:\n" indent;
        List.iter (fun { caseExp_test; caseExp_body } ->
          Printf.bprintf buf "%s    test:\n" indent;
          print_exp buf indent3 caseExp_test;
          Printf.bprintf buf "%s    body:\n" indent;
          List.iter (fun exp ->
            print_exp buf indent3 exp) caseExp_body
        ) selectExp_cases;
        Printf.bprintf buf "%s  default:\n" indent;
        List.iter (fun exp ->
          print_exp buf indent2 exp;
        ) selectExp_default

      | TryCatchExp { tryCatchExp_tryme; tryCatchExp_catchme } ->
        Printf.bprintf buf "%sTryCatchExp\n" indent;
        let indent2 = indent ^ "    " in
        Printf.bprintf buf "%s  try:\n" indent;
        List.iter (fun exp ->
          print_exp buf indent2 exp;
        ) tryCatchExp_tryme;
        Printf.bprintf buf "%s  catch:\n" indent;
        List.iter (fun exp ->
          print_exp buf indent2 exp;
        ) tryCatchExp_catchme

      | WhileExp { whileExp_test; whileExp_body } ->
        Printf.bprintf buf "%sWhileExp\n" indent;
        Printf.bprintf buf "%stest:\n" indent;
        let indent2 = indent ^ "    " in
        print_exp buf indent2 whileExp_test;
        Printf.bprintf buf "%sbody:\n" indent;
        print_exp buf indent2 whileExp_body
    end

  | Dec dec ->
    Printf.bprintf buf "%sDec\n" indent;
    let indent = indent ^ "    " in
    begin
      match dec with

      | VarDec varDec ->
        print_varDec buf indent varDec

      | FunctionDec {
        functionDec_symbol;
        functionDec_args;
        functionDec_returns;
        functionDec_body;
      } ->
        Printf.bprintf buf "%sFunctionDec %S\n" indent functionDec_symbol;
        let indent2 = indent ^ "    " in
        Printf.bprintf buf "%s  args:\n" indent;
        Array.iter (fun var -> print_var buf indent2 var) functionDec_args;
        Printf.bprintf buf "%s  returns:\n" indent;
        Array.iter (fun var -> print_var buf indent2 var) functionDec_returns;
        Printf.bprintf buf "%s  body:\n" indent;
        print_exp buf indent2 functionDec_body

    end

  | FieldExp { fieldExp_name; fieldExp_tail } ->
    Printf.bprintf buf "%sFieldExp\n" indent;
    let indent2 = indent ^ "    " in
    Printf.bprintf buf "%s  name:\n" indent;
    print_exp buf indent2 fieldExp_name;
    Printf.bprintf buf "%s  tail:\n" indent;
    print_exp buf indent2 fieldExp_tail;

  | ListExp { listExp_start; listExp_step; listExp_end } ->
    Printf.bprintf buf "%sListExp\n" indent;
    let indent2 = indent ^ "    " in
    Printf.bprintf buf "%s  start:\n" indent;
    print_exp buf indent2 listExp_start;
    Printf.bprintf buf "%s  step:\n" indent;
    print_exp buf indent2 listExp_step;
    Printf.bprintf buf "%s  end:\n" indent;
    print_exp buf indent2 listExp_end;

  | Var var ->
    Printf.bprintf buf "%sVar\n" indent;
    let indent2 = indent ^ "  " in
    print_var buf indent2 var

  | ArrayListExp array ->
    Printf.bprintf buf "%sArrayListExp\n" indent;
    let indent2 = indent ^ "  " in
    Array.iter (fun exp ->
      print_exp buf indent2 exp;
    ) array

  | AssignListExp array ->
    Printf.bprintf buf "%sAssignListExp\n" indent;
    let indent2 = indent ^ "  " in
    Array.iter (fun exp ->
      print_exp buf indent2 exp;
    ) array

  | MathExp mathExp ->
    begin
      match mathExp with
      | MatrixExp { matrixExp_lines } ->
        Printf.bprintf buf "%sMatrixExp\n" indent;
        let indent2 = indent ^ "    " in
        Array.iter (fun { matrixLineExp_columns } ->
          Printf.bprintf buf "%s  line:\n" indent;
          Array.iter (fun exp ->
            print_exp buf indent2 exp
          ) matrixLineExp_columns
        ) matrixExp_lines

      | MatrixLineExp exps ->
        Printf.bprintf buf "%sMatrixLineExp\n" indent;
        let indent2 = indent ^ "  " in
        Array.iter (fun exp ->
          print_exp buf indent2 exp;
        ) exps

      | NotExp { notExp_exp } ->
        Printf.bprintf buf "%sNotExp\n" indent;
        let indent2 = indent ^ "  " in
        print_exp buf indent2 notExp_exp

      | OpExp (opExp_oper, opExp_args) ->
        print_opExp buf indent "OpExp"  opExp_args
          (string_of_opExp_Oper opExp_oper)

      | LogicalOpExp (opExp_oper, opExp_args) ->
        print_opExp buf indent "LogicalOpExp" opExp_args
          (string_of_logicalOpExp_Oper opExp_oper)

      | TransposeExp { transposeExp_exp; transposeExp_conjugate } ->
        Printf.bprintf buf "%sTransposeExp %s\n" indent
          (match transposeExp_conjugate with
            Conjugate -> "Conjugate"
          | NonConjugate -> "NonConjugate");
        let indent2 = indent ^ "  " in
        print_exp buf indent2 transposeExp_exp
    end

and print_opExp buf indent name opExp oper =
  Printf.bprintf buf "%s%s %s %s\n" indent name oper
    (string_of_opExp_Kind opExp.opExp_kind);
  let indent2 = indent ^ "    " in
  Printf.bprintf buf "%s  left:\n" indent;
  print_exp buf indent2 opExp.opExp_left;
  match opExp.opExp_right with
    None -> ()
  | Some opExp_right ->
    Printf.bprintf buf "%s  right:\n" indent;
    print_exp buf indent2 opExp_right


and print_varDec buf indent {
  varDec_name; varDec_init; varDec_kind
} =
  Printf.bprintf buf "%sVarDec %S (%s)\n" indent varDec_name
    (match varDec_kind with
      VarDec_invalid_kind -> "invalid_kind"
    | VarDec_evaluation_kind -> "evaluation_kind"
    | VarDec_assignment_kind -> "assignment_kind");
  let indent2 = indent ^ "    " in
  Printf.bprintf buf "%sinit:\n" indent;
  print_exp buf indent2 varDec_init

and print_var buf indent { var_desc } =
  match var_desc with
  | ColonVar ->
    Printf.bprintf buf "%sColonVar\n" indent
  | DollarVar ->
    Printf.bprintf buf "%sDollarVar\n" indent
  | SimpleVar s ->
    Printf.bprintf buf "%sSimpleVar %S\n" indent s
  | ArrayListVar vars ->
    Printf.bprintf buf "%sArrayListVar\n" indent;
    let indent2 = indent ^ "  " in
    Array.iter (fun var -> print_var buf indent2 var) vars

let to_string ast =
  let b = Buffer.create 1024 in
  print_exp b "" ast;
  Buffer.contents b