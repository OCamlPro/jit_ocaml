%{
  open ScilabAst
  open Lexing

  let create_loc start_pos end_pos =
    { first_line = start_pos.pos_lnum; 
      first_column = (start_pos.pos_cnum - start_pos.pos_bol); 
      last_line = end_pos.pos_lnum;
      last_column = (end_pos.pos_cnum - end_pos.pos_bol) }

  let create_exp loc desc =
    let infos = 
      { is_verbose = false;
        is_break = false;
        is_breakable = false;
        is_return = false;
        is_returnable = false;
        is_continue = false;
        is_continuable  = false} in
    {exp_location = loc; exp_desc = desc; exp_info = infos}
      

%}

%token LBRACK RBRACK LPAREN RPAREN LBRACE RBRACE DOLLAR SPACES
%token COMMA EOL DOLLAR SEMI IF THEN ELSE ELSEIF END WHILE DO COMMENT
%token<float> VARINT
%token<float> VARFLOAT
%token<float> NUM
%token EOF

%start program
%type <ScilabAst.ast>program

%%
program :
| expressions                                   { $1 }

expressions :
| expression                                    { let seqexp = SeqExp [$1] in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  Exp (create_exp loc seqexp)}

expression :
| whileControl                                  { $1 }
| ifControl                                     { $1 }
| variable                                      { $1 }


condition :
| variable                                      { $1 }

/* IF THEN ELSE */

ifControl :
| IF condition thenTok thenBody END                   { let ifexp = IfExp 
                                                          { ifExp_test = $2;
                                                            ifExp_then = $4;
                                                            ifExp_else = None;
                                                            ifExp_kind = 
                                                              IfExp_expression_kind } in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 5 in
                                                        let loc = 
                                                          create_loc off_st off_end in
                                                        create_exp loc (ControlExp ifexp) }
| IF condition thenTok thenBody elseTok elseBody END  { let ifexp = IfExp 
                                                          { ifExp_test = $2;
                                                            ifExp_then = $4;
                                                            ifExp_else = $6;
                                                            ifExp_kind = 
                                                              IfExp_expression_kind } in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 7 in
                                                        let loc = 
                                                          create_loc off_st off_end in
                                                        create_exp loc (ControlExp ifexp) }
| IF condition thenTok thenBody elseIfControl END        { let ifexp = IfExp 
                                                             { ifExp_test = $2;
                                                               ifExp_then = $4;
                                                               ifExp_else = Some $5;
                                                               ifExp_kind = 
                                                                 IfExp_expression_kind } in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 7 in
                                                        let loc = 
                                                          create_loc off_st off_end in
                                                        create_exp loc (ControlExp ifexp) }     
                                                          

thenBody :
| /* Empty */                                  { let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 1 in
                                                 let loc = 
                                                   create_loc off_st off_end in
                                                 create_exp loc (SeqExp []) }
| expression                                   { $1 }


elseBody :
| /* Empty */                                  { None }
| expression                                   { Some $1 }


ifConditionBreak :
| SEMI						{ }
| SEMI EOL					{ }
| COMMA						{ }
| COMMA EOL					{ }
| EOL						{ }


thenTok :
| THEN                                          { }
| ifConditionBreak THEN				{ }
| ifConditionBreak THEN EOL			{ }
| THEN ifConditionBreak				{ }
| ifConditionBreak				{ }
| /* Empty */                                   { }


elseTok :
| ELSE						{ }
| ELSE COMMA					{ }
| ELSE SEMI					{ }
| ELSE EOL					{ }
| ELSE COMMA EOL				{ }
| ELSE SEMI EOL					{ }

elseIfControl :
| ELSEIF condition thenTok thenBody                   { let ifexp = 
                                                          ControlExp 
                                                            (IfExp 
                                                               { ifExp_test = $2;
                                                                 ifExp_then = $4;
                                                                 ifExp_else = None;
                                                                 ifExp_kind = 
                                                                   IfExp_expression_kind }) in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 4 in
                                                        let loc = 
                                                          create_loc off_st off_end in
                                                        create_exp loc 
                                                          (SeqExp [create_exp loc ifexp]) }
| ELSEIF condition thenTok thenBody elseTok elseBody  { let ifexp = 
                                                          ControlExp 
                                                            (IfExp 
                                                               { ifExp_test = $2;
                                                                 ifExp_then = $4;
                                                                 ifExp_else = $6;
                                                                 ifExp_kind = 
                                                                   IfExp_expression_kind }) in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 6 in
                                                        let loc = 
                                                          create_loc off_st off_end in
                                                        create_exp loc 
                                                          (SeqExp [create_exp loc ifexp]) }
| ELSEIF condition thenTok thenBody elseIfControl     { let ifexp = ControlExp 
                                                          (IfExp 
                                                             { ifExp_test = $2;
                                                               ifExp_then = $4;
                                                               ifExp_else = Some $5;
                                                               ifExp_kind = 
                                                                 IfExp_expression_kind }) in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 6 in
                                                        let loc = 
                                                          create_loc off_st off_end in
                                                        create_exp loc 
                                                          (SeqExp [create_exp loc ifexp]) }

/* WHILE */
whileControl :
| WHILE condition whileConditionBreak whileBody END   { let wexp = 
                                                          WhileExp 
                                                            { whileExp_test = $2;
                                                              whileExp_body = $4 } in
                                                        let off_st = Parsing.rhs_start_pos 1 in
                                                        let off_end = Parsing.rhs_end_pos 5 in
                                                        let loc = create_loc off_st off_end in
                                                        create_exp loc (ControlExp wexp) }

whileBody :
| /* Empty */           { let off_st = Parsing.rhs_start_pos 1 in
                          let off_end = Parsing.rhs_end_pos 1 in
                          let loc = 
                            create_loc off_st off_end in
                          create_exp loc (SeqExp []) }
| expression            { $1 }

whileConditionBreak :
| COMMA                 { }
| SEMI                  { }
| DO                    { }
| DO COMMA              { }
| DO SEMI               { }
| THEN                  { }
| THEN COMMA            { }
| THEN SEMI             { }
| COMMENT EOL           { }
| EOL                   { }
| COMMA EOL             { }
| SEMI EOL              { }
| DO EOL                { }
| DO COMMA EOL          { }
| DO SEMI EOL           { }
| THEN EOL              { }
| THEN COMMA EOL        { }
| THEN SEMI EOL         { }
    
    
    
variable :
| matrix                                        { $1 }
| VARINT                                        { let doubleexp =
                                                    DoubleExp { doubleExp_value = $1;
                                                                doubleExp_bigDouble = ()} in
                                                  let off_st = Parsing.rhs_start_pos 1 in
                                                  let off_end = Parsing.rhs_end_pos 1 in
                                                  let loc = create_loc off_st off_end in
                                                  create_exp loc (ConstExp doubleexp)}


/* Matrix */

matrix :
| LBRACK matrixOrCellLines RBRACK             { let mle = Array.of_list (List.rev $2) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 3 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL matrixOrCellLines RBRACK         { let mle = Array.of_list (List.rev $3) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 4 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK matrixOrCellColumns RBRACK           { let st_line = Parsing.rhs_start_pos 2 in
                                                let end_line = Parsing.rhs_end_pos 2 in
                                                let loc_line = create_loc st_line end_line in
                                                let mlec = 
                                                  { matrixLineExp_location = loc_line;
                                                    matrixLineExp_columns = Array.of_list (List.rev $2) } in
                                                let mle = Array.of_list [mlec] in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 3 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL matrixOrCellColumns RBRACK        { let st_line = Parsing.rhs_start_pos 3 in
                                                 let end_line = Parsing.rhs_end_pos 3 in
                                                 let loc_line = create_loc st_line end_line in
                                                 let mlec = 
                                                   { matrixLineExp_location = loc_line;
                                                     matrixLineExp_columns = Array.of_list (List.rev $3) } in
                                                 let mle = Array.of_list [mlec] in
                                                 let mathexp =
                                                   MatrixExp { matrixExp_lines = mle } in
                                                 let off_st = Parsing.rhs_start_pos 1 in
                                                 let off_end = Parsing.rhs_end_pos 4 in
                                                 let loc = create_loc off_st off_end in
                                                 create_exp loc (MathExp mathexp) }
| LBRACK matrixOrCellLines matrixOrCellColumns RBRACK 
                                              { let st_line = Parsing.rhs_start_pos 3 in
                                                let end_line = Parsing.rhs_end_pos 3 in
                                                let loc_line = create_loc st_line end_line in
                                                let col = 
                                                  { matrixLineExp_location = loc_line; 
                                                    matrixLineExp_columns = Array.of_list $3 } in
                                                let mle = Array.of_list (List.rev (col::$2)) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 4 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL matrixOrCellLines matrixOrCellColumns RBRACK 
                                              { let st_line = Parsing.rhs_start_pos 3 in
                                                let end_line = Parsing.rhs_end_pos 3 in
                                                let loc_line = create_loc st_line end_line in
                                                let col = 
                                                  { matrixLineExp_location = loc_line; 
                                                    matrixLineExp_columns = Array.of_list $4 } in
                                                let mle = Array.of_list (List.rev (col::$3)) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 4 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK EOL RBRACK                           { let mle = 
                                                  (Array.of_list []:matrixLineExp array) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 3 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
| LBRACK RBRACK                               { let mle = 
                                                  (Array.of_list []:matrixLineExp array) in
                                                let mathexp =
                                                  MatrixExp { matrixExp_lines = mle } in
                                                let off_st = Parsing.rhs_start_pos 1 in
                                                let off_end = Parsing.rhs_end_pos 2 in
                                                let loc = create_loc off_st off_end in
                                                create_exp loc (MathExp mathexp) }
    

matrixOrCellLines: /* Use of list then cast to array */
| matrixOrCellLines matrixOrCellLine	              { $2::$1 }
| matrixOrCellLine                                    { [$1]}


matrixOrCellLineBreak :
| SEMI                                                          {  }
| EOL                                                           {  }
| matrixOrCellLineBreak EOL                                     {  }
| matrixOrCellLineBreak SEMI                                    {  }


matrixOrCellLine :
| matrixOrCellColumns matrixOrCellLineBreak                          { let st_line = Parsing.rhs_start_pos 1 in
                                                                       let end_line = Parsing.rhs_end_pos 1 in
                                                                       let loc_line = create_loc st_line end_line in
                                                                       { matrixLineExp_location = loc_line; 
                                                                         matrixLineExp_columns = 
                                                                           Array.of_list (List.rev $1) } }
| matrixOrCellColumns matrixOrCellColumnsBreak matrixOrCellLineBreak { let st_line = Parsing.rhs_start_pos 1 in
                                                                       let end_line = Parsing.rhs_end_pos 1 in
                                                                       let loc_line = create_loc st_line end_line in
                                                                       { matrixLineExp_location = loc_line; 
                                                                         matrixLineExp_columns = 
                                                                           Array.of_list (List.rev $1) } }
    
matrixOrCellColumns :
| matrixOrCellColumns matrixOrCellColumnsBreak variable         { $3::$1 }
| matrixOrCellColumns variable                                  { $2::$1 }
| variable                                                      { [$1] }


matrixOrCellColumnsBreak :
| matrixOrCellColumnsBreak COMMA				{  }
| COMMA								{  }


/*

*/














  
