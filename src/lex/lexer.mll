{

  open Lexing
  open Parser

  let newline lexbuf =
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
      pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1;
      pos_bol =  lexbuf.lex_curr_p.pos_cnum;
    }
}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']

let spaces    = [' ' '\t']

let integer   = ['0'-'9']+
let number    = ['0'-'9']+(".")['0'-'9']*
let little    = (".")['0'-'9']+

let lbrack    = "["
let rbrack    = "]"

let lparen    = "("
let rparen    = ")"

let lbrace    = "{"
let rbrace    = "}"

let dollar    = "$"

let semicolon = ";"
let comma     = ","
let colon     = ":"
    

rule token = parse
  | blank                        { token lexbuf }
  | newline                      { Printf.printf "\n"; newline lexbuf; EOL}
  | "if"                         { IF }
  | "then"                       { THEN }
  | "else"                       { ELSE }
  | "elseif"                     { ELSEIF }
  | "end"                        { END }
  | comma                        { Printf.printf ","; COMMA }
  | semicolon                    { Printf.printf ";"; SEMI }
  | integer as inum              { let num = float_of_string inum in
                                   Printf.printf "%f" num;VARINT num }
  | number as nnum               { let num = float_of_string nnum in
                                   Printf.printf "%f" num;
                                   NUM num }
  | little as lnum               { let num = float_of_string lnum in
                                   Printf.printf "%f" num;
                                   NUM num }
  | lbrack                       { LBRACK }
  | rbrack                       { RBRACK }
  | dollar                       { DOLLAR }
  | eof                          { exit 0 }
      

(* and matrix = parse *)
(*   | spaces+ *)
(*       { Printf.printf " "; *)
(*         matrix lexbuf} *)
(*   | integer as inum *)
(*       { let num = int_of_string inum in *)
(*         Printf.printf "%d" num; *)
(*         matrix lexbuf} *)
(*   | number as nnum *)
(*       { let num = float_of_string nnum in *)
(*         Printf.printf "%f" num; *)
(*         matrix lexbuf} *)
(*   | little as lnum *)
(*       { let num = float_of_string lnum in *)
(*         Printf.printf "%f" num; *)
(*         matrix lexbuf} *)
(*   | rbrack *)
(*       {token lexbuf} *)
      
    
















