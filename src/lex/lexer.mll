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

let utf2  = (['\xC2'-'\xDF']['\x80'-'\xBF'])

let utf31 = (['\xE0']['\xA0'-'\xBF']['\x80'-'\xBF'])
let utf32 = (['\xE1'-'\xEC']['\x80'-'\xBF']['\x80'-'\xBF'])
let utf33 = (['\xED']['\x80'-'\x9F']['\x80'-'\xBF'])
let utf34 = (['\xEE'-'\xEF']['\x80'-'\xBF']['\x80'-'\xBF'])
let utf41 = (['\xF0']['\x90'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF'])
let utf42 = (['\xF1'-'\xF3']['\x80'-'\xBF']['\x80'-'\xBF']['\x80'-'\xBF'])
let utf43 = (['\xF4']['\x80'-'\x8F']['\x80'-'\xBF']['\x80'-'\xBF'])

let utf3 = (utf31 | utf32 | utf33 | utf34)
let utf4 = (utf41 | utf42 | utf43)

let utf  = (utf2 | utf3 | utf4)

let id   = ((['a'-'z''A'-'Z''_''%''#''?''$'] | utf)(['a'-'z''A'-'Z''_''0'-'9''#''?''$'] | utf))*

let booltrue  = "%t" | "%T"
let boolfalse = "%f" | "%F" 

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

let assign    = "=" 
    

rule token = parse
  | blank                        { token lexbuf }
  | newline                      { Printf.printf "\n"; newline lexbuf; EOL}
  | "if"                         { IF }
  | "then"                       { THEN }
  | "else"                       { ELSE }
  | "elseif"                     { ELSEIF }
  | "end"                        { END }
  | "while"                      { WHILE }
  | "do"                         { DO }
  | "assign"                     { ASSIGN }
  | comma                        { Printf.printf ","; COMMA }
  | semicolon                    { Printf.printf ";"; SEMI }
  | integer as inum              { let num = float_of_string inum in
                                   VARINT num }
  | number as nnum               { let num = float_of_string nnum in
                                   Printf.printf "%f" num;
                                   NUM num }
  | little as lnum               { let num = float_of_string lnum in
                                   Printf.printf "%f" num;
                                   NUM num }
  | lparen                       { LPAREN }
  | rparen                       { RPAREN }    
  | lbrack                       { LBRACK }
  | rbrack                       { RBRACK }
  | dollar                       { DOLLAR }
  | booltrue                     { BOOLTRUE }
  | boolfalse                    { BOOLFALSE }
  | id as str                    { Printf.printf "ID = %s" str;ID str }
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
      
    
















