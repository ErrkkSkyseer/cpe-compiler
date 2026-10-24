{
open Parser
exception SyntaxError of string * Lexing.position
let error msg lexbuf = raise (SyntaxError (msg, Lexing.lexeme_start_p lexbuf))

let keywords = Hashtbl.create 16
(* let () = List.iter (fun (k, t) -> Hashtbl.add keywords k t)
  [ "def", DEF; "import", IMPORT; "as", AS; "data", DATA; "type", TYPE;
    "match", MATCH; "with", WITH; "let", LET; "and", AND; "in", IN;
    "if", IF; "then", THEN; "else", ELSE ] *)
let () = List.iter (fun (k, t) -> Hashtbl.add keywords k t)
  [ "if", IF; "then", THEN; "else", ELSE ]

let int_lit s lexbuf =
  match int_of_string_opt s with
  | Some n -> INT_LIT n
  | None -> error ("integer literal out of range: " ^ s) lexbuf

let unescape lexbuf = function
  | '\\' -> '\\' | '\'' -> '\'' | '"' -> '"' | 'n' -> '\n'
  | 'r' -> '\r' | 't' -> '\t' | 'b' -> '\b'
  | c -> error (Printf.sprintf "bad escape \\%c" c) lexbuf
}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let idchar = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']
let lident = lower idchar*
let uident = upper idchar*

rule read = parse
  | [' ' '\t' '\r']        { read lexbuf }
  | '\n'                   { Lexing.new_line lexbuf; read lexbuf }
  | '#' [^ '\n']*          { read lexbuf }
  | "(*"                   { comment lexbuf; read lexbuf }
  | digit+ '.' digit+ (['e' 'E'] ['+' '-']? digit+)? as s { FLOAT_LIT (float_of_string s) }
  | digit+ as s            { int_lit s lexbuf }
  | "True"                 { TRUE }
  | "False"                { FALSE }
  | lident '!' lident as s { IDENT s }            (* namespace!name *)
  | lident as s            { match Hashtbl.find_opt keywords s with
                             | Some t -> t | None -> IDENT s }
  (* | uident as s            { UIDENT s } *)
  | uident as s            { IDENT s }
  (* | '_'                    { UNDERSCORE } *)
  | '\'' ([^ '\\' '\'' '\n'] as c) '\''         { CHAR_LIT c }
  | '\'' '\\' (_ as c) '\''                      { CHAR_LIT (unescape lexbuf c) }
  | '"'                    { string (Buffer.create 16) lexbuf }
  (* | "->" { ARROW } | "=>" { FATARROW } *)
  | "==" { EQ } | "!=" { NEQ } | "<=" { LE } | ">=" { GE }
  | "<" { LT } | ">" { GT } 
  (* | "=" { ASSIGN } *)
  | "&&" { ANDAND } | "||" { OROR }
  | "::" { CONS } 
  (* | ":" { COLON }  *)
  | "++" { APPEND }
  | "+" { PLUS } | "-" { MINUS } | "*" { TIMES }
  | "//" { IDIV } | "/" { DIV } | "%" { MOD }
  | "." { DOT } | "$" { DOLLAR } 
  (* | "\\" { BACKSLASH } 
  | "|" { BAR } *)
  | "," { COMMA } | "(" { LPAREN } | ")" { RPAREN }
  | "[" { LBRACKET } | "]" { RBRACKET }
  | eof                    { EOF }
  | _ as c                 { error (Printf.sprintf "unexpected character %C" c) lexbuf }

and comment = parse
  | "*)"  { () }
  | '\n'  { Lexing.new_line lexbuf; comment lexbuf }
  | eof   { error "unterminated comment" lexbuf }
  | _     { comment lexbuf }

and string buf = parse
  | '"'              { STRING_LIT (Buffer.contents buf) }
  | '\\' (_ as c)    { Buffer.add_char buf (unescape lexbuf c); string buf lexbuf }
  | '\n'             { error "newline in string literal" lexbuf }
  | eof              { error "unterminated string" lexbuf }
  | _ as c           { Buffer.add_char buf c; string buf lexbuf }
