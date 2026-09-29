(*ส่วนที่ 1: ส่วนหัว (header: optional imports & setup) *)

{
open Token

(* คำสงวน (keyword) ทั้งหมดของภาษา CPE *)
let keywords =
  [ "import"; "as"; "def"; "data"; "type"; "let"; "and"; "in";
    "if"; "then"; "else"; "match"; "with" ]


(*escape และ error*)

(* เพราะแต่ละชิ้นต้องจัดการไม่เหมือนกันเลยต้องเก็บแยกกันแล้วค่อยเอามาประกอบกัน*)
let unescape = function
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | 'b' -> '\b'
  | c   -> c

(* สร้าง Error จากข้อความที่เพิ่งอ่านได้ พร้อมตำแหน่งเริ่มต้นของมัน *)
let error lexbuf = Error (Lexing.lexeme lexbuf, Lexing.lexeme_start_p lexbuf)


(*แปลงตัวเลข*)

(* สเปกกำหนดว่าจำนวนเต็มต้องอยู่ระหว่าง 0 ถึง 2^62 - 1 *)
let max_int_lit = (1 lsl 62) - 1

(* แปลงข้อความเป็น IntLit ถ้าค่าเกินช่วงที่กำหนด ให้คืน Error *)
let int_lit s lexbuf =
  match int_of_string_opt s with
  | Some n when n >= 0 && n <= max_int_lit -> IntLit n
  | _ -> error lexbuf

(* แปลงข้อความเป็น FloatLit ต้องเก็บใน double 64 บิตได้
   ถ้าค่าใหญ่เกินจนกลายเป็น infinity ให้คืน Error *)
let float_lit s lexbuf =
  match float_of_string_opt s with
  | Some f when Float.is_finite f -> FloatLit f
  | _ -> error lexbuf
}




(*ส่วนที่ 2: นิยาม regular expression*)

(*name*)

(* ตัวอักษรที่ใช้ในชื่อได้ รวมถึง underscore และ prime เช่น x' *)
let idchar  = ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']

(* ขึ้นต้นด้วยตัวเล็กหรือ underscore = ชื่อตัวแปร/ฟังก์ชัน เช่น map, xs, _tmp *)
let lower   = ['a'-'z'] idchar* | '_' idchar+

(* ขึ้นต้นด้วยตัวใหญ่ = ชื่อ type หรือ constructor เช่น Tree, Leaf, Integer *)
let upper   = ['A'-'Z'] idchar*


(*number*)

let digit   = ['0'-'9']
let int     = digit+

(* เลขทศนิยม: ต้องมีตัวเลขทั้งสองข้างของจุด เช่น 3.14, 2.5e-3, 1e10
   ไม่รับ 1. หรือ .5 เพราะจุดตัวเดียวใช้เป็น function composition *)
let exp     = ['e' 'E'] ['+' '-']? digit+
let float   = digit+ '.' digit+ exp? | digit+ exp


(*char & string*)

(* ตัวอักษรที่ตามหลัง backslash ได้*)
let escchar = ['\\' '\'' '"' 'n' 'r' 't' 'b']

(* ตัวอักษรที่ใส่ได้ตรง ๆ โดยไม่ต้อง escape
   newline, CR, tab, backspace ต้อง escape เสมอ *)
let plain   = [^ '\\' '\'' '"' '\n' '\r' '\t' '\b']


(*Symbol(เครื่องหมาย)*)

(* ตัวที่ยาวกว่าจะถูกเลือกก่อน (longest match) เช่น ++ ได้ Symbol ++ ไม่ใช่ + สองตัว *)
let symbol =
    "**" | "//" | "==" | "!=" | "<=" | ">=" | "&&" | "||" | "::" | "++" | "->" | "=>"
  | '+' | '-' | '*' | '/' | '%' | '<' | '>' | '.' | '$' | '=' | '|' | ':'
  | ',' | '!' | '\\' | '(' | ')' | '[' | ']' | '_'




(*
   ส่วนที่ 3: กฎหลัก read (อ่าน token ถัดไปทีละตัว)*)
rule read = parse

  (*ข้าม: ช่องว่างและคอมเมนต์*)
  | [' ' '\t' '\r']+          { read lexbuf }
  | '\n'                      { Lexing.new_line lexbuf; read lexbuf }  (* นับบรรทัดเพิ่ม *)
  | '#' [^ '\n']*             { read lexbuf }                          (* คอมเมนต์ท้ายบรรทัด *)
  | "(*"                      { comment (Lexing.lexeme_start_p lexbuf) lexbuf }


  (*boolean และชื่อ*)
  | "True"                    { BoolLit true }
  | "False"                   { BoolLit false }

  (* ชื่อตัวเล็ก: ถ้าเป็นคำสงวนให้เป็น Keyword ถ้าไม่ใช่ให้เป็น VariableName *)
  | lower as s                { if List.mem s keywords then Keyword s
                                else VariableName s }

  (* ชื่อตัวใหญ่: ชื่อ type หรือ constructor *)
  | upper as s                { TypeOrConstructorName s }


  (*ตัวเลข*)
  (* ใช้ longest match ถ้าเป็น 3.14 จะได้ FloatLit ไม่ใช่ IntLit 3 *)
  | float as s                { float_lit s lexbuf }
  | int as s                  { int_lit s lexbuf }


  (*char*)
  | '\'' (plain as c) '\''            { CharLit c }
  | '\'' '\\' (escchar as c) '\''     { CharLit (unescape c) }

  (* char ที่ผิดรูป เช่น ว่างเปล่า, ยาวเกิน 1 ตัว, escape ที่ไม่มีในสเปก *)
  | '\'' ('\\' [^ '\n'] | [^ '\n' '\''])? '\''?
                                      { error lexbuf }


  (*string*)
  (* เจอ double quote เปิดแล้วไปอ่านต่อที่กฎ string ด้านล่าง
     หลังอ่านเสร็จ ตั้งตำแหน่งเริ่มให้ชี้ที่ quote เปิด *)
  | '"'   { let start = Lexing.lexeme_start_p lexbuf in
            let raw = Buffer.create 16 in
            Buffer.add_char raw '"';
            let tok = string start raw (Buffer.create 16) lexbuf in
            lexbuf.Lexing.lex_start_p <- start;
            tok }


  (*symbol*)
  | symbol as s               { Symbol s }


  (*จบไฟล์และ error*)
  | eof                       { EOF }
  | _                         { error lexbuf }   (* ตัวอักษรที่ไม่รู้จัก เช่น @ หรือ & ตัวเดียว *)




(*ส่วนที่ 4: กฎย่อย comment (คอมเมนต์หลายบรรทัด)*)
and comment start = parse
  | "*)"         { read lexbuf }
  | '\n'         { Lexing.new_line lexbuf; comment start lexbuf }
  | eof          { Error ("(*", start) }
  | _            { comment start lexbuf }




(*ส่วนที่ 5: กฎย่อย string (อ่านเนื้อหาในstringทีละชิ้น)
   แต่ละชิ้นต้องจัดการไม่เหมือนกัน จึงอ่านทีละชิ้น แปลงให้ถูก แล้วต่อเข้า Buffer*)
and string start raw buf = parse

  (*success*)
  (* เจอ quote ปิด: ประกอบทุกชิ้นเป็นสตริงเดียว *)
  | '"'                    { StringLit (Buffer.contents buf) }

  (* escape ที่ถูกต้อง: แปลงแล้วเก็บ *)
  | '\\' (escchar as c)    { Buffer.add_string raw (Lexing.lexeme lexbuf);
                             Buffer.add_char buf (unescape c);
                             string start raw buf lexbuf }

  (* ตัวอักษรธรรมดา: เก็บตามเดิม *)
  | plain as c             { Buffer.add_char raw c;
                             Buffer.add_char buf c;
                             string start raw buf lexbuf }


  (*error*)
  (* จบไฟล์ก่อนปิดสตริง *)
  | eof                    { Error (Buffer.contents raw, start) }

  (* escape ที่ไม่มีในสเปก หรือตัวอักษรที่ต้อง escape แต่ไม่ได้ escape
     เช่น newline หรือ quote เดี่ยวในสตริง *)
  | '\\' [^ '\n'] | _      { Buffer.add_string raw (Lexing.lexeme lexbuf);
                             Error (Buffer.contents raw, start) }
