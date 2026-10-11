type pos = Lexing.position * Lexing.position

(* ---------------- type expressions ---------------- *)
type typeexpr =
  | TypeVar of { pos : pos; name : string }
  | ListType of { pos : pos; elem : typeexpr }
  | PairType of { pos : pos; left : typeexpr; right : typeexpr }
  | FuncType of { pos : pos; arg : typeexpr; ret : typeexpr }
  | TypeApp of { pos : pos; func : typeexpr; arg : typeexpr }

(* ---------------- parameters ---------------- *)
type param =
  | UnitParam of { pos : pos }
  | Param of { pos : pos; name : string; ty : typeexpr }

(* ---------------- operators ---------------- *)
type unop = Neg

type op =
  | Add | Sub | Mult | Div | IDiv | Mod
  | LOr | LAnd
  | Eq | Neq | Lt | Gt | Le | Ge
  | Cons | Append | Compose | Apply

(* ---------------- patterns ---------------- *)
type pattern =
  | PWild of { pos : pos }
  | PVar of { pos : pos; name : string }
  | PCon of { pos : pos; name : string; args : pattern list }
  | PPair of { pos : pos; left : pattern; right : pattern }
  | PCons of { pos : pos; head : pattern; tail : pattern }
  | PInt of { pos : pos; value : int }
  | PChar of { pos : pos; value : char }
  | PString of { pos : pos; value : string }
  | PUnit of { pos : pos }
  | PNil of { pos : pos }

(* ---------------- expressions ---------------- *)
type 't expr =
  | IntLit of { prop : 't; pos : pos; value : int }
  | BoolLit of { prop : 't; pos : pos; value : bool }
  | FloatLit of { prop : 't; pos : pos; value : float }
  | CharLit of { prop : 't; pos : pos; value : char }
  | StringLit of { prop : 't; pos : pos; value : string }
  | UnitLit of { prop : 't; pos : pos }
  | EmptyListLit of { prop : 't; pos : pos }
  | Var of { prop : 't; pos : pos; name : string }
  | PairExpr of { prop : 't; pos : pos; left : 't expr; right : 't expr }
  | App of { prop : 't; pos : pos; func : 't expr; arg : 't expr }
  | BinExpr of { prop : 't; pos : pos; op : op; left : 't expr; right : 't expr }
  | UnaryExpr of { prop : 't; pos : pos; op : unop; expr : 't expr }
  | IfExpr of { prop : 't; pos : pos; cond : 't expr;
                then_expr : 't expr; else_expr : 't expr }
  | LetExpr of { prop : 't; pos : pos; bindings : 't letbinding list; body : 't expr }
  | Lambda of { prop : 't; pos : pos; params : param list;
                return_type : typeexpr; body : 't expr }
  | MatchExpr of { prop : 't; pos : pos; scrutinee : 't expr; cases : 't case list }

and 't letbinding =
  | LetValue of { pos : pos; name : string; value : 't expr }
  | LetFunction of { pos : pos; name : string; params : param list;
                     return_type : typeexpr; body : 't expr }

and 't case = { case_pos : pos; ptrn : pattern; case_body : 't expr }

(* ---------------- top level ---------------- *)
type typedecl = { td_name : string; td_params : string list }
type constructor = { con_name : string; con_args : typeexpr list }

type 't defn =
  | Def of { pos : pos; name : string; params : param list;
             return_type : typeexpr; body : 't expr }
  | Data of { pos : pos; decl : typedecl; constructors : constructor list }
  | TypeSyn of { pos : pos; decl : typedecl; body : typeexpr }

type import = { imp_pos : pos; imp_name : string; namespace : string option }

type 't prog = { prog_pos : pos; imports : import list; defns : 't defn list }

let prop_of_expr (e : 't expr) : 't = match e with
  | IntLit r -> r.prop | BoolLit r -> r.prop | FloatLit r -> r.prop
  | CharLit r -> r.prop | StringLit r -> r.prop | UnitLit r -> r.prop
  | EmptyListLit r -> r.prop | Var r -> r.prop | PairExpr r -> r.prop
  | App r -> r.prop | BinExpr r -> r.prop | UnaryExpr r -> r.prop
  | IfExpr r -> r.prop | LetExpr r -> r.prop | Lambda r -> r.prop
  | MatchExpr r -> r.prop

(* ---------------- pretty-printing (S-expression output) ---------------- *)
let sexp_list f xs = "(" ^ String.concat " " (List.map f xs) ^ ")"

let escape_char = function
  | '\\' -> "\\\\" | '\'' -> "\\'" | '"' -> "\\\"" | '\n' -> "\\n"
  | '\r' -> "\\r" | '\t' -> "\\t" | '\b' -> "\\b" | c -> String.make 1 c

let escape_string s =
  String.concat "" (List.init (String.length s) (fun i -> escape_char s.[i]))

let rec string_of_typeexpr = function
  | TypeVar r -> r.name
  | ListType r -> "([] " ^ string_of_typeexpr r.elem ^ ")"
  | PairType r -> "(, " ^ string_of_typeexpr r.left ^ " " ^ string_of_typeexpr r.right ^ ")"
  | FuncType r -> "(-> " ^ string_of_typeexpr r.arg ^ " " ^ string_of_typeexpr r.ret ^ ")"
  | TypeApp r -> "(" ^ string_of_typeexpr r.func ^ " " ^ string_of_typeexpr r.arg ^ ")"

let string_of_param = function
  | UnitParam _ -> "()"
  | Param r -> "(" ^ r.name ^ " " ^ string_of_typeexpr r.ty ^ ")"

let string_of_unop = function Neg -> "-"

let string_of_op = function
  | Add -> "+" | Sub -> "-" | Mult -> "*" | Div -> "/" | IDiv -> "//" | Mod -> "%"
  | LOr -> "||" | LAnd -> "&&"
  | Eq -> "==" | Neq -> "!=" | Lt -> "<" | Gt -> ">" | Le -> "<=" | Ge -> ">="
  | Cons -> "::" | Append -> "++" | Compose -> "." | Apply -> "$"

let rec string_of_pattern = function
  | PWild _ -> "_"
  | PVar r -> r.name
  | PCon { name; args = []; _ } -> name
  | PCon r -> "(" ^ String.concat " " (r.name :: List.map string_of_pattern r.args) ^ ")"
  | PPair r -> "(, " ^ string_of_pattern r.left ^ " " ^ string_of_pattern r.right ^ ")"
  | PCons r -> "(:: " ^ string_of_pattern r.head ^ " " ^ string_of_pattern r.tail ^ ")"
  | PInt r -> string_of_int r.value
  | PChar r -> "'" ^ escape_char r.value ^ "'"
  | PString r -> "\"" ^ escape_string r.value ^ "\""
  | PUnit _ -> "()"
  | PNil _ -> "[]"

let rec string_of_expr e = match e with
  | IntLit r -> string_of_int r.value
  | BoolLit r -> if r.value then "True" else "False"
  | FloatLit r -> string_of_float r.value
  | CharLit r -> "'" ^ escape_char r.value ^ "'"
  | StringLit r -> "\"" ^ escape_string r.value ^ "\""
  | UnitLit _ -> "()"
  | EmptyListLit _ -> "[]"
  | Var r -> r.name
  | PairExpr r -> "(, " ^ string_of_expr r.left ^ " " ^ string_of_expr r.right ^ ")"
  | App r -> "(" ^ string_of_expr r.func ^ " " ^ string_of_expr r.arg ^ ")"
  | BinExpr r ->
      "(" ^ String.concat " "
              [string_of_op r.op; string_of_expr r.left; string_of_expr r.right] ^ ")"
  | UnaryExpr r -> "(" ^ string_of_unop r.op ^ " " ^ string_of_expr r.expr ^ ")"
  | IfExpr r ->
      "(if " ^ string_of_expr r.cond ^ " " ^ string_of_expr r.then_expr
      ^ " " ^ string_of_expr r.else_expr ^ ")"
  | LetExpr r ->
      "(let " ^ sexp_list string_of_letbinding r.bindings ^ " " ^ string_of_expr r.body ^ ")"
  | Lambda r ->
      "(\\ " ^ sexp_list string_of_param r.params ^ " " ^ string_of_typeexpr r.return_type
      ^ " " ^ string_of_expr r.body ^ ")"
  | MatchExpr r ->
      "(match " ^ string_of_expr r.scrutinee ^ " " ^ sexp_list string_of_case r.cases ^ ")"

and string_of_letbinding = function
  | LetValue r -> "(" ^ r.name ^ " " ^ string_of_expr r.value ^ ")"
  | LetFunction r ->
      "(" ^ r.name ^ " " ^ sexp_list string_of_param r.params ^ " "
      ^ string_of_typeexpr r.return_type ^ " " ^ string_of_expr r.body ^ ")"

and string_of_case c = "(-> " ^ string_of_pattern c.ptrn ^ " " ^ string_of_expr c.case_body ^ ")"

let string_of_typedecl d =
  if d.td_params = [] then d.td_name
  else "(" ^ String.concat " " (d.td_name :: d.td_params) ^ ")"

let string_of_constructor c =
  if c.con_args = [] then c.con_name
  else "(" ^ String.concat " " (c.con_name :: List.map string_of_typeexpr c.con_args) ^ ")"

let string_of_defn = function
  | Def r ->
      "(def " ^ r.name ^ " " ^ sexp_list string_of_param r.params ^ " "
      ^ string_of_typeexpr r.return_type ^ " " ^ string_of_expr r.body ^ ")"
  | Data r ->
      "(data " ^ string_of_typedecl r.decl ^ " "
      ^ sexp_list string_of_constructor r.constructors ^ ")"
  | TypeSyn r -> "(type " ^ string_of_typedecl r.decl ^ " " ^ string_of_typeexpr r.body ^ ")"

let string_of_import i = match i.namespace with
  | None -> "(import " ^ i.imp_name ^ ")"
  | Some ns -> "(import " ^ i.imp_name ^ " " ^ ns ^ ")"

(* program: (imports defns) *)
let string_of_prog p =
  "(" ^ sexp_list string_of_import p.imports ^ " " ^ sexp_list string_of_defn p.defns ^ ")"
