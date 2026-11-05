%{
open Ast
let bin loc op l r = BinExpr { prop = (); pos = loc; op; left = l; right = r }
%}

%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <string> IDENT        (* lowercase identifier, may be ns!name *)
%token <string> UIDENT       (* uppercase identifier: type / constructor *)

%token TRUE "True" FALSE "False"
%token DEF "def" IMPORT "import" AS "as" DATA "data" TYPE "type"
%token MATCH "match" WITH "with" LET "let" AND "and" IN "in"
%token IF "if" THEN "then" ELSE "else"
%token UNDERSCORE "_"

%token PLUS "+" MINUS "-" TIMES "*" DIV "/" IDIV "//" MOD "%"
%token OROR "||" ANDAND "&&"
%token EQ "==" NEQ "!=" LT "<" GT ">" LE "<=" GE ">="
%token CONS "::" APPEND "++" DOT "." DOLLAR "$"
%token ASSIGN "=" COLON ":" ARROW "->" FATARROW "=>" BACKSLASH "\\" BAR "|" COMMA ","
%token LPAREN "(" RPAREN ")" LBRACKET "[" RBRACKET "]"
%token EOF

(* lowest precedence first *)
%nonassoc LOWEST_CASES
%nonassoc IN ELSE FATARROW        (* let / if / lambda extend as far right as possible *)
%nonassoc ARROW                   (* match case body *)
%nonassoc BAR
%right DOLLAR
%right OROR
%right ANDAND
%nonassoc EQ NEQ LT GT LE GE
%right CONS APPEND
%left PLUS MINUS
%left TIMES DIV IDIV MOD
%nonassoc UMINUS
%right DOT

%start <unit Ast.prog> start

%%

start:
  | imports = list(import); defns = list(defn); EOF
    { {imports; defns } }

import:
  | "import"; n = IDENT                  { { imp_pos = $loc; imp_name = n; namespace = None } }
  | "import"; n = IDENT; "as"; ns = IDENT { { imp_pos = $loc; imp_name = n; namespace = Some ns } }

defn:
  | "def"; name = IDENT; params = list(param); ":"; return_type = typeexpr; "="; body = expr
    { Def { pos = $loc; name; params; return_type; body } }
  | "data"; decl = typedecl; "="; constructors = separated_nonempty_list("|", constructor)
    { Data { pos = $loc; decl; constructors } }
  | "type"; decl = typedecl; "="; body = typeexpr
    { TypeSyn { pos = $loc; decl; body } }

typedecl:
  | n = UIDENT; ps = list(IDENT) { { td_name = n; td_params = ps } }

constructor:
  | n = UIDENT; args = list(atomic_type) { { con_name = n; con_args = args } }

(* ---------------- types ---------------- *)
typeexpr:
  | t = app_type { t }
  | a = app_type; "->"; r = typeexpr
    { FuncType { pos = $loc; arg = a; ret = r } }

app_type:
  | t = atomic_type { t }
  | f = app_type; a = atomic_type { TypeApp { pos = $loc; func = f; arg = a } }

atomic_type:
  | x = IDENT   { TypeVar { pos = $loc; name = x } }
  | x = UIDENT  { TypeVar { pos = $loc; name = x } }
  | "("; ")"    { TypeVar { pos = $loc; name = "()" } }
  | "["; t = typeexpr; "]" { ListType { pos = $loc; elem = t } }
  | "("; l = typeexpr; ","; r = typeexpr; ")" { PairType { pos = $loc; left = l; right = r } }
  | "("; t = typeexpr; ")" { t }

(* ---------------- parameters ---------------- *)
param:
  | "("; ")"  { UnitParam { pos = $loc } }
  | "("; x = IDENT; ":"; t = typeexpr; ")" { Param { pos = $loc; name = x; ty = t } }

(* ---------------- expressions ---------------- *)
expr:
  | e1 = expr; "$";  e2 = expr { bin $loc Apply e1 e2 }
  | e1 = expr; "||"; e2 = expr { bin $loc LOr e1 e2 }
  | e1 = expr; "&&"; e2 = expr { bin $loc LAnd e1 e2 }
  | e1 = expr; "==" ; e2 = expr { bin $loc Eq e1 e2 }
  | e1 = expr; "!="; e2 = expr { bin $loc Neq e1 e2 }
  | e1 = expr; "<";  e2 = expr { bin $loc Lt e1 e2 }
  | e1 = expr; ">";  e2 = expr { bin $loc Gt e1 e2 }
  | e1 = expr; "<="; e2 = expr { bin $loc Le e1 e2 }
  | e1 = expr; ">="; e2 = expr { bin $loc Ge e1 e2 }
  | e1 = expr; "::"; e2 = expr { bin $loc Cons e1 e2 }
  | e1 = expr; "++"; e2 = expr { bin $loc Append e1 e2 }
  | e1 = expr; "+";  e2 = expr { bin $loc Add e1 e2 }
  | e1 = expr; "-";  e2 = expr { bin $loc Sub e1 e2 }
  | e1 = expr; "*";  e2 = expr { bin $loc Mult e1 e2 }
  | e1 = expr; "/";  e2 = expr { bin $loc Div e1 e2 }
  | e1 = expr; "//"; e2 = expr { bin $loc IDiv e1 e2 }
  | e1 = expr; "%";  e2 = expr { bin $loc Mod e1 e2 }
  | e1 = expr; ".";  e2 = expr { bin $loc Compose e1 e2 }
  | "-"; e = expr %prec UMINUS
    { UnaryExpr { prop = (); pos = $loc; op = Neg; expr = e } }
  | e = app_expr { e }
  | "if"; c = expr; "then"; t = expr; "else"; e = expr
    { IfExpr { prop = (); pos = $loc; cond = c; then_expr = t; else_expr = e } }
  | "let"; bs = separated_nonempty_list("and", letbinding); "in"; body = expr
    { LetExpr { prop = (); pos = $loc; bindings = bs; body } }
  | "\\"; params = nonempty_list(param); ":"; return_type = typeexpr; "=>"; body = expr
    { Lambda { prop = (); pos = $loc; params; return_type; body } }
  | "match"; e = expr; "with"; cs = cases
    { MatchExpr { prop = (); pos = $loc; scrutinee = e; cases = cs } }

app_expr:
  | e = atom { e }
  | f = app_expr; x = atom { App { prop = (); pos = $loc; func = f; arg = x } }

atom:
  | "True"  { BoolLit { prop = (); pos = $loc; value = true } }
  | "False" { BoolLit { prop = (); pos = $loc; value = false } }
  | n = INT_LIT    { IntLit { prop = (); pos = $loc; value = n } }
  | f = FLOAT_LIT  { FloatLit { prop = (); pos = $loc; value = f } }
  | c = CHAR_LIT   { CharLit { prop = (); pos = $loc; value = c } }
  | s = STRING_LIT { StringLit { prop = (); pos = $loc; value = s } }
  | x = IDENT      { Var { prop = (); pos = $loc; name = x } }
  | x = UIDENT     { Var { prop = (); pos = $loc; name = x } }
  | "("; ")"       { UnitLit { prop = (); pos = $loc } }
  | "["; "]"       { EmptyListLit { prop = (); pos = $loc } }
  | "("; e = expr; ")" { e }
  | "("; l = expr; ","; r = expr; ")"
    { PairExpr { prop = (); pos = $loc; left = l; right = r } }

letbinding:
  | name = IDENT; "="; value = expr
    { LetValue { pos = $loc; name; value } }
  | name = IDENT; params = nonempty_list(param); ":"; return_type = typeexpr; "="; body = expr
    { LetFunction { pos = $loc; name; params; return_type; body } }

(* ---------------- match ---------------- *)
cases:
  | c = case %prec LOWEST_CASES { [c] }
  | c = case; cs = cases        { c :: cs }

case:
  | "|"; p = pattern; "->"; e = expr
    { { case_pos = $loc; ptrn = p; case_body = e } }

(* ---------------- patterns ---------------- *)
pattern:
  | p = app_pattern { p }
  | h = app_pattern; "::"; t = pattern { PCons { pos = $loc; head = h; tail = t } }

app_pattern:
  | p = simple_pattern { p }
  | c = UIDENT; args = list(atomic_pattern)
    { PCon { pos = $loc; name = c; args } }

atomic_pattern:
  | p = simple_pattern { p }
  | c = UIDENT { PCon { pos = $loc; name = c; args = [] } }

simple_pattern:
  | "_"   { PWild { pos = $loc } }
  | x = IDENT { PVar { pos = $loc; name = x } }
  | "True"  { PCon { pos = $loc; name = "True"; args = [] } }
  | "False" { PCon { pos = $loc; name = "False"; args = [] } }
  | n = INT_LIT    { PInt { pos = $loc; value = n } }
  | c = CHAR_LIT   { PChar { pos = $loc; value = c } }
  | s = STRING_LIT { PString { pos = $loc; value = s } }
  | "("; ")" { PUnit { pos = $loc } }
  | "["; "]" { PNil { pos = $loc } }
  | "("; p = pattern; ")" { p }
  | "("; l = pattern; ","; r = pattern; ")" { PPair { pos = $loc; left = l; right = r } }
