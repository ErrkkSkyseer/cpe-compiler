(** header section **)
%{
open Ast
%}

(** declarations section **)

(* token declarations *)
//-------------------- keywords --------------------
// %token IMPORT "import"
// %token DEF "def"
// %token TYPE "type"
%token IF "if"
%token ELSE "else"
// %token WITH "with"
// %token AS "as"
// %token DATA "data"
%token LET "let"
%token IN "in"
%token THEN "then"
// %token MATCH "match"
%token AND "and"
%token <string> IDENT
%token <string> UIDENT

//-------------------- literals --------------------
%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token TRUE "true"
%token FALSE "false"

//-------------------- symbols --------------------
%token IDIV "//"        
%token EQ "=="
%token NEQ "!="
%token LE "<="
%token GE ">="
%token ANDAND "&&"
%token OROR "||" 
%token CONS "::"
%token APPEND "++"
%token ARROW "->"
%token FATARROW "=>"
%token PLUS "+" 
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token MOD "%"
%token LT "<"
%token GT ">"
%token DOT "."
%token DOLLAR "$"
%token ASSIGN "="
// %token BAR "|"
%token COLON ":"
%token COMMA ","
// %token EXC "!" 
%token BACKSLASH "\\"
%token LPAREN "("
%token RPAREN ")"
%token LBRACKET "["
%token RBRACKET "]"
%token UNDERSCORE "_"

//-------------------- end of file --------------------
%token EOF

(* precedence and associativity declarations *)
// %nonassoc LOW
// %nonassoc BAR
// %right DOLLAR
// %right OROR
// %right ANDAND
// %nonassoc EQ NEQ LT LE GE GT
// %right CONS APPEND
// %left PLUS MINUS
// %left TIMES DIV IDIV MOD
// %nonassoc UMINUS
// %right DOT

(* starting nonterminal *)
%start <unit prog> start
%start <unit expr> expr_entry
%start <typeexpr> type_entry
%start <param> param_entry
%start <pattern> pattern_entry
%%
//-------------------- blind operators --------------------
%inline comp_op:
  | EQ { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | LE { Le }
  | GE { Ge }
  | GT { Gt }

%inline list_op:
  | CONS { Cons }
  | APPEND { Append }

%inline add_op:
  | PLUS { Add }
  | MINUS { Sub }

%inline mul_op:
  | TIMES { Mult }
  | DIV { Div }
  | IDIV { IDiv }
  | MOD { Mod }

(** rules section **)
start:
  | prog = prog; EOF { prog }

expr_entry:
  | e = expr; EOF { e }

type_entry:
  | t = typeexpr; EOF { t }

  param_entry:
    | p = param; EOF { p }

  pattern_entry:
    | p = pattern; EOF { p }

// //-------------------- program --------------------
prog:
  | e = expr
    { { imports = [];
        defns = [ Def {
          pos = $loc;
          name = "test";
          params = [];
          return_type = TypeVar { pos = $loc; name = "_" };
          body = e
        } ] } }

//-------------------- expressions --------------------
expr:
  | e = let_expr    { e }
  | e = lambda_expr { e }
  // | e = match_expr  { e }
  | e = if_expr     { e }
  | e = dollar_expr { e }

//-------------------- let expressions --------------------
let_expr:
  | LET; bs = separated_nonempty_list(AND, letbinding); IN; b = expr
      { LetExpr { prop = (); pos = $loc; bindings = bs; body = b } }
    
letbinding:
  | x = IDENT; ASSIGN; v = expr
      { LetValue { pos = $loc; name = x; value = v } }
  | x = IDENT; ps = nonempty_list(param); COLON; t = typeexpr; ASSIGN; b = expr
      { LetFunction { pos = $loc; name = x; params = ps; return_type = t; body = b } }

//-------------------- lambda expressions --------------------
lambda_expr: 
  | BACKSLASH; ps = nonempty_list(param); COLON; t = typeexpr; FATARROW; b = expr
      { Lambda { prop = (); pos = $loc; params = ps; return_type = t; body = b } }

//-------------------- if expressions --------------------
if_expr:
  | IF; c = expr; THEN; t = expr; ELSE; e = expr
    { IfExpr {
        prop = ();
        pos = $loc;
        cond = c;
        then_expr = t;
        else_expr = e
      }
    }

dollar_expr:
  | l = or_expr; DOLLAR; r = dollar_expr 
    { BinExpr {
        prop = ();
        pos = $loc;
        op = Apply;
        left = l;
        right = r
      } 
    }
  | e = or_expr { e }

or_expr:
  | l = and_expr; OROR; r = or_expr
    { BinExpr {
        prop = ();
        pos = $loc;
        op = LOr;
        left = l;
        right = r
      }
    }
  | e = and_expr { e }

and_expr:
  | l = eq_expr; ANDAND; r = and_expr
    { BinExpr {
        prop = ();
        pos = $loc;
        op = LAnd;
        left = l;
        right = r
      }
    }
  | e = eq_expr { e }

eq_expr:
  | l = cons_expr; op = comp_op; r = cons_expr 
    { BinExpr {
        prop = ();
        pos = $loc;
        op = op;
        left = l;
        right = r
      }
    }
  | e = cons_expr { e }

  cons_expr:
    | l = add_expr; op = list_op; r = cons_expr
      { BinExpr {
          prop = ();
          pos = $loc;
          op = op;
          left = l;
          right = r
        }
      }
    | e = add_expr { e }
  
  add_expr:
    | l = add_expr; op = add_op; r = mul_expr
      { BinExpr {
          prop = ();
          pos = $loc;
          op = op;
          left = l;
          right = r
        }
      }
    | e = mul_expr { e }
  
  mul_expr:
    | l = mul_expr; op = mul_op; r = neg_expr
      { BinExpr {
          prop = ();
          pos = $loc;
          op = op;
          left = l;
          right = r
        }
      }
    | e = neg_expr { e }

  neg_expr:
    | MINUS; e = neg_expr
      { UnaryExpr {
          prop = ();
          pos = $loc;
          op = Neg;
          expr = e
        }
      }
    | e = comp_expr { e }

  comp_expr:
    | l = app_expr; DOT; r = comp_expr
      { BinExpr {
          prop = ();
          pos = $loc;
          op = Compose;
          left = l;
          right = r
        }
      }
    | e = app_expr { e }

  app_expr:
    | f = app_expr; x = expr_atom                              
      { App {
          prop = ();
          pos = $loc;
          func = f;
          arg = x
        }
      }
    | e = expr_atom { e }

  expr_atom:
    | n = INT_LIT      { IntLit { prop = (); pos = $loc; value = n } }
    | x = FLOAT_LIT    { FloatLit { prop = (); pos = $loc; value = x } }
    | c = CHAR_LIT     { CharLit { prop = (); pos = $loc; value = c } }
    | s = STRING_LIT   { StringLit { prop = (); pos = $loc; value = s } }
    | TRUE             { BoolLit { prop = (); pos = $loc; value = true } }
    | FALSE            { BoolLit { prop = (); pos = $loc; value = false } }
    | x = IDENT        { Var { prop = (); pos = $loc; name = x } }
    | LPAREN; RPAREN   { UnitLit { prop = (); pos = $loc } }
    | LBRACKET; RBRACKET { EmptyListLit { prop = (); pos = $loc } }
    | LPAREN; l = expr; COMMA; r = expr; RPAREN
      { PairExpr { prop = (); pos = $loc; left = l; right = r } }
    | LPAREN; e = expr; RPAREN { e }
  
  //-------------------- type expressions --------------------
  typeexpr:                                      
  | l = type_app; ARROW; r = typeexpr
      { FuncType { pos = $loc; arg = l; ret = r } }
  | t = type_app { t }

  type_app:                                           
    | f = type_app; x = type_atom
        { TypeApp { pos = $loc; func = f; arg = x } }
    | t = type_atom { t }

    type_atom:
    | x = IDENT   { TypeVar { pos = $loc; name = x } } // variable identifiers
    | x = UIDENT  { TypeVar { pos = $loc; name = x } } // type identifiers
    | LBRACKET; t = typeexpr; RBRACKET
        { ListType { pos = $loc; elem = t } }
    | LPAREN; l = typeexpr; COMMA; r = typeexpr; RPAREN
        { PairType { pos = $loc; left = l; right = r } }
    | LPAREN; t = typeexpr; RPAREN { t }

//-------------------- parameters --------------------
  param:
    | LPAREN; RPAREN
        { UnitParam { pos = $loc } }
    | LPAREN; x = IDENT; COLON; t = typeexpr; RPAREN
        { Param { pos = $loc; name = x; ty = t } }

//-------------------- patterns --------------------
  pattern:
    | h = pat_con; CONS; t = pattern
        { PCons { pos = $loc; head = h; tail = t } }
    | p = pat_con { p }

    pat_con:
    | c = UIDENT; args = nonempty_list(pat_atom)
        { PCon { pos = $loc; name = c; args = args } }
    | p = pat_atom { p }

    pat_atom:
    | UNDERSCORE     { PWild { pos = $loc } }
    | x = IDENT      { PVar { pos = $loc; name = x } }
    | c = UIDENT     { PCon { pos = $loc; name = c; args = [] } }
    | TRUE           { PCon { pos = $loc; name = "True"; args = [] } }
    | FALSE          { PCon { pos = $loc; name = "False"; args = [] } }
    | n = INT_LIT    { PInt { pos = $loc; value = n } }
    | c = CHAR_LIT   { PChar { pos = $loc; value = c } }
    | s = STRING_LIT { PString { pos = $loc; value = s } }
    | LPAREN; RPAREN { PUnit { pos = $loc } }
    | LBRACKET; RBRACKET { PNil { pos = $loc } }
    | LPAREN; l = pattern; COMMA; r = pattern; RPAREN
        { PPair { pos = $loc; left = l; right = r } }
    | LPAREN; p = pattern; RPAREN { p }