open Utils
open Ast
 
(* TODO: define your own types *)
type ty = 
  | Unit 
  | Int 
  | Float 
  | Bool 
  | Char 
  | String 
  | List of ty option
  | Func of ty option * ty option 

 
let rec string_of_type (t : ty) : string =
  match t with
  | Unit -> "()"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "string"
  | List t' ->( 
    match t' with
    | Some t -> "["^ string_of_type t ^"]"
    | None -> "[]"
    )
  | Func (a,b) -> 
    let a' =  
      match a with
      | Some t' -> string_of_type t' 
      | None -> "" in
    let b' = 
      match b with
      | Some t' -> string_of_type t' 
      | None -> "" in
    a' ^ "->" ^ b'

let uop_err_msg (pos : pos) (expect : ty) (got : ty) = 
  prerr_string
    (string_of_positions pos
      ^ ": expected " 
      ^ string_of_type expect 
      ^ " operands, but found " 
      ^ string_of_type got
      ^ "\n"
    ); None 

let binop_err_msg pos expect l r = 
  prerr_string
    (string_of_positions pos
      ^ ": expected " 
      ^ String.concat "," (List.map string_of_type expect )
      ^ " operands, but found "
      ^ string_of_type l
      ^ ", "
      ^ string_of_type r
      ^ "\n"
    ); None

let mk_binop_expr (prop : ty) pos op (l : ty expr) (r : ty expr) : ty expr option =
  Some (
    BinExpr {
      prop = prop; 
      pos = pos; 
      op = op;
      left = l ;
      right = r;
  }) 

let rec typecheck_expr (e : unit expr) : ty expr option =
  match e with
  (* Literals *)
  | UnitLit r -> Some (UnitLit { r with prop = Unit })
  | IntLit r -> Some (IntLit { r with prop = Int })
  | FloatLit r -> Some (FloatLit { r with prop = Float })
  | CharLit r -> Some (CharLit { r with prop = Char })
  | BoolLit r -> Some (BoolLit { r with prop = Bool })
  | StringLit r -> Some (StringLit { r with prop = String })
  | EmptyListLit r -> Some (EmptyListLit { r with prop = List None })

  (* Unary Operation *)
  | UnaryExpr ({op = Neg; _} as r) -> (
    match typecheck_expr r.expr with
    | Some r' -> (
        match prop_of_expr r' with
        | Int -> Some (UnaryExpr {r with prop = Int; expr = r'})
        | Float -> Some (UnaryExpr {r with prop = Float; expr = r'})
        | t -> uop_err_msg r.pos Int t 
      )
    | None -> None
    )

  (* Binary Operation *)
  (* Numbers *)
  (* Can Become Both Integer & Floating Point *)
  | BinExpr ({op = Add | Sub | Mult; _} as r) 
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
          match (tl, tr) with
          (* Both is Int --> Int *)
          | (Int, Int) -> 
            mk_binop_expr Int r.pos r.op left' right'
          (* Either one is Float --> Float, 3 cases *)
          | (Int | Float), (Int | Float) -> 
            mk_binop_expr Float r.pos r.op left' right'
          (* Wrong Type *)
          | _ -> 
            binop_err_msg r.pos [Int;Float] tl tr
        )
      | _ -> None
    )

  | BinExpr ({op = IDiv; _} as r)
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
          match (tl, tr) with
          | (Int | Float), Int -> 
            mk_binop_expr Int r.pos r.op left' right'
          | _ -> 
            binop_err_msg r.pos [Int;Float] tl tr
        )
      | _ -> None
    )

  | BinExpr ({op = Div; _} as r) 
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
          match (tl, tr) with
          | (Int | Float), (Int | Float)-> 
            mk_binop_expr Float r.pos r.op left' right'
          | _ -> 
            binop_err_msg r.pos [Float] tl tr
        )
      | _ -> None
    )

  | BinExpr ({op = Mod; _} as r) 
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
          match (tl, tr) with
          | (Int, Int) -> 
            mk_binop_expr Int r.pos r.op left' right'
          | (Float, Int)-> 
            mk_binop_expr Float r.pos r.op left' right'
          | _ -> 
            binop_err_msg r.pos [Int;Float] tl tr
        )
      | _ -> None
    )
 
  (* Logical Operator *)
  | BinExpr ({op = LAnd | LOr; _} as r)
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
          match (tl, tr) with
          | (Bool, Bool) -> 
            mk_binop_expr Bool r.pos r.op left' right'
          | _ -> 
            binop_err_msg r.pos [Bool] tl tr
        )
      | _ -> None
    )
  
  (* Comparasion Operator *)
  | BinExpr ({op = Eq | Neq; _} as r)
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
        if tl == tr 
          then mk_binop_expr Bool r.pos r.op left' right'
          else (prerr_string (string_of_positions r.pos 
            ^ ": expected same type but got "
            ^ string_of_type tl 
            ^"," 
            ^ string_of_type tr );
            None)
        )
      | _ -> None
    )

  (* Number Comparasion Operator *)
  | BinExpr ({op = Ge | Gt | Le | Lt; _} as r)
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
        match (tl,tr) with
        | (Int | Float),(Int | Float) -> mk_binop_expr Bool r.pos r.op left' right' 
        | _ -> binop_err_msg r.pos [Int;Float] tl tr
        )
      | _ -> None
    )

  (* List Concat *)
  | BinExpr ({op = Cons; _} as r)
   -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
        match (tl,tr) with
        | (tl',List Some tr') -> if tl' == tr' 
          then mk_binop_expr tr r.pos r.op left' right'
          else (prerr_string "List Concat Error message" ;None) 
        | _ -> binop_err_msg r.pos [List None] tl tr
        )
      | _ -> None
    )

  (* List Append *)
  | BinExpr ({op = Append; _} as r)
   -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
        match (tl,tr) with
        | (List tl',List tr') -> if tl' == tr' 
          then mk_binop_expr tr r.pos r.op left' right'
          else (prerr_string "List Concat Error message" ;None) 
        | _ -> binop_err_msg r.pos [List None; List None] tl tr
        )
      | _ -> None
    )

  (* Function Composition *)
  | BinExpr ({op = Compose; _} as r)
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
        match (tl,tr) with
        | (Func (a,lb),Func (rb,c)) -> if lb == rb 
          then mk_binop_expr (Func (a,c)) r.pos r.op left' right'
          else (prerr_string "Function Composition Error message" ;None) 
        | _ -> (prerr_string "Function Composition Error message" ;None) 
        )
      | _ -> None
    )

  (* Function Application Operator *)
  | BinExpr ({op = Apply; _} as r)
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
        match (tl,tr) with
        | (Func (Some a,Some b),tr') -> if tr' == a 
          then mk_binop_expr b r.pos r.op left' right'
          else (prerr_string "Function Application Error message" ;None) 
        | _ -> (prerr_string "Function Application Error message" ;None) 
        )
      | _ -> None
    )

  (* temporary *)
  | _ -> None
 
let typecheck (p : unit prog) : bool =
  let checkdefn (defn : unit defn) =
    match defn with
      | Def d -> (
          match typecheck_expr d.body with
          | Some _ -> true
          | None -> false)
      | Data _ -> true
      | TypeSyn _ -> true (* not checked yet *)
  in List.for_all checkdefn p.defns