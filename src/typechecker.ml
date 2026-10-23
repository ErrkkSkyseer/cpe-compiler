open Utils
open Ast
 
(* TODO: define your own types *)
type ty = Unit | Int | Float | Bool | Char | String | List 
 
let string_of_type (t : ty) : string =
  match t with
  | Unit -> "()"
  | Int -> "int"
  | Float -> "float"
  | Bool -> "bool"
  | Char -> "char"
  | String -> "String"
  | List -> "[]"
 

let rec uop_err_msg (pos : pos) (expect : ty) (got : ty) : unit = 
  prerr_string
    (string_of_positions pos
      ^ ": expected " 
      ^ string_of_type expect 
      ^ "operands, but found" 
      ^ string_of_type got
      ^ "\n"
    )

let binop_err_msg pos expect l r : unit = 
  prerr_string
    (string_of_positions pos
      ^ ": expected " 
      ^ string_of_type expect 
      ^ "operands, but found" 
      ^ string_of_type l
      ^ ", "
      ^ string_of_type r
      ^ "\n"
    )

let binop_err_msg_tstr pos expect l r : unit = 
  prerr_string
    (string_of_positions pos
      ^ ": expected " 
      ^ expect 
      ^ "operands, but found" 
      ^ string_of_type l
      ^ ", "
      ^ string_of_type r
      ^ "\n"
    )

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
  | EmptyListLit r -> Some (EmptyListLit { r with prop = List })

  (* Unary Operation *)
  | UnaryExpr ({op = Neg; _} as r) -> (
    match typecheck_expr r.expr with
    | Some r' -> (
        match prop_of_expr r' with
        | Int -> Some (UnaryExpr {r with prop = Int; expr = r'})
        | t -> uop_err_msg r.pos Int t ;None
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
          | (Float, Int) | (Int, Float) | (Float, Float) -> 
            mk_binop_expr Float r.pos r.op left' right'
          (* Wrong Type *)
          | _ -> 
            binop_err_msg_tstr r.pos "Int or Float" tl tr;
            None
        )
      | _ -> None
    )

  | BinExpr ({op = IDiv; _} as r)
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
          match (tl, tr) with
          | (Int, Int) | (Float, Int) -> 
            mk_binop_expr Int r.pos r.op left' right'
          | _ -> 
            binop_err_msg_tstr r.pos "Float or Int" tl tr;
            None
        )
      | _ -> None
    )

  | BinExpr ({op = Div; _} as r) 
  -> (match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
        let (tl,tr) = (prop_of_expr left',prop_of_expr right') in
          match (tl, tr) with
          | (Int, Int) | (Float, Int) | (Int, Float) | (Float, Float)-> 
            mk_binop_expr Float r.pos r.op left' right'
          | _ -> 
            binop_err_msg r.pos Float tl tr;
            None
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
            binop_err_msg_tstr r.pos "Int or Float" tl tr;
            None
        )
      | _ -> None
    )
 
  (* Boolean *)
 
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