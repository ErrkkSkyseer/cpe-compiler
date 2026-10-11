open Utils
open Ast
 
(* TODO: define your own types *)
type ty = Int | Bool
 
let string_of_type (t : ty) : string =
  match t with
  | Int -> "int"
  | Bool -> "bool"
 
(* TODO: define your own type checking *)
let rec typecheck_expr (e : unit expr) : ty expr option =
  match e with
  | IntLit r ->
      Some (IntLit { r with prop = Int })
 
  | BoolLit r ->
      Some (BoolLit { r with prop = Bool })
 
  | BinExpr ({op = Add; _} as r)
  | BinExpr ({op = Mult; _} as r) -> (
      match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
          match (prop_of_expr left', prop_of_expr right') with
          | (Int, Int) ->
              Some (BinExpr {
                r with
                prop = Int;
                left = left';
                right = right';
              })
 
          | (t1, t2) ->
              prerr_string
                (string_of_positions r.pos
                 ^ ": expected int operands, but found "
                 ^ string_of_type t1
                 ^ ", "
                 ^ string_of_type t2
                 ^ "\n");
              None
        )
 
      | _ -> None
    )
 
  | BinExpr ({op = LOr; _} as r)
  | BinExpr ({op = LAnd; _} as r) -> (
      match (typecheck_expr r.left, typecheck_expr r.right) with
      | (Some left', Some right') -> (
          match (prop_of_expr left', prop_of_expr right') with
          | (Bool, Bool) ->
              Some (BinExpr {
                r with
                prop = Bool;
                left = left';
                right = right';
              })
 
          | (t1, t2) ->
              prerr_string
                (string_of_positions r.pos
                 ^ ": expected bool operands, but found "
                 ^ string_of_type t1
                 ^ ", "
                 ^ string_of_type t2
                 ^ "\n");
              None
        )
 
      | _ -> None
    )
 
  (* temporary *)
  | _ -> None
 
let typecheck (p : unit prog) : bool =
  List.for_all
    (fun defn ->
      match defn with
      | Def d -> (
          match typecheck_expr d.body with
          | Some _ -> true
          | None -> false)
      | Data _ | TypeSyn _ -> true (* not checked yet *))
    p.defns