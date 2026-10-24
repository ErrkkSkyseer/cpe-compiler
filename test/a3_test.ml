open OUnit2
open CpecmuCompiler.A3_typechecking

let name_prog_val = [
  ("2+3","def test () : Integer = 2 + 3",true);
  ("True && False","def test () : Boolean = True && False",true);
  ("1 + True","def test () : Boolean = 1+True",false);
  ("1 || True","def test () : Boolean = 1||True",false);
]

let unfold npv = List.map(fun (n,p,v) -> n >:: (fun _ -> assert_equal v (typecheck_string p));) npv

let tests = "test suite for sample typechecking" >::: unfold name_prog_val

(* [

  "2+3" >:: (fun _ -> assert_equal true (typecheck_string "def test () : Integer = 2 + 3"));
  (* "true && false" >:: (fun _ -> assert_equal true (typecheck_string "def test () : Bool = true && false"));
  "1+true" >:: (fun _ -> assert_equal false (typecheck_string "def test () : Bool = 1+true"));
  "1 || true" >:: (fun _ -> assert_equal false (typecheck_string "def test () : Bool = 1+true")); *)
] *)

let _ = run_test_tt_main tests
