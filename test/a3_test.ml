open OUnit2
open CpecmuCompiler.A3_typechecking

let mkprog t p = "def test () : "^ t ^ " = " ^ p

let name_prog_val = [
  (* given program *)

  ("2+3","def test () : Integer = 2 + 3",true);
  ("True && False","def test () : Boolean = True && False",true);
  ("1 + True","def test () : Boolean = 1+True",false);
  ("1 || True","def test () : Boolean = 1||True",false);

  (* number *)
  ("+Int Int", mkprog "Integer"   "1+1" , true);
  ("+Int Double", mkprog "Double" "1+1.0" , true);
  ("+Double Int", mkprog "Double" "1.0+1" , true);
  ("+Int Double", mkprog "Double" "1.0+1.0" , true);

  ("-Int Int", mkprog "Integer"   "1  -1" , true);
  ("-Int Double", mkprog "Double" "1  -1.0" , true);
  ("-Double Int", mkprog "Double" "1.0-1" , true);
  ("-Int Double", mkprog "Double" "1.0-1.0" , true);

  ("*Int Int", mkprog "Integer"   "1  *1" , true);
  ("*Int Double", mkprog "Double" "1  *1.0" , true);
  ("*Double Int", mkprog "Double" "1.0*1" , true);
  ("*Int Double", mkprog "Double" "1.0*1.0" , true);

  ("/Int Int", mkprog "Double"    "1  /1" , true);
  ("/Int Double", mkprog "Double" "1  /1.0" , true);
  ("/Double Int", mkprog "Double" "1.0/1" , true);
  ("/Int Double", mkprog "Double" "1.0/1.0" , true);

  ("//Int Int", mkprog "Integer"    "1   // 1" , true);
  ("//Int Double", mkprog "Double"  "1   // 1.0" , false);
  ("//Double Int", mkprog "Integer" "1.0 // 1" , true);
  ("//Int Double", mkprog "Integer" "1.0 // 1.0" , false);

  ("%Int Int", mkprog "Integer"    "1  %1" , true);
  ("%Int Double", mkprog "Double"  "1  %1.0" , false);
  ("%Double Int", mkprog "Double"  "1.0%1" , true);
  ("%Int Double", mkprog "Integer" "1.0%1.0" , false);

  (* wrong type *)
  ("+wrong", mkprog "()"  "1   +   True",false);
  ("-wrong", mkprog "()"  "1   -   True",false);
  ("*wrong", mkprog "()"  "1   *   True",false);
  ("/wrong", mkprog "()"  "1   /   True",false);
  ("//wrong", mkprog "()" "1   //  True",false);
  ("%wrong", mkprog "()"  "1   %   True",false);

  (* boolean *)
  ("True && False", mkprog "Boolean" "True && False" , true);
  ("True || False", mkprog "Boolean" "True || False" , true);

  (* Compare *)
  ("==Int_Int", mkprog "Boolean" "1        ==  1", true);
  ("==Int_Double", mkprog "Boolean" "1        ==  1.0", true);
  ("==Double_Int", mkprog "Boolean" "1.0      ==  1", true);
  ("==Double_Double", mkprog "Boolean" "1.0      ==  1.0", true);
  ("==Char", mkprog "Boolean" "'a'      ==  'a'", true);
  ("==Str", mkprog "Boolean" "\"abc\"  ==  \"abc\"", true);
  ("==Bool", mkprog "Boolean" "True     ==  False", true);
  ("==Unit", mkprog "Boolean" "()       ==  ()", true);
  ("==List", mkprog "Boolean" "[]       ==  []", true);

  ("!=Int_Int", mkprog "Boolean" "1        !=  1", true);
  ("!=Int_Double", mkprog "Boolean" "1        !=  1.0", true);
  ("!=Double_Int", mkprog "Boolean" "1.0      !=  1", true);
  ("!=Double_Double", mkprog "Boolean" "1.0      !=  1.0", true);
  ("!=Char", mkprog "Boolean" "'a'      !=  'a'", true);
  ("!=Str", mkprog "Boolean" "\"abc\"  !=  \"abc\"", true);
  ("!=Bool", mkprog "Boolean" "True     !=  False", true);
  ("!=Unit", mkprog "Boolean" "()       !=  ()", true);
  ("!=List", mkprog "Boolean" "[]       !=  []", true);

  (">Int_Int", mkprog "Boolean" "1   > 1", true);
  (">Int_Double", mkprog "Boolean" "1   > 1.0", true);
  (">Double_Int", mkprog "Boolean" "1.0 > 1", true);
  (">Double_Double", mkprog "Boolean" "1.0 > 1.0", true);

  (">=Int_Int", mkprog "Boolean" "1   >= 1", true);
  (">=Int_Double", mkprog "Boolean" "1   >= 1.0", true);
  (">=Double_Int", mkprog "Boolean" "1.0 >= 1", true);
  (">=Double_Double", mkprog "Boolean" "1.0 >= 1.0", true);

  ("<Int_Int", mkprog "Boolean" "1   < 1", true);
  ("<Int_Double", mkprog "Boolean" "1   < 1.0", true);
  ("<Double_Int", mkprog "Boolean" "1.0 < 1", true);
  ("<Double_Double", mkprog "Boolean" "1.0 < 1.0", true);

  ("<=Int_Int", mkprog "Boolean" "1   <= 1", true);
  ("<=Int_Double", mkprog "Boolean" "1   <= 1.0", true);
  ("<=Double_Int", mkprog "Boolean" "1.0 <= 1", true);
  ("<=Double_Double", mkprog "Boolean" "1.0 <= 1.0", true);

]


let unfold npv = List.map(fun (n,p,v) -> n >:: (fun _ -> assert_equal v (typecheck_string p true));) npv

let tests = "test suite for sample typechecking" >::: unfold name_prog_val

(* [

  "2+3" >:: (fun _ -> assert_equal true (typecheck_string "def test () : Integer = 2 + 3"));
  (* "true && false" >:: (fun _ -> assert_equal true (typecheck_string "def test () : Bool = true && false"));
  "1+true" >:: (fun _ -> assert_equal false (typecheck_string "def test () : Bool = 1+true"));
  "1 || true" >:: (fun _ -> assert_equal false (typecheck_string "def test () : Bool = 1+true")); *)
] *)

let _ = run_test_tt_main tests
