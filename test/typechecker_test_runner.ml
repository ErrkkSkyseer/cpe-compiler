open CpecmuCompiler.Typechecker
open CpecmuCompiler.Ast


let dummypos' : Lexing.position = {
  pos_fname = "";
  pos_lnum = 0;
  pos_bol = 0;
  pos_cnum = 0;
}
let dummypos : pos = (dummypos',dummypos') 

let expression_testcases = [
  ("Sample",
    (IntLit {prop = (); pos = dummypos; value = 0;}),
    (Some (IntLit {prop = Int; pos = dummypos; value = 0;})));
]

let test_typecheck_expr ast ty =
  let res = typecheck_expr ast in
  match res with
  | Some t -> 
    print_string ("got : " 
      ^ string_of_type (prop_of_expr t) 
      ^ " = expect : " 
      ^ (if ty == None then "None" else string_of_type (prop_of_expr (Option.get ty))) (*hehe*)
      ^ " "
    ); 
    res = ty
  | None -> print_string "Gae";
    res = ty

let test_expr expr_tup  =
  List.iter (fun explr -> 
    let (name,expr_in,expr_expect) = explr in
    print_string (name ^ " : ");
    if test_typecheck_expr expr_in expr_expect 
      then print_string "<PASS>" 
      else print_string "<FAIL>";
    print_newline();
    ) expr_tup

let run_typechecker_test testfunc =
  List.iter (fun f -> f ()) testfunc

(* let testrun () =
  if test_typecheck_expr 
    (IntLit {prop = (); pos = dummypos; value = 10;}) 
    (Some (IntLit {prop = Int; pos = dummypos; value = 10;}))
  then
    print_string "success"
  else
    print_string "fail" *)

let _ = run_typechecker_test [
  (* testrun; *)
  (fun f _ -> f ) (test_expr expression_testcases);
]

