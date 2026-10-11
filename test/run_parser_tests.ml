(* โฟลเดอร์ที่เก็บ test case ของ Parsing *)
let valid_dir = "tests/02-parsing/ss"           (* ต้อง parse ผ่าน *)
let invalid_dir = "tests/02-parsing/ss_invalid" (* ต้อง parse ไม่ผ่าน (error) *)

(* โฟลเดอร์ผลลัพธ์: out/02-parsing/tests/<ชื่อ test>.out (1 test ต่อ 1 ไฟล์)
   เทสที่ต้อง error จะอยู่ใน out/02-parsing/tests/invalid/ *)
let out_root = "out/02-parsing/tests"
let out_ext = ".out"

(* ตั้ง UPDATE=1 เพื่อเขียนไฟล์ .expected ใหม่จากผลลัพธ์ปัจจุบัน *)
let update_mode = Sys.getenv_opt "UPDATE" <> None

(* ตรวจสอบว่าไฟล์ลงท้ายด้วย .cpe หรือไม่ *)
let is_cpe_file filename =
  Filename.check_suffix filename ".cpe"

(* รายชื่อไฟล์ .cpe ในโฟลเดอร์ (เรียงตามชื่อ) *)
let list_cpe_files dir =
  if Sys.file_exists dir && Sys.is_directory dir then
    Sys.readdir dir
    |> Array.to_list
    |> List.filter is_cpe_file
    |> List.sort String.compare
  else []

let read_file path =
  let ic = open_in_bin path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let write_file path s =
  let oc = open_out_bin path in
  output_string oc s;
  close_out oc

(* สร้างโฟลเดอร์ (รวม parent) *)
let mkdir_p dir =
  if Sys.command ("mkdir -p " ^ Filename.quote dir) <> 0 then
    Printf.eprintf "warning: cannot create %s (มีไฟล์ชื่อ 'out' อยู่หรือเปล่า?)\n" dir

(* ล้างผลลัพธ์รอบก่อน เพื่อไม่ให้มีไฟล์ค้างจาก test ที่ถูกลบ/เปลี่ยนชื่อ *)
let reset_out () =
  ignore (Sys.command ("rm -rf " ^ Filename.quote out_root));
  mkdir_p out_root;
  mkdir_p (Filename.concat out_root "invalid")

(* เขียนผลลัพธ์ของ test หนึ่งไฟล์ (ถ้าเขียนไม่ได้ก็ข้าม ไม่ให้ runner ล้ม) *)
let write_out subdir filename content =
  let path =
    Filename.concat (Filename.concat out_root subdir)
      (Filename.remove_extension filename ^ out_ext)
  in
  try write_file path content with Sys_error _ -> ()

(* ขึ้นบรรทัดใหม่ก่อนแต่ละ top-level item เพื่อให้ .expected อ่านง่าย *)
let normalize s =
  let n = String.length s in
  let b = Buffer.create (2 * n) in
  let starts_with i p =
    let l = String.length p in
    i + l <= n && String.sub s i l = p
  in
  let heads = [ "(def "; "(data "; "(type "; "(import " ] in
  for i = 0 to n - 1 do
    if List.exists (starts_with i) heads then begin
      let len = Buffer.length b in
      if len > 0 && Buffer.nth b (len - 1) = ' ' then Buffer.truncate b (len - 1);
      Buffer.add_char b '\n'
    end;
    Buffer.add_char b s.[i]
  done;
  Buffer.contents b

(* เรียก parse ของ assignment; exception ใด ๆ ถือว่า parse ไม่ผ่าน *)
let safe_parse path =
  try CpecmuCompiler.A2_parsing.parse_file path with _ -> None

let passed = ref 0
let failures : string list ref = ref []

let record_fail path reason =
  failures := Printf.sprintf "%s (%s)" path reason :: !failures

(* test ที่ต้อง parse ผ่าน; ถ้ามี .expected จะเทียบผลลัพธ์ด้วย *)
let run_valid filename =
  let path = Filename.concat valid_dir filename in
  Printf.printf "========================================\n";
  Printf.printf "Testing: %s\n" path;
  (match safe_parse path with
   | None ->
       Printf.printf "RESULT: FAIL\n";
       write_out "" filename "RESULT: FAIL\n";
       record_fail path "should parse"
   | Some prog ->
       let out = normalize (CpecmuCompiler.Ast.string_of_prog prog) in
       let expected_path = Filename.remove_extension path ^ ".expected" in
       if update_mode then write_file expected_path (out ^ "\n");
       if Sys.file_exists expected_path
          && String.trim (read_file expected_path) <> String.trim out
       then begin
         Printf.printf "RESULT: FAIL (output differs from %s)\n" expected_path;
         print_endline out;
         write_out "" filename
           ("RESULT: FAIL (output differs from .expected)\n" ^ out ^ "\n");
         record_fail path "output differs from .expected"
       end else begin
         Printf.printf "RESULT: PASS\n";
         print_endline out;
         write_out "" filename ("RESULT: PASS\n" ^ out ^ "\n");
         incr passed
       end);
  Printf.printf "Done.\n\n"

(* test ที่ต้อง parse ไม่ผ่าน *)
let run_invalid filename =
  let path = Filename.concat invalid_dir filename in
  Printf.printf "========================================\n";
  Printf.printf "Testing (expect error): %s\n" path;
  (match safe_parse path with
   | None ->
       Printf.printf "RESULT: PASS (rejected as expected)\n";
       write_out "invalid" filename "RESULT: PASS (rejected as expected)\n";
       incr passed
   | Some _ ->
       Printf.printf "RESULT: FAIL (should have been rejected)\n";
       write_out "invalid" filename "RESULT: FAIL (should have been rejected)\n";
       record_fail path "should be rejected");
  Printf.printf "Done.\n\n"

let () =
  let valid = list_cpe_files valid_dir in
  let invalid = list_cpe_files invalid_dir in
  Printf.printf "Found %d valid and %d invalid parser test(s).\n\n"
    (List.length valid) (List.length invalid);
  reset_out ();
  List.iter run_valid valid;
  List.iter run_invalid invalid;
  Printf.printf "========================================\n";
  Printf.printf "All parser tests completed: %d passed, %d failed.\n"
    !passed (List.length !failures);
  List.iter (fun f -> Printf.printf "  FAILED: %s\n" f) (List.rev !failures);
  (* สรุปรวมลงไฟล์ด้วย *)
  (try
     write_file (Filename.concat out_root "summary.txt")
       (Printf.sprintf "%d passed, %d failed\n%s" !passed (List.length !failures)
          (String.concat "" (List.map (fun f -> "FAILED: " ^ f ^ "\n") (List.rev !failures))))
   with Sys_error _ -> ());
  if !failures <> [] then exit 1