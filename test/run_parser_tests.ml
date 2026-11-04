open CpecmuCompiler

(* ---------- parse แต่ละชนิด ---------- *)
let parse kind s =
  let lb = Lexing.from_string s in
  match kind with
  | "expr" -> Ast.string_of_expr (Parser.expr_entry Lexer.read lb)
  | "type" -> Ast.string_of_typeexpr (Parser.type_entry Lexer.read lb)
  | "param" -> Ast.string_of_param (Parser.param_entry Lexer.read lb)
  | k -> failwith ("unknown kind: " ^ k)

let run kind s =
  try parse kind s with
  | Parser.Error -> "ERROR"
  | Lexer.SyntaxError _ -> "ERROR"

(* ---------- helper ---------- *)
let split_on sep s =
  let n = String.length sep and m = String.length s in
  let rec go i =
    if i + n > m then None
    else if String.sub s i n = sep then
      Some (String.sub s 0 i, String.sub s (i + n) (m - i - n))
    else go (i + 1)
  in
  go 0

let read_lines path =
  let ic = open_in path in
  let rec go acc =
    match input_line ic with
    | l -> go (l :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  go []

(* ---------- รันหนึ่งไฟล์ คืน (passed, failed) ---------- *)
let run_file path =
  let pass = ref 0 and fail = ref 0 in
  List.iteri
    (fun idx raw ->
      let line = String.trim raw in
      if line <> "" && line.[0] <> '#' then begin
        (* แยก [name] ออกจากต้นบรรทัด *)
        let name, line =
          if line.[0] = '[' then
            match String.index_opt line ']' with
            | Some j ->
                ( String.sub line 1 (j - 1),
                  String.trim (String.sub line (j + 1) (String.length line - j - 1)) )
            | None -> ("", line)
          else ("", line)
        in
        match String.index_opt line ':', split_on " ==> " line with
        | Some i, Some (lhs, expected) ->
            let kind = String.sub lhs 0 i in
            let input = String.trim (String.sub lhs (i + 1) (String.length lhs - i - 1)) in
            let expected = String.trim expected in
            let actual = run kind input in
            if actual = expected then incr pass
            else begin
              incr fail;
              Printf.printf "FAIL %s:%d [%s] %s: %s\n  expected: %s\n  actual:   %s\n"
                path (idx + 1) name kind input expected actual
            end
        | _ ->
            incr fail;
            Printf.printf "BAD LINE %s:%d: %s\n" path (idx + 1) raw
      end)
    (read_lines path);
  (!pass, !fail)

(* ---------- main ---------- *)
let () =
  let target =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "tests/02-parsing/ss"
  in
  let files =
    if Sys.is_directory target then
      Sys.readdir target |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".cpe")
      |> List.sort compare
      |> List.map (Filename.concat target)
    else [ target ]
  in
  let total_pass = ref 0 and total_fail = ref 0 in
  List.iter
    (fun path ->
      let p, f = run_file path in
      Printf.printf "%-40s %d passed, %d failed\n" path p f;
      total_pass := !total_pass + p;
      total_fail := !total_fail + f)
    files;
  Printf.printf "\nTOTAL: %d files, %d passed, %d failed\n"
    (List.length files) !total_pass !total_fail;
  if !total_fail > 0 then exit 1