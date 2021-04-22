#!ocaml

(* This script updates .gitlab/ci/tezt.yml using the --suggest-jobs option of Tezt.
   To use it, first run all Tezt tests with the --record option:

       dune exec tezt/tests/main.exe -- --record <FILENAME>

   (replace <FILENAME> with, for instance, "tezt-record").
   Then, run Tezt with --suggest-jobs and feed the result to this script:

       dune exec tezt/tests/main.exe -- --suggest-jobs <FILENAME> --job-count 3 \
           | scripts/update_tezt_test.ml

   Don't forget to "git add .gitlab/ci/tezt.yml". *)

let target = ".gitlab/ci/tezt.yml"
let part = target ^ ".part"

let () =
  (* Open target file and temporary output file. *)
  let input = open_in target in
  Fun.protect ~finally: (fun () -> close_in input) @@ fun () ->
  let output = open_out part in
  Fun.protect
    ~finally: (fun () -> close_out output; if Sys.file_exists part then Sys.remove part)
  @@ fun () ->

  (* Copy until BEGIN_TEZTTEST. *)
  let rec copy_begin () =
    let line =
      try
        input_line input
      with End_of_file ->
        prerr_endline "Not found: line equal to ##BEGIN_TEZTTEST##";
        raise Exit
    in
    output_string output line;
    output_char output '\n';
    if line <> "##BEGIN_TEZTTEST##" then copy_begin ()
  in
  copy_begin ();

  (* Ignore until END_TEZTTEST. *)
  let rec ignore_middle () =
    let line =
      try
        input_line input
      with End_of_file ->
        prerr_endline "Not found: line equal to ##END_TEZTTEST##";
        raise Exit
    in
    if line <> "##END_TEZTTEST##" then ignore_middle ()
  in
  ignore_middle ();

  (* Generate jobs, reading each job parameters on standard input. *)
  let rec generate_jobs index =
    match read_line () with
      | exception End_of_file ->
          ()
      | line ->
          if index > 1 then output_char output '\n';
          output_string output ("tezt:" ^ string_of_int index ^ ":");
          output_string output {|
  extends: .tezt_template
  script:
    - 'dune exec tezt/tests/main.exe -- --color --log-buffer-size 5000 --log-file tezt.log --global-timeout 3300 --time |};
          let double_single_quotes s =
            String.split_on_char '\'' s |> String.concat "''"
          in
          output_string output (double_single_quotes line);
          output_string output "'\n";
          generate_jobs (index + 1)
  in
  generate_jobs 1;
  output_string output "##END_TEZTTEST##\n";

  (* Copy until end of file. *)
  let rec copy_end () =
    match input_line input with
      | exception End_of_file ->
          ()
      | line ->
          output_string output line;
          output_char output '\n';
          copy_end ()
  in
  copy_end ();

  (* Everything went well, replace target. *)
  Sys.rename part target
