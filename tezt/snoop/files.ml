(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(**
   File hierarchy:

   _snoop
   ├── benchmark_results
   │  ├── KAbs_int_alpha_raw.csv
   │  ├── KAbs_int_alpha.workload
   │  ├── ...
   │  └── TYPECHECKING_CODE_alpha.workload
   ├── bench_config.json
   ├── michelson_data
   │  ├── code.mich
   │  └── data.mich
   └── sapling_data
       ├── tx_0.sapling
       ├── ...
       └── tx_n.sapling
*)

let working_dir = "_snoop"

let data_generation_cfg_file = "data_generation.json"

let sapling_data_dir = "sapling_data"

let sapling_txs_file n = sf "tx_%d.sapling" n

let michelson_data_dir = "michelson_data"

let michelson_code_file = "code.mich"

let michelson_data_file = "data.mich"

let benchmark_results_dir = "benchmark_results"

let workload name = sf "%s.workload" name

let csv name = sf "%s_raw.csv" name

let inference_results_dir = "inference_results"

let codegen_results_dir = "generated_code"

let solution_csv = "inferred.csv"

let solution_bin = "inferred.sol"

let report_tex = "report.tex"

let dep_graph = "graph.dot"
(* ------------------------------------------------------------------------- *)
(* Helpers *)

type dirclass = Does_not_exist | Exists_and_is_not_a_dir | Exists

let classify_dirname dir =
  match Unix.stat dir with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Does_not_exist
  | {Unix.st_kind; _} ->
      if st_kind = Unix.S_DIR then Exists else Exists_and_is_not_a_dir

let create_dir dir =
  match classify_dirname dir with
  | Does_not_exist -> Lwt_unix.mkdir dir 0o700
  | Exists -> Lwt.return_unit
  | Exists_and_is_not_a_dir ->
      Test.fail "Can't create directory: file %s exists, aborting" dir

let copy =
  let buffer_size = 8192 in
  let buffer = Bytes.create buffer_size in
  fun input_name output_name ->
    let open Lwt_unix in
    let* fd_in = openfile input_name [O_RDONLY] 0 in
    let* fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o660 in
    let rec copy_loop () =
      let* nread = read fd_in buffer 0 buffer_size in
      match nread with
      | 0 -> return ()
      | r ->
          let* _nwritten = write fd_out buffer 0 r in
          copy_loop ()
    in
    let* () = copy_loop () in
    let* () = close fd_in in
    close fd_out

let fold_dir f dirname =
  let open Unix in
  let d = opendir dirname in
  let rec loop acc =
    match readdir d with
    | entry -> loop (f entry acc)
    | exception End_of_file ->
        closedir d ;
        List.rev acc
  in
  loop []

let is_directory_nonempty dir =
  let* st = Lwt_unix.stat dir in
  match st.Unix.st_kind with
  | S_DIR -> (
      let entries = fold_dir (fun x acc -> x :: acc) dir in
      let entries =
        List.filter
          (fun entry ->
            entry <> Filename.current_dir_name
            && entry <> Filename.parent_dir_name)
          entries
      in
      match entries with [] -> return false | _ -> return true)
  | S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK ->
      Test.fail "Expected %s to be a directory" dir

let read_json name =
  let ic = Stdlib.open_in name in
  let json = Ezjsonm.from_channel ic in
  Stdlib.close_in ic ;
  json

let write_json json file =
  Base.with_open_out file (fun oc ->
      Ezjsonm.value_to_channel oc json ;
      flush oc)

let unlink_if_present file =
  match Unix.stat file with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | {Unix.st_kind; _} -> (
      match st_kind with
      | Unix.S_REG -> (
          Log.info "Removing existing %s" file ;
          try Unix.unlink file with Unix.Unix_error (Unix.ENOENT, _, _) -> ())
      | _ -> Test.fail "%s is not a regular file" file)

let rec cleanup dir =
  match classify_dirname dir with
  | Does_not_exist -> create_dir dir
  | Exists_and_is_not_a_dir ->
      unlink_if_present dir ;
      cleanup dir
  | Exists ->
      let _ =
        Sys.readdir dir
        |> Array.iter (fun x -> Sys.remove (Filename.concat dir x))
      in
      Sys.rmdir dir ;
      cleanup dir
