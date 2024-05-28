(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(* Testing
   -------
   Component:    Snoop
   Invocation:   dune exec tezt/tests/main.exe -- --file snoop_protocol_codegen.ml
   Subject:      Tests for snoop codegen of protocol files.
*)

let generate_code_using_solution_test () =
  Test.register
    ~title:"snoop protocol codegen"
    ~tags:["codegen"; "protocol"]
    ~uses:[Constant.octez_snoop]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
  @@ fun () ->
  let open Lwt.Syntax in
  let snoop = Snoop.create () in
  let temp_dir = Temp.dir "results" in
  let module Set = Set.Make (String) in
  let excluded =
    Set.of_list [Filename.current_dir_name; Filename.parent_dir_name]
  in
  let filter_f suffix stream =
    Lwt_stream.filter
      (fun filename ->
        let is_excluded = Set.mem filename excluded in
        let has_suffix = Filename.check_suffix filename suffix in
        (not is_excluded) && has_suffix)
      stream
  in
  (* cp src/proto_alpha/lib_protocol/*_costs.ml $temp_dir *)
  let protocol_file_stream =
    Lwt_unix.files_of_directory "src/proto_alpha/lib_protocol"
    |> filter_f "costs.ml"
  in
  let* () =
    Lwt_stream.iter_s
      (fun f ->
        Lwt_io.lines_of_file (Filename.concat "src/proto_alpha/lib_protocol" f)
        |> Lwt_io.lines_to_file (Filename.concat temp_dir f))
      protocol_file_stream
  in
  let* _ =
    Snoop.generate_code_for_solutions
      ~solution:"src/proto_alpha/lib_protocol/gas_parameters.json"
      ~fixed_point:"tezt/tests/snoop_protocol_codegen/fp.json"
      ~split_to:temp_dir
      snoop
  in
  let generated_protocol_files =
    Lwt_unix.files_of_directory "src/proto_alpha/lib_protocol"
    |> filter_f "generated.ml"
  in
  let* diff_list =
    generated_protocol_files
    |> Lwt_stream.filter_map (fun file ->
           let existing_file =
             Filename.concat "src/proto_alpha/lib_protocol" file
           in
           let diff =
             Diff.files existing_file (Filename.concat temp_dir file)
           in
           if diff.different then Some diff else None)
    |> Lwt_stream.to_list
  in
  match diff_list with
  | [] -> Lwt.return_unit
  | diff_list ->
      List.iter
        (fun diff -> Diff.reduce_context diff |> Diff.log ~level:Error)
        diff_list ;
      failwith
        "Some of the gas cost functions are not generated from \
         gas_parameters.json"

let register_protocol_independent () = generate_code_using_solution_test ()
