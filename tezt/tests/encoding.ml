(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
   Component: encoding tests
   Invocation: dune exec tezt/tests/main.exe encoding

               To only run the regression tests:
               [dune exec tezt/tests/main.exe encoding regression]

               Note that to reset the regression outputs, one can use
               the [--reset-regressions] option. When doing so, it is
               recommended to clear the output directory
               [tezt/_regressions/encoding] first to remove unused files in case
               some paths change.
   Subject: Encoding regression tests capture the output of encoding/decoding
            using the [octez-codec] and compare it with the output from the
            previous run. The test passes only if the outputs match exactly.

            The other test checks that the [octez-codec] can successfully dump
            the list of encodings.
*)

let sample_as_tags sample = String.split_on_char '.' sample

let check_dump_encodings () =
  Test.register
    ~__FILE__
    ~title:"octez-codec dump encodings"
    ~tags:["dump"]
    ~uses:[Constant.octez_codec]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let* (_ : JSON.t) = Codec.dump_encodings () in
  unit

let test_michelson_primitives_encoding =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Michelson primitives regression"
    ~tags:["michelson"; "primitives"; "encoding"]
    ~uses:(fun _ -> [Constant.octez_codec])
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun protocol ->
  let encoding_id = sf "%s.script.prim" (Protocol.encoding_prefix protocol) in
  let* encoding_dump = Codec.dump_encoding ~id:encoding_id () in
  let prim_list =
    JSON.(
      encoding_dump |-> "binary" |-> "fields" |=> 0 |-> "encoding" |-> "cases"
      |> as_list)
  in
  let () =
    List.iter
      (fun json ->
        match JSON.as_list json with
        | [tag; prim] ->
            let tag = JSON.as_int tag in
            let prim = JSON.as_string prim in
            Regression.capture (sf "%d: %s" tag prim)
        | _ -> Test.fail "expected a list of length 2")
      prim_list
  in
  unit

let check_sample ~name ~file =
  let* json_string = Tezos_stdlib_unix.Lwt_utils_unix.read_file file in
  let original_json = JSON.parse ~origin:json_string json_string in
  let* binary = Codec.encode ~name (JSON.unannotate original_json) in
  let () =
    Regression.capture
      (sf
         "%s: %s"
         Filename.(file |> basename |> remove_extension |> remove_extension)
         binary)
  in
  let* decoded_json = Codec.decode ~name binary in
  Check.(
    (original_json = decoded_json)
      json
      ~__LOC__
      ~error_msg:
        "The converted JSON doesn't match the original.\n\
         Expected: %L\n\
         Actual: %R") ;
  return ()

let iter_sample_s base_path func =
  Sys.readdir base_path |> Array.to_list |> List.sort String.compare
  |> Lwt_list.iter_s (fun file -> func (base_path // file))

(** The given sample must be included in registered encodings. These can be
    found with [octez-codec list encodings]. *)
let check_protocol_sample_encoding ?title ?supports sample =
  let title, tags =
    match title with
    | None -> (sample, sample_as_tags sample)
    | Some title -> (title, sample_as_tags title)
  in
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "protocol encoding regression test: %s" title)
    ~tags:(["encoding"; "protocol"] @ tags)
    ~uses:(fun _protocol -> [Constant.octez_codec])
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ?supports
  @@ fun protocol ->
  let base_path =
    "tezt" // "tests" // "encoding_samples" // Protocol.tag protocol // sample
  in
  iter_sample_s base_path @@ fun file ->
  check_sample ~name:(Protocol.encoding_prefix protocol ^ "." ^ sample) ~file

(** The given sample must be included in registered encodings. These can be
    found with [octez-codec list encodings]. *)
let check_shell_sample_encoding sample =
  Regression.register
    ~__FILE__
    ~title:(sf "shell encoding regression test: %s" sample)
    ~tags:(["encoding"; "shell"] @ sample_as_tags sample)
    ~uses:[Constant.octez_codec]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
  @@ fun () ->
  let base_path =
    "tezt" // "tests" // "encoding_samples" // "shell" // sample
  in
  iter_sample_s base_path @@ fun file -> check_sample ~name:sample ~file

let check_samples protocols =
  let protocol_sample ?title ?supports name =
    check_protocol_sample_encoding ?title ?supports name protocols
  in
  check_shell_sample_encoding "network_version" ;
  protocol_sample "block_header" ;
  protocol_sample "block_header.raw" ;
  protocol_sample "block_header.unsigned" ;
  protocol_sample "contract" ;
  protocol_sample "cycle" ;
  protocol_sample "fitness" ;
  protocol_sample "gas.cost" ;
  protocol_sample "gas" ;
  protocol_sample "lazy_storage_diff" ;
  protocol_sample "level" ;
  protocol_sample "nonce" ;
  protocol_sample "operation.internal" ;
  protocol_sample "operation" ;
  protocol_sample "operation.raw" ;
  protocol_sample "operation.unsigned" ;
  protocol_sample ~supports:(From_protocol 023) "operation.bls_mode_unsigned" ;
  protocol_sample
    ~supports:(From_protocol 023)
    "operation.internal_and_metadata" ;
  protocol_sample ~supports:(From_protocol 023) "operation.data_and_metadata" ;
  protocol_sample "period" ;
  protocol_sample "raw_level" ;
  protocol_sample "seed" ;
  protocol_sample "tez" ;
  protocol_sample "timestamp" ;
  protocol_sample "vote.ballot" ;
  protocol_sample "vote.ballots" ;
  protocol_sample "vote.listings" ;
  protocol_sample "voting_period.kind" ;
  protocol_sample "voting_period" ;
  ()

let register ~protocols =
  check_dump_encodings () ;
  check_samples protocols ;
  test_michelson_primitives_encoding protocols
