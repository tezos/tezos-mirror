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
            using the [tezos-codec] and compare it with the output from the
            previous run. The test passes only if the outputs match exactly.

            The other test checks that the [tezos-codec] can successfully dump
            the list of encodings.
*)

let check_dump_encodings () =
  Test.register
    ~__FILE__
    ~title:"tezos-codec dump encodings"
    ~tags:["codec"; "dump"]
  @@ fun () ->
  let* (_ : JSON.t) = Codec.dump_encodings () in
  unit

let rec equal_json (a : JSON.u) (b : JSON.u) =
  match (a, b) with
  | (`O object_a, `O object_b) ->
      let sort_object =
        List.sort (fun (key_a, _) (key_b, _) -> compare key_a key_b)
      in
      List.compare_lengths object_a object_b = 0
      && List.for_all2
           (fun (key_a, val_a) (key_b, val_b) ->
             key_a = key_b && equal_json val_a val_b)
           (sort_object object_a)
           (sort_object object_b)
  | (`Bool bool_a, `Bool bool_b) -> bool_a = bool_b
  | (`Float float_a, `Float float_b) -> Float.equal float_a float_b
  | (`A array_a, `A array_b) -> List.for_all2 equal_json array_a array_b
  | (`Null, `Null) -> true
  | (`String string_a, `String string_b) -> string_a = string_b
  | _ -> false

let check_sample ~name ~file =
  let* json_string = Tezos_stdlib_unix.Lwt_utils_unix.read_file file in
  let json = JSON.parse ~origin:json_string json_string in
  let* binary =
    Codec.encode ~hooks:Regression.hooks ~name (JSON.unannotate json)
  in
  let* decoded_json = Codec.decode ~hooks:Regression.hooks ~name binary in
  if not @@ equal_json (JSON.unannotate json) (JSON.unannotate decoded_json)
  then
    Test.fail
      "The converted JSON doesn't match the original.\nExpected: %s\nActual: %s"
      (JSON.encode json)
      (JSON.encode decoded_json) ;
  return ()

(** The given samples must be included in registered encodings. These can be
    found with [tezos-codec list encodings]. *)
let check_samples_encoding ~protocols ~samples =
  List.iter
    (fun sample ->
      Protocol.register_regression_test
        ~__FILE__
        ~title:(sf "encoding regression test: %s" sample)
        ~tags:["encoding"]
        ~output_file:(fun p -> "encoding" // sf "%s.%s" (Protocol.tag p) sample)
        (fun protocol ->
          let base_path =
            "tezt" // "tests" // "encoding_samples" // Protocol.tag protocol
            // sample
          in
          Sys.readdir base_path |> Array.to_list |> List.sort String.compare
          |> Lwt_list.iter_s (fun file ->
                 check_sample
                   ~name:(Protocol.encoding_prefix protocol ^ "." ^ sample)
                   ~file:(base_path // file)))
        protocols)
    samples

let default_samples =
  [
    "block_header";
    "block_header.raw";
    "block_header.unsigned";
    "contract";
    "cycle";
    "delegate.frozen_balance";
    "delegate.frozen_balance_by_cycles";
    "fitness";
    "gas.cost";
    "gas";
    "level";
    "nonce";
    "operation.internal";
    "operation";
    "operation.raw";
    "operation.unsigned";
    "period";
    "raw_level";
    "roll";
    "seed";
    "tez";
    "timestamp";
    "vote.ballot";
    "vote.ballots";
    "vote.listings";
    "voting_period.kind";
    "voting_period";
  ]

let alpha_samples =
  [
    "block_header";
    "block_header.raw";
    "block_header.unsigned";
    "contract";
    "cycle";
    "fitness";
    "gas.cost";
    "gas";
    "level";
    "nonce";
    "operation.internal";
    "operation";
    "operation.raw";
    "operation.unsigned";
    "period";
    "raw_level";
    "seed";
    "tez";
    "timestamp";
    "vote.ballot";
    "vote.ballots";
    "vote.listings";
    "voting_period.kind";
    "voting_period";
  ]

let register ~protocols =
  check_dump_encodings () ;
  List.iter
    (fun protocol ->
      let samples =
        if Protocol.(protocol = Alpha || protocol = Ithaca) then alpha_samples
        else default_samples
      in
      check_samples_encoding ~protocols:[protocol] ~samples)
    protocols
