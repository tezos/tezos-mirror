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

open Data_encoding

let rec equal_json (a : json) (b : json) =
  match (a, b) with
  | (`O object_a, `O object_b) ->
      let sort_object =
        List.sort (fun (key_a, _) (key_b, _) -> compare key_a key_b)
      in
      List.length object_a = List.length object_b
      && List.for_all2
           (fun (key_a, val_a) (key_b, val_b) ->
             key_a = key_b && equal_json val_a val_b)
           (sort_object object_a)
           (sort_object object_b)
  | (`Bool bool_a, `Bool bool_b) ->
      bool_a = bool_b
  | (`Float float_a, `Float float_b) ->
      Float.equal float_a float_b
  | (`A array_a, `A array_b) ->
      List.for_all2 equal_json array_a array_b
  | (`Null, `Null) ->
      true
  | (`String string_a, `String string_b) ->
      string_a = string_b
  | _ ->
      false

let check_sample ~name ~file =
  let* json_string = Tezos_stdlib_unix.Lwt_utils_unix.read_file file in
  match Data_encoding.Json.from_string json_string with
  | Error err ->
      Test.fail
        "An error occurred while trying to decode JSON sample.\n\
         Error: %s\n\
         JSON: %s"
        err
        json_string
  | Ok json -> (
      let json_string = Json.to_string json in
      let* binary =
        Process.run_and_read_stdout
          Constant.tezos_codec
          ["encode"; name; "from"; json_string]
      in
      let* converted_json_string =
        Process.run_and_read_stdout
          Constant.tezos_codec
          ["decode"; name; "from"; binary]
      in
      match Data_encoding.Json.from_string converted_json_string with
      | Error err ->
          Test.fail
            "An error occurred while trying to decode the converted JSON \
             string.\n\
             Error: %s\n\
             JSON: %s"
            err
            converted_json_string
      | Ok decoded_json ->
          if not @@ equal_json json decoded_json then
            Test.fail
              "the converted json doesn't match the original.\n\
               Expected: %s\n\
               Actual: %s"
              (Json.to_string json)
              (Json.to_string decoded_json) ;
          return () )

(** The given samples must be included in registered encodings. These can be
    found with [tezos-codec list encodings]. *)
let check_samples_encoding ~group_name ~samples =
  List.iter
    (fun sample ->
      Regression.run
        ~__FILE__
        ~title:(sf "%s encoding regression test: %s" group_name sample)
        ~tags:["encoding"; group_name]
        ~output_file:(sf "%s.%s" group_name sample)
      @@ fun () ->
      let base_path =
        "tezt" // "tests" // "encoding_samples" // group_name // sample
      in
      Sys.readdir base_path |> Array.to_list |> List.sort String.compare
      |> Lwt_list.iter_s (fun file ->
             check_sample ~name:sample ~file:(base_path // file)))
    samples

let run () =
  check_samples_encoding
    ~group_name:"protocol"
    ~samples:
      [ "alpha.block_header";
        "alpha.block_header.raw";
        "alpha.block_header.unsigned";
        "alpha.contract";
        "alpha.cycle";
        "alpha.delegate.balance_updates";
        "alpha.delegate.frozen_balance";
        "alpha.delegate.frozen_balance_by_cycles";
        "alpha.fitness";
        "alpha.gas.cost";
        "alpha.gas";
        "alpha.level";
        "alpha.nonce";
        "alpha.operation.internal";
        "alpha.operation";
        "alpha.operation.raw";
        "alpha.operation.unsigned";
        "alpha.period";
        "alpha.raw_level";
        "alpha.roll";
        "alpha.seed";
        "alpha.tez";
        "alpha.timestamp";
        "alpha.vote.ballot";
        "alpha.vote.ballots";
        "alpha.vote.listings";
        "alpha.voting_period.kind";
        "alpha.voting_period" ]
