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

(** Testing
    -------
    Component:  Lib_dac Certificate_repr
    Invocation: dune exec src/lib_dac/test/main.exe -- --file test_certificate.ml
    Subject:    Tests for the Certificate_repr.
*)

let assert_equal_certificate (c1 : Certificate_repr.t) (c2 : Certificate_repr.t)
    =
  Certificate_repr.get_witnesses c1 = Certificate_repr.get_witnesses c2
  && Certificate_repr.get_aggregate_signature c1
     = Certificate_repr.get_aggregate_signature c2
  && Certificate_repr.get_root_hash c1 = Certificate_repr.get_root_hash c2
  && Certificate_repr.get_version c1 = Certificate_repr.get_version c2

let raw_hash_of_hex hex = Hex.to_bytes (`Hex hex)

let create_certificate root_hash signature witnesses =
  let root_hash = raw_hash_of_hex root_hash in
  let signature = Tezos_crypto.Aggregate_signature.of_b58check_opt signature in
  let witnesses = Z.of_int witnesses in
  match (root_hash, signature, witnesses) with
  | Some root_hash, Some signature, witnesses ->
      Certificate_repr.(
        V0
          (V0.make (Dac_plugin.raw_hash_of_bytes root_hash) signature witnesses))
  | _ -> assert false

(** When creating a [Certificate.V0.t] with [make] function
    Version field in created certificate must be equal to 0. *)
let test_version_is_0 () =
  let open Lwt_result_syntax in
  let certificate =
    create_certificate
      "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4"
      "asigHmGKUsRdN53gbjZoZ36akZEhJn6Cp2dsDzbypqMeaAxoHebs8xLkUpi4xtWpaLaxRkXSsyUeN7ZJsQ8no7B2nP75Kd9D5XKLQmfTE3qRYomyaAX9rRjSMk9hFEK4HJRnizgHA36zc"
      1
  in
  return @@ assert (Certificate_repr.get_version certificate = 0)

(** Encode to binary a certificate fails. *)
let binary_encode () =
  let open Lwt_result_syntax in
  let certificate =
    create_certificate
      "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4"
      "asigHmGKUsRdN53gbjZoZ36akZEhJn6Cp2dsDzbypqMeaAxoHebs8xLkUpi4xtWpaLaxRkXSsyUeN7ZJsQ8no7B2nP75Kd9D5XKLQmfTE3qRYomyaAX9rRjSMk9hFEK4HJRnizgHA36zc"
      1
  in
  let encoded_certificate =
    Data_encoding.Binary.to_bytes Certificate_repr.encoding certificate
  in

  match encoded_certificate with
  | Ok _ -> return @@ assert false
  | Error _ -> return @@ assert true

let json_encode_decode () =
  let open Lwt_result_syntax in
  let certificate =
    create_certificate
      "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4"
      "asigHmGKUsRdN53gbjZoZ36akZEhJn6Cp2dsDzbypqMeaAxoHebs8xLkUpi4xtWpaLaxRkXSsyUeN7ZJsQ8no7B2nP75Kd9D5XKLQmfTE3qRYomyaAX9rRjSMk9hFEK4HJRnizgHA36zc"
      1
  in
  let encoded_certificate =
    Data_encoding.Json.construct Certificate_repr.encoding certificate
  in
  let decoded_certificate =
    Data_encoding.Json.destruct Certificate_repr.encoding encoded_certificate
  in
  return @@ assert (assert_equal_certificate certificate decoded_certificate)

let json_decode () =
  let open Lwt_result_syntax in
  let certificate =
    create_certificate
      "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4"
      "asigHmGKUsRdN53gbjZoZ36akZEhJn6Cp2dsDzbypqMeaAxoHebs8xLkUpi4xtWpaLaxRkXSsyUeN7ZJsQ8no7B2nP75Kd9D5XKLQmfTE3qRYomyaAX9rRjSMk9hFEK4HJRnizgHA36zc"
      1
  in
  let encoded_certificate =
    Data_encoding.Json.construct Certificate_repr.encoding certificate
  in
  let expected_json =
    Data_encoding.Json.from_string
      {|
{
  "version": 0,
  "root_hash": "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4",
  "aggregate_signature": "asigHmGKUsRdN53gbjZoZ36akZEhJn6Cp2dsDzbypqMeaAxoHebs8xLkUpi4xtWpaLaxRkXSsyUeN7ZJsQ8no7B2nP75Kd9D5XKLQmfTE3qRYomyaAX9rRjSMk9hFEK4HJRnizgHA36zc",
  "witnesses": "1"
}
     |}
  in
  match expected_json with
  | Ok expected_json -> return @@ assert (encoded_certificate = expected_json)
  | _ -> assert false

let json_encode () =
  let open Lwt_result_syntax in
  let certificate =
    create_certificate
      "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4"
      "asigHmGKUsRdN53gbjZoZ36akZEhJn6Cp2dsDzbypqMeaAxoHebs8xLkUpi4xtWpaLaxRkXSsyUeN7ZJsQ8no7B2nP75Kd9D5XKLQmfTE3qRYomyaAX9rRjSMk9hFEK4HJRnizgHA36zc"
      1
  in
  let json =
    Data_encoding.Json.from_string
      {|
{
  "version": 0,
  "root_hash": "00dce42551fb786c51b29b723f4abba3ea04eb3d239a9a59ec5a5434e151e105e4",
  "aggregate_signature": "asigHmGKUsRdN53gbjZoZ36akZEhJn6Cp2dsDzbypqMeaAxoHebs8xLkUpi4xtWpaLaxRkXSsyUeN7ZJsQ8no7B2nP75Kd9D5XKLQmfTE3qRYomyaAX9rRjSMk9hFEK4HJRnizgHA36zc",
  "witnesses": "1"
}
     |}
  in
  match json with
  | Ok expected_json ->
      let decoded_certificate =
        Data_encoding.Json.destruct Certificate_repr.encoding expected_json
      in
      return @@ assert (certificate = decoded_certificate)
  | _ -> assert false

let tests =
  [
    Tztest.tztest
      "Verify Certificate_repr.version is equal to 0"
      `Quick
      test_version_is_0;
    Tztest.tztest "Simple Binary encode" `Quick binary_encode;
    Tztest.tztest "Simple JSON encode decode" `Quick json_encode_decode;
    Tztest.tztest "Simple JSON decode" `Quick json_decode;
    Tztest.tztest "Simple JSON encode" `Quick json_encode;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "lib_dac" [("Certificate_repr.ml", tests)]
  |> Lwt_main.run
