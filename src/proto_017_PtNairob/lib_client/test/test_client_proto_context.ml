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

(* Testing
   -------
   Component:    Client
   Invocation:   dune exec src/proto_017_PtNairob/lib_client/test/main.exe \
                  -- --file test_client_proto_context.ml
   Subject:      Tests roundtrips of batch_transfer_operation_encoding
*)

open Protocol
open Alpha_context
open Qcheck2_helpers

let binary_roundtrip ?pp encoding t =
  let b = Data_encoding.Binary.to_bytes_exn encoding t in
  let actual = Data_encoding.Binary.of_bytes_exn encoding b in
  qcheck_eq' ?pp ~expected:t ~actual ()

let gen_batch_transfer_operation_encoding =
  let open QCheck2.Gen in
  let gen_z = sized @@ fun n -> map Z.of_bits (string_size (return n)) in
  let gen_gas_arith_integral =
    map Gas.Arith.integral_of_int_exn (int_range 0 (Int.div Int.max_int 1000))
  in
  let* destination = string in
  let* fee = opt string in
  let* gas_limit = opt gen_gas_arith_integral in
  let* storage_limit = opt gen_z in
  let* amount = string in
  let* arg = opt string in
  let* entrypoint = opt (string_size (1 -- 31)) in
  let entrypoint = Option.map Entrypoint.of_string_strict_exn entrypoint in
  return
    Client_proto_context.
      {destination; fee; gas_limit; storage_limit; amount; arg; entrypoint}

let tests =
  [
    QCheck2.Test.make
      ~name:"test_batch_transfer_operation_encoding_roundtrip"
      gen_batch_transfer_operation_encoding
      (binary_roundtrip Client_proto_context.batch_transfer_operation_encoding);
  ]

let () = Alcotest.run ~__FILE__ Protocol.name [("Encodings", qcheck_wrap tests)]
