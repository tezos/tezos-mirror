(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Protocol Library
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/pbt/test_sc_rollup_encoding.exe
    Subject:      SC rollup encoding
*)

open Protocol
open QCheck2
open Lib_test.Qcheck2_helpers

(** {2 Generators} *)

let gen_state_hash =
  let open Gen in
  let* bytes = bytes_fixed_gen Sc_rollup_repr.State_hash.size in
  return (Sc_rollup_repr.State_hash.of_bytes_exn bytes)

let gen_raw_level =
  let open Gen in
  let* level = map Int32.abs int32 in
  return (Raw_level_repr.of_int32_exn level)

let gen_commitment_hash =
  let open Gen in
  let* bytes = bytes_fixed_gen Sc_rollup_commitment_repr.Hash.size in
  return (Sc_rollup_commitment_repr.Hash.of_bytes_exn bytes)

let gen_number_of_messages =
  let open Gen in
  let open Sc_rollup_repr.Number_of_messages in
  let* v = int32_range_gen min_int max_int in
  return (WithExceptions.Option.get ~loc:__LOC__ (of_int32 v))

let gen_number_of_ticks =
  let open Gen in
  let open Sc_rollup_repr.Number_of_ticks in
  let* v = int32_range_gen min_int max_int in
  return (WithExceptions.Option.get ~loc:__LOC__ (of_int32 v))

let gen_commitment =
  let open Gen in
  let* compressed_state = gen_state_hash
  and* inbox_level = gen_raw_level
  and* predecessor = gen_commitment_hash
  and* number_of_messages = gen_number_of_messages
  and* number_of_ticks = gen_number_of_ticks in
  return
    Sc_rollup_commitment_repr.
      {
        compressed_state;
        inbox_level;
        predecessor;
        number_of_messages;
        number_of_ticks;
      }

let gen_versioned_commitment =
  let open Gen in
  let* commitment = gen_commitment in
  return (Sc_rollup_commitment_repr.to_versioned commitment)

(** {2 Tests} *)

let test_commitment =
  test_roundtrip
    ~count:1_000
    ~title:"Sc_rollup_commitment.t"
    ~gen:gen_commitment
    ~eq:( = )
    Sc_rollup_commitment_repr.encoding

let test_versioned_commitment =
  test_roundtrip
    ~count:1_000
    ~title:"Sc_rollup_commitment.versioned"
    ~gen:gen_versioned_commitment
    ~eq:( = )
    Sc_rollup_commitment_repr.versioned_encoding

let tests = [test_commitment; test_versioned_commitment]

let () = Alcotest.run "SC rollup encoding" [("roundtrip", qcheck_wrap tests)]
