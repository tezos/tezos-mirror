(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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
    Component:  Protocol (tx rollup l2)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "tx rollup l2"
    Subject:    test the layer-2 implementation of transaction rollup
*)

open Tztest
open Tx_rollup_l2_helpers

type Environment.Error_monad.error += Test

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2362
   Use the Irmin store provided by [lib_context] for layer-2
   solutions, once available.
   As of now, we define a ad-hoc [STORAGE] implementation to run our
   tests, but eventually we need to actually make use of the same
   implementation as the transaction rollup node and the protocol. *)

(** [test_irmin_storage] checks that the implementation of [STORAGE]
    has the expected properties. *)
let test_irmin_storage () =
  let open Irmin_storage.Syntax in
  let store = empty_storage in

  let k1 = Bytes.of_string "k1" in
  let k2 = Bytes.of_string "k2" in
  let v1 = Bytes.of_string "v1" in
  let v2 = Bytes.of_string "v2" in

  (* 1. get (set store k1 v1) k1 == Some v1 *)
  let* store = Irmin_storage.set store k1 v1 in
  let* v1' = Irmin_storage.get store k1 in
  assert (v1' = Some v1) ;

  (* 2. k1 != k2 -> get (set store k2 v2) k1 = get store k1*)
  let* store = Irmin_storage.set store k2 v2 in
  let* v1'' = Irmin_storage.get store k1 in
  assert (v1' = v1'') ;

  (* 3. catch (fail e) f return == e *)
  let* e = catch (fail Test) (fun _ -> assert false) return in
  assert (e = Test) ;

  return_unit

let wrap_test t () =
  t () >|= function
  | Ok x -> Ok x
  | Error err -> Error [Environment.Ecoproto_error err]

let tests = [tztest "test irmin storage" `Quick @@ wrap_test test_irmin_storage]
