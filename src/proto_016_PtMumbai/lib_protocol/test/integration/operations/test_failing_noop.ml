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

(** Testing
    -------
    Component:    Protocol
    Invocation:   dune exec src/proto_016_PtMumbai/lib_protocol/test/integration/operations/main.exe \
                  -- --file test_failing_noop.ml
    Subject:      The Failing_noop operation was added bearing in mind the
                  possibility for the end user to sign arbitrary bytes,
                  encapsulate in the operation, with the absolute garanty that
                  the signed bytes can't be used for something against the
                  user's will. The Failing_noop operation always fails when
                  applied.
 *)

(** try to apply a failing_noop and assert that the operation fails *)
let failing_noop_must_fail_when_injected () =
  Context.init1 () >>=? fun (blk, contract) ->
  let source = Context.Contract.pkh contract in
  Op.failing_noop (B blk) source "tezos" >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Protocol.Validate_errors.Failing_noop_error -> true
      | _ -> false)

let tests =
  [Tztest.tztest "injection fails" `Quick failing_noop_must_fail_when_injected]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("failing_noop operation", tests)]
  |> Lwt_main.run
