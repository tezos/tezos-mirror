(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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
    Component:  Protocol (delegate_consensus_key)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] Delegate_consensus_key.ml"
    Subject:    Functions from the module `Delegate_consensus_key`
*)

open Protocol

let create () =
  let open Lwt_result_syntax in
  let accounts = Account.generate_accounts 1 in
  let account =
    match accounts with [(account, _, _)] -> account | _ -> assert false
  in
  let* ctxt = Block.alpha_context accounts in
  return (Alpha_context.Internal_for_tests.to_raw ctxt, account)

module Consensus_key = struct
  let active_key ctxt pkh =
    Delegate_consensus_key.active_key ctxt pkh >|= Environment.wrap_tzresult

  let active_pubkey ctxt pkh =
    Delegate_consensus_key.active_pubkey ctxt pkh >|= Environment.wrap_tzresult

  let active_pubkey_for_cycle ctxt pkh cycle =
    Delegate_consensus_key.active_pubkey_for_cycle
      ctxt
      pkh
      (Cycle_repr.of_int32_exn (Int32.of_int cycle))
    >|= Environment.wrap_tzresult

  let pending_updates ctxt pkh =
    Delegate_consensus_key.pending_updates ctxt pkh
    >|= Environment.wrap_tzresult

  let register_update ctxt pkh pk =
    Delegate_consensus_key.register_update ctxt pkh pk
    >|= Environment.wrap_tzresult

  let activate ctxt ~new_cycle =
    Delegate_consensus_key.activate
      ctxt
      ~new_cycle:(Cycle_repr.of_int32_exn (Int32.of_int new_cycle))
    >|= Environment.wrap_tzresult
end

module Assert = struct
  include Assert

  let equal_pkh ~__LOC__ a b =
    Assert.equal
      ~loc:__LOC__
      Signature.Public_key_hash.equal
      "pkh"
      Signature.Public_key_hash.pp
      a
      b

  let equal_pk ~__LOC__ a b =
    Assert.equal
      ~loc:__LOC__
      Signature.Public_key.equal
      "pk"
      Signature.Public_key.pp
      a
      b

  let active_keys ~__LOC__ ctxt delegate l =
    List.iter_es
      (fun (c, pk) ->
        let open Lwt_result_syntax in
        let* active_pk =
          Consensus_key.active_pubkey_for_cycle ctxt delegate c
        in
        equal_pk ~__LOC__ active_pk pk)
      l
end

let test_consensus_key_storage () =
  let open Lwt_result_syntax in
  let* ctxt, delegate = create () in
  let a1 = Account.new_account () in
  let a2 = Account.new_account () in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt delegate.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh delegate.pkh
  in
  let* () =
    let* active_pk = Consensus_key.active_pubkey ctxt delegate.pkh in
    Assert.equal_pk ~__LOC__ active_pk delegate.pk
  in
  let* () =
    let* active_pk =
      Consensus_key.active_pubkey_for_cycle ctxt delegate.pkh 5
    in
    Assert.equal_pk ~__LOC__ active_pk delegate.pk
  in
  let* ctxt = Consensus_key.register_update ctxt delegate.pkh a1.pk in
  let* ctxt = Consensus_key.register_update ctxt delegate.pkh a2.pk in
  let* () =
    Assert.active_keys
      ~__LOC__
      ctxt
      delegate.pkh
      [
        (0, delegate.pk);
        (1, delegate.pk);
        (2, a1.pk);
        (3, a1.pk);
        (4, a1.pk);
        (5, a2.pk);
        (6, a2.pk);
      ]
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt delegate.pkh a1.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 2l
        | _ -> false)
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt delegate.pkh a1.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 2l
        | _ -> false)
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt delegate.pkh a1.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 2l
        | _ -> false)
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt delegate.pkh a2.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 5l
        | _ -> false)
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt delegate.pkh a2.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 5l
        | _ -> false)
  in
  let* ctxt = Consensus_key.activate ctxt ~new_cycle:1 in
  let* ctxt = Consensus_key.activate ctxt ~new_cycle:2 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt delegate.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh a1.pkh
  in
  let* ctxt = Consensus_key.activate ctxt ~new_cycle:3 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt delegate.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh a1.pkh
  in
  let* ctxt = Consensus_key.activate ctxt ~new_cycle:4 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt delegate.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh a1.pkh
  in
  let* ctxt = Consensus_key.activate ctxt ~new_cycle:5 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt delegate.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh a2.pkh
  in
  let* ctxt = Consensus_key.activate ctxt ~new_cycle:6 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt delegate.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh a2.pkh
  in
  return ()

let tests =
  [Tztest.tztest "consensus_key_storage" `Quick test_consensus_key_storage]
