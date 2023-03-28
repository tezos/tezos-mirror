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
    Invocation: dune exec src/proto_016_PtMumbai/lib_protocol/test/unit/main.exe
    Subject:    Functions from the module `Delegate_consensus_key`
*)

open Protocol

let create () =
  let open Lwt_result_syntax in
  let*? accounts = Account.generate_accounts 2 in
  let a1, a2 = match accounts with [a1; a2] -> (a1, a2) | _ -> assert false in
  let* ctxt = Block.alpha_context (Account.make_bootstrap_accounts accounts) in
  return (Alpha_context.Internal_for_tests.to_raw ctxt, a1, a2)

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
    Delegate_consensus_key.activate ctxt ~new_cycle
    >|= Environment.wrap_tzresult
end

module Assert = struct
  include Assert

  let equal_pkh ~__LOC__ a b =
    Assert.equal
      ~loc:__LOC__
      Tezos_crypto.Signature.Public_key_hash.equal
      "pkh"
      Tezos_crypto.Signature.Public_key_hash.pp
      a
      b

  let equal_pk ~__LOC__ a b =
    Assert.equal
      ~loc:__LOC__
      Tezos_crypto.Signature.Public_key.equal
      "pk"
      Tezos_crypto.Signature.Public_key.pp
      a
      b

  let active_keys ~__LOC__ ctxt delegate l =
    List.iter_es
      (fun (c, pk) ->
        let open Lwt_result_syntax in
        let* active_pk =
          Consensus_key.active_pubkey_for_cycle ctxt delegate c
        in
        equal_pk ~__LOC__ active_pk.consensus_pk pk)
      l
end

let rec add_cycles ctxt n =
  if n <= 0 then return ctxt
  else
    let open Lwt_result_syntax in
    let current_level = Raw_context.current_level ctxt in
    let new_cycle = Cycle_repr.succ current_level.cycle in
    let* ctxt = Consensus_key.activate ctxt ~new_cycle in
    let ctxt = Raw_context.Internal_for_tests.add_cycles ctxt 1 in
    add_cycles ctxt (n - 1)

let test_consensus_key_storage () =
  let open Lwt_result_syntax in
  let* ctxt, del1, del2 = create () in
  let a1 = Account.new_account () in
  let a2 = Account.new_account () in
  let preserved_cycles = Constants_storage.preserved_cycles ctxt in
  let* () = Assert.equal_int ~loc:__LOC__ preserved_cycles 3 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt del1.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh.consensus_pkh del1.pkh
  in
  let* () =
    let* active_pk = Consensus_key.active_pubkey ctxt del1.pkh in
    Assert.equal_pk ~__LOC__ active_pk.consensus_pk del1.pk
  in
  let* () =
    let* active_pk = Consensus_key.active_pubkey_for_cycle ctxt del1.pkh 3 in
    Assert.equal_pk ~__LOC__ active_pk.consensus_pk del1.pk
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt del1.pkh del2.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_active -> true
        | _ -> false)
  in
  let* ctxt = Consensus_key.register_update ctxt del1.pkh a1.pk in
  let* () =
    let*! err = Consensus_key.register_update ctxt del1.pkh a1.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 4l
        | _ -> false)
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt del2.pkh a1.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_active -> true
        | _ -> false)
  in
  let* ctxt = Consensus_key.register_update ctxt del2.pkh del1.pk in
  let* () =
    Assert.active_keys
      ~__LOC__
      ctxt
      del1.pkh
      [
        (0, del1.pk);
        (1, del1.pk);
        (2, del1.pk);
        (2, del1.pk);
        (3, del1.pk);
        (4, a1.pk);
        (5, a1.pk);
      ]
  in
  let* ctxt = add_cycles ctxt 1 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt del1.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh.consensus_pkh del1.pkh
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt del1.pkh a1.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 4l
        | _ -> false)
  in
  let* ctxt = Consensus_key.register_update ctxt del1.pkh a2.pk in
  let* ctxt = Consensus_key.register_update ctxt del2.pkh a1.pk in
  let* ctxt = Consensus_key.register_update ctxt del2.pkh del2.pk in
  let* () =
    Assert.active_keys
      ~__LOC__
      ctxt
      del1.pkh
      [
        (1, del1.pk);
        (2, del1.pk);
        (2, del1.pk);
        (3, del1.pk);
        (4, a1.pk);
        (5, a2.pk);
        (6, a2.pk);
      ]
  in
  let* ctxt = add_cycles ctxt 2 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt del1.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh.consensus_pkh del1.pkh
  in
  let* () =
    let*! err = Consensus_key.register_update ctxt del1.pkh a2.pk in
    Assert.proto_error ~loc:__LOC__ err (function
        | Delegate_consensus_key.Invalid_consensus_key_update_noop c ->
            c = Cycle_repr.of_int32_exn 5l
        | _ -> false)
  in
  let* ctxt = Consensus_key.register_update ctxt del1.pkh a1.pk in
  let* () =
    Assert.active_keys
      ~__LOC__
      ctxt
      del1.pkh
      [(3, del1.pk); (4, a1.pk); (5, a2.pk); (6, a2.pk); (7, a1.pk); (8, a1.pk)]
  in
  let* ctxt = add_cycles ctxt 1 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt del1.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh.consensus_pkh a1.pkh
  in
  let* ctxt = add_cycles ctxt 1 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt del1.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh.consensus_pkh a2.pkh
  in
  let* ctxt = add_cycles ctxt 1 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt del1.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh.consensus_pkh a2.pkh
  in
  let* ctxt = add_cycles ctxt 1 in
  let* () =
    let* active_pkh = Consensus_key.active_key ctxt del1.pkh in
    Assert.equal_pkh ~__LOC__ active_pkh.consensus_pkh a1.pkh
  in
  return ()

let tests =
  [Tztest.tztest "consensus_key_storage" `Quick test_consensus_key_storage]
