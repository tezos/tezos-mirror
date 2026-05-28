(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Michelson runtime capacity benchmark
   Invocation:   dune exec etherlink/tezt/benchmarks/main.exe -- \
                   --file fa_transfer.ml [opts]
   Subject:      Macro-benchmark for the FA-token [transfer] entrypoint.
                 The ledger is pre-populated at origination time (every
                 lane signer starts with a large balance), so the
                 measured loop only exercises the [transfer] entrypoint —
                 no deposit/mint on the hot path (cf. the deposit phase in
                 [evm_node_capacity.ml]). Each lane transfers from its
                 own signer to a fixed peer in the ring; tracks
                 per-block Michelson gas via the Tezlink operations RPC
                 and reports median/p90/wall capacity in MGas/s.

                 Reuses the shared procedure in [Michelson_capacity]: runs
                 against a single standalone EVM node in sandbox mode with
                 the Tezos (Michelson) runtime enabled. No L1 node, no
                 rollup node, no protocol activation.
*)

open Etherlink_benchmark_lib

(* Per-lane balance minted into the FA contract's [ledger] before the
   measured loop. Picked large enough that even a long run (iterations ×
   lanes × value) never approaches the per-lane balance. *)
let initial_balance = 1_000_000_000_000

(* Amount transferred per call. Kept tiny — the capacity metric measures
   gas, not token movement. *)
let transfer_value = 1

(* Origination storage with the ledger pre-populated: every account in
   [ledger_pkhs] starts with [balance] tokens, so the measured loop can
   [transfer] immediately, with no separate mint phase. Michelson requires
   big_map [Elt] entries in strictly ascending key order; for implicit
   accounts that order is [Public_key_hash.compare] — the very comparison
   the protocol uses on Michelson [address] keys — which the base58 string
   order does not match, hence the explicit sort. *)
let build_initial_storage ~admin ~ledger_pkhs ~balance =
  let module Pkh = Tezos_crypto.Signature.Public_key_hash in
  let sorted =
    List.sort
      (fun a b -> Pkh.compare (Pkh.of_b58check_exn a) (Pkh.of_b58check_exn b))
      ledger_pkhs
  in
  let ledger =
    String.concat
      " ; "
      (List.map (fun pkh -> sf "Elt %S (Pair %d {})" pkh balance) sorted)
  in
  let total_supply = balance * List.length sorted in
  sf "Pair { %s } (Pair %S (Pair False %d))" ledger admin total_supply

let transfer_arg_str ~from_ ~to_ ~value =
  sf "Pair %S (Pair %S %d)" from_ to_ value

(* Each signer transfers from itself to a fixed peer: the next signer in
   the ring. Same destination across iterations; the capacity metric does
   not depend on rotating destinations and a fixed receiver makes the gas
   profile most stable. *)
let transfer_call : Michelson_capacity.call_spec =
  {
    entrypoint = "transfer";
    mk_arg =
      (fun ~index ~signers ->
        let from_ = signers.(index) in
        let to_ = signers.((index + 1) mod Array.length signers) in
        transfer_arg_str
          ~from_:from_.public_key_hash
          ~to_:to_.public_key_hash
          ~value:transfer_value);
  }

let register () =
  Michelson_capacity.register_benchmark
    ~__FILE__
    ~name:"fa_transfer"
    ~title:"Capacity of Michelson runtime (FA transfer)"
    ~extra_tags:["fa"; "fa_transfer"]
    ~code:Contracts.Michelson.fa
    ~mk_init:(fun ~admin ~signers ->
      build_initial_storage
        ~admin:admin.Account.public_key_hash
        ~ledger_pkhs:
          (List.map (fun (s : Account.key) -> s.public_key_hash) signers)
        ~balance:initial_balance)
      (* Pre-populating the ledger burns storage proportional to
         [--accounts]; budget generously so large runs don't hit the cap. *)
    ~burn_cap:(Tez.of_int 1000)
    ~call:transfer_call
    ()
