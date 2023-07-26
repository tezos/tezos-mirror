(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com>            *)
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

(** This module is used to originate contracts for liquidity baking during
    protocol stitching: a CPMM (constant product market making) contract and a
    liquidity token FA1.2 contract, with the storage of each containing the
    other's address.

    The CPMM's storage contains a token address, which corresponds to tzBTC when
    originated on mainnet and a reference FA1.2 contract when originated for
    testing.

    The test FA1.2 contract uses the same script as the liquidity token. Its
    manager is initialized to the first bootstrap account. Before originating it,
    we make sure we are not on mainnet by both checking for the existence of the
    tzBTC contract and that the level is sufficiently low.

    The Michelson and Ligo code, as well as Coq proofs, for the CPMM and
    liquidity token contracts are available here:
    https://gitlab.com/dexter2tz/dexter2tz/-/tree/liquidity_baking

    All contracts were generated from Ligo at revision
    4d10d07ca05abe0f8a5fb97d15267bf5d339d9f4 and converted to OCaml using
    `octez-client convert`.
*)

open Michelson_v1_primitives
open Micheline

let null_address =
  Bytes.of_string
    "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let mainnet_tzBTC_address =
  Contract_hash.of_b58check_exn "KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn"

(** If token_pool, xtz_pool, or lqt_total are ever zero the CPMM will be
    permanently broken. Therefore, we initialize it with the null address
    registered as a liquidity provider with 1 satoshi tzBTC and 100 mutez
    (roughly the current exchange rate).  *)
let cpmm_init_storage ~token_address ~lqt_address =
  Script_repr.lazy_expr
    (Micheline.strip_locations
       (Prim
          ( 0,
            D_Pair,
            [
              Int (1, Z.one);
              Int (2, Z.of_int 100);
              Int (3, Z.of_int 100);
              String (4, token_address);
              String (5, lqt_address);
            ],
            [] )))

let lqt_init_storage cpmm_address =
  Script_repr.lazy_expr
    (Micheline.strip_locations
       (Prim
          ( 0,
            D_Pair,
            [
              Seq
                ( 1,
                  [
                    Prim
                      ( 2,
                        D_Elt,
                        [Bytes (3, null_address); Int (4, Z.of_int 100)],
                        [] );
                  ] );
              Seq (5, []);
              String (6, cpmm_address);
              Int (7, Z.of_int 100);
            ],
            [] )))

let test_fa12_init_storage manager =
  Script_repr.lazy_expr
    (Micheline.strip_locations
       (Prim
          ( 0,
            D_Pair,
            [
              Seq (1, []);
              Seq (2, []);
              String (3, manager);
              Int (4, Z.of_int 10_000);
            ],
            [] )))

let originate ctxt address_hash ~balance script =
  Contract_storage.raw_originate
    ctxt
    ~prepaid_bootstrap_storage:true
    address_hash
    ~script
  >>=? fun ctxt ->
  let address = Contract_repr.Originated address_hash in
  Contract_storage.used_storage_space ctxt address >>=? fun size ->
  Fees_storage.burn_origination_fees
    ~origin:Protocol_migration
    ctxt
    ~storage_limit:(Z.of_int64 Int64.max_int)
    ~payer:`Liquidity_baking_subsidies
  >>=? fun (ctxt, _, origination_updates) ->
  Fees_storage.burn_storage_fees
    ~origin:Protocol_migration
    ctxt
    ~storage_limit:(Z.of_int64 Int64.max_int)
    ~payer:`Liquidity_baking_subsidies
    size
  >>=? fun (ctxt, _, storage_updates) ->
  Token.transfer
    ~origin:Protocol_migration
    ctxt
    `Liquidity_baking_subsidies
    (`Contract address)
    balance
  >>=? fun (ctxt, transfer_updates) ->
  let balance_updates =
    origination_updates @ storage_updates @ transfer_updates
  in
  let result : Migration_repr.origination_result =
    {
      balance_updates;
      originated_contracts = [address_hash];
      storage_size = size;
      paid_storage_size_diff = size;
    }
  in
  return (ctxt, result)

let originate_test_fa12 ~typecheck ctxt admin =
  Contract_storage.fresh_contract_from_current_nonce ctxt
  >>?= fun (ctxt, fa12_address) ->
  let script =
    Script_repr.
      {
        code = Script_repr.lazy_expr Liquidity_baking_lqt.script;
        storage =
          test_fa12_init_storage (Signature.Public_key_hash.to_b58check admin);
      }
  in
  typecheck ctxt script >>=? fun (script, ctxt) ->
  originate ctxt fa12_address ~balance:(Tez_repr.of_mutez_exn 1_000_000L) script
  >|=? fun (ctxt, origination_result) ->
  (ctxt, fa12_address, [origination_result])

(* hardcoded from lib_parameters *)
let first_bootstrap_account =
  Signature.Public_key.hash
    (Signature.Public_key.of_b58check_exn
       "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav")

let check_tzBTC ~typecheck current_level ctxt f =
  Contract_storage.exists ctxt (Contract_repr.Originated mainnet_tzBTC_address)
  >>= function
  | true ->
      (* If tzBTC exists, we're on mainnet and we use it as the token address in the CPMM. *)
      f ctxt mainnet_tzBTC_address []
  | false ->
      (* If the tzBTC contract does not exist, we originate a test FA1.2 contract using the same script as the LQT. This is so that we can test the contracts after performing the same protocol migration that will be done on mainnet.

         First, we check current level is below mainnet level roughly around 010 injection so we do not accidentally originate the test token contract on mainnet. *)
      if Compare.Int32.(current_level < 1_437_862l) then
        originate_test_fa12 ~typecheck ctxt first_bootstrap_account
        (* Token contract admin *)
        >>=? fun (ctxt, token_address, token_result) ->
        f ctxt token_address token_result
      else
        (* If we accidentally entered the tzBTC address incorrectly, but current level indicates this could be mainnet, we do not originate any contracts *)
        return (ctxt, [])

let init ctxt ~typecheck =
  (* We use a custom origination nonce because it is unset when stitching from 009 *)
  let nonce = Operation_hash.hash_string ["Drip, drip, drip."] in
  let ctxt = Raw_context.init_origination_nonce ctxt nonce in
  Storage.Liquidity_baking.Toggle_ema.init ctxt 0l >>=? fun ctxt ->
  let current_level =
    Raw_level_repr.to_int32 (Level_storage.current ctxt).level
  in
  Contract_storage.fresh_contract_from_current_nonce ctxt
  >>?= fun (ctxt, cpmm_address) ->
  Contract_storage.fresh_contract_from_current_nonce ctxt
  >>?= fun (ctxt, lqt_address) ->
  Storage.Liquidity_baking.Cpmm_address.init ctxt cpmm_address >>=? fun ctxt ->
  check_tzBTC
    ~typecheck
    current_level
    ctxt
    (fun ctxt token_address token_result ->
      let cpmm_script =
        Script_repr.
          {
            code = Script_repr.lazy_expr Liquidity_baking_cpmm.script;
            storage =
              cpmm_init_storage
                ~token_address:(Contract_hash.to_b58check token_address)
                ~lqt_address:(Contract_hash.to_b58check lqt_address);
          }
      in
      typecheck ctxt cpmm_script >>=? fun (cpmm_script, ctxt) ->
      let lqt_script =
        Script_repr.
          {
            code = Script_repr.lazy_expr Liquidity_baking_lqt.script;
            storage = lqt_init_storage (Contract_hash.to_b58check cpmm_address);
          }
      in
      typecheck ctxt lqt_script >>=? fun (lqt_script, ctxt) ->
      originate
        ctxt
        cpmm_address
        ~balance:(Tez_repr.of_mutez_exn 100L)
        cpmm_script
      >>=? fun (ctxt, cpmm_result) ->
      originate ctxt lqt_address ~balance:Tez_repr.zero lqt_script
      >|=? fun (ctxt, lqt_result) ->
      (* Unsets the origination nonce, which is okay because this is called after other originations in stitching. *)
      let ctxt = Raw_context.unset_origination_nonce ctxt in
      (ctxt, [cpmm_result; lqt_result] @ token_result))
