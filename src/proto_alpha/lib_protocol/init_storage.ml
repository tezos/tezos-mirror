(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(*
  To add invoices, you can use a helper function like this one:

  (** Invoice a contract at a given address with a given amount. Returns the
      updated context and a  balance update receipt (singleton list). The address
      must be a valid base58 hash, otherwise this is no-op and returns an empty
      receipts list.

      Do not fail if something goes wrong.
  *)
  let invoice_contract ctxt ~address ~amount_mutez =
    match Tez_repr.of_mutez amount_mutez with
    | None ->
        Lwt.return (ctxt, [])
    | Some amount -> (
        Contract_repr.of_b58check address
        >>?= (fun recipient ->
              Contract_storage.credit ctxt recipient amount
              >|=? fun ctxt ->
              ( ctxt,
                Receipt_repr.
                  [(Contract recipient, Credited amount, Protocol_migration)] ))
        >|= function Ok res -> res | Error _ -> (ctxt, []) )
*)

let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block ~level ~timestamp ~fitness ctxt
  >>=? fun (previous_protocol, ctxt) ->
  match previous_protocol with
  | Genesis param ->
      (* This is the genesis protocol: initialise the state *)
      Commitment_storage.init ctxt param.commitments >>=? fun ctxt ->
      Roll_storage.init ctxt >>=? fun ctxt ->
      Seed_storage.init ctxt >>=? fun ctxt ->
      Contract_storage.init ctxt >>=? fun ctxt ->
      Bootstrap_storage.init
        ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
      >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt >>=? fun ctxt ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0 >>=? fun ctxt ->
      Vote_storage.update_listings ctxt >>=? fun ctxt ->
      (* Must be called after other originations since it unsets the origination nonce.*)
      Liquidity_baking_migration.init ctxt ~typecheck
      >>=? fun (ctxt, operation_results) ->
      Storage.Pending_migration.Operation_results.init ctxt operation_results
  | Florence_009 ->
      (* Only the starting position of the voting period is shifted by
         one level into the future, so that voting periods are again
         aligned with cycles. The period kind does not change, as a new
         voting period has just started. *)
      Voting_period_storage.get_current ctxt >>=? fun voting_period ->
      Storage.Vote.Current_period.update
        ctxt
        {
          voting_period with
          start_position = Int32.succ voting_period.start_position;
        }
      >>=? fun ctxt ->
      Lazy_storage_diff.cleanup_edo_florence_dangling_lazy_storage ctxt
      >>= fun ctxt ->
      (* Add balance updates receipts to be attached on the first block of this
         protocol - see [[prepare]] function below. Any balance updates attached
         in the migration should use the [[Receipt_repr.Migration]] constructor.
      *)
      (* To add invoices, use something like that:
          invoice_contract
            ctxt
            ~address:"tz1..."
            ~amount_mutez:123_456L
          >>= fun (ctxt, balance_updates) ->
          Storage.Pending_migration_balance_updates.init ctxt balance_updates
      *)
      (* Must be called after other originations since it unsets the origination nonce.*)
      Liquidity_baking_migration.init ctxt ~typecheck
      >>=? fun (ctxt, operation_results) ->
      Storage.Pending_migration.Operation_results.init ctxt operation_results

let prepare ctxt ~level ~predecessor_timestamp ~timestamp ~fitness =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun ctxt -> Storage.Pending_migration.remove ctxt
