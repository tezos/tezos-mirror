(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2021 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

module Patch_legacy_contracts_for_J = struct
  open Legacy_script_patches_for_J

  let contract_patches =
    [
      ( "KT1MzfYSbq18fYr4f44aQRoZBQN72BAtiz5j",
        exprtgpMFzTtyg1STJqANLQsjsMXmkf8UuJTuczQh8GPtqfw18x6Lc );
      ( "KT1Kfbk3B6NYPCPohPBDU3Hxf5Xeyy9PdkNp",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1JW6PwhfaEJu6U3ENsxUeja48AdtqSoekd",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1VsSxSXUkgw6zkBGgUuDXXuJs9ToPqkrCg",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1TcAHw5gpejyemwRtdNyFKGBLc4qwA5gtw",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1FN5fcNNcgieGjzxbVEPWUpJGwZEpzNGA8",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1Um7ieBEytZtumecLqGeL56iY6BuWoBgio",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1CSKPf2jeLpMmrgKquN2bCjBTkAcAdRVDy",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1D5NmtDtgCwPxYNb2ZK2But6dhNLs1T1bV",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1VvXEpeBpreAVpfp4V8ZujqWu2gVykwXBJ",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1TzamC1SCj68ia2E4q2GWZeT24yRHvUZay",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1LZFMGrdnPjRLsCZ1aEDUAF5myA5Eo4rQe",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1PDAELuX7CypUHinUgFgGFskKs7ytwh5Vw",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT19xDbLsvQKnp9xqfDNPWJbKJJmV93dHDUa",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1Cz7TyVFvHxXpxLS57RFePrhTGisUpPhvD",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1LQ99RfGcmFe98PiBcGXuyjBkWzAcoXXhW",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1Gow8VzXZx3Akn5kvjACqnjnyYBxQpzSKr",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1DnfT4hfikoMY3uiPE9mQV4y3Xweramb2k",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1FuFDZGdw86p6krdBUKoZfEMkcUmezqX5o",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1SLWhfqPtQq7f4zLomh8BNgDeprF9B6d2M",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1THsDNgHtN56ew9VVCAUWnqPC81pqAxCEp",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1CM1g1o9RKDdtDKgcBWE59X2KgTc2TcYtC",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1W148mcjmfvr9J2RvWcGHxsAFApq9mcfgT",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1HvwFnXteMbphi7mfPDhCWkZSDvXEz8iyv",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1RUT25eGgo9KKWXfLhj1xYjghAY1iZ2don",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1EWLAQGPMF2uhtVRPaCH2vtFVN36Njdr6z",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1WPEis2WhAc2FciM2tZVn8qe6pCBe9HkDp",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1Msatnmdy24sQt6knzpALs4tvHfSPPduA2",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1A56dh8ivKNvLiLVkjYPyudmnY2Ti5Sba3",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1KRyTaxCAM3YRquifEe29BDbUKNhJ6hdtx",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1FL3C6t9Lyfskyb6rQrCRQTnf7M9t587VM",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1Q1kfbvzteafLvnGz92DGvkdypXfTGfEA3",
        exprucjN3PgUnqQHFXQmemT44DjkacU35NrSSKyz18JSSjJB9vtUEw );
      ( "KT1CjfCztmRpsyUee1nLa9Wcpfr7vgwqRZmk",
        expruqNpURkmjQk5RGHjLrnS1U3DZnEsQCvQQNLSpN1powRmJeQgoJ );
      ( "KT1MHDHRLugz3A4qP6KqZDpa7FFmZfcJauV4",
        expruwujdJkc5y4iPzr83Sd3KrJhzxSUb67JdCZmXNKiTTNvEkMrRU );
      ( "KT1BvVxWM6cjFuJNet4R9m64VDCN2iMvjuGE",
        expruZKjVy3JbWcJmjtnvMGvRamkDhMgM3urGGdne3pkN9VKgK7VnD );
      ( "KT1PyX9b8WmShQjqNgDQsvxqj9UYdmHLr3xg",
        exprv98vtze1uwbDXdpb27R8RQabWZMZDXGNAwaAZwCg6WSvXu8fw3 );
      ( "KT1XTXBsEauzcv3uPvVXW92mVqrx99UGsb9T",
        expru1ukk6ZqdA32rFYFG7j1eGjfsatbdUZWz8Mi1kXWZYRZm4FZVe );
      ( "KT1Puc9St8wdNoGtLiD2WXaHbWU7styaxYhD",
        expru1ukk6ZqdA32rFYFG7j1eGjfsatbdUZWz8Mi1kXWZYRZm4FZVe );
      ( "KT19c8n5mWrqpxMcR3J687yssHxotj88nGhZ",
        expru1ukk6ZqdA32rFYFG7j1eGjfsatbdUZWz8Mi1kXWZYRZm4FZVe );
      ( "KT1DrJV8vhkdLEj76h1H9Q4irZDqAkMPo1Qf",
        expru1ukk6ZqdA32rFYFG7j1eGjfsatbdUZWz8Mi1kXWZYRZm4FZVe );
      ( "KT1D68BvUm9N1fcq6uaZnyZvmBkBvj9biyPu",
        expru1ukk6ZqdA32rFYFG7j1eGjfsatbdUZWz8Mi1kXWZYRZm4FZVe );
      ( "KT1CT7S2b9hXNRxRrEcany9sak1qe4aaFAZJ",
        exprubv5oQmAUP8BwktmDgMWqTizYDJVhzHhJESGZhJ2GkHESZ1VWg );
    ]

  let patch_script (address, {legacy_script_hash; patched_code}) ctxt =
    Contract_repr.of_b58check address >>?= fun contract ->
    Storage.Contract.Code.find ctxt contract >>=? fun (ctxt, code_opt) ->
    Logging.log Notice "Patching %s... " address ;
    match code_opt with
    | Some old_code ->
        let old_bin = Data_encoding.force_bytes old_code in
        let old_hash = Script_expr_hash.hash_bytes [old_bin] in
        if Script_expr_hash.equal old_hash legacy_script_hash then (
          let new_code = Script_repr.lazy_expr patched_code in
          Logging.log Notice "Contract %s successfully patched" address ;
          Storage.Contract.Code.update ctxt contract new_code
          >>=? fun (ctxt, size_diff) ->
          let size_diff = Z.of_int size_diff in
          Storage.Contract.Used_storage_space.get ctxt contract
          >>=? fun prev_size ->
          let new_size = Z.add prev_size size_diff in
          Storage.Contract.Used_storage_space.update ctxt contract new_size
          >>=? fun ctxt ->
          if Z.(gt size_diff zero) then
            Storage.Contract.Paid_storage_space.get ctxt contract
            >>=? fun prev_paid_size ->
            let paid_size = Z.add prev_paid_size size_diff in
            Storage.Contract.Paid_storage_space.update ctxt contract paid_size
          else return ctxt)
        else (
          Logging.log
            Error
            "Patching %s was skipped because its script does not have the \
             expected hash (expected: %a, found: %a)"
            address
            Script_expr_hash.pp
            legacy_script_hash
            Script_expr_hash.pp
            old_hash ;
          return ctxt)
    | None ->
        Logging.log
          Error
          "Patching %s was skipped because no script was found for it in the \
           context."
          address ;
        return ctxt
end

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
    | None -> Lwt.return (ctxt, [])
    | Some amount -> (
        ( Contract_repr.of_b58check address >>?= fun recipient ->
          Token.transfer
            ~origin:Protocol_migration
            ctxt
            `Invoice
            (`Contract recipient)
            amount )
        >|= function
        | Ok res -> res
        | Error _ -> (ctxt, []))
*)

let prepare_first_block ctxt ~typecheck ~level ~timestamp =
  Raw_context.prepare_first_block ~level ~timestamp ctxt
  >>=? fun (previous_protocol, ctxt) ->
  let parametric = Raw_context.constants ctxt in
  ( Raw_context.Cache.set_cache_layout
      ctxt
      (Constants_repr.cache_layout parametric)
  >|= fun ctxt -> Raw_context.Cache.clear ctxt )
  >>= fun ctxt ->
  (match previous_protocol with
  | Genesis param ->
      (* This is the genesis protocol: initialise the state *)
      Storage.Block_round.init ctxt Round_repr.zero >>=? fun ctxt ->
      Raw_level_repr.of_int32 level >>?= fun first_level ->
      Storage.Tenderbake.First_level.init ctxt first_level >>=? fun ctxt ->
      let init_commitment (ctxt, balance_updates)
          Commitment_repr.{blinded_public_key_hash; amount} =
        Token.transfer
          ctxt
          `Initial_commitments
          (`Collected_commitments blinded_public_key_hash)
          amount
        >>=? fun (ctxt, new_balance_updates) ->
        return (ctxt, new_balance_updates @ balance_updates)
      in
      List.fold_left_es init_commitment (ctxt, []) param.commitments
      >>=? fun (ctxt, commitments_balance_updates) ->
      Storage.Stake.Last_snapshot.init ctxt 0 >>=? fun ctxt ->
      Seed_storage.init ?initial_seed:param.constants.initial_seed ctxt
      >>=? fun ctxt ->
      Contract_storage.init ctxt >>=? fun ctxt ->
      Bootstrap_storage.init
        ctxt
        ~typecheck
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
      >>=? fun (ctxt, bootstrap_balance_updates) ->
      Stake_storage.init_first_cycles ctxt Delegate_storage.pubkey
      >>=? fun ctxt ->
      let cycle = (Raw_context.current_level ctxt).cycle in
      Delegate_storage.freeze_deposits_do_not_call_except_for_migration
        ~new_cycle:cycle
        ~balance_updates:[]
        ctxt
      >>=? fun (ctxt, deposits_balance_updates) ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Vote_storage.update_listings ctxt >>=? fun ctxt ->
      (* Must be called after other originations since it unsets the origination nonce. *)
      Liquidity_baking_migration.init ctxt ~typecheck
      >>=? fun (ctxt, operation_results) ->
      Storage.Pending_migration.Operation_results.init ctxt operation_results
      >>=? fun ctxt ->
      return
        ( ctxt,
          commitments_balance_updates @ bootstrap_balance_updates
          @ deposits_balance_updates )
  | Ithaca_012 ->
      (* TODO (#2233): fix endorsement of migration block in Ithaca baker *)
      Raw_level_repr.of_int32 level >>?= fun first_level ->
      Storage.Tenderbake.First_level.update ctxt first_level >>=? fun ctxt ->
      Storage.Vote.Legacy_listings_size.remove ctxt >>= fun ctxt ->
      Vote_storage.update_listings ctxt >>=? fun ctxt ->
      Liquidity_baking_migration.Migration_from_Ithaca.update ctxt
      >>=? fun ctxt -> return (ctxt, []))
  >>=? fun (ctxt, balance_updates) ->
  Receipt_repr.group_balance_updates balance_updates >>?= fun balance_updates ->
  Storage.Pending_migration.Balance_updates.add ctxt balance_updates
  >>= fun ctxt ->
  Patch_legacy_contracts_for_J.(
    List.fold_right_es patch_script contract_patches ctxt)
  >>=? fun ctxt -> return ctxt

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ctxt
  >>=? fun ctxt -> Storage.Pending_migration.remove ctxt
