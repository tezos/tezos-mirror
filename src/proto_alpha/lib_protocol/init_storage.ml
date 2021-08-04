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

(* do nothing for now *)
let log fmt = Format.kasprintf (fun _ -> ()) fmt

(* Remove me after protocol H *)
module Flatten_storage_for_H = struct
  (* /tree_abs_key/key/*/*/*/*/*
     => /tree_abs_key/key/rename( */*/*/*/* )
  *)
  let flatten ~tree ~key ~depth ~rename ~init =
    Raw_context.Tree.fold
      tree
      key
      ~depth:(`Eq depth)
      ~init
      ~f:(fun old_key tree dst_tree ->
        let new_key = rename old_key in
        Raw_context.Tree.add_tree dst_tree new_key tree)
    >>= fun dst_tree ->
    (* rm -rf $index_key
       mv $tmp_index_key $index_key *)
    Raw_context.Tree.add_tree tree key dst_tree

  (* /abs_key/*(depth')/mid_key/*(depth)
     => /abs_key/*(depth')/mid_key/rename( *(depth) )
  *)
  let fold_flatten ctxt abs_key depth' mid_key ~depth ~rename =
    Raw_context.fold
      ~depth:(`Eq depth')
      ctxt
      abs_key
      ~init:ctxt
      ~f:(fun key tree ctxt ->
        (* tree at /abs_key/*(depth') *)
        flatten
          ~tree
          ~key:mid_key
          ~depth
          ~rename
          ~init:(Raw_context.Tree.empty ctxt)
        >>= fun tree -> Raw_context.add_tree ctxt (abs_key @ key) tree)

  let flatten_storage ctxt =
    log "Flattening the context storage.  It takes several minutes." ;
    let rec drop n xs =
      match (n, xs) with
      | (0, _) -> xs
      | (_, []) -> assert false
      | (_, _ :: xs) -> drop (n - 1) xs
    in
    let rename_blake2b = function
      | n1 :: n2 :: n3 :: n4 :: n5 :: n6 :: rest ->
          String.concat "" [n1; n2; n3; n4; n5; n6] :: rest
      | _ -> assert false
    in
    let rename_public_key_hash = function
      | (("ed25519" | "secp256k1" | "p256") as k)
        :: n1 :: n2 :: n3 :: n4 :: n5 :: n6 :: rest ->
          k :: String.concat "" [n1; n2; n3; n4; n5; n6] :: rest
      | _ -> assert false
    in
    (* /contracts/index/xx/xx/xx/xx/xx/xx/yyyyyyyyyy
       => /contracts/index/yyyyyyyyyy
    *)
    fold_flatten ctxt ["contracts"; "index"] 0 [] ~depth:7 ~rename:(drop 6)
    >>= fun ctxt ->
    (* *)
    (* /contracts/index/yyyyyyyyyy/delegated/xx/xx/xx/xx/xx/xx/zzzzzzzzzz
       => /contracts/index/yyyyyyyyyy/delegated/zzzzzzzzzz
    *)
    fold_flatten
      ctxt
      ["contracts"; "index"]
      1
      ["delegated"]
      ~depth:7
      ~rename:(drop 6)
    >>= fun ctxt ->
    (* *)
    (* /big_maps/index/xx/xx/xx/xx/xx/xx/n
       => /big_maps/index/n
    *)
    fold_flatten ctxt ["big_maps"; "index"] 0 [] ~depth:7 ~rename:(drop 6)
    >>= fun ctxt ->
    (* *)
    (* /big_maps/index/n/contents/yy/yy/yy/yy/yy/yyyyyyyy
       => /big_maps/index/n/contents/yyyyyyyyyyyyyyyyyy
    *)
    fold_flatten
      ctxt
      ["big_maps"; "index"]
      1
      ["contents"]
      ~depth:6
      ~rename:rename_blake2b
    >>= fun ctxt ->
    (* *)
    (* /rolls/index/x/y/n
       => /rolls/index/n
    *)
    fold_flatten ctxt ["rolls"; "index"] 0 [] ~depth:3 ~rename:(drop 2)
    >>= fun ctxt ->
    (* *)
    (* /rolls/owner/current/x/y/n
       => /rolls/owner/current/n
    *)
    fold_flatten
      ctxt
      ["rolls"; "owner"; "current"]
      0
      []
      ~depth:3
      ~rename:(drop 2)
    >>= fun ctxt ->
    (* *)
    (* /rolls/owner/snapshot/n1/n2/x/y/n3
       => /rolls/owner/snapshot/n1/n2/n3
    *)
    fold_flatten
      ctxt
      ["rolls"; "owner"; "snapshot"]
      2
      []
      ~depth:3
      ~rename:(drop 2)
    >>= fun ctxt ->
    (* *)
    (* /commitments/xx/xx/xx/xx/xx/xxxxxx
       => /commitments/xxxxxxxxxxxxxxxx
    *)
    fold_flatten ctxt ["commitments"] 0 [] ~depth:6 ~rename:rename_blake2b
    >>= fun ctxt ->
    (* *)
    (* /votes/listings/kk/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /votes/listings/kk/xxxxxxxxxxxxxxxxxx
    *)
    fold_flatten
      ctxt
      ["votes"; "listings"]
      0
      []
      ~depth:7
      ~rename:rename_public_key_hash
    >>= fun ctxt ->
    (* *)
    (* /votes/ballots/kk/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /votes/ballots/KK/xxxxxxxxxxxxxxxxxxxx
    *)
    fold_flatten
      ctxt
      ["votes"; "ballots"]
      0
      []
      ~depth:7
      ~rename:rename_public_key_hash
    >>= fun ctxt ->
    (* *)
    (* /votes/proposals_count/kk/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /votes/proposals_count/kk/xxxxxxxxxxxxxxxxxxxx
    *)
    fold_flatten
      ctxt
      ["votes"; "proposals_count"]
      0
      []
      ~depth:7
      ~rename:rename_public_key_hash
    >>= fun ctxt ->
    (* *)
    (* /votes/proposals/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /votes/proposals/xxxxxxxxxxxxxxxxxxxx
    *)
    fold_flatten
      ctxt
      ["votes"; "proposals"]
      0
      []
      ~depth:6
      ~rename:rename_blake2b
    >>= fun ctxt ->
    (* *)
    (* /votes/proposals/yyyyyyyyyyyyyyyyyyyy/kk/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /votes/proposals/yyyyyyyyyyyyyyyyyyyy/KK/xxxxxxxxxxxxxxxxxxxx
    *)
    fold_flatten
      ctxt
      ["votes"; "proposals"]
      1
      []
      ~depth:7
      ~rename:rename_public_key_hash
    >>= fun ctxt ->
    (* *)
    (* /delegates/kk/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /delegates/KK/xxxxxxxxxxxxxxxxxxxx
    *)
    fold_flatten ctxt ["delegates"] 0 [] ~depth:7 ~rename:rename_public_key_hash
    >>= fun ctxt ->
    (* *)
    (* /active_delegates_with_rolls/kk/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /active_delegates_with_rolls/KK/xxxxxxxxxxxxxxxxxxxx
    *)
    fold_flatten
      ctxt
      ["active_delegates_with_rolls"]
      0
      []
      ~depth:7
      ~rename:rename_public_key_hash
    >>= fun ctxt ->
    (* *)
    (* /delegates_with_frozen_balance/n/kk/xx/xx/xx/xx/xx/xx/xxxxxxxx
       => /delegates_with_frozen_balance/n/KK/xxxxxxxxxxxxxxxxxxxx
    *)
    fold_flatten
      ctxt
      ["delegates_with_frozen_balance"]
      1
      []
      ~depth:7
      ~rename:rename_public_key_hash
    >|= fun ctxt ->
    log "Flattened the context storage." ;
    ctxt
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
      Flatten_storage_for_H.flatten_storage ctxt >>= fun ctxt ->
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
