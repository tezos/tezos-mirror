(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let from_Some = function Some x -> x | None -> assert false

module Lazy : sig
  type 'a t

  val make : (unit -> 'a) -> 'a t

  val force : 'a t -> 'a
end = struct
  type 'a desc = Value of 'a | Thunk of (unit -> 'a) | Exception of exn

  type 'a t = 'a desc ref

  let make f = ref (Thunk f)

  let force r =
    match !r with
    | Value a ->
        a
    | Exception e ->
        raise e
    | Thunk f -> (
      try
        let a = f () in
        r := Value a ;
        a
      with e ->
        r := Exception e ;
        raise e )
end

let find_with_encoding tree path encoding =
  Raw_context.Tree.find tree path
  >|= Option.map (fun bytes ->
          (* XXX better message *)
          match Data_encoding.Binary.of_bytes encoding bytes with
          | Some d ->
              d
          | None ->
              failwith "encoding failure")

let clear_tree ctxt key =
  Raw_context.get_tree ctxt key
  >|=? fun tree ->
  Raw_context.Tree.clear tree ;
  ctxt

let map_contracts_index ctxt f =
  let index_key = ["contracts"; "index"] in
  Raw_context.get_tree ctxt index_key
  >>=? fun index_tree ->
  Raw_context.Tree.fold
    index_tree
    []
    ~depth:(`Eq 7)
    ~init:(ok (index_tree, ctxt))
    ~f:(fun key tree st ->
      st
      >>?= fun (index_tree, ctxt) ->
      f key tree ctxt
      >>=? fun (modified, tree, ctxt) ->
      ( if modified then Raw_context.Tree.add_tree index_tree key tree
      else Lwt.return index_tree )
      >|= fun index_tree -> ok (index_tree, ctxt))
  >>=? fun (index_tree, ctxt) ->
  Raw_context.update_tree ctxt index_key index_tree
  >>=? fun ctxt -> clear_tree ctxt index_key

(* Baker account migration create just around 3000 [Baker_hash.t]
   and uses it millions of times.  The queries must be pre-built
   for the performance.
*)
module Bakers : sig
  type t

  val empty : t

  val add :
    t ->
    Raw_context.t ->
    Signature.Public_key.t ->
    Signature.Public_key_hash.t ->
    Baker_hash.t ->
    t Lwt.t

  val find_tree : t -> Baker_hash.t -> Raw_context.tree option

  val find_by_pkh : t -> Signature.Public_key_hash.t -> Baker_hash.t option

  val find_by_pkh_result :
    t -> Signature.Public_key_hash.t -> Baker_hash.t tzresult

  val find_by_pkh_bytes : t -> bytes -> Baker_hash.t option

  val find_by_pk_bytes : t -> bytes -> Baker_hash.t option
end = struct
  module Public_key_map = Map.Make (Signature.Public_key)
  module Bytes_map = Map.Make (Bytes)

  (* [from_*_bytes] is necessary for performance since decoding back
     to Public_key.t is costly. *)
  type t = {
    trees : Raw_context.tree Baker_hash.Map.t;
    from_pkh : Baker_hash.t Signature.Public_key_hash.Map.t;
    from_pk : Baker_hash.t Public_key_map.t;
    from_pkh_bytes : Baker_hash.t Bytes_map.t;
    from_pk_bytes : Baker_hash.t Bytes_map.t;
  }

  let empty =
    {
      trees = Baker_hash.Map.empty;
      from_pkh = Signature.Public_key_hash.Map.empty;
      from_pk = Public_key_map.empty;
      from_pkh_bytes = Bytes_map.empty;
      from_pk_bytes = Bytes_map.empty;
    }

  let add t ctxt pk pkh baker_hash =
    Raw_context.Tree.add
      (Raw_context.Tree.empty ctxt)
      []
      (Data_encoding.Binary.to_bytes_exn Baker_hash.encoding baker_hash)
    >|= fun tree ->
    let trees = Baker_hash.Map.add baker_hash tree t.trees in
    let from_pkh =
      Signature.Public_key_hash.Map.add pkh baker_hash t.from_pkh
    in
    let from_pk = Public_key_map.add pk baker_hash t.from_pk in
    let from_pkh_bytes =
      Bytes_map.add
        (Data_encoding.Binary.to_bytes_exn
           Signature.Public_key_hash.encoding
           pkh)
        baker_hash
        t.from_pkh_bytes
    in
    let from_pk_bytes =
      Bytes_map.add
        (Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding pk)
        baker_hash
        t.from_pk_bytes
    in
    {trees; from_pkh; from_pk; from_pkh_bytes; from_pk_bytes}

  let find_tree t bh = Baker_hash.Map.find_opt bh t.trees

  let find_by_pkh t pkh = Signature.Public_key_hash.Map.find_opt pkh t.from_pkh

  let find_by_pkh_result t pkh =
    match find_by_pkh t pkh with
    | Some x ->
        ok x
    | None ->
        let open Raw_context in
        storage_error
          (Missing_key
             ( ["baker"; "consensus_key_rev_index"]
               @ Signature.Public_key_hash.to_path pkh ["consensus_key_rev"],
               Get ))

  let find_by_pkh_bytes t pkh_bytes =
    Bytes_map.find_opt pkh_bytes t.from_pkh_bytes

  let find_by_pk_bytes t pk_bytes = Bytes_map.find_opt pk_bytes t.from_pk_bytes
end

let migrate_baker ctxt baker_pkh bakers =
  Contract_storage.get_public_key ctxt baker_pkh
  >>=? fun pk ->
  let storage = Baker_script_repr.storage ~threshold:1 ~owner_keys:[pk] in
  Baker_storage.fresh_baker_from_current_nonce ctxt
  >>?= fun (ctxt, baker_hash) ->
  let from = baker_pkh in
  let to_ = baker_hash in
  let from_contract = Contract_repr.implicit_contract from in
  let to_contract = Contract_repr.baker_contract to_ in
  Storage.Baker.Registered.add ctxt baker_hash
  >>= fun ctxt ->
  Storage.Contract.Storage.init
    ctxt
    to_contract
    (Script_repr.lazy_expr storage)
  >>=? fun (ctxt, storage_size) ->
  let total_size = Z.of_int storage_size in
  Storage.Contract.Paid_storage_space.init ctxt to_contract total_size
  >>=? fun ctxt ->
  Storage.Contract.Used_storage_space.init ctxt to_contract total_size
  >>=? fun ctxt ->
  Storage.Contract.Global_counter.get ctxt
  >>=? fun counter ->
  Storage.Contract.Counter.init ctxt to_contract counter
  >>=? fun ctxt ->
  Storage.Baker.Consensus_key.init ctxt baker_hash pk
  >>=? fun ctxt ->
  let pkh = Signature.Public_key.hash pk in
  Storage.Baker.Consensus_key_rev.init ctxt pkh baker_hash
  >>=? fun ctxt ->
  Storage.Delegates_008.remove ctxt baker_pkh
  >>= fun ctxt ->
  (* Migrate storages with original baker pkh as a key *)
  Storage.Contract.Balance.get ctxt from_contract
  >>=? fun balance ->
  Storage.Contract.Balance.init ctxt to_contract balance
  >>=? fun ctxt ->
  Storage.Contract.Balance.remove ctxt from_contract
  >>= fun ctxt ->
  (* Roll.Delegate_roll_list -> Roll.Baker_roll_list *)
  Storage.Roll.Delegate_roll_list_008.find ctxt from
  >>=? (function
         | None ->
             return ctxt
         | Some roll ->
             Storage.Roll.Delegate_roll_list_008.remove ctxt from
             >>= fun ctxt -> Storage.Roll.Baker_roll_list.init ctxt to_ roll)
  >>=? fun ctxt ->
  (* Roll.Delegate_change -> Roll.Baker_change *)
  Storage.Roll.Delegate_change_008.find ctxt from
  >>=? (function
         | None ->
             return ctxt
         | Some change ->
             Storage.Roll.Delegate_change_008.remove ctxt from
             >>= fun ctxt -> Storage.Roll.Baker_change.init ctxt to_ change)
  >>=? fun ctxt ->
  (* Contract.Delegated -> Baker.Delegators *)
  Storage.Contract.Delegated_008.fold
    (ctxt, from_contract)
    ~init:ctxt
    ~f:(fun contract ctxt ->
      match Contract_repr.is_implicit contract with
      | Some pkh when Signature.Public_key_hash.equal pkh from ->
          (* if it's self-delegation, just remove the association *)
          Lwt.return ctxt
      | _ ->
          Storage.Baker.Delegators.add (ctxt, to_) contract)
  >>= fun ctxt ->
  Storage.Contract.Delegated_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_deposits -> Baker.Frozen_deposits *)
  Storage.Contract.Frozen_deposits_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle deposit acc ->
      acc
      >>?= fun ctxt ->
      Storage.Baker.Frozen_deposits.init (ctxt, to_) cycle deposit)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_deposits_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_fees -> Baker.Frozen_fees *)
  Storage.Contract.Frozen_fees_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle fee acc ->
      acc >>?= fun ctxt -> Storage.Baker.Frozen_fees.init (ctxt, to_) cycle fee)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_fees_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_rewards -> Baker.Frozen_rewards *)
  Storage.Contract.Frozen_rewards_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle rewards acc ->
      acc
      >>?= fun ctxt ->
      Storage.Baker.Frozen_rewards.init (ctxt, to_) cycle rewards)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_rewards_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  Bakers.add bakers ctxt pk pkh baker_hash >|= fun bakers -> ok (ctxt, bakers)

(* Contract.Delegate *)
let contract_delegate tree baker bakers =
  let delegate_key = ["delegate"] in
  Raw_context.Tree.find tree delegate_key
  >>= function
  | None ->
      Lwt.return_none
  | Some pkh_bytes ->
      let delegate_tree =
        match Bakers.find_by_pkh_bytes bakers pkh_bytes with
        | None ->
            (* NOTE There's one contract
               "tz1gNQpjio7F5HsR9bs37vBYZXfbWpZ34tpL" that's not registered
               as baker, but because of a bug in protocol 001 it has two
               originated contracts delegated to it:
                - KT1LLKuQLmxciFUif4yt6UyXFqEE3235emHE
                - KT19MYkajshb4DnpDyNC37Qm7DCbPh5GP5Sg
               We drop the delegations here.
            *)
            None
        | Some baker_hash -> (
          match Lazy.force baker with
          | None ->
              (* always Some *)
              Bakers.find_tree bakers baker_hash
          | Some _ ->
              (* Baker contracts are no longer self-delegated, so we
                 don't migrate this relation, instead we delete it *)
              None )
      in
      Raw_context.Tree.add_or_remove_tree tree delegate_key delegate_tree
      >|= fun tree -> Some tree

(* Contract.Inactive_delegate -> Baker.Inactive *)
let inactive_delegate tree baker_hash_of_contract ctxt =
  let inactive_delegate_key = ["inactive_delegate"] in
  Raw_context.Tree.find tree inactive_delegate_key
  >>= function
  | None ->
      Lwt.return_none
  | Some _ (* "inited" *) ->
      Raw_context.Tree.remove tree inactive_delegate_key
      >>= fun tree ->
      Storage.Baker.Inactive.add ctxt (Lazy.force baker_hash_of_contract)
      >|= fun ctxt -> Some (tree, ctxt)

(* Contract.Delegate_desactivation -> Baker.Inactive *)
let delegate_desactivation tree baker_hash_of_contract ctxt =
  let delegate_desactivation_key = ["delegate_desactivation"] in
  find_with_encoding tree delegate_desactivation_key Cycle_repr.encoding
  >>= function
  | None ->
      Lwt.return_none
  | Some cycle ->
      Raw_context.Tree.remove tree delegate_desactivation_key
      >>= fun tree ->
      Storage.Baker.Deactivation.add
        ctxt
        (Lazy.force baker_hash_of_contract)
        cycle
      >|= fun ctxt -> Some (tree, ctxt)

(* This is the genesis protocol: initialise the state *)
let prepare_first_block ctxt ~typecheck ~level ~timestamp ~fitness =
  Raw_context.prepare_first_block ~level ~timestamp ~fitness ctxt
  >>=? fun (previous_protocol, ctxt) ->
  match previous_protocol with
  | Genesis param ->
      Commitment_storage.init ctxt param.commitments
      >>=? fun ctxt ->
      Roll_storage.init ctxt
      >>=? fun ctxt ->
      Seed_storage.init ctxt
      >>=? fun ctxt ->
      Contract_storage.init ctxt
      >>=? fun ctxt ->
      Bootstrap_storage.init
        ctxt
        ~typecheck
        ?ramp_up_cycles:param.security_deposit_ramp_up_cycles
        ?no_reward_cycles:param.no_reward_cycles
        param.bootstrap_accounts
        param.bootstrap_contracts
        param.bootstrap_bakers
      >>=? fun ctxt ->
      Roll_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Baker_storage.init_first_cycles ctxt
      >>=? fun ctxt ->
      Vote_storage.init
        ctxt
        ~start_position:(Level_storage.current ctxt).level_position
      >>=? fun ctxt ->
      Storage.Block_priority.init ctxt 0
      >>=? fun ctxt -> Vote_storage.update_listings ctxt
  | Edo_008 ->
      (* 0. Invoices:
         Do them before baker accounts migration to avoid breaking
         baking accounts invariants.
      *)
      (* Add balance updates receipts to be attached on the first block of this
         protocol - see [prepare] function below. Any balance updates attached
         in the migration should use the [Receipt_repr.Protocol_migration]
         as their [update_origin].
      *)
      invoice_contract
        ctxt
        ~address:"tz1abmz7jiCV2GH2u81LRrGgAFFgvQgiDiaf"
        ~amount_mutez:100_000_000L
      >>= fun (ctxt, invoice_balance_updates) ->
      (* 1. Baker accounts migration *)
      let nonce =
        Operation_hash.hash_bytes
          [ Bytes.of_string
              "Things will not calm down, Daniel Jackson. They will, in fact, \
               calm up." ]
      in
      let ctxt = Raw_context.init_origination_nonce ctxt nonce in
      Storage.Delegates_008.fold
        ctxt
        ~init:(ok (ctxt, Bakers.empty))
        ~f:(fun baker_pkh acc ->
          acc >>?= fun (ctxt, bakers) -> migrate_baker ctxt baker_pkh bakers)
      >>=? fun (ctxt, bakers) ->
      Storage.Delegates_008.clear ctxt
      >>= fun ctxt ->
      (* The number of bakers is small (< 3000).  We keep them
         in memory to speed up the migration *)
      let current_cycle = Raw_context.(current_level ctxt).cycle in
      (* Take a snapshot of the consensus keys *)
      Storage.Baker.Consensus_key.snapshot ctxt current_cycle
      >>=? fun ctxt ->
      (* Because the migration runs on the last block of a predecessor protocol,
         we also need to take the snapshot for the following cycle, otherwise
         the baker process which looks up consensus for the future block would
         fail. *)
      Storage.Baker.Consensus_key.snapshot ctxt (Cycle_repr.succ current_cycle)
      >>=? fun ctxt ->
      (* Migrate storages with references to original baker pkhs *)
      (* Contract.Delegate + Contract.Inactive_delegate + Contract.Delegate_desactivation + DFS patching *)
      (* Update Contract.Delegate, Contract.Inactive_delegate, and
         Contract.Delegate_desactivation in one folding *)
      map_contracts_index ctxt (fun key tree ctxt ->
          let baker =
            Lazy.make (fun () ->
                (* always succeeds *)
                let contract = from_Some (Contract_repr.Index.of_path key) in
                match Contract_repr.is_implicit contract with
                | None ->
                    None
                | Some pkh ->
                    Bakers.find_by_pkh bakers pkh)
          in
          let baker_hash_of_contract =
            Lazy.make (fun () ->
                match Lazy.force baker with
                | None ->
                    failwith
                    @@ Format.asprintf
                         "baker \"%a\" is not an implicit account"
                         Contract_repr.pp
                         (from_Some (Contract_repr.Index.of_path key))
                | Some baker ->
                    baker)
          in
          let modified = false in
          contract_delegate tree baker bakers
          >|= (function None -> (modified, tree) | Some tree -> (true, tree))
          >>= fun (modified, tree) ->
          inactive_delegate tree baker_hash_of_contract ctxt
          >|= (function
                | None ->
                    (modified, tree, ctxt)
                | Some (tree, ctxt) ->
                    (true, tree, ctxt))
          >>= fun (modified, tree, ctxt) ->
          delegate_desactivation tree baker_hash_of_contract ctxt
          >|= function
          | None ->
              ok (modified, tree, ctxt)
          | Some (tree, ctxt) ->
              ok (true, tree, ctxt))
      >>=? fun ctxt ->
      (* Active_delegates_with_rolls -> Baker.Active_with_rolls *)
      Storage.Active_delegates_with_rolls_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh acc ->
          acc
          >>?= fun ctxt ->
          Bakers.find_by_pkh_result bakers pkh
          >>?= fun baker ->
          Storage.Baker.Active_with_rolls.add ctxt baker >|= ok)
      >>=? fun ctxt ->
      Storage.Active_delegates_with_rolls_008.clear ctxt
      >>= fun ctxt ->
      let preserved = Constants_storage.preserved_cycles ctxt in
      (* Delegates_with_frozen_balance -> Baker.With_frozen_balance *)
      let rec migrate_frozen_balance ctxt cycle =
        Storage.Delegates_with_frozen_balance_008.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun pkh acc ->
            acc
            >>?= fun ctxt ->
            Bakers.find_by_pkh_result bakers pkh
            >>?= fun baker ->
            Storage.Baker.With_frozen_balance.add (ctxt, cycle) baker >|= ok)
        >>=? fun ctxt ->
        Storage.Delegates_with_frozen_balance_008.clear (ctxt, cycle)
        >>= fun ctxt ->
        match Cycle_repr.pred cycle with
        | None ->
            return ctxt
        | Some prev_cycle ->
            if Cycle_repr.(current_cycle = add prev_cycle (preserved + 1)) then
              return ctxt
            else migrate_frozen_balance ctxt prev_cycle
      in
      migrate_frozen_balance ctxt current_cycle
      >>=? fun ctxt ->
      (* replace public_keys by baker_hashes *)
      let rewrite_rolls ctxt key depth =
        Raw_context.get_tree ctxt []
        >>=? fun root_tree ->
        Raw_context.Tree.fold
          root_tree
          key
          ~depth:(`Eq depth)
          ~init:(Raw_context.Tree.empty ctxt)
          ~f:(fun key pkbytes_tree acc_tree ->
            Raw_context.Tree.to_value pkbytes_tree
            >>= function
            | None ->
                assert false
            | Some pkbytes ->
                let baker_hash =
                  (* never fails *)
                  from_Some (Bakers.find_by_pk_bytes bakers pkbytes)
                in
                Raw_context.Tree.add_tree
                  acc_tree
                  key
                  (* never fails *)
                  (from_Some (Bakers.find_tree bakers baker_hash)))
        >>= fun acc_tree ->
        Raw_context.update_tree ctxt key acc_tree
        >|=? fun ctxt ->
        Raw_context.Tree.clear root_tree ;
        ctxt
      in
      (* Roll.Owner *)
      rewrite_rolls ctxt ["rolls"; "owner"; "current"] 3
      >>=? fun ctxt ->
      (* We split the folding into 2 to save memory consumption *)
      Raw_context.fold
        ~depth:(`Eq 2)
        ctxt
        ["rolls"; "owner"; "snapshot"]
        ~init:[]
        ~f:(fun k _ ks -> Lwt.return (k :: ks))
      >>= fun ks ->
      fold_left_s
        (fun ctxt k ->
          rewrite_rolls ctxt ("rolls" :: "owner" :: "snapshot" :: k) 3)
        ctxt
        ks
      >>=? fun ctxt ->
      (* Vote.Listings *)
      Storage.Vote.Listings_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh listings acc ->
          acc
          >>?= fun ctxt ->
          Bakers.find_by_pkh_result bakers pkh
          >>?= fun baker ->
          Storage.Vote.Listings_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Listings.init ctxt baker listings)
      >>=? fun ctxt ->
      (* Vote.Proposals *)
      Storage.Vote.Proposals_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun (proposal, pkh) acc ->
          acc
          >>?= fun ctxt ->
          Bakers.find_by_pkh_result bakers pkh
          >>?= fun baker ->
          Storage.Vote.Proposals_008.remove ctxt (proposal, pkh)
          >>= fun ctxt ->
          Storage.Vote.Proposals.add ctxt (proposal, baker) >|= ok)
      >>=? fun ctxt ->
      (* Vote.Proposals_count *)
      Storage.Vote.Proposals_count_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh count acc ->
          acc
          >>?= fun ctxt ->
          Bakers.find_by_pkh_result bakers pkh
          >>?= fun baker ->
          Storage.Vote.Proposals_count_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Proposals_count.init ctxt baker count)
      >>=? fun ctxt ->
      (* Vote.Ballots *)
      Storage.Vote.Ballots_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh ballot acc ->
          acc
          >>?= fun ctxt ->
          Bakers.find_by_pkh_result bakers pkh
          >>?= fun baker ->
          Storage.Vote.Ballots_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Ballots.init ctxt baker ballot)
      >>=? fun ctxt ->
      (* Storage.Seed.Nonce *)
      let rec migrate_cycle_nonce ctxt cycle =
        Storage.Cycle_008.Nonce.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun level nonce acc ->
            acc
            >>?= fun ctxt ->
            ( match nonce with
            | Unrevealed {nonce_hash; delegate; rewards; fees} ->
                Bakers.find_by_pkh_result bakers delegate
                >|? fun baker ->
                Storage.Cycle.Unrevealed {nonce_hash; baker; rewards; fees}
            | Revealed s ->
                ok (Storage.Cycle.Revealed s) )
            >>?= fun nonce ->
            (* because the path has not changed, this will override 008 value *)
            Storage.Cycle.Nonce.update (ctxt, cycle) level nonce)
        >>=? fun ctxt ->
        match Cycle_repr.pred cycle with
        | None ->
            return ctxt
        | Some prev_cycle ->
            migrate_cycle_nonce ctxt prev_cycle
      in
      migrate_cycle_nonce ctxt current_cycle
      >>=? fun ctxt ->
      (* 2. balance update receipts for baker migration *)
      Storage.Baker.Consensus_key_rev.fold
        ctxt
        ~init:ok_nil
        ~f:(fun pkh baker_hash acc ->
          acc
          >>?= fun updates ->
          let original_contract = Contract_repr.implicit_contract pkh in
          let new_contract = Contract_repr.baker_contract baker_hash in
          Storage.Contract.Balance.get ctxt new_contract
          >|=? fun moved_balance ->
          Receipt_repr.
            ( Contract original_contract,
              Debited moved_balance,
              Protocol_migration )
          :: Receipt_repr.
               ( Contract new_contract,
                 Credited moved_balance,
                 Protocol_migration )
          :: updates)
      >>=? fun baker_balance_updates ->
      Storage.Pending_migration_balance_updates.init
        ctxt
        (invoice_balance_updates @ baker_balance_updates)

let prepare ctxt ~level ~predecessor_timestamp ~timestamp ~fitness =
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun ctxt ->
  Storage.Pending_migration_balance_updates.find ctxt
  >>=? function
  | Some balance_updates ->
      Storage.Pending_migration_balance_updates.remove ctxt
      >>= fun ctxt ->
      (* When applying balance updates in a migration, we must attach receipts.
         The balance updates returned from here will be applied in the first
         block of the new protocol. *)
      return (ctxt, balance_updates)
  | None ->
      return (ctxt, [])
