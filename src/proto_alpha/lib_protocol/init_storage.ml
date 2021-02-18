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

open Logging

let migrate_baker ctxt baker_pkh =
  Contract_storage.get_public_key
    ctxt
    (Contract_repr.implicit_contract baker_pkh)
  >>=? fun pk ->
  let storage = Baker_script_repr.storage ~threshold:1 ~owner_keys:[pk] in
  Baker_storage.fresh_baker_from_current_nonce ctxt
  >>=? fun (ctxt, baker_hash) ->
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
    ~init:(ok ctxt)
    ~f:(fun contract acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      ( match Contract_repr.is_implicit contract with
      | None ->
          return_some contract
      | Some pkh ->
          (* if it's self-delegation, remove the association *)
          if Signature.Public_key_hash.equal pkh from then return_none
          else return_some contract )
      >>=? function
      | Some contract ->
          Storage.Baker.Delegators.add (ctxt, to_) contract
          >>= fun ctxt -> return ctxt
      | None ->
          return ctxt)
  >>=? fun ctxt ->
  Storage.Contract.Delegated_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_deposits -> Baker.Frozen_deposits *)
  Storage.Contract.Frozen_deposits_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle deposit acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      Storage.Baker.Frozen_deposits.init (ctxt, to_) cycle deposit)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_deposits_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_fees -> Baker.Frozen_fees *)
  Storage.Contract.Frozen_fees_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle fee acc ->
      Lwt.return acc
      >>=? fun ctxt -> Storage.Baker.Frozen_fees.init (ctxt, to_) cycle fee)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_fees_008.clear (ctxt, from_contract)
  >>= fun ctxt ->
  (* Contract.Frozen_rewards -> Baker.Frozen_rewards *)
  Storage.Contract.Frozen_rewards_008.fold
    (ctxt, from_contract)
    ~init:(ok ctxt)
    ~f:(fun cycle rewards acc ->
      Lwt.return acc
      >>=? fun ctxt ->
      Storage.Baker.Frozen_rewards.init (ctxt, to_) cycle rewards)
  >>=? fun ctxt ->
  Storage.Contract.Frozen_rewards_008.clear (ctxt, from_contract)
  >>= fun ctxt -> return (ctxt, (pk, pkh, baker_hash))

let clear_tree ctxt key =
  Raw_context.get_tree ctxt key
  >>=? fun tree ->
  Raw_context.Tree.clear tree ;
  return ctxt

let map_contracts_index ctxt ~init f =
  let index_key = ["contracts"; "index"] in
  Raw_context.get_tree ctxt index_key
  >>=? fun index_tree ->
  Raw_context.Tree.fold
    index_tree
    []
    ~depth:(`Eq 7)
    ~init:(ok (0, index_tree, init))
    ~f:(fun key tree st ->
      Lwt.return st
      >>=? fun (nmod, index_tree, acc) ->
      f key tree acc
      >>=? fun (modified, tree, acc) ->
      ( if modified then Raw_context.Tree.add_tree index_tree key tree
      else Lwt.return index_tree )
      >|= fun index_tree ->
      ok ((if modified then nmod + 1 else nmod), index_tree, acc))
  >>=? fun (nmod, index_tree, acc) ->
  Raw_context.update_tree ctxt index_key index_tree
  >>=? fun ctxt -> clear_tree ctxt index_key >|=? fun ctxt -> (ctxt, nmod, acc)

module Baker_hash_map = Map.Make (Baker_hash)

let build_find_baker_hash_tree ctxt bakers =
  Lwt_list.fold_left_s
    (fun map (_, _, bh) ->
      Raw_context.Tree.add
        (Raw_context.Tree.empty ctxt)
        []
        (Data_encoding.Binary.to_bytes_exn Baker_hash.encoding bh)
      >|= fun tree -> Baker_hash_map.add bh tree map)
    Baker_hash_map.empty
    bakers
  >|= fun map bh -> Baker_hash_map.find_opt bh map

let build_find_baker_hash bakers =
  let module Map = Map.Make (struct
    type t = Signature.Public_key_hash.t

    let compare = Signature.Public_key_hash.compare
  end) in
  let map =
    List.fold_left
      (fun m (_pk, pkh, baker_hash) -> Map.add pkh baker_hash m)
      Map.empty
      bakers
  in
  fun pkh -> Map.find_opt pkh map

let build_find_baker_hash_by_pkhbytes bakers =
  let module Map = Map.Make (struct
    type t = bytes

    let compare = Compare.Bytes.compare
  end) in
  let map =
    List.fold_left
      (fun m (_, pkh, baker_hash) ->
        let pkhbytes =
          Data_encoding.Binary.to_bytes_exn
            Signature.Public_key_hash.encoding
            pkh
        in
        Map.add pkhbytes baker_hash m)
      Map.empty
      bakers
  in
  fun pkhbytes -> Map.find_opt pkhbytes map

let build_find_baker_hash_by_pkbytes bakers =
  let module Map = Map.Make (struct
    type t = bytes

    let compare = Compare.Bytes.compare
  end) in
  let map =
    List.fold_left
      (fun m (pk, _, baker_hash) ->
        let pkbytes =
          Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding pk
        in
        Map.add pkbytes baker_hash m)
      Map.empty
      bakers
  in
  fun pkbytes -> Map.find_opt pkbytes map

let from_Some = function Some x -> x | None -> assert false

(* Stitch contracts that produce more than one operation.

   Those contracts might be affected by the semantic change of switching from
   BFS to DFS for inter-contract calls. To guarantee they aren't, we enforce
   that the uncontrolled addresses (from parameters but also the SENDER) are
   implicit accounts by asserting that they are actually the SOURCE.

   Moreover, we sometimes swap the order of operations when two of them
   are emmitted to enforce that the uncontrolled address is called last so even
   if it performs a DFS call it cannot access the contract in a state that was
   inaccessible in BFS.
 *)

let update_contract_script (m : ('l, 'p) Micheline.node) :
    ('l, 'p) Micheline.node =
  let open Micheline in
  let open Michelson_v1_primitives in
  let destruct_seq = function Seq (_location, l) -> l | _ -> assert false in
  let rec destruct_if_left = function
    | Prim (_, I_IF_LEFT, [a; b], _) ->
        destruct_if_left a @ destruct_if_left b
    | Seq (_, [a]) ->
        destruct_if_left a
    | a ->
        [a]
  in
  let prim0 p = Prim (0, p, [], []) in
  let (parameter, storage, code) =
    match destruct_seq m with
    | [parameter; storage; Prim (_, K_code, [Seq (_, s)], _)] ->
        (parameter, storage, s)
    | _ ->
        assert false
  in
  let (cast, if_lefts) =
    match code with
    | [cast; _; _; _; if_lefts] ->
        (cast, if_lefts)
    | _ ->
        assert false
  in
  let (ep_1, ep_2, ep_3, ep_4, ep_5, ep_6, ep_7, ep_8, ep_9, ep_10, ep_11) =
    match destruct_if_left if_lefts with
    | [ep_1; ep_2; ep_3; ep_4; ep_5; ep_6; ep_7; ep_8; ep_9; ep_10; ep_11] ->
        (ep_1, ep_2, ep_3, ep_4, ep_5, ep_6, ep_7, ep_8, ep_9, ep_10, ep_11)
    | _ ->
        assert false
  in
  let assert_is_source =
    [ prim0 I_SOURCE;
      prim0 I_COMPARE;
      prim0 I_EQ;
      Prim
        (0, I_IF, [Seq (0, []); Seq (0, [prim0 I_UNIT; prim0 I_FAILWITH])], [])
    ]
  in
  let sender = prim0 I_SENDER in
  let dup = prim0 I_DUP in
  let car = prim0 I_CAR in
  let cdr = prim0 I_CDR in
  (* In "NIL operation; DIG 2; CONS; SWAP; CONS", replace "DIG 2" by "SWAP" **)
  let rec patch_ep_5_tail l =
    match l with
    | (Prim (_, I_NIL, _, _) as nil) :: _ :: l ->
        nil :: prim0 I_SWAP :: l
    | a :: l ->
        a :: patch_ep_5_tail l
    | [] ->
        assert false
  in
  (* Replace "NIL operation; SWAP; CONS; DIP {SWAP}; SWAP; CONS"
       by "NIL operation; DIG 3; CONS; SWAP; CONS" *)
  let rec patch_ep_6_tail l =
    match l with
    | (Prim (_, I_NIL, _, _) as nil) :: _ :: cons :: _ :: l ->
        nil :: Prim (0, I_DIG, [Int (0, Z.of_int 3)], []) :: cons :: l
    | a :: l ->
        a :: patch_ep_6_tail l
    | [] ->
        assert false
  in
  let ep_2 =
    Seq
      ( 0,
        (sender :: assert_is_source)
        @ (dup :: car :: car :: assert_is_source)
        @ destruct_seq ep_2 )
  in
  let ep_3 =
    Seq
      ( 0,
        (sender :: assert_is_source)
        @ (dup :: car :: car :: assert_is_source)
        @ (dup :: car :: cdr :: car :: assert_is_source)
        @ destruct_seq ep_3 )
  in
  let ep_4 =
    Seq
      ( 0,
        (sender :: assert_is_source)
        @ (dup :: car :: assert_is_source)
        @ destruct_seq ep_4 )
  in
  let ep_5 =
    Seq
      ( 0,
        (sender :: assert_is_source)
        @ (dup :: car :: dup :: car :: assert_is_source)
        @ (cdr :: assert_is_source)
        @ patch_ep_5_tail (destruct_seq ep_5) )
  in
  let ep_6 =
    Seq
      ( 0,
        (sender :: assert_is_source)
        @ (dup :: car :: cdr :: cdr :: assert_is_source)
        @ (dup :: cdr :: car :: assert_is_source)
        @ patch_ep_6_tail (destruct_seq ep_6) )
  in
  let seq_wrap x = match x with Seq _ -> x | x -> Seq (0, [x]) in
  let if_left a b = Prim (0, I_IF_LEFT, [seq_wrap a; seq_wrap b], []) in
  Seq
    ( 0,
      [ parameter;
        storage;
        Prim
          ( 0,
            K_code,
            [ Seq
                ( 0,
                  [ cast;
                    dup;
                    car;
                    Prim (0, I_DIP, [Seq (0, [cdr])], []);
                    if_left
                      (if_left
                         (if_left ep_1 ep_2)
                         (if_left ep_3 (if_left ep_4 ep_5)))
                      (if_left
                         (if_left ep_6 (if_left ep_7 ep_8))
                         (if_left ep_9 (if_left ep_10 ep_11))) ] ) ],
            [] ) ] )

(* The hash of the script to stitch. *)
let contract_hash : Script_expr_hash.t =
  Script_expr_hash.of_bytes_exn @@ Hex.to_bytes
  @@ `Hex "41ebb2d762dfef4d53b8b4a7277c056d472ee028888c878d8f227787bfffdcae"

let find_with_encoding tree path encoding =
  Raw_context.Tree.find tree path
  >>= function
  | None ->
      return None
  | Some bytes -> (
    match Data_encoding.Binary.of_bytes encoding bytes with
    | Some d ->
        return (Some d)
    | None ->
        failwith "encoding failure" )

let target_code_hash =
  Context_hash.of_b58check_exn
    "CoVXASXxJNRof7q1gxoSGuMsWKNRNcXThtA5MVWCqeCmn4GYN7sc"

(* Patch the contract code of the specific hash *)
let migrate_contract newcode tree =
  Raw_context.Tree.find_tree tree ["data"; "code"]
  >>= function
  | Some code_tree
    when Context_hash.( = ) (Raw_context.Tree.hash code_tree) target_code_hash
    ->
      ( match !newcode with
      | Some code ->
          return code
      | None -> (
          find_with_encoding code_tree [] Script_repr.lazy_expr_encoding
          >>=? function
          | None ->
              assert false (* never happens *)
          | Some code ->
              Script_repr.force_decode code
              >>?= fun (code, _gas_cost) ->
              let bytes =
                Data_encoding.Binary.to_bytes_exn
                  Script_repr.expr_encoding
                  code
              in
              let hash = Script_expr_hash.hash_bytes [bytes] in
              (* only one script is patched, it is used in a few contracts *)
              assert (Script_expr_hash.(hash = contract_hash)) ;
              (* [code] is always the same, therefore [migrated_code] is a constant
                 we could save some time by precomputing it but it won't be called
                 often so we don't mind. *)
              let migrated_code =
                Script_repr.lazy_expr @@ Micheline.strip_locations
                @@ update_contract_script @@ Micheline.root code
              in
              let migrated_code_bytes =
                Data_encoding.Binary.to_bytes_exn
                  Script_repr.lazy_expr_encoding
                  migrated_code
              in
              newcode := Some migrated_code_bytes ;
              return migrated_code_bytes ) )
      >>=? fun migrated_code_bytes ->
      Raw_context.Tree.update tree ["data"; "code"] migrated_code_bytes
      >>=? fun tree ->
      Raw_context.Tree.update
        tree
        ["len"; "code"]
        Data_encoding.(
          Binary.to_bytes_exn int31 (Bytes.length migrated_code_bytes))
      >|=? fun tree -> Some tree
  | _ ->
      return None

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
      (* 1. Baker accounts migration *)
      lwt_log_notice "1. Baker accounts migration"
      >>= fun () ->
      let nonce =
        Operation_hash.hash_bytes
          [ Bytes.of_string
              "Things will not calm down, Daniel Jackson. They will, in fact, \
               calm up." ]
      in
      let ctxt = Raw_context.init_origination_nonce ctxt nonce in
      Storage.Delegates_008.fold
        ctxt
        ~init:(ok (ctxt, []))
        ~f:(fun baker_pkh acc ->
          Lwt.return acc
          >>=? fun (ctxt, bakers) ->
          migrate_baker ctxt baker_pkh
          >|=? fun (ctxt, baker) -> (ctxt, baker :: bakers))
      >>=? fun (ctxt, bakers) ->
      Storage.Delegates_008.clear ctxt
      >>= fun ctxt ->
      (* The number of the bakers are small (< 3000).  We keep them
         in memory to speed up the migration *)
      (* baker_hash -> the tree of baker_hash *)
      build_find_baker_hash_tree ctxt bakers
      >>= fun find_baker_hash_tree ->
      (* public_key_hash -> baker_hash *)
      let find_baker_hash = build_find_baker_hash bakers in
      let find_baker_hash_result pkh =
        match find_baker_hash pkh with
        | Some x ->
            ok x
        | None ->
            let open Raw_context in
            storage_error
              (Missing_key
                 ( ["baker"; "consensus_key_rev_index"]
                   @ Signature.Public_key_hash.to_path pkh ["consensus_key_rev"],
                   Get ))
      in
      (* public_key_hash binary -> baker_hash *)
      let find_baker_hash_by_pkhbytes =
        build_find_baker_hash_by_pkhbytes bakers
      in
      (* public_key binary -> baker_hash *)
      let find_baker_hash_by_pkbytes =
        build_find_baker_hash_by_pkbytes bakers
      in
      let current_cycle = Raw_context.(current_level ctxt).cycle in
      (* Take a snapshot of the consensus keys *)
      lwt_log_notice "Take a snapshot of the consensus keys"
      >>= fun () ->
      Storage.Baker.Consensus_key.snapshot ctxt current_cycle
      >>=? fun ctxt ->
      lwt_log_notice "Take another snapshot of the consensus keys"
      >>= fun () ->
      (* Because the migration runs on the last block of a predecessor protocol,
         we also need to take the snapshot for the following cycle, otherwise
         the baker process which looks-up consensus for the future block would
         fail. *)
      Storage.Baker.Consensus_key.snapshot ctxt (Cycle_repr.succ current_cycle)
      >>=? fun ctxt ->
      (* Migrate storages with references to original baker pkhs *)
      (* Contract.Delegate + Contract.Inactive_delegate + Contract.Delegate_desactivation *)
      lwt_log_notice
        "Contract.Delegate + Contract.Inactive_delegate + \
         Contract.Delegate_desactivation"
      >>= fun () ->
      (* Update Contract.Delegate, Contract.Inactive_delegate,
         Contract.Delegate_desactivation and contract patching in one folding *)
      let code_cache = ref None in
      map_contracts_index
        ctxt
        ~init:(0, [], [])
        (fun key tree (delegates, inactive_bakers, desactivations) ->
          let get_baker_res () =
            let contract =
              (* always succeeds *)
              from_Some (Contract_repr.Index.of_path key)
            in
            match Contract_repr.is_implicit contract with
            | None ->
                Error contract
            | Some baker ->
                Ok baker
          in
          let get_baker () =
            match get_baker_res () with
            | Error contract ->
                let message =
                  Format.asprintf
                    "baker \"%a\" is not implicit account"
                    Contract_repr.pp
                    contract
                in
                failwith message
            | Ok baker ->
                baker
          in
          (* Contract.Delegate *)
          let delegate_key = ["delegate"] in
          Raw_context.Tree.find tree delegate_key
          >>= (function
                | None ->
                    return (false, tree, delegates)
                | Some pkh_bytes ->
                    let delegate =
                      match find_baker_hash_by_pkhbytes pkh_bytes with
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
                        match get_baker_res () with
                        | Error _ ->
                            Some baker_hash
                        | Ok contract_pkh -> (
                          (* Check this the contract is a baker *)
                          match find_baker_hash contract_pkh with
                          | None ->
                              Some baker_hash
                          | Some _ ->
                              (* Baker contracts are no longer self-delegated, so we
                               don't migrate this relation, instead we delete it *)
                              None ) )
                    in
                    let delegate_tree =
                      Option.map
                        (fun bh -> from_Some (find_baker_hash_tree bh))
                        (* never fails *)
                        delegate
                    in
                    Raw_context.Tree.add_or_remove_tree
                      tree
                      delegate_key
                      delegate_tree
                    >|= fun tree -> ok (true, tree, delegates + 1))
          >>=? fun (modified, tree, delegates) ->
          (* Contract.Inactive_delegate -> Baker.Inactive *)
          let inactive_delegate_key = ["inactive_delegate"] in
          Raw_context.Tree.find tree inactive_delegate_key
          >>= (function
                | None ->
                    return (modified, tree, inactive_bakers) (* no update *)
                | Some _ (* "inited" *) ->
                    let baker = get_baker () in
                    let migrated_baker =
                      from_Some (find_baker_hash baker)
                      (* never fails *)
                    in
                    let inactive_bakers = migrated_baker :: inactive_bakers in
                    Raw_context.Tree.remove tree inactive_delegate_key
                    >>= fun tree -> return (true, tree, inactive_bakers))
          >>=? fun (modified, tree, inactive_bakers) ->
          (* Contract.Delegate_desactivation -> Baker.Inactive *)
          let delegate_desactivation_key = ["delegate_desactivation"] in
          Raw_context.Tree.find tree delegate_desactivation_key
          >>= (function
                | None ->
                    return (modified, tree, desactivations) (* no update *)
                | Some cycle_bytes ->
                    let cycle =
                      (* never fails *)
                      from_Some
                        (Data_encoding.Binary.of_bytes
                           Cycle_repr.encoding
                           cycle_bytes)
                    in
                    let baker = get_baker () in
                    let migrated_baker =
                      (* never fails *)
                      from_Some (find_baker_hash baker)
                    in
                    let desactivations =
                      (migrated_baker, cycle) :: desactivations
                    in
                    Raw_context.Tree.remove tree delegate_desactivation_key
                    >>= fun tree -> return (true, tree, desactivations))
          >>=? fun (modified, tree, desactivations) ->
          (* Coontract code patching *)
          migrate_contract code_cache tree
          >|=? function
          | None ->
              (modified, tree, (delegates, inactive_bakers, desactivations))
          | Some tree ->
              (true, tree, (delegates, inactive_bakers, desactivations)))
      >>=? fun (ctxt, nmod, (delegates, inactive_bakers, desactivations)) ->
      lwt_log_notice "%d contracts updated" nmod
      >>= fun () ->
      lwt_log_notice "%d Cotract.Delegate updated" delegates
      >>= fun () ->
      Lwt_list.fold_left_s
        (fun ctxt baker_hash -> Storage.Baker.Inactive.add ctxt baker_hash)
        ctxt
        inactive_bakers
      >>= fun ctxt ->
      lwt_log_notice
        "%d Contract.Inactive_delegate updated"
        (List.length inactive_bakers)
      >>= fun () ->
      Lwt_list.fold_left_s
        (fun ctxt (migrated_baker, cycle) ->
          Storage.Baker.Deactivation.add ctxt migrated_baker cycle)
        ctxt
        desactivations
      >>= fun ctxt ->
      lwt_log_notice
        "%d Contract.Delegate_desactivations updated"
        (List.length desactivations)
      >>= fun () ->
      (* Active_delegates_with_rolls -> Baker.Active_with_rolls *)
      lwt_log_notice "Active_delegates_with_rolls -> Baker.Active_with_rolls"
      >>= fun () ->
      Storage.Active_delegates_with_rolls_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Baker.Active_with_rolls.add ctxt baker >|= ok)
      >>=? fun ctxt ->
      Storage.Active_delegates_with_rolls_008.clear ctxt
      >>= fun ctxt ->
      let preserved = Constants_storage.preserved_cycles ctxt in
      (* Delegates_with_frozen_balance -> Baker.With_frozen_balance *)
      lwt_log_notice
        "Delegates_with_frozen_balance -> Baker.With_frozen_balance"
      >>= fun () ->
      let rec migrate_frozen_balance ctxt cycle =
        lwt_log_notice
          "migrating bakers with frozen balance for cycle %a"
          Cycle_repr.pp
          cycle
        >>= fun () ->
        Storage.Delegates_with_frozen_balance_008.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun pkh acc ->
            Lwt.return acc
            >>=? fun ctxt ->
            Lwt.return (find_baker_hash_result pkh)
            >>=? fun baker ->
            Storage.Baker.With_frozen_balance.add (ctxt, cycle) baker
            >>= fun ctxt -> return ctxt)
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
          ~init:(0, Raw_context.Tree.empty ctxt)
          ~f:(fun key pkbytes_tree (i, acc_tree) ->
            Raw_context.Tree.to_value pkbytes_tree
            >>= function
            | None ->
                assert false
            | Some pkbytes ->
                let baker_hash =
                  (* never fails *)
                  from_Some (find_baker_hash_by_pkbytes pkbytes)
                in
                Raw_context.Tree.add_tree
                  acc_tree
                  key
                  (from_Some (find_baker_hash_tree baker_hash))
                (* never fails *)
                >|= fun acc_tree -> (i + 1, acc_tree))
        >>= fun (i, acc_tree) ->
        Raw_context.update_tree ctxt key acc_tree
        >|=? fun ctxt ->
        Raw_context.Tree.clear root_tree ;
        (i, ctxt)
      in
      (* Roll.Owner *)
      lwt_log_notice "Roll.Owner"
      >>= fun () ->
      rewrite_rolls ctxt ["rolls"; "owner"; "current"] 3
      >>=? fun (n, ctxt) ->
      lwt_log_notice "Roll.Owner: updated %d" n
      >>= fun () ->
      lwt_log_notice "Roll.Owner.Snapshot"
      >>= fun () ->
      (* We split the folding into 2 to save memory consumption *)
      Raw_context.fold
        ~depth:(`Eq 2)
        ctxt
        ["rolls"; "owner"; "snapshot"]
        ~init:[]
        ~f:(fun k _ ks -> Lwt.return (k :: ks))
      >>= fun ks ->
      Lwt_list.fold_left_s
        (fun acc k ->
          Lwt.return acc
          >>=? fun ctxt ->
          lwt_log_notice "Roll.Owner.Snapshot %s" (String.concat "/" k)
          >>= fun () ->
          rewrite_rolls ctxt (["rolls"; "owner"; "snapshot"] @ k) 3
          >>=? fun (n, ctxt) ->
          lwt_log_notice "Roll.Owner.Snapshot %s: %d" (String.concat "/" k) n
          >|= fun () -> ok ctxt)
        (ok ctxt)
        ks
      >>=? fun ctxt ->
      (* Vote.Listings *)
      lwt_log_notice "Vote.Listings"
      >>= fun () ->
      Storage.Vote.Listings_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh listings acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Listings_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Listings.init ctxt baker listings)
      >>=? fun ctxt ->
      (* Vote.Proposals *)
      lwt_log_notice "Vote.Proposals"
      >>= fun () ->
      Storage.Vote.Proposals_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun (proposal, pkh) acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Proposals_008.remove ctxt (proposal, pkh)
          >>= fun ctxt ->
          Storage.Vote.Proposals.add ctxt (proposal, baker)
          >>= fun ctxt -> return ctxt)
      >>=? fun ctxt ->
      (* Vote.Proposals_count *)
      lwt_log_notice "Vote.Proposals_count"
      >>= fun () ->
      Storage.Vote.Proposals_count_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh count acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Proposals_count_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Proposals_count.init ctxt baker count)
      >>=? fun ctxt ->
      (* Vote.Ballots *)
      lwt_log_notice "Vote.Ballots"
      >>= fun () ->
      Storage.Vote.Ballots_008.fold
        ctxt
        ~init:(ok ctxt)
        ~f:(fun pkh ballot acc ->
          Lwt.return acc
          >>=? fun ctxt ->
          Lwt.return (find_baker_hash_result pkh)
          >>=? fun baker ->
          Storage.Vote.Ballots_008.remove ctxt pkh
          >>= fun ctxt -> Storage.Vote.Ballots.init ctxt baker ballot)
      >>=? fun ctxt ->
      (* Storage.Seed.Nonce *)
      lwt_log_notice "Storage.Seed.Nonce"
      >>= fun () ->
      let rec migrate_cycle_nonce ctxt cycle =
        Storage.Cycle_008.Nonce.fold
          (ctxt, cycle)
          ~init:(ok ctxt)
          ~f:(fun level nonce acc ->
            Lwt.return acc
            >>=? fun ctxt ->
            ( match nonce with
            | Unrevealed {nonce_hash; delegate; rewards; fees} ->
                Lwt.return (find_baker_hash_result delegate)
                >>=? fun baker ->
                return
                  (Storage.Cycle.Unrevealed {nonce_hash; baker; rewards; fees})
            | Revealed s ->
                return (Storage.Cycle.Revealed s) )
            >>=? fun nonce ->
            (* because the path has not changed, this will override 008 value *)
            Storage.Cycle.Nonce.update (ctxt, cycle) level nonce
            >>=? fun ctxt -> return ctxt)
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
      lwt_log_notice "2. balance update receipts for baker migration"
      >>= fun () ->
      Storage.Baker.Consensus_key_rev.fold
        ctxt
        ~init:(ok [])
        ~f:(fun pkh baker_hash acc ->
          Lwt.return acc
          >>=? fun updates ->
          let original_contract = Contract_repr.implicit_contract pkh in
          let new_contract = Contract_repr.baker_contract baker_hash in
          Storage.Contract.Balance.get ctxt new_contract
          >>=? fun moved_balance ->
          return
          @@ Receipt_repr.
               ( Contract original_contract,
                 Debited moved_balance,
                 Protocol_migration )
             :: Receipt_repr.
                  ( Contract new_contract,
                    Credited moved_balance,
                    Protocol_migration )
             :: updates)
      >>=? fun baker_balance_updates ->
      (* Add balance updates receipts to be attached on the first block of this
         protocol - see [prepare] function below. Any balance updates attached
         in the migration should use the [Receipt_repr.Protocol_migration]
         as their [update_origin].
      *)
      lwt_log_notice "Add balance updates receipts"
      >>= fun () ->
      let balance_updates = [] in
      Storage.Pending_migration_balance_updates.init
        ctxt
        (balance_updates @ baker_balance_updates)
      >>=? fun ctxt -> return ctxt

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
