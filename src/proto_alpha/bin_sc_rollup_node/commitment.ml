(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** The rollup node stores and publishes commitments for the PVM every
    [Constants.sc_rollup_commitment_period_in_blocks] levels.

    Every time a finalized block is processed by the rollup node, the latter
    determines whether the last commitment that the node has produced referred
    to [sc_rollup.commitment_period_in_blocks] blocks earlier. For mainnet,
    [sc_rollup.commitment_period_in_blocks = 30]. In this case, it computes and
    stores a new commitment in a level-indexed map.

    Stored commitments are signed by the rollup node operator
    and published on the layer1 chain. To ensure that commitments
    produced by the rollup node are eventually published,
    storing and publishing commitments are decoupled. Every time
    a new head is processed, the node tries to publish the oldest
    commitment that was not published already.
*)

open Protocol
open Alpha_context

module type Mutable_level_store =
  Store_sigs.Mutable_value
    with type value = Raw_level.t
     and type store = Store.t

(* We persist the number of ticks to be included in the
   next commitment on disk, in a map that is indexed by
   inbox level. Note that we do not risk to increase
   these counters when the wrong branch is tracked by the rollup
   node, as only finalized heads are processed to build commitments.
*)
module Number_of_ticks :
  Store_sigs.Map
    with type store = Store.t
     and type key = Raw_level.t
     and type value = Z.t =
  Store.Make_append_only_map
    (struct
      let path = ["commitments"; "in_progress"; "number_of_ticks"]

      (* We only access the number of ticks for either the
         current or previous level being processed by the
         commitment module. Therefore, by keeping the
         last two entries in memory, we ensure that
         the information about ticks is always recovered
         from the main memory. *)
      let keep_last_n_entries_in_memory = Some 2
    end)
    (struct
      type key = Raw_level.t

      let to_path_representation key = Int32.to_string @@ Raw_level.to_int32 key
    end)
    (struct
      type value = Z.t

      let name = "ticks"

      let encoding = Data_encoding.z
    end)

let sc_rollup_commitment_period node_ctxt =
  Int32.of_int
  @@ node_ctxt.Node_context.protocol_constants.parametric.sc_rollup
       .commitment_period_in_blocks

let sc_rollup_challenge_window node_ctxt =
  Int32.of_int
    node_ctxt.Node_context.protocol_constants.parametric.sc_rollup
      .challenge_window_in_blocks

let last_commitment_level (module Last_commitment_level : Mutable_level_store)
    store =
  Last_commitment_level.find store

let last_commitment_with_hash
    (module Last_commitment_level : Mutable_level_store) store =
  let open Lwt_option_syntax in
  let* last_commitment_level =
    last_commitment_level (module Last_commitment_level) store
  in
  let*! commitment_with_hash =
    Store.Commitments.get store last_commitment_level
  in
  return commitment_with_hash

let next_commitment_level node_ctxt
    (module Last_commitment_level : Mutable_level_store) =
  let open Lwt_syntax in
  let+ last_commitment_level_opt =
    last_commitment_level
      (module Last_commitment_level)
      node_ctxt.Node_context.store
  in
  let last_commitment_level =
    Option.value last_commitment_level_opt ~default:node_ctxt.genesis_info.level
  in
  Raw_level.of_int32
  @@ Int32.add
       (Raw_level.to_int32 last_commitment_level)
       (sc_rollup_commitment_period node_ctxt)

let last_commitment_hash node_ctxt
    (module Last_commitment_level : Mutable_level_store) =
  let open Lwt_syntax in
  let+ last_commitment =
    last_commitment_with_hash
      (module Last_commitment_level)
      node_ctxt.Node_context.store
  in
  match last_commitment with
  | Some (_commitment, hash) -> hash
  | None -> node_ctxt.genesis_info.Sc_rollup.Commitment.commitment_hash

let must_store_commitment node_ctxt current_level =
  let open Lwt_result_syntax in
  let+ next_commitment_level =
    next_commitment_level node_ctxt (module Store.Last_stored_commitment_level)
  in

  Raw_level.equal current_level next_commitment_level

let update_last_stored_commitment store (commitment : Sc_rollup.Commitment.t) =
  let open Lwt_syntax in
  let commitment_hash = Sc_rollup.Commitment.hash_uncarbonated commitment in
  let inbox_level = commitment.inbox_level in
  let* lcc_level = Store.Last_cemented_commitment_level.get store in
  (* Do not change the order of these two operations. This guarantees that
     whenever `Store.Last_stored_commitment_level.get` returns `Some hash`,
     then the call to `Store.Commitments.get hash` will succeed.
  *)
  let* () =
    Store.Commitments.add store inbox_level (commitment, commitment_hash)
  in
  let* () = Store.Last_stored_commitment_level.set store inbox_level in
  let* () = Commitment_event.commitment_stored commitment_hash commitment in
  if commitment.inbox_level <= lcc_level then
    Commitment_event.commitment_will_not_be_published lcc_level commitment
  else return ()

module Make (PVM : Pvm.S) : Commitment_sig.S with module PVM = PVM = struct
  module PVM = PVM

  let build_commitment node_ctxt block_hash =
    let open Lwt_result_syntax in
    let lsc =
      (module Store.Last_stored_commitment_level : Mutable_level_store)
    in
    let*! predecessor = last_commitment_hash node_ctxt lsc in
    let* inbox_level =
      Lwt.map Environment.wrap_tzresult @@ next_commitment_level node_ctxt lsc
    in
    let* ctxt = Node_context.checkout_context node_ctxt block_hash in
    let*! pvm_state = PVM.State.find ctxt in
    let* compressed_state =
      match pvm_state with
      | Some pvm_state ->
          let*! hash = PVM.state_hash pvm_state in
          return hash
      | None ->
          failwith
            "PVM state for block hash not available %s"
            (Block_hash.to_string block_hash)
    in
    let*! number_of_ticks = Number_of_ticks.get node_ctxt.store inbox_level in
    let+ number_of_ticks =
      match
        Sc_rollup.Number_of_ticks.of_value @@ Z.to_int64 number_of_ticks
      with
      | Some number_of_ticks -> return number_of_ticks
      | None ->
          failwith "Invalid number of ticks %s" (Z.to_string number_of_ticks)
    in
    Sc_rollup.Commitment.
      {predecessor; inbox_level; number_of_ticks; compressed_state}

  let store_commitment_if_necessary node_ctxt current_level block_hash =
    let open Lwt_result_syntax in
    let* must_store_commitment =
      Lwt.map Environment.wrap_tzresult
      @@ must_store_commitment node_ctxt current_level
    in
    if must_store_commitment then
      let*! () = Commitment_event.compute_commitment block_hash current_level in
      let* commitment = build_commitment node_ctxt block_hash in
      let*! () = update_last_stored_commitment node_ctxt.store commitment in
      return_unit
    else return_unit

  let update_ticks (node_ctxt : Node_context.t) current_level block_hash =
    let open Lwt_result_syntax in
    let*! last_stored_commitment_level_opt =
      last_commitment_level
        (module Store.Last_stored_commitment_level)
        node_ctxt.store
    in
    let last_stored_commitment_level =
      Option.value
        ~default:node_ctxt.genesis_info.level
        last_stored_commitment_level_opt
    in
    let*! previous_level_num_ticks =
      match Raw_level.pred current_level with
      | None ->
          (* This happens if the current_level is zero: it is safe to assume
             that there are 0 ticks computed so far. *)
          Lwt.return Z.zero
      | Some level ->
          if Raw_level.(level = last_stored_commitment_level) then
            (* We are at the first level of a new commitment, so the initial amount
               of ticks should be 0. *)
            Lwt.return Z.zero
          else if Raw_level.(level < node_ctxt.genesis_info.level) then
            (* If the previous level was before the genesis level, then the
               number of ticks at that level should be 0. *)
            Lwt.return Z.zero
          else
            (* Otherwise we need to increment the number of ticks from the number
               of ticks for the previous level. The number of ticks for such a
               level should be in the store, otherwise the state of the rollup node
               is corrupted. *)
            Number_of_ticks.get node_ctxt.store level
    in
    let*! {num_ticks; _} = Store.StateInfo.get node_ctxt.store block_hash in
    Number_of_ticks.add
      node_ctxt.store
      current_level
      (Z.add previous_level_num_ticks num_ticks)

  let process_head (node_ctxt : Node_context.t) Layer1.{level; hash} =
    let open Lwt_result_syntax in
    let current_level = Raw_level.of_int32_exn level in
    let*! () = update_ticks node_ctxt current_level hash in
    store_commitment_if_necessary node_ctxt current_level hash

  let sync_last_cemented_commitment_hash_with_level
      ({cctxt; rollup_address; store; _} : Node_context.t) =
    let open Lwt_result_syntax in
    let* hash, inbox_level =
      Plugin.RPC.Sc_rollup.last_cemented_commitment_hash_with_level
        cctxt
        (cctxt#chain, cctxt#block)
        rollup_address
    in
    let*! () = Store.Last_cemented_commitment_level.set store inbox_level in
    let*! () = Store.Last_cemented_commitment_hash.set store hash in
    let*! () =
      Commitment_event.last_cemented_commitment_updated hash inbox_level
    in
    return_unit

  let get_commitment_and_publish ~check_lcc_hash
      ({store; _} as node_ctxt : Node_context.t) next_level_to_publish =
    let open Lwt_result_syntax in
    let*! is_commitment_available =
      Store.Commitments.mem store next_level_to_publish
    in
    if is_commitment_available then
      let*! commitment, _commitment_hash =
        Store.Commitments.get store next_level_to_publish
      in
      let* () =
        if check_lcc_hash then
          let open Lwt_result_syntax in
          let*! lcc_hash = Store.Last_cemented_commitment_hash.get store in
          if Sc_rollup.Commitment.Hash.equal lcc_hash commitment.predecessor
          then return ()
          else
            let*! () =
              Commitment_event.commitment_parent_is_not_lcc
                commitment.inbox_level
                commitment.predecessor
                lcc_hash
            in
            tzfail
              (Sc_rollup_node_errors.Commitment_predecessor_should_be_LCC
                 commitment)
        else return_unit
      in
      let operator = Node_context.get_operator node_ctxt Publish in
      match operator with
      | None ->
          (* Configured to not publish commitments *)
          return_unit
      | Some source ->
          let publish_operation =
            Sc_rollup_publish {rollup = node_ctxt.rollup_address; commitment}
          in
          let* () = Injector.add_pending_operation ~source publish_operation in
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/3462
             Decouple commitments from head processing

             Move the following, in a part where we know the operation is
             included. *)
          let*! () =
            Store.Last_published_commitment_level.set
              store
              commitment.inbox_level
          in
          return_unit
    else return_unit

  let publish_commitment node_ctxt =
    let open Lwt_result_syntax in
    (* Check level of next publishable commitment and avoid publishing if it is
       on or before the last cemented commitment.
    *)
    let* next_lcc_level =
      Lwt.map Environment.wrap_tzresult
      @@ next_commitment_level
           node_ctxt
           (module Store.Last_cemented_commitment_level)
    in
    let* next_publishable_level =
      Lwt.map Environment.wrap_tzresult
      @@ next_commitment_level
           node_ctxt
           (module Store.Last_published_commitment_level)
    in
    let check_lcc_hash, level_to_publish =
      if Raw_level.(next_publishable_level < next_lcc_level) then
        (*

           This situation can happen if the rollup node has been
           shutdown and the rollup has been progressing in the
           meantime. In that case, the rollup node must wait to reach
           [lcc_level + commitment_frequency] to publish the
           commitment. ([lcc_level] is a multiple of
           commitment_frequency.)

           We need to check that the published commitment comes
           immediately after the last cemented commitment, otherwise
           that's an invalid commitment.

        *)
        (true, next_lcc_level)
      else (false, next_publishable_level)
    in
    get_commitment_and_publish node_ctxt level_to_publish ~check_lcc_hash

  let earliest_cementing_level node_ctxt commitment_hash =
    let open Lwt_option_syntax in
    let+ published_at_level =
      Store.Commitments_published_at_level.find
        node_ctxt.Node_context.store
        commitment_hash
    in
    Int32.add
      (Raw_level.to_int32 published_at_level)
      (sc_rollup_challenge_window node_ctxt)

  let can_be_cemented node_ctxt earliest_cementing_level head_level
      commitment_hash =
    let {Node_context.cctxt; rollup_address; _} = node_ctxt in
    let open Lwt_result_syntax in
    if earliest_cementing_level <= head_level then
      Plugin.RPC.Sc_rollup.can_be_cemented
        cctxt
        (cctxt#chain, cctxt#block)
        rollup_address
        commitment_hash
        ()
    else return_false

  let cement_commitment (node_ctxt : Node_context.t) commitment_hash =
    let open Lwt_result_syntax in
    let operator = Node_context.get_operator node_ctxt Cement in
    match operator with
    | None ->
        (* Configured to not cement commitments *)
        return_unit
    | Some source ->
        let cement_operation =
          Sc_rollup_cement
            {rollup = node_ctxt.rollup_address; commitment = commitment_hash}
        in
        Injector.add_pending_operation ~source cement_operation

  let cement_commitment_if_possible node_ctxt Layer1.{level = head_level; _} =
    let open Lwt_result_syntax in
    let* next_level_to_cement =
      Lwt.map Environment.wrap_tzresult
      @@ next_commitment_level
           node_ctxt
           (module Store.Last_cemented_commitment_level)
    in
    let*! commitment_with_hash =
      Store.Commitments.find node_ctxt.store next_level_to_cement
    in
    match commitment_with_hash with
    (* If `commitment_with_hash` is defined, the commitment to be cemented has
       been stored but not necessarily published by the rollup node. *)
    | Some (_commitment, commitment_hash) -> (
        let*! earliest_cementing_level =
          earliest_cementing_level node_ctxt commitment_hash
        in
        match earliest_cementing_level with
        (* If `earliest_cementing_level` is well defined, then the rollup node
           has previously published `commitment`, which means that the rollup
           is indirectly staked on it. *)
        | Some earliest_cementing_level ->
            let* green_flag =
              can_be_cemented
                node_ctxt
                earliest_cementing_level
                head_level
                commitment_hash
            in
            if green_flag then cement_commitment node_ctxt commitment_hash
            else return ()
        | None -> return ())
    | None -> return ()

  let start () = Commitment_event.starting ()
end
