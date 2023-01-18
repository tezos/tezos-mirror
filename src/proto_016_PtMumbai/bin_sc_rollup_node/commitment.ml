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

let add_level level increment =
  (* We only use this function with positive increments so it is safe *)
  if increment < 0 then invalid_arg "Commitment.add_level negative increment" ;
  Raw_level.Internal_for_tests.add level increment

let sc_rollup_commitment_period node_ctxt =
  node_ctxt.Node_context.protocol_constants.parametric.sc_rollup
    .commitment_period_in_blocks

let sc_rollup_challenge_window node_ctxt =
  node_ctxt.Node_context.protocol_constants.parametric.sc_rollup
    .challenge_window_in_blocks

let next_lcc_level node_ctxt =
  add_level
    node_ctxt.Node_context.lcc.level
    (sc_rollup_commitment_period node_ctxt)

(** Returns the next level for which a commitment can be published, i.e. the
    level that is [commitment_period] blocks after the last published one. It
    returns [None] if this level is not finalized because we only publish
    commitments for inbox of finalized L1 blocks. *)
let next_publishable_level node_ctxt =
  let open Lwt_option_syntax in
  let lpc_level =
    match node_ctxt.Node_context.lpc with
    | None -> node_ctxt.genesis_info.level
    | Some lpc -> lpc.inbox_level
  in
  let next_level =
    add_level lpc_level (sc_rollup_commitment_period node_ctxt)
  in
  let* finalized_level = State.get_finalized_head_opt node_ctxt.store in
  if Raw_level.(of_int32_exn finalized_level.level < next_level) then fail
  else return next_level

let next_commitment_level node_ctxt last_commitment_level =
  add_level last_commitment_level (sc_rollup_commitment_period node_ctxt)

module Make (PVM : Pvm.S) : Commitment_sig.S with module PVM = PVM = struct
  module PVM = PVM

  let tick_of_level (node_ctxt : _ Node_context.t) inbox_level =
    let open Lwt_result_syntax in
    let* block_hash =
      State.hash_of_level node_ctxt (Raw_level.to_int32 inbox_level)
    in
    let*! block = Store.L2_blocks.get node_ctxt.store block_hash in
    return (Sc_rollup_block.final_tick block)

  let build_commitment (node_ctxt : _ Node_context.t)
      (prev_commitment : Sc_rollup.Commitment.Hash.t) ~prev_commitment_level
      ~inbox_level ctxt =
    let open Lwt_result_syntax in
    let*! pvm_state = PVM.State.find ctxt in
    let*? pvm_state =
      match pvm_state with
      | Some pvm_state -> Ok pvm_state
      | None ->
          error_with
            "PVM state for commitment at level %a is not available"
            Raw_level.pp
            inbox_level
    in
    let*! compressed_state = PVM.state_hash pvm_state in
    let*! tick = PVM.get_tick pvm_state in
    let* prev_commitment_tick = tick_of_level node_ctxt prev_commitment_level in
    let number_of_ticks =
      Sc_rollup.Tick.distance tick prev_commitment_tick
      |> Z.to_int64 |> Sc_rollup.Number_of_ticks.of_value
    in
    let*? number_of_ticks =
      match number_of_ticks with
      | Some number_of_ticks ->
          if number_of_ticks = Sc_rollup.Number_of_ticks.zero then
            error_with "A 0-tick commitment is impossible"
          else Ok number_of_ticks
      | None -> error_with "Invalid number of ticks for commitment"
    in
    return
      Sc_rollup.Commitment.
        {
          predecessor = prev_commitment;
          inbox_level;
          number_of_ticks;
          compressed_state;
        }

  let create_commitment_if_necessary (node_ctxt : _ Node_context.t) ~predecessor
      current_level ctxt =
    let open Lwt_result_syntax in
    if Raw_level.(current_level = node_ctxt.genesis_info.level) then
      let+ genesis_commitment =
        Plugin.RPC.Sc_rollup.commitment
          node_ctxt.cctxt
          (node_ctxt.cctxt#chain, `Head 0)
          node_ctxt.rollup_address
          node_ctxt.genesis_info.commitment_hash
      in
      Some genesis_commitment
    else
      let* last_commitment_hash =
        let*! pred = Store.L2_blocks.find node_ctxt.store predecessor in
        match pred with
        | None -> failwith "Missing block %a" Block_hash.pp predecessor
        | Some pred ->
            return (Sc_rollup_block.most_recent_commitment pred.header)
      in
      let*! last_commitment =
        Store.Commitments.get node_ctxt.store last_commitment_hash
      in
      let next_commitment_level =
        next_commitment_level node_ctxt last_commitment.inbox_level
      in
      if Raw_level.(current_level = next_commitment_level) then
        let*! () = Commitment_event.compute_commitment current_level in
        let+ commitment =
          build_commitment
            node_ctxt
            last_commitment_hash
            ~prev_commitment_level:last_commitment.inbox_level
            ~inbox_level:current_level
            ctxt
        in
        Some commitment
      else return_none

  let process_head (node_ctxt : _ Node_context.t) ~predecessor Layer1.{level; _}
      ctxt =
    let open Lwt_result_syntax in
    let current_level = Raw_level.of_int32_exn level in
    let* commitment =
      create_commitment_if_necessary node_ctxt ~predecessor current_level ctxt
    in
    match commitment with
    | None -> return_none
    | Some commitment ->
        let commitment_hash =
          Sc_rollup.Commitment.hash_uncarbonated commitment
        in
        let*! () =
          Store.Commitments.add node_ctxt.store commitment_hash commitment
        in
        return_some commitment_hash

  let block_of_known_level (node_ctxt : _ Node_context.t) level =
    let open Lwt_option_syntax in
    let* head = State.last_processed_head_opt node_ctxt.store in
    if Raw_level.(head.header.level < level) then
      (* Level is not known yet  *)
      fail
    else
      let*! block_hash =
        State.hash_of_level node_ctxt (Raw_level.to_int32 level)
      in
      let*? block_hash = Result.to_option block_hash in
      Store.L2_blocks.find node_ctxt.store block_hash

  let get_commitment_and_publish ~check_lcc_hash node_ctxt next_level_to_publish
      =
    let open Lwt_result_syntax in
    let*! commitment =
      let open Lwt_option_syntax in
      let* block = block_of_known_level node_ctxt next_level_to_publish in
      let*? commitment_hash = block.header.commitment_hash in
      Store.Commitments.find node_ctxt.store commitment_hash
    in
    match commitment with
    | None ->
        (* Commitment not available *)
        return_unit
    | Some commitment -> (
        let* () =
          if check_lcc_hash then
            let open Lwt_result_syntax in
            if
              Sc_rollup.Commitment.Hash.equal
                node_ctxt.lcc.commitment
                commitment.predecessor
            then return ()
            else
              let*! () =
                Commitment_event.commitment_parent_is_not_lcc
                  commitment.inbox_level
                  commitment.predecessor
                  node_ctxt.lcc.commitment
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
            let* _hash =
              Injector.add_pending_operation ~source publish_operation
            in
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/3462
               Decouple commitments from head processing

               Move the following, in a part where we know the operation is
               included. *)
            node_ctxt.lpc <- Some commitment ;
            return_unit)

  let publish_commitment node_ctxt =
    let open Lwt_result_syntax in
    (* Check level of next publishable commitment and avoid publishing if it is
       on or before the last cemented commitment.
    *)
    let next_lcc_level = next_lcc_level node_ctxt in
    let*! next_publishable_level = next_publishable_level node_ctxt in
    match next_publishable_level with
    | None -> return_unit
    | Some next_publishable_level ->
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
    add_level published_at_level (sc_rollup_challenge_window node_ctxt)

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
    else return_false

  let cement_commitment (node_ctxt : _ Node_context.t) commitment_hash =
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
        let* _hash = Injector.add_pending_operation ~source cement_operation in
        return_unit

  let cement_commitment_if_possible node_ctxt Layer1.{level = head_level; _} =
    let open Lwt_result_syntax in
    let next_level_to_cement = next_lcc_level node_ctxt in
    let*! block = block_of_known_level node_ctxt next_level_to_cement in
    match block with
    | None | Some {header = {commitment_hash = None; _}; _} ->
        (* Commitment not available *)
        return_unit
    | Some {header = {commitment_hash = Some commitment_hash; _}; _} -> (
        (* If `commitment_hash` is defined, the commitment to be cemented has
           been stored but not necessarily published by the rollup node. *)
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
                (Raw_level.of_int32_exn head_level)
                commitment_hash
            in
            if green_flag then cement_commitment node_ctxt commitment_hash
            else return_unit
        | None -> return_unit)

  let start () = Commitment_event.starting ()
end
