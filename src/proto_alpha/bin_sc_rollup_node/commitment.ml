(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** The rollup node stores and publishes commitments for the PVM 
    every 20 levels.

    Every time a finalized block is processed  by the rollup node, 
    the latter determines whether the last commitment that the node 
    has produced referred to 20 blocks earlier. In this case, it 
    computes and stores a new commitment in a level-indexed map. 

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
  Store.Mutable_value with type value = Raw_level.t

(* We keep the number of messages and ticks to be included in the
   next commitment in memory. Note that we do not risk to increase
   these counters when the wrong branch is tracked by the rollup
   node, as only finalized heads are processed to build commitments.
*)

module Mutable_counter = struct
  module Make () = struct
    let x = ref Z.zero

    let add z = x := Z.add !x z

    let reset () = x := Z.zero

    let get () = !x
  end
end

module Number_of_messages = Mutable_counter.Make ()

module Number_of_ticks = Mutable_counter.Make ()

let sc_rollup_commitment_frequency =
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2977
     Use effective on-chain protocol parameter. *)
  Int32.of_int
    Default_parameters.constants_mainnet
      .sc_rollup_commitment_frequency_in_blocks

let last_commitment_level (module Last_commitment_level : Mutable_level_store)
    store =
  Last_commitment_level.find store

let last_commitment (module Last_commitment_level : Mutable_level_store) store =
  let open Lwt_syntax in
  let* last_commitment_level =
    last_commitment_level (module Last_commitment_level) store
  in
  match last_commitment_level with
  | Some level ->
      let+ commitment = Store.Commitments.get store level in
      Some commitment
  | None -> return None

let next_commitment_level (module Last_commitment_level : Mutable_level_store)
    ~origination_level store =
  let open Lwt_syntax in
  let+ last_commitment_level_opt =
    last_commitment_level (module Last_commitment_level) store
  in
  let last_commitment_level =
    Option.value last_commitment_level_opt ~default:origination_level
  in
  Raw_level.of_int32
  @@ Int32.add
       (Raw_level.to_int32 last_commitment_level)
       sc_rollup_commitment_frequency

let last_commitment_hash (module Last_commitment_level : Mutable_level_store)
    store =
  let open Lwt_syntax in
  let+ last_commitment = last_commitment (module Last_commitment_level) store in
  match last_commitment with
  | Some commitment -> Sc_rollup.Commitment.hash commitment
  | None -> Sc_rollup.Commitment_hash.zero

let must_store_commitment ~origination_level current_level store =
  let open Lwt_result_syntax in
  let+ next_commitment_level =
    next_commitment_level
      (module Store.Last_stored_commitment_level)
      ~origination_level
      store
  in
  Raw_level.equal current_level next_commitment_level

let update_last_stored_commitment store (commitment : Sc_rollup.Commitment.t) =
  let open Lwt_syntax in
  let inbox_level = commitment.inbox_level in
  let* lcc_level = Store.Last_cemented_commitment_level.get store in
  (* Do not change the order of these two operations. This guarantees that
     whenever `Store.Last_stored_commitment_level.get` returns `Some hash`,
     then the call to `Store.Commitments.get hash` will succeed.
  *)
  let* () = Store.Commitments.add store inbox_level commitment in
  let* () = Store.Last_stored_commitment_level.set store inbox_level in
  let* () = Commitment_event.commitment_stored commitment in
  if commitment.inbox_level <= lcc_level then
    Commitment_event.commitment_will_not_be_published lcc_level commitment
  else return ()

module type S = sig
  module PVM : Pvm.S

  (** [process_head node_ctxt store head] checks whether a new
      commitment needs to be computed and stored, by looking at the level of
      [head] and checking whether it is a multiple of 20 levels away from
      [node_ctxt.initial_level]. It uses the functionalities of [PVM] to
      compute the hash of to be included in the commitment.
  *)

  val process_head :
    Node_context.t -> Store.t -> Layer1.head -> unit tzresult Lwt.t

  (** [get_last_cemented_commitment_hash_with_level node_ctxt store] 
      fetches and stores information about the last cemented commitment 
      in the layer1 chain.
    *)
  val get_last_cemented_commitment_hash_with_level :
    Node_context.t -> Store.t -> unit tzresult Lwt.t

  (** [publish_commitment node_ctxt store] publishes the earliest commitment
      stored in [store] that has not been published yet, unless its inbox level
      is below or equal to the inbox level of the last cemented commitment in
      the layer1 chain. In this case, the rollup node checks whether it has
      computed a commitment whose inbox level is
      [sc_rollup_commitment_frequency] levels after the inbox level of the last
      cemented commitment: 
      {ul
      {li if the commitment is found and its predecessor hash coincides with
       the hash of the LCC, the rollup node will try to publish that commitment
      instead; }
      {li if the commitment is found but its predecessor hash differs from the
        hash of the LCC, the rollup node will stop its execution;}
      {li if no commitment is found, no action is taken by the rollup node;
        in particular, no commitment is published.}
    }
  *)
  val publish_commitment : Node_context.t -> Store.t -> unit tzresult Lwt.t

  (** [start ()] only emits the event that the commitment manager
      for the rollup node has started. *)
  val start : unit -> unit Lwt.t
end

module Make (PVM : Pvm.S) : S with module PVM = PVM = struct
  module PVM = PVM

  let build_commitment ~origination_level store block_hash =
    let open Lwt_result_syntax in
    let lsc =
      (module Store.Last_stored_commitment_level : Mutable_level_store)
    in
    let*! predecessor = last_commitment_hash lsc store in
    let* inbox_level =
      Lwt.map Environment.wrap_tzresult
      @@ next_commitment_level ~origination_level lsc store
    in
    let*! pvm_state = Store.PVMState.find store block_hash in
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
    let number_of_messages = Number_of_messages.get () in
    let* number_of_messages =
      match
        Sc_rollup.Number_of_messages.of_int32 @@ Z.to_int32 number_of_messages
      with
      | Some number_of_messages -> return number_of_messages
      | None ->
          failwith
            "Invalid number of messages %s"
            (Z.to_string number_of_messages)
    in
    let number_of_ticks = Number_of_ticks.get () in
    let+ number_of_ticks =
      match
        Sc_rollup.Number_of_ticks.of_int32 @@ Z.to_int32 number_of_ticks
      with
      | Some number_of_ticks -> return number_of_ticks
      | None ->
          failwith "Invalid number of ticks %s" (Z.to_string number_of_ticks)
    in
    (* Reset counters for messages as the commitment to be published
       has been built.
    *)
    let () = Number_of_messages.reset () in
    let () = Number_of_ticks.reset () in
    Sc_rollup.Commitment.
      {
        predecessor;
        inbox_level;
        number_of_messages;
        number_of_ticks;
        compressed_state;
      }

  let store_commitment_if_necessary ~origination_level store current_level
      block_hash =
    let open Lwt_result_syntax in
    let* must_store_commitment =
      Lwt.map Environment.wrap_tzresult
      @@ must_store_commitment ~origination_level current_level store
    in
    if must_store_commitment then
      let*! () = Commitment_event.compute_commitment block_hash current_level in
      let* commitment = build_commitment ~origination_level store block_hash in
      let*! () = update_last_stored_commitment store commitment in
      return_unit
    else return_unit

  let update_ticks_and_messages store block_hash =
    let open Lwt_result_syntax in
    let*! {num_messages; num_ticks} = Store.StateInfo.get store block_hash in
    let () = Number_of_messages.add num_messages in
    return @@ Number_of_ticks.add num_ticks

  let process_head (node_ctxt : Node_context.t) store
      Layer1.(Head {level; hash}) =
    let open Lwt_result_syntax in
    let current_level = Raw_level.of_int32_exn level in
    let origination_level = node_ctxt.initial_level in
    let* () = update_ticks_and_messages store hash in
    store_commitment_if_necessary ~origination_level store current_level hash

  let get_last_cemented_commitment_hash_with_level
      ({cctxt; rollup_address; _} : Node_context.t) store =
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
      ({cctxt; rollup_address; _} as node_ctxt : Node_context.t)
      next_level_to_publish store =
    let open Lwt_result_syntax in
    let*! is_commitment_available =
      Store.Commitments.mem store next_level_to_publish
    in
    if is_commitment_available then
      let*! commitment = Store.Commitments.get store next_level_to_publish in
      let*! () =
        if check_lcc_hash then
          let open Lwt_syntax in
          let* lcc_hash = Store.Last_cemented_commitment_hash.get store in
          if Sc_rollup.Commitment_hash.equal lcc_hash commitment.predecessor
          then return ()
          else
            let+ () =
              Commitment_event.commitment_parent_is_not_lcc
                commitment.inbox_level
                commitment.predecessor
                lcc_hash
            in
            exit 1
        else Lwt.return ()
      in
      let* source, src_pk, src_sk = Node_context.get_operator_keys node_ctxt in
      let* _, _, Manager_operation_result {operation_result; _} =
        Client_proto_context.sc_rollup_publish
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~commitment
          ~source
          ~rollup:rollup_address
          ~src_pk
          ~src_sk
          ~fee_parameter:Configuration.default_fee_parameter
          ()
      in
      let open Apply_results in
      let*! () =
        match operation_result with
        | Applied (Sc_rollup_publish_result _) ->
            let open Lwt_syntax in
            let* () =
              Store.Last_published_commitment_level.set
                store
                commitment.inbox_level
            in
            Commitment_event.commitment_published commitment
        | Failed (Sc_rollup_publish_manager_kind, _errors) ->
            Commitment_event.commitment_failed commitment
        | Backtracked (Sc_rollup_publish_result _, _errors) ->
            Commitment_event.commitment_backtracked commitment
        | Skipped Sc_rollup_publish_manager_kind ->
            Commitment_event.commitment_skipped commitment
      in
      return_unit
    else return_unit

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2869
     use the Injector to publish commitments. *)
  let publish_commitment node_ctxt store =
    let open Lwt_result_syntax in
    let open Node_context in
    let origination_level = node_ctxt.initial_level in
    (* Check level of next publishable commitment and avoid publishing if it is
       on or before the last cemented commitment.
    *)
    let* next_lcc_level =
      Lwt.map Environment.wrap_tzresult
      @@ next_commitment_level
           (module Store.Last_cemented_commitment_level)
           ~origination_level
           store
    in
    let* next_publishable_level =
      Lwt.map Environment.wrap_tzresult
      @@ next_commitment_level
           (module Store.Last_published_commitment_level)
           ~origination_level
           store
    in
    let check_lcc_hash, level_to_publish =
      if Raw_level.(next_publishable_level < next_lcc_level) then
        (true, next_lcc_level)
      else (false, next_publishable_level)
    in
    get_commitment_and_publish node_ctxt level_to_publish store ~check_lcc_hash

  let start () = Commitment_event.starting ()
end
