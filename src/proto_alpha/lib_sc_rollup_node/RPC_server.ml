(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

open Tezos_rpc_http
open Tezos_rpc_http_server
open RPC_directory_helpers
open Protocol

let get_head_hash_opt node_ctxt =
  let open Lwt_result_syntax in
  let+ res = Node_context.last_processed_head_opt node_ctxt in
  Option.map
    (fun Sc_rollup_block.{header = {block_hash; _}; _} -> block_hash)
    res

let get_head_level_opt node_ctxt =
  let open Lwt_result_syntax in
  let+ res = Node_context.last_processed_head_opt node_ctxt in
  Option.map (fun Sc_rollup_block.{header = {level; _}; _} -> level) res

module Slot_pages_map = struct
  open Protocol
  open Alpha_context
  include Map.Make (Dal.Slot_index)
end

let get_dal_processed_slots node_ctxt block =
  Node_context.list_slots_statuses node_ctxt ~confirmed_in_block_hash:block

module Global_directory = Make_directory (struct
  include Sc_rollup_services.Global

  type context = Node_context.rw

  type subcontext = Node_context.ro

  let context_of_prefix node_ctxt () = return (Node_context.readonly node_ctxt)
end)

module Proof_helpers_directory = Make_directory (struct
  include Sc_rollup_services.Global.Helpers

  (* The context needs to be accessed with write permissions because we need to
     commit on disk to generate the proofs. *)
  type context = Node_context.rw

  (* The context needs to be accessed with write permissions because we need to
     commit on disk to generate the proofs. *)
  type subcontext = Node_context.rw

  let context_of_prefix node_ctxt () = return node_ctxt
end)

module Local_directory = Make_directory (struct
  include Sc_rollup_services.Local

  type context = Node_context.rw

  type subcontext = Node_context.ro

  let context_of_prefix node_ctxt () = return (Node_context.readonly node_ctxt)
end)

module Block_directory = Make_directory (struct
  include Sc_rollup_services.Global.Block

  type context = Node_context.rw

  type subcontext = Node_context.ro * Block_hash.t

  let context_of_prefix node_ctxt (((), block) : prefix) =
    let open Lwt_result_syntax in
    let+ block = Block_directory_helpers.block_of_prefix node_ctxt block in
    (Node_context.readonly node_ctxt, block)
end)

module Outbox_directory = Make_directory (struct
  include Sc_rollup_services.Global.Block.Outbox

  type context = Node_context.rw

  type subcontext = Node_context.ro * Block_hash.t * Alpha_context.Raw_level.t

  let context_of_prefix node_ctxt (((), block), level) =
    let open Lwt_result_syntax in
    let+ block = Block_directory_helpers.block_of_prefix node_ctxt block in
    (Node_context.readonly node_ctxt, block, level)
end)

module Common = struct
  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.block
    @@ fun (node_ctxt, block) () () ->
    Node_context.get_full_l2_block node_ctxt block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.num_messages
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let* l2_block = Node_context.get_l2_block node_ctxt block in
    let+ num_messages =
      Node_context.get_num_messages
        node_ctxt
        (Sc_rollup_proto_types.Merkelized_payload_hashes_hash.of_octez
           l2_block.header.inbox_witness)
    in
    Z.of_int num_messages

  let () =
    Global_directory.register0 Sc_rollup_services.Global.sc_rollup_address
    @@ fun node_ctxt () () -> return @@ node_ctxt.rollup_address

  let () =
    Global_directory.register0 Sc_rollup_services.Global.current_tezos_head
    @@ fun node_ctxt () () -> get_head_hash_opt node_ctxt

  let () =
    Global_directory.register0 Sc_rollup_services.Global.current_tezos_level
    @@ fun node_ctxt () () -> get_head_level_opt node_ctxt

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.hash
    @@ fun (_node_ctxt, block) () () -> return block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.level
    @@ fun (node_ctxt, block) () () ->
    Node_context.level_of_hash node_ctxt block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.inbox
    @@ fun (node_ctxt, block) () () ->
    Node_context.get_inbox_by_block_hash node_ctxt block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.ticks
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let+ l2_block = Node_context.get_l2_block node_ctxt block in
    Z.of_int64 l2_block.num_ticks
end

let get_state (node_ctxt : _ Node_context.t) block_hash =
  let open Lwt_result_syntax in
  let* ctxt = Node_context.checkout_context node_ctxt block_hash in
  let*! state = Context.PVMState.find ctxt in
  match state with None -> failwith "No state" | Some state -> return state

let simulate_messages (node_ctxt : Node_context.ro) block ~reveal_pages
    ~insight_requests messages =
  let open Lwt_result_syntax in
  let open Alpha_context in
  let module PVM = (val node_ctxt.pvm) in
  let reveal_map =
    match reveal_pages with
    | Some pages ->
        let map =
          List.fold_left
            (fun map page ->
              let hash =
                Sc_rollup_reveal_hash.(hash_string ~scheme:Blake2B [page])
              in
              Sc_rollup_reveal_hash.Map.add hash page map)
            Sc_rollup_reveal_hash.Map.empty
            pages
        in
        Some map
    | None -> None
  in
  let* level = Node_context.level_of_hash node_ctxt block in
  let* sim =
    Simulation.start_simulation
      node_ctxt
      ~reveal_map
      Layer1.{hash = block; level}
  in
  let* sim, num_ticks_0 = Simulation.simulate_messages node_ctxt sim messages in
  let* {state; inbox_level; _}, num_ticks_end =
    Simulation.end_simulation node_ctxt sim
  in
  let*! insights =
    List.map_p
      (function
        | Sc_rollup_services.Pvm_state_key key -> PVM.State.lookup state key
        | Durable_storage_key key -> PVM.Inspect_durable_state.lookup state key)
      insight_requests
  in
  let num_ticks = Z.(num_ticks_0 + num_ticks_end) in
  let*! outbox = PVM.get_outbox inbox_level state in
  let output =
    List.filter
      (fun Sc_rollup.{outbox_level; _} -> outbox_level = inbox_level)
      outbox
  in
  let*! state_hash = PVM.state_hash state in
  let* parametric_constants =
    let cctxt = node_ctxt.cctxt in
    Protocol.Constants_services.parametric cctxt (cctxt#chain, `Level level)
  in
  let*! status =
    PVM.get_status
      ~is_reveal_enabled:
        (Sc_rollup.is_reveal_enabled_predicate
           parametric_constants.sc_rollup.reveal_activation_level)
      state
  in
  let status = PVM.string_of_status status in
  return
    Sc_rollup_services.
      {state_hash; status; output; inbox_level; num_ticks; insights}

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.total_ticks
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let module PVM = (val node_ctxt.pvm) in
  let*! tick = PVM.get_tick state in
  return tick

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.state_hash
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let module PVM = (val node_ctxt.pvm) in
  let*! hash = PVM.state_hash state in
  return hash

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.state_current_level
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let module PVM = (val node_ctxt.pvm) in
  let*! current_level = PVM.get_current_level state in
  return current_level

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.state_value
  @@ fun (node_ctxt, block) {key} () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let path = String.split_on_char '/' key in
  let*! value = Context.PVMState.lookup state path in
  match value with
  | None -> failwith "No such key in PVM state"
  | Some value ->
      Format.eprintf "Encoded %S\n@.%!" (Bytes.to_string value) ;
      return value

let () =
  Global_directory.register0 Sc_rollup_services.Global.last_stored_commitment
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  let* head = Node_context.last_processed_head_opt node_ctxt in
  match head with
  | None -> return_none
  | Some head ->
      let commitment_hash =
        Sc_rollup_block.most_recent_commitment head.header
        |> Sc_rollup_proto_types.Commitment_hash.of_octez
      in
      let+ commitment =
        Node_context.find_commitment node_ctxt commitment_hash
      in
      Option.map (fun c -> (c, commitment_hash)) commitment

let () =
  Local_directory.register0 Sc_rollup_services.Local.last_published_commitment
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  match Reference.get node_ctxt.lpc with
  | None -> return_none
  | Some commitment ->
      let hash =
        Alpha_context.Sc_rollup.Commitment.hash_uncarbonated commitment
      in
      (* The corresponding level in Store.Commitments.published_at_level is
         available only when the commitment has been published and included
         in a block. *)
      let* published_at_level_info =
        Node_context.commitment_published_at_level node_ctxt hash
      in
      let first_published, published =
        match published_at_level_info with
        | None -> (None, None)
        | Some {first_published_at_level; published_at_level} ->
            (Some first_published_at_level, published_at_level)
      in
      return_some (commitment, hash, first_published, published)

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.status
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let module PVM = (val node_ctxt.pvm) in
  let* current_level = Node_context.level_of_hash node_ctxt block in
  let* parametric_constants =
    let cctxt = node_ctxt.cctxt in
    Protocol.Constants_services.parametric
      cctxt
      (cctxt#chain, `Level current_level)
  in
  let*! status =
    PVM.get_status
      ~is_reveal_enabled:
        (Alpha_context.Sc_rollup.is_reveal_enabled_predicate
           parametric_constants.sc_rollup.reveal_activation_level)
      state
  in
  return (PVM.string_of_status status)

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.dal_slots
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* slots =
    Node_context.get_all_slot_headers node_ctxt ~published_in_block_hash:block
  in
  return slots

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.dal_processed_slots
  @@ fun (node_ctxt, block) () () -> get_dal_processed_slots node_ctxt block

let () =
  Outbox_directory.register0 Sc_rollup_services.Global.Block.Outbox.messages
  @@ fun (node_ctxt, block, outbox_level) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let module PVM = (val node_ctxt.pvm) in
  let*! outbox = PVM.get_outbox outbox_level state in
  return outbox

let () =
  Proof_helpers_directory.register0
    Sc_rollup_services.Global.Helpers.outbox_proof
  @@ fun node_ctxt output () -> Outbox.proof_of_output node_ctxt output

let () =
  Block_directory.register0 Sc_rollup_services.Global.Block.simulate
  @@ fun (node_ctxt, block) () {messages; reveal_pages; insight_requests} ->
  let messages =
    List.map Alpha_context.Sc_rollup.Inbox_message.unsafe_of_string messages
  in
  simulate_messages node_ctxt block ~reveal_pages ~insight_requests messages

let () =
  Local_directory.register0 Sc_rollup_services.Local.injection
  @@ fun _node_ctxt () messages -> Batcher.register_messages messages

let () =
  Local_directory.register0 Sc_rollup_services.Local.batcher_queue
  @@ fun _node_ctxt () () ->
  let open Lwt_result_syntax in
  let*? queue = Batcher.get_queue () in
  return queue

(** [commitment_level_of_inbox_level node_ctxt inbox_level] returns the level
      of the commitment which should include the inbox of level
      [inbox_level].

      It is computed with the following formula:
      {v
      commitment_level(inbox_level) =
        last_commitment -
         ((last_commitment - inbox_level) / commitment_period
          * commitment_period)
      v}
  *)
let commitment_level_of_inbox_level (node_ctxt : _ Node_context.t) inbox_level =
  let open Alpha_context in
  let open Option_syntax in
  let+ last_published_commitment = Reference.get node_ctxt.lpc in
  let commitment_period =
    Int32.of_int
      node_ctxt.protocol_constants.parametric.sc_rollup
        .commitment_period_in_blocks
  in
  let last_published =
    Raw_level.to_int32 last_published_commitment.inbox_level
  in
  let open Int32 in
  div (sub last_published inbox_level) commitment_period
  |> mul commitment_period |> sub last_published |> Raw_level.of_int32_exn

let inbox_info_of_level (node_ctxt : _ Node_context.t) inbox_level =
  let open Alpha_context in
  let open Lwt_result_syntax in
  let+ finalized_level = Node_context.get_finalized_level node_ctxt in
  let finalized = Compare.Int32.(inbox_level <= finalized_level) in
  let lcc = Reference.get node_ctxt.lcc in
  let cemented = Compare.Int32.(inbox_level <= Raw_level.to_int32 lcc.level) in
  (finalized, cemented)

let () =
  Local_directory.register1 Sc_rollup_services.Local.batcher_message
  @@ fun node_ctxt hash () () ->
  let open Lwt_result_syntax in
  let*? batch_status = Batcher.message_status hash in
  let* status =
    match batch_status with
    | None -> return (None, Sc_rollup_services.Unknown)
    | Some (batch_status, msg) -> (
        let return status = return (Some msg, status) in
        match batch_status with
        | Pending_batch -> return Sc_rollup_services.Pending_batch
        | Batched l1_hash -> (
            match Injector.operation_status l1_hash with
            | None -> return Sc_rollup_services.Unknown
            | Some (Pending op) ->
                return (Sc_rollup_services.Pending_injection op)
            | Some (Injected {op; oph; op_index}) ->
                return
                  (Sc_rollup_services.Injected
                     {op = op.operation; oph; op_index})
            | Some (Included {op; oph; op_index; l1_block; l1_level}) -> (
                let* finalized, cemented =
                  inbox_info_of_level node_ctxt l1_level
                in
                let commitment_level =
                  commitment_level_of_inbox_level node_ctxt l1_level
                in
                match commitment_level with
                | None ->
                    return
                      (Sc_rollup_services.Included
                         {
                           op = op.operation;
                           oph;
                           op_index;
                           l1_block;
                           l1_level;
                           finalized;
                           cemented;
                         })
                | Some commitment_level -> (
                    let* block =
                      Node_context.find_l2_block_by_level
                        node_ctxt
                        (Alpha_context.Raw_level.to_int32 commitment_level)
                    in
                    match block with
                    | None ->
                        (* Commitment not computed yet for inbox *)
                        return
                          (Sc_rollup_services.Included
                             {
                               op = op.operation;
                               oph;
                               op_index;
                               l1_block;
                               l1_level;
                               finalized;
                               cemented;
                             })
                    | Some block -> (
                        let commitment_hash =
                          WithExceptions.Option.get
                            ~loc:__LOC__
                            block.header.commitment_hash
                          |> Sc_rollup_proto_types.Commitment_hash.of_octez
                        in
                        (* Commitment computed *)
                        let* published_at =
                          Node_context.commitment_published_at_level
                            node_ctxt
                            commitment_hash
                        in
                        match published_at with
                        | None | Some {published_at_level = None; _} ->
                            (* Commitment not published yet *)
                            return
                              (Sc_rollup_services.Included
                                 {
                                   op = op.operation;
                                   oph;
                                   op_index;
                                   l1_block;
                                   l1_level;
                                   finalized;
                                   cemented;
                                 })
                        | Some
                            {
                              first_published_at_level;
                              published_at_level = Some published_at_level;
                            } ->
                            (* Commitment published *)
                            let* commitment =
                              Node_context.get_commitment
                                node_ctxt
                                commitment_hash
                            in
                            return
                              (Sc_rollup_services.Committed
                                 {
                                   op = op.operation;
                                   oph;
                                   op_index;
                                   l1_block;
                                   l1_level;
                                   finalized;
                                   cemented;
                                   commitment;
                                   commitment_hash;
                                   first_published_at_level;
                                   published_at_level;
                                 }))))))
  in

  return status

let register (node_ctxt : _ Node_context.t) =
  let module PVM = (val Pvm_rpc.of_kind node_ctxt.kind) in
  List.fold_left
    (fun dir f -> Tezos_rpc.Directory.merge dir (f node_ctxt))
    Tezos_rpc.Directory.empty
    [
      Global_directory.build_directory;
      Local_directory.build_directory;
      Block_directory.build_directory;
      Proof_helpers_directory.build_directory;
      Outbox_directory.build_directory;
      PVM.build_directory;
    ]

let start node_ctxt configuration =
  let open Lwt_result_syntax in
  let Configuration.{rpc_addr; rpc_port; _} = configuration in
  let rpc_addr = P2p_addr.of_string_exn rpc_addr in
  let host = Ipaddr.V6.to_string rpc_addr in
  let node = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let dir = register node_ctxt in
  let server =
    RPC_server.init_server dir ~acl ~media_types:Media_type.all_media_types
  in
  protect @@ fun () ->
  let*! () =
    RPC_server.launch
      ~host
      server
      ~callback:(RPC_server.resto_callback server)
      node
  in
  return server

let shutdown = RPC_server.shutdown
