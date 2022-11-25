(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Tezos_rpc_http
open Tezos_rpc_http_server
open Protocol

let get_head store =
  let open Lwt_result_syntax in
  let*! head = State.last_processed_head_opt store in
  match head with None -> failwith "No head" | Some {hash; _} -> return hash

let get_finalized store =
  let open Lwt_result_syntax in
  let*! head = State.get_finalized_head_opt store in
  match head with
  | None -> failwith "No finalized head"
  | Some {hash; _} -> return hash

let get_last_cemented store =
  let open Lwt_result_syntax in
  protect @@ fun () ->
  let*! lcc_level = Store.Last_cemented_commitment_level.get store in
  let*! lcc_hash =
    State.hash_of_level store (Alpha_context.Raw_level.to_int32 lcc_level)
  in
  return lcc_hash

let get_head_hash_opt store =
  let open Lwt_option_syntax in
  let+ {hash; _} = State.last_processed_head_opt store in
  hash

let get_head_level_opt store =
  let open Lwt_option_syntax in
  let+ {level; _} = State.last_processed_head_opt store in
  level

let get_state_info_exn store block =
  let open Lwt_result_syntax in
  let*! state = Store.StateInfo.get store block in
  return state

module Slot_pages_map = struct
  open Protocol
  open Alpha_context
  include Map.Make (Dal.Slot_index)
end

let get_dal_confirmed_slot_pages store block =
  let open Lwt_result_syntax in
  let*! slot_pages =
    Store.Dal_slot_pages.list_secondary_keys_with_values
      store
      ~primary_key:block
  in
  (* Slot pages are sorted in lexicographic order of slot index and page
     number.*)
  let slot_rev_pages_map =
    List.fold_left
      (fun map ((index, _page), page) ->
        Slot_pages_map.update
          index
          (function None -> Some [page] | Some pages -> Some (page :: pages))
          map)
      Slot_pages_map.empty
      slot_pages
  in
  let slot_pages_map =
    Slot_pages_map.map (fun pages -> List.rev pages) slot_rev_pages_map
  in
  return @@ Slot_pages_map.bindings slot_pages_map

let get_dal_slot_page store block slot_index slot_page =
  let open Lwt_result_syntax in
  let*! processed =
    Store.Dal_processed_slots.find
      store
      ~primary_key:block
      ~secondary_key:slot_index
  in
  match processed with
  | None -> return ("Slot page has not been downloaded", None)
  | Some `Unconfirmed -> return ("Slot was not confirmed", None)
  | Some `Confirmed -> (
      let*! contents_opt =
        Store.Dal_slot_pages.find
          store
          ~primary_key:block
          ~secondary_key:(slot_index, slot_page)
      in
      match contents_opt with
      | None -> assert false
      | Some _contents -> return ("Slot page is available", contents_opt))

module type PARAM = sig
  include Sc_rollup_services.PREFIX

  type context

  val context_of_prefix : Node_context.rw -> prefix -> context tzresult Lwt.t
end

module Make_directory (S : PARAM) = struct
  open S

  let directory : context tzresult Tezos_rpc.Directory.t ref =
    ref Tezos_rpc.Directory.empty

  let register service f =
    directory := Tezos_rpc.Directory.register !directory service f

  let register0 service f =
    let open Lwt_result_syntax in
    register (Tezos_rpc.Service.subst0 service) @@ fun ctxt query input ->
    let*? ctxt = ctxt in
    f ctxt query input

  let register1 service f =
    let open Lwt_result_syntax in
    register (Tezos_rpc.Service.subst1 service)
    @@ fun (ctxt, arg) query input ->
    let*? ctxt = ctxt in
    f ctxt arg query input

  let build_directory node_ctxt =
    !directory
    |> Tezos_rpc.Directory.map (fun prefix ->
           context_of_prefix node_ctxt prefix)
    |> Tezos_rpc.Directory.prefix prefix
end

module Global_directory = Make_directory (struct
  include Sc_rollup_services.Global

  type context = Node_context.ro

  let context_of_prefix node_ctxt () = return (Node_context.readonly node_ctxt)
end)

module Proof_helpers_directory = Make_directory (struct
  include Sc_rollup_services.Global.Helpers

  (* The context needs to be accessed with write permissions because we need to
     commit on disk to generate the proofs. *)
  type context = Node_context.rw

  let context_of_prefix node_ctxt () = return node_ctxt
end)

module Local_directory = Make_directory (struct
  include Sc_rollup_services.Local

  type context = Node_context.ro

  let context_of_prefix node_ctxt () = return (Node_context.readonly node_ctxt)
end)

module Block_directory = Make_directory (struct
  include Sc_rollup_services.Global.Block

  type context = Node_context.ro * Tezos_crypto.Block_hash.t

  let context_of_prefix node_ctxt (((), block) : prefix) =
    let open Lwt_result_syntax in
    let+ block =
      match block with
      | `Head -> get_head node_ctxt.Node_context.store
      | `Hash b -> return b
      | `Level l -> State.hash_of_level node_ctxt.store l >>= return
      | `Finalized -> get_finalized node_ctxt.Node_context.store
      | `Cemented -> get_last_cemented node_ctxt.Node_context.store
    in
    (Node_context.readonly node_ctxt, block)
end)

module Outbox_directory = Make_directory (struct
  include Sc_rollup_services.Global.Block.Outbox

  type context =
    Node_context.ro * Tezos_crypto.Block_hash.t * Alpha_context.Raw_level.t

  let context_of_prefix node_ctxt (((), block), level) =
    let open Lwt_result_syntax in
    let+ block =
      match block with
      | `Head -> get_head node_ctxt.Node_context.store
      | `Hash b -> return b
      | `Level l -> State.hash_of_level node_ctxt.store l >>= return
      | `Finalized -> get_finalized node_ctxt.Node_context.store
      | `Cemented -> get_last_cemented node_ctxt.Node_context.store
    in
    (Node_context.readonly node_ctxt, block, level)
end)

module Common = struct
  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.num_messages
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let+ state_info = get_state_info_exn node_ctxt.store block in
    state_info.num_messages

  let () =
    Global_directory.register0 Sc_rollup_services.Global.sc_rollup_address
    @@ fun node_ctxt () () -> return @@ node_ctxt.rollup_address

  let () =
    Global_directory.register0 Sc_rollup_services.Global.current_tezos_head
    @@ fun node_ctxt () () -> get_head_hash_opt node_ctxt.store >>= return

  let () =
    Global_directory.register0 Sc_rollup_services.Global.current_tezos_level
    @@ fun node_ctxt () () -> get_head_level_opt node_ctxt.store >>= return

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.hash
    @@ fun (_node_ctxt, block) () () -> return block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.level
    @@ fun (node_ctxt, block) () () -> State.level_of_hash node_ctxt.store block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.inbox
    @@ fun (node_ctxt, block) () () -> Inbox.inbox_of_hash node_ctxt block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.ticks
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let+ state = get_state_info_exn node_ctxt.store block in
    state.num_ticks
end

module Make (Simulation : Simulation.S) (Batcher : Batcher.S) = struct
  module PVM = Simulation.PVM
  module Interpreter = Simulation.Interpreter
  module Outbox = Outbox.Make (PVM)
  module Free_pvm = Interpreter.Free_pvm

  let get_state (node_ctxt : _ Node_context.t) block_hash =
    let open Lwt_result_syntax in
    let* ctxt = Node_context.checkout_context node_ctxt block_hash in
    let*! state = PVM.State.find ctxt in
    match state with None -> failwith "No state" | Some state -> return state

  let simulate_messages (node_ctxt : Node_context.ro) block ~reveal_pages
      messages =
    let open Lwt_result_syntax in
    let open Alpha_context in
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
    let* level = State.level_of_hash node_ctxt.store block in
    let* sim =
      Simulation.start_simulation
        node_ctxt
        ~reveal_map
        Layer1.{hash = block; level}
    in
    let messages =
      List.map (fun m -> Sc_rollup.Inbox_message.External m) messages
    in
    let* sim, num_ticks_0 =
      Simulation.simulate_messages node_ctxt sim messages
    in
    let* {state; inbox_level; _}, num_ticks_end =
      Simulation.end_simulation node_ctxt sim
    in
    let num_ticks = Z.(num_ticks_0 + num_ticks_end) in
    let*! outbox = PVM.get_outbox inbox_level state in
    let output =
      List.filter
        (fun Sc_rollup.{outbox_level; _} -> outbox_level = inbox_level)
        outbox
    in
    let*! state_hash = PVM.state_hash state in
    let*! status = PVM.get_status state in
    let status = PVM.string_of_status status in
    return
      Sc_rollup_services.{state_hash; status; output; inbox_level; num_ticks}

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.total_ticks
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let* state = get_state node_ctxt block in
    let*! tick = PVM.get_tick state in
    return tick

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.state_hash
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let* state = get_state node_ctxt block in
    let*! hash = PVM.state_hash state in
    return hash

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.state_value
    @@ fun (node_ctxt, block) {key} () ->
    let open Lwt_result_syntax in
    let* state = get_state node_ctxt block in
    let path = String.split_on_char '/' key in
    let*! value = PVM.State.lookup state path in
    match value with
    | None -> failwith "No such key in PVM state"
    | Some value ->
        Format.eprintf "Encoded %S\n@.%!" (Bytes.to_string value) ;
        return value

  let () =
    Global_directory.register0 Sc_rollup_services.Global.last_stored_commitment
    @@ fun node_ctxt () () ->
    let open Lwt_result_syntax in
    let*! commitment_with_hash =
      Commitment.last_commitment_with_hash
        (module Store.Last_stored_commitment_level)
        node_ctxt.store
    in
    return
      (commitment_with_hash
      |> Option.map (fun (commitment, hash) -> (commitment, hash, None)))

  let () =
    Local_directory.register0 Sc_rollup_services.Local.last_published_commitment
    @@ fun node_ctxt () () ->
    let open Lwt_result_syntax in
    let*! result =
      let open Lwt_option_syntax in
      let* commitment, hash =
        Commitment.last_commitment_with_hash
          (module Store.Last_published_commitment_level)
          node_ctxt.store
      in
      (* The corresponding level in Store.Commitments.published_at_level is
         available only when the commitment has been published and included
         in a block. *)
      let*! published_at_level =
        Store.Commitments_published_at_level.find node_ctxt.store hash
      in
      return (commitment, hash, published_at_level)
    in
    return result

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.status
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let* state = get_state node_ctxt block in
    let*! status = PVM.get_status state in
    return (PVM.string_of_status status)

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.dal_slots
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let*! slots =
      Store.Dal_slots_headers.list_values node_ctxt.store ~primary_key:block
    in
    return slots

  let () =
    Block_directory.register0
      Sc_rollup_services.Global.Block.dal_confirmed_slot_pages
    @@ fun (node_ctxt, block) () () ->
    get_dal_confirmed_slot_pages node_ctxt.store block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.dal_slot_page
    @@ fun (node_ctxt, block) {index; page} () ->
    get_dal_slot_page node_ctxt.store block index page

  let () =
    Outbox_directory.register0 Sc_rollup_services.Global.Block.Outbox.messages
    @@ fun (node_ctxt, block, outbox_level) () () ->
    let open Lwt_result_syntax in
    let* state = get_state node_ctxt block in
    let*! outbox = PVM.get_outbox outbox_level state in
    return outbox

  let () =
    Proof_helpers_directory.register0
      Sc_rollup_services.Global.Helpers.outbox_proof
    @@ fun node_ctxt output () -> Outbox.proof_of_output node_ctxt output

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.simulate
    @@ fun (node_ctxt, block) () {messages; reveal_pages} ->
    simulate_messages node_ctxt block ~reveal_pages messages

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
  let commitment_level_of_inbox_level (node_ctxt : _ Node_context.t) inbox_level
      =
    let open Alpha_context in
    let open Lwt_option_syntax in
    let+ last_published =
      Store.Last_published_commitment_level.find node_ctxt.store
    in
    let commitment_period =
      Int32.of_int
        node_ctxt.protocol_constants.parametric.sc_rollup
          .commitment_period_in_blocks
    in
    let last_published = Raw_level.to_int32 last_published in
    let open Int32 in
    div (sub last_published inbox_level) commitment_period
    |> mul commitment_period |> sub last_published |> Raw_level.of_int32_exn

  let inbox_info_of_level (node_ctxt : _ Node_context.t) inbox_level =
    let open Alpha_context in
    let open Lwt_syntax in
    let* lcc = Store.Last_cemented_commitment_level.find node_ctxt.store in
    let+ finalized_head = State.get_finalized_head_opt node_ctxt.store in
    let finalized =
      match finalized_head with
      | None -> false
      | Some {level = finalized_level; _} ->
          Compare.Int32.(inbox_level <= finalized_level)
    in
    let cemented =
      match lcc with
      | None -> false
      | Some lcc -> Compare.Int32.(inbox_level <= Raw_level.to_int32 lcc)
    in
    Sc_rollup_services.{finalized; cemented}

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
              | Some (Injected info) ->
                  return (Sc_rollup_services.Injected info)
              | Some (Included info) -> (
                  let*! inbox_info =
                    inbox_info_of_level node_ctxt info.l1_level
                  in
                  let*! commitment_level =
                    commitment_level_of_inbox_level node_ctxt info.l1_level
                  in
                  match commitment_level with
                  | None ->
                      return (Sc_rollup_services.Included (info, inbox_info))
                  | Some commitment_level -> (
                      let*! commitment =
                        Store.Commitments.find node_ctxt.store commitment_level
                      in
                      match commitment with
                      | None ->
                          (* Commitment not computed yet for inbox *)
                          return
                            (Sc_rollup_services.Included (info, inbox_info))
                      | Some (commitment, commitment_hash) -> (
                          (* Commitment computed *)
                          let*! published_at =
                            Store.Commitments_published_at_level.find
                              node_ctxt.store
                              commitment_hash
                          in
                          match published_at with
                          | None ->
                              (* Commitment not published yet *)
                              return
                                (Sc_rollup_services.Included (info, inbox_info))
                          | Some published_at ->
                              (* Commitment published *)
                              let commitment_info =
                                Sc_rollup_services.
                                  {commitment; commitment_hash; published_at}
                              in
                              return
                                (Sc_rollup_services.Committed
                                   (info, inbox_info, commitment_info)))))))
    in

    return status

  let register node_ctxt =
    List.fold_left
      (fun dir f -> Tezos_rpc.Directory.merge dir (f node_ctxt))
      Tezos_rpc.Directory.empty
      [
        Global_directory.build_directory;
        Local_directory.build_directory;
        Block_directory.build_directory;
        Proof_helpers_directory.build_directory;
        Outbox_directory.build_directory;
      ]

  let start node_ctxt configuration =
    let open Lwt_result_syntax in
    let Configuration.{rpc_addr; rpc_port; _} = configuration in
    let rpc_addr = P2p_addr.of_string_exn rpc_addr in
    let host = Ipaddr.V6.to_string rpc_addr in
    let node = `TCP (`Port rpc_port) in
    let acl = RPC_server.Acl.default rpc_addr in
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
end
