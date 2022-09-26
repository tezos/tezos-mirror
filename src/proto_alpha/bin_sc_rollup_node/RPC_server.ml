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

open Tezos_rpc
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

let get_dal_slot_pages store block =
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
  let*! contents_opt_opt =
    Store.Dal_slot_pages.find
      store
      ~primary_key:block
      ~secondary_key:(slot_index, slot_page)
  in
  return
  @@
  match contents_opt_opt with
  | None -> ("Slot page has not been downloaded", None)
  | Some contents_opt -> (
      match contents_opt with
      | None -> ("Slot was not confirmed", None)
      | Some contents -> ("Slot page is available", Some contents))

module type PARAM = sig
  include Sc_rollup_services.PREFIX

  type context

  val context_of_prefix : Node_context.t -> prefix -> context tzresult Lwt.t
end

module Make_directory (S : PARAM) = struct
  open S

  let directory : context tzresult RPC_directory.t ref = ref RPC_directory.empty

  let register service f =
    directory := RPC_directory.register !directory service f

  let register0 service f =
    let open Lwt_result_syntax in
    register (RPC_service.subst0 service) @@ fun ctxt query input ->
    let*? ctxt = ctxt in
    f ctxt query input

  let build_directory node_ctxt =
    !directory
    |> RPC_directory.map (fun prefix -> context_of_prefix node_ctxt prefix)
    |> RPC_directory.prefix prefix
end

module Global_directory = Make_directory (struct
  include Sc_rollup_services.Global

  type context = Node_context.t

  let context_of_prefix node_ctxt () = return node_ctxt
end)

module Local_directory = Make_directory (struct
  include Sc_rollup_services.Local

  type context = Node_context.t

  let context_of_prefix node_ctxt () = return node_ctxt
end)

module Block_directory = Make_directory (struct
  include Sc_rollup_services.Global.Block

  type context = Node_context.t * Block_hash.t

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
    (node_ctxt, block)
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

module Make (PVM : Pvm.S) = struct
  module PVM = PVM
  module Outbox = Outbox.Make (PVM)

  let get_state (node_ctxt : Node_context.t) block_hash =
    let open Lwt_result_syntax in
    let* ctxt = Node_context.checkout_context node_ctxt block_hash in
    let*! state = PVM.State.find ctxt in
    match state with None -> failwith "No state" | Some state -> return state

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
    Block_directory.register0
      Sc_rollup_services.Global.Block.dal_slot_subscriptions
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let*! subs = Store.Dal_slot_subscriptions.find node_ctxt.store block in
    return subs

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.dal_slots
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let*! slots =
      Store.Dal_slots_headers.list_values node_ctxt.store ~primary_key:block
    in
    return slots

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.dal_slot_pages
    @@ fun (node_ctxt, block) () () -> get_dal_slot_pages node_ctxt.store block

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.dal_slot_page
    @@ fun (node_ctxt, block) {index; page} () ->
    get_dal_slot_page node_ctxt.store block index page

  let () =
    Block_directory.register0 Sc_rollup_services.Global.Block.outbox
    @@ fun (node_ctxt, block) () () ->
    let open Lwt_result_syntax in
    let* state = get_state node_ctxt block in
    let*! outbox = PVM.get_outbox state in
    return outbox

  let () =
    Global_directory.register0 Sc_rollup_services.Global.outbox_proof
    @@ fun node_ctxt output () -> Outbox.proof_of_output node_ctxt output

  let register node_ctxt =
    List.fold_left
      (fun dir f -> RPC_directory.merge dir (f node_ctxt))
      RPC_directory.empty
      [
        Global_directory.build_directory;
        Local_directory.build_directory;
        Block_directory.build_directory;
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
