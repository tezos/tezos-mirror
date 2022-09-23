(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let get_head_opt store = State.last_processed_head_opt store

let get_head_hash_opt store =
  let open Lwt_option_syntax in
  let+ {hash; _} = State.last_processed_head_opt store in
  hash

let get_head_level_opt store =
  let open Lwt_option_syntax in
  let+ {level; _} = State.last_processed_head_opt store in
  level

let get_state_info_exn store =
  let open Lwt_result_syntax in
  let* head = get_head store in
  let*! state = Store.StateInfo.get store head in
  return state

let get_dal_slot_subscriptions_exn store =
  let open Lwt_result_syntax in
  let* head = get_head store in
  let*! slot_subscriptions = Store.Dal_slot_subscriptions.find store head in
  match slot_subscriptions with
  | None -> failwith "No slot subscriptions"
  | Some slot_subscriptions -> return slot_subscriptions

let get_dal_slots store =
  let open Lwt_result_syntax in
  let* head = get_head store in
  let*! slot_headers =
    Store.Dal_slots_headers.list_values store ~primary_key:head
  in
  return slot_headers

module Slot_pages_map = struct
  open Protocol
  open Alpha_context
  include Map.Make (Dal.Slot_index)
end

let get_dal_slot_pages store =
  let open Lwt_result_syntax in
  let* head = get_head store in
  let*! slot_pages =
    Store.Dal_slot_pages.list_secondary_keys_with_values store ~primary_key:head
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

let get_dal_slot_page store slot_index slot_page =
  let open Lwt_result_syntax in
  let* head = get_head store in
  let*! contents_opt_opt =
    Store.Dal_slot_pages.find
      store
      ~primary_key:head
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

let commitment_with_hash commitment =
  ( Protocol.Alpha_context.Sc_rollup.Commitment.hash_uncarbonated commitment,
    commitment )

module Common = struct
  let register_current_num_messages store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_num_messages
      (fun () () ->
        let open Lwt_result_syntax in
        let* state_info = get_state_info_exn store in
        return state_info.num_messages)

  let register_sc_rollup_address configuration dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.sc_rollup_address
      (fun () () -> return @@ configuration.Configuration.sc_rollup_address)

  let register_current_tezos_head store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_tezos_head
      (fun () () -> get_head_hash_opt store >>= return)

  let register_current_tezos_level store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_tezos_level
      (fun () () -> get_head_level_opt store >>= return)

  let register_current_inbox node_ctxt dir =
    RPC_directory.opt_register0
      dir
      Sc_rollup_services.Global.current_inbox
      (fun () () ->
        let open Lwt_result_syntax in
        let*! head = get_head_hash_opt node_ctxt.Node_context.store in
        match head with
        | Some head_hash ->
            let* inbox = Inbox.inbox_of_hash node_ctxt head_hash in
            return_some inbox
        | None -> return_none)

  let register_current_ticks store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_ticks
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state_info_exn store in
        return state.num_ticks)

  let start configuration dir =
    let open Lwt_result_syntax in
    let Configuration.{rpc_addr; rpc_port; _} = configuration in
    let rpc_addr = P2p_addr.of_string_exn rpc_addr in
    let host = Ipaddr.V6.to_string rpc_addr in
    let node = `TCP (`Port rpc_port) in
    let acl = RPC_server.Acl.default rpc_addr in
    let server =
      RPC_server.init_server dir ~acl ~media_types:Media_type.all_media_types
    in
    Lwt.catch
      (fun () ->
        let*! () =
          RPC_server.launch
            ~host
            server
            ~callback:(RPC_server.resto_callback server)
            node
        in
        return server)
      fail_with_exn

  let shutdown = RPC_server.shutdown
end

module type S = sig
  module PVM : Pvm.S

  val shutdown : RPC_server.server -> unit Lwt.t

  val register : Node_context.t -> Configuration.t -> unit RPC_directory.t

  val start :
    Node_context.t -> Configuration.t -> RPC_server.server tzresult Lwt.t
end

module Make (PVM : Pvm.S) : S with module PVM = PVM = struct
  include Common
  module PVM = PVM
  module Outbox = Outbox.Make (PVM)

  let get_context ?block_hash (node_ctxt : Node_context.t) =
    let open Lwt_result_syntax in
    let* block_hash =
      match block_hash with
      | None -> get_head node_ctxt.store
      | Some block_hash -> return block_hash
    in
    let* ctxt = Node_context.checkout_context node_ctxt block_hash in
    return ctxt

  let get_state ?block_hash (node_ctxt : Node_context.t) =
    let open Lwt_result_syntax in
    let* ctxt = get_context ?block_hash node_ctxt in
    let*! state = PVM.State.find ctxt in
    match state with None -> failwith "No state" | Some state -> return state

  let register_current_total_ticks node_ctxt dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_total_ticks
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state node_ctxt in
        let*! tick = PVM.get_tick state in
        return tick)

  let register_current_state_hash node_ctxt dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_state_hash
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state node_ctxt in
        let*! hash = PVM.state_hash state in
        return hash)

  let register_current_state_value node_ctxt dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Local.current_state_value
      (fun {key} () ->
        let open Lwt_result_syntax in
        let* state = get_state node_ctxt in
        let path = String.split_on_char '/' key in
        let*! value = PVM.State.lookup state path in
        match value with
        | None -> failwith "No such key in PVM state"
        | Some value ->
            Format.eprintf "Encoded %S\n@.%!" (Bytes.to_string value) ;
            return value)

  let register_last_stored_commitment store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.last_stored_commitment
      (fun () () ->
        let open Lwt_result_syntax in
        let*! commitment_with_hash =
          Commitment.last_commitment_with_hash
            (module Store.Last_stored_commitment_level)
            store
        in
        return
          (commitment_with_hash
          |> Option.map (fun (commitment, hash) -> (commitment, hash, None))))

  let register_last_published_commitment store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Local.last_published_commitment
      (fun () () ->
        let open Lwt_result_syntax in
        let*! result =
          let open Lwt_option_syntax in
          let* commitment, hash =
            Commitment.last_commitment_with_hash
              (module Store.Last_published_commitment_level)
              store
          in
          (* The corresponding level in Store.Commitments.published_at_level is
             available only when the commitment has been published and included
             in a block. *)
          let*! published_at_level =
            Store.Commitments_published_at_level.find store hash
          in
          return (commitment, hash, published_at_level)
        in
        return result)

  let register_current_status node_ctxt dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_status
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state node_ctxt in
        let*! status = PVM.get_status state in
        return (PVM.string_of_status status))

  let register_dal_slot_subscriptions store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.dal_slot_subscriptions
      (fun () () -> get_dal_slot_subscriptions_exn store)

  let register_dal_slots store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.dal_slots
      (fun () () -> get_dal_slots store)

  let register_current_outbox node_ctxt dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.current_outbox
      (fun () () ->
        let open Lwt_result_syntax in
        let store = node_ctxt.Node_context.store in
        let*! lcc_level = Store.Last_cemented_commitment_level.get store in
        let*! block_hash =
          State.hash_of_level store (Alpha_context.Raw_level.to_int32 lcc_level)
        in
        let* state = get_state ~block_hash node_ctxt in
        let*! outbox = PVM.get_outbox state in
        return outbox)

  let register_outbox_proof node_ctxt dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.outbox_proof
      (fun output () -> Outbox.proof_of_output node_ctxt output)

  let register_dal_slot_pages store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.dal_slot_pages
      (fun () () -> get_dal_slot_pages store)

  let register_dal_slot_page store dir =
    RPC_directory.register0
      dir
      Sc_rollup_services.Global.dal_slot_page
      (fun {index; page} () -> get_dal_slot_page store index page)

  let register (node_ctxt : Node_context.t) configuration =
    RPC_directory.empty
    |> register_sc_rollup_address configuration
    |> register_current_tezos_head node_ctxt.store
    |> register_current_inbox node_ctxt
    |> register_current_ticks node_ctxt.store
    |> register_current_total_ticks node_ctxt
    |> register_current_num_messages node_ctxt.store
    |> register_current_state_hash node_ctxt
    |> register_current_state_value node_ctxt
    |> register_current_status node_ctxt
    |> register_last_stored_commitment node_ctxt.store
    |> register_last_published_commitment node_ctxt.store
    |> register_dal_slot_subscriptions node_ctxt.store
    |> register_dal_slots node_ctxt.store
    |> register_current_outbox node_ctxt
    |> register_outbox_proof node_ctxt
    |> register_dal_slot_pages node_ctxt.store
    |> register_dal_slot_page node_ctxt.store

  let start node_ctxt configuration =
    Common.start configuration (register node_ctxt configuration)
end
