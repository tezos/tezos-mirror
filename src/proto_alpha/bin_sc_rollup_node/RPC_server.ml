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

let get_head_exn store =
  let open Lwt_result_syntax in
  let*! head = Layer1.current_head_hash store in
  match head with None -> failwith "No head" | Some head -> return head

let get_state_exn store =
  let open Lwt_result_syntax in
  let* head = get_head_exn store in
  let*! state = Store.PVMState.find store head in
  match state with None -> failwith "No state" | Some state -> return state

let get_state_info_exn store =
  let open Lwt_result_syntax in
  let* head = get_head_exn store in
  let*! state = Store.StateInfo.get store head in
  return state

module Common = struct
  let register_current_num_messages store dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.current_num_messages ())
      (fun () () ->
        let open Lwt_result_syntax in
        let* state_info = get_state_info_exn store in
        return state_info.num_messages)

  let register_sc_rollup_address configuration dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.sc_rollup_address ())
      (fun () () -> return @@ configuration.Configuration.sc_rollup_address)

  let register_current_tezos_head store dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.current_tezos_head ())
      (fun () () -> Layer1.current_head_hash store >>= return)

  let register_current_tezos_level store dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.current_tezos_level ())
      (fun () () -> Layer1.current_level store >>= return)

  let register_current_inbox node_ctxt store dir =
    let open Lwt_result_syntax in
    RPC_directory.opt_register0
      dir
      (Sc_rollup_services.current_inbox ())
      (fun () () ->
        Layer1.current_head_hash store >>= function
        | Some head_hash ->
            let*! inbox = Inbox.inbox_of_hash node_ctxt store head_hash in
            return_some inbox
        | None -> return None)

  let register_current_ticks store dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.current_ticks ())
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state_info_exn store in
        return state.num_ticks)

  let start configuration dir =
    let Configuration.{rpc_addr; rpc_port; _} = configuration in
    let rpc_addr = P2p_addr.of_string_exn rpc_addr in
    let host = Ipaddr.V6.to_string rpc_addr in
    let node = `TCP (`Port rpc_port) in
    let acl = RPC_server.Acl.default rpc_addr in
    Lwt.catch
      (fun () ->
        RPC_server.launch
          ~media_types:Media_type.all_media_types
          ~host
          ~acl
          node
          dir
        >>= return)
      fail_with_exn

  let shutdown = RPC_server.shutdown
end

module Make (PVM : Pvm.S) = struct
  include Common

  let register_current_total_ticks store dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.current_total_ticks ())
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state_exn store in
        let*! tick = PVM.get_tick state in
        return tick)

  let register_current_state_hash store dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.current_state_hash ())
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state_exn store in
        let*! hash = PVM.state_hash state in
        return hash)

  let register_current_status store dir =
    RPC_directory.register0
      dir
      (Sc_rollup_services.current_status ())
      (fun () () ->
        let open Lwt_result_syntax in
        let* state = get_state_exn store in
        let*! status = PVM.get_status state in
        return (PVM.string_of_status status))

  let register node_ctxt store configuration =
    RPC_directory.empty
    |> register_sc_rollup_address configuration
    |> register_current_tezos_head store
    |> register_current_inbox node_ctxt store
    |> register_current_ticks store
    |> register_current_total_ticks store
    |> register_current_num_messages store
    |> register_current_state_hash store
    |> register_current_status store

  let start node_ctxt store configuration =
    Common.start configuration (register node_ctxt store configuration)
end

module Arith = Make (Arith_pvm)
