(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech, <contact@trili.tech>                  *)
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

exception Status_already_ready

let store_path_prefix = "store"

type dac_plugin_module = (module Dac_plugin.T)

type ready_ctxt = {
  dac_plugin : dac_plugin_module;
  hash_streamer : Dac_plugin.hash Data_streamer.t;
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4895
         This could be problematic in case coordinator and member/observer
         use two different plugins that bind different underlying hashes. *)
}

type status = Ready of ready_ctxt | Starting

type t = {
  mutable status : status;
  config : Configuration.t;
  tezos_node_cctxt : Client_context.full;
  coordinator_opt : Dac_node_client.cctxt option;
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4896
         [coordinator_opt] is meant to be used for running integration tests
         in the multiple dac node setup, where all nodes are in the [legacy]
         mode. In this setup we normally try to mimic the role of coordinator
         with one node, whereas the others want to interact with it.
         This is done via [Dac_node_client.cctxt].

         Eventually, once the legacy mode is removed we should revisit the
         need for this fieeld.*)
  page_store : Page_store.Filesystem.t;
  node_store : Store_sigs.rw Store.Irmin_store.t;
}

let init config cctxt coordinator_opt =
  let open Lwt_result_syntax in
  let+ node_store =
    Store.Irmin_store.load
      Store_sigs.Read_write
      (Configuration.data_dir_path config store_path_prefix)
  in
  {
    status = Starting;
    config;
    tezos_node_cctxt = cctxt;
    coordinator_opt;
    page_store =
      Page_store.Filesystem.init (Configuration.reveal_data_dir config);
    node_store;
  }

let set_ready ctxt dac_plugin =
  match ctxt.status with
  | Starting ->
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4681
         Currently, Dac only supports coordinator functionalities but we might
         want to filter this capability out depending on the profile.
      *)
      ctxt.status <- Ready {dac_plugin; hash_streamer = Data_streamer.init ()}
  | Ready _ -> raise Status_already_ready

type error +=
  | Node_not_ready
  | Invalid_operation_for_mode of {mode : string; operation : string}

let () =
  register_error_kind
    `Permanent
    ~id:"dac.node.not.ready"
    ~title:"DAC Node not ready"
    ~description:"DAC node is starting. It's not ready to respond to RPCs."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "DAC node is starting. It's not ready to respond to RPCs.")
    Data_encoding.(unit)
    (function Node_not_ready -> Some () | _ -> None)
    (fun () -> Node_not_ready) ;
  register_error_kind
    `Permanent
    ~id:"dac_unexpected_mode"
    ~title:"Invalid operation for the current mode of Dac node."
    ~description:
      "An operation was called that it not supported by the current Dac node."
    ~pp:(fun ppf (mode, operation) ->
      Format.fprintf
        ppf
        "An operation was called that it not supported by the current Dac \
         node. Mode: %s; Unsupported_operation: %s"
        mode
        operation)
    Data_encoding.(
      obj2 (req "mode" (string' Plain)) (req "operation" (string' Plain)))
    (function
      | Invalid_operation_for_mode {mode; operation} -> Some (mode, operation)
      | _ -> None)
    (fun (mode, operation) -> Invalid_operation_for_mode {mode; operation})

let get_ready ctxt =
  let open Result_syntax in
  match ctxt.status with
  | Ready ctxt -> Ok ctxt
  | Starting -> fail [Node_not_ready]

let get_config ctxt = ctxt.config

let get_status ctxt = ctxt.status

let get_tezos_node_cctxt ctxt = ctxt.tezos_node_cctxt

let get_dac_plugin ctxt =
  let open Result_syntax in
  match ctxt.status with
  | Ready {dac_plugin; hash_streamer = _} -> Ok dac_plugin
  | Starting -> tzfail Node_not_ready

let get_page_store ctxt = ctxt.page_store

let get_node_store (type a) ctxt (access_mode : a Store_sigs.mode) :
    a Store.Irmin_store.t =
  match access_mode with
  | Store_sigs.Read_only -> Store.Irmin_store.readonly ctxt.node_store
  | Store_sigs.Read_write -> ctxt.node_store

let get_committee_members ctxt =
  let open Result_syntax in
  match Configuration.mode ctxt.config with
  | Legacy legacy ->
      Ok (Configuration.Legacy.committee_members_addresses legacy)
  | Coordinator coordinator ->
      Ok (Configuration.Coordinator.committee_members_addresses coordinator)
  | Observer _ ->
      tzfail
      @@ Invalid_operation_for_mode
           {mode = "observer"; operation = "get_committee_members"}
  | Committee_member _ ->
      tzfail
      @@ Invalid_operation_for_mode
           {mode = "dac_member"; operation = "get_committee_members"}
