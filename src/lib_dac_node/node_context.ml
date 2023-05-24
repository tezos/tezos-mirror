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

module Coordinator = struct
  type t = {
    committee_members : Wallet_account.Coordinator.t list;
    hash_streamer : Dac_plugin.raw_hash Data_streamer.t;
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4895
           This could be problematic in case coordinator and member/observer
           use two different plugins that bind different underlying hashes. *)
    certificate_streamers : Certificate_streamers.t;
  }

  let get_all_committee_members_public_keys committee_members_public_keys =
    List.map_es
      Wallet_account.Coordinator.of_committee_member_public_key
      committee_members_public_keys

  let init coordinator_config =
    let open Lwt_result_syntax in
    let Configuration.Coordinator.{committee_members; _} = coordinator_config in
    let+ committee_members =
      get_all_committee_members_public_keys committee_members
    in
    let hash_streamer = Data_streamer.init () in
    let certificate_streamers = Certificate_streamers.init () in
    {committee_members; hash_streamer; certificate_streamers}

  let public_keys_opt t =
    List.map
      (fun Wallet_account.Coordinator.{public_key; _} -> Some public_key)
      t.committee_members

  let committee_members t =
    List.map
      (fun Wallet_account.Coordinator.{public_key_hash; _} -> public_key_hash)
      t.committee_members
end

module Committee_member = struct
  type t = {
    committee_member : Wallet_account.Committee_member.t;
    coordinator_cctxt : Dac_node_client.cctxt;
  }

  let init committee_member_config cctxt =
    let open Lwt_result_syntax in
    let Configuration.Committee_member.
          {address; coordinator_rpc_address; coordinator_rpc_port} =
      committee_member_config
    in
    let+ committee_member =
      Wallet_account.Committee_member.of_committee_member_address address cctxt
    in
    let coordinator_cctxt =
      Dac_node_client.make_unix_cctxt
        ~scheme:"http"
        ~host:coordinator_rpc_address
        ~port:coordinator_rpc_port
    in
    {committee_member; coordinator_cctxt}

  let secret_key_uri t =
    let Wallet_account.Committee_member.{secret_key_uri; _} =
      t.committee_member
    in
    secret_key_uri
end

module Observer = struct
  type t = {
    coordinator_cctxt : Dac_node_client.cctxt;
    committee_cctxts : Dac_node_client.cctxt list;
    timeout : int;
  }

  let init observer_config =
    let open Lwt_result_syntax in
    let Configuration.Observer.
          {
            coordinator_rpc_address;
            coordinator_rpc_port;
            committee_rpc_addresses;
            timeout;
          } =
      observer_config
    in
    let coordinator_cctxt =
      Dac_node_client.make_unix_cctxt
        ~scheme:"http"
        ~host:coordinator_rpc_address
        ~port:coordinator_rpc_port
    in
    let committee_cctxts =
      List.map
        (fun (host, port) ->
          Dac_node_client.make_unix_cctxt ~scheme:"http" ~host ~port)
        committee_rpc_addresses
    in
    return {coordinator_cctxt; committee_cctxts; timeout}
end

module Legacy = struct
  type t = {
    committee_members : Wallet_account.Legacy.t list;
    coordinator_cctxt : Dac_node_client.cctxt option;
    hash_streamer : Dac_plugin.raw_hash Data_streamer.t;
    committee_member_opt : Wallet_account.Legacy.t option;
  }

  let get_all_committee_members_keys committee_members_addresses ~threshold
      cctxt =
    let open Lwt_result_syntax in
    let* wallet_accounts =
      List.map_es
        (fun public_key_hash ->
          Wallet_account.Legacy.of_committee_member_address
            public_key_hash
            cctxt)
        committee_members_addresses
    in
    let*! valid_wallets =
      List.filter_s
        (fun Wallet_account.Legacy.{public_key_hash; secret_key_uri_opt; _} ->
          if Option.is_some secret_key_uri_opt then Lwt.return true
          else
            let*! () =
              Event.(emit committee_member_cannot_sign public_key_hash)
            in
            Lwt.return false)
        wallet_accounts
    in
    let recovered_keys = List.length valid_wallets in
    let*! () =
      (* We emit a warning if the threshold of dac accounts needed to sign a
         root page hash is not reached. We also emit a warning for each DAC
         account whose secret key URI was not recovered.
         We do not stop the dac node at this stage.
      *)
      if recovered_keys < threshold then
        Event.(emit dac_threshold_not_reached (recovered_keys, threshold))
      else Event.(emit committee_keys_imported) ()
    in
    return wallet_accounts

  let init legacy_config cctxt =
    let open Lwt_result_syntax in
    let Configuration.Legacy.
          {
            threshold;
            committee_members_addresses;
            dac_cctxt_config;
            committee_member_address_opt;
          } =
      legacy_config
    in
    let* committee_members =
      get_all_committee_members_keys
        committee_members_addresses
        ~threshold
        cctxt
    in
    let+ committee_member_opt =
      Option.map_es
        (fun address ->
          Wallet_account.Legacy.of_committee_member_address address cctxt)
        committee_member_address_opt
    in
    let coordinator_cctxt =
      Option.map
        (fun Configuration.{host; port} ->
          Dac_node_client.make_unix_cctxt ~scheme:"http" ~host ~port)
        dac_cctxt_config
    in
    let hash_streamer = Data_streamer.init () in
    {committee_members; coordinator_cctxt; hash_streamer; committee_member_opt}

  let public_keys_opt t =
    List.map
      (fun Wallet_account.Legacy.{public_key_opt; _} -> public_key_opt)
      t.committee_members

  let secret_key_uris_opt t =
    List.map
      (fun Wallet_account.Legacy.{secret_key_uri_opt; _} -> secret_key_uri_opt)
      t.committee_members

  let committee_members t =
    List.map
      (fun Wallet_account.Legacy.{public_key_hash; _} -> public_key_hash)
      t.committee_members
end

type mode =
  | Coordinator of Coordinator.t
  | Committee_member of Committee_member.t
  | Observer of Observer.t
  | Legacy of Legacy.t

type ready_ctxt = {dac_plugin : dac_plugin_module}

type status = Ready of ready_ctxt | Starting

type t = {
  mutable status : status;
  reveal_data_dir : string;
  tezos_node_cctxt : Client_context.full;
  page_store : Page_store.Filesystem.t;
  node_store : Store_sigs.rw Store.Irmin_store.t;
  mode : mode;
}

let init_mode Configuration.{mode; _} cctxt =
  let open Lwt_result_syntax in
  match mode with
  | Coordinator config ->
      let+ mode_node_ctxt = Coordinator.init config in
      Coordinator mode_node_ctxt
  | Committee_member config ->
      let+ mode_node_ctxt = Committee_member.init config cctxt in
      Committee_member mode_node_ctxt
  | Observer config ->
      let+ mode_node_ctxt = Observer.init config in
      Observer mode_node_ctxt
  | Legacy config ->
      let+ mode_node_ctxt = Legacy.init config cctxt in
      Legacy mode_node_ctxt

let init config cctxt =
  let open Lwt_result_syntax in
  let* node_store =
    Store.Irmin_store.load
      Store_sigs.Read_write
      (Configuration.data_dir_path config store_path_prefix)
  in
  let+ mode = init_mode config cctxt in
  {
    status = Starting;
    reveal_data_dir = Configuration.reveal_data_dir config;
    tezos_node_cctxt = cctxt;
    page_store =
      Page_store.Filesystem.init (Configuration.reveal_data_dir config);
    node_store;
    mode;
  }

let get_mode node_ctxt = node_ctxt.mode

let set_ready ctxt dac_plugin =
  match ctxt.status with
  | Starting ->
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4681
         Currently, Dac only supports coordinator functionalities but we might
         want to filter this capability out depending on the profile.
      *)
      ctxt.status <- Ready {dac_plugin}
  | Ready _ -> raise Status_already_ready

type error += Node_not_ready

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
    (fun () -> Node_not_ready)

let get_ready ctxt =
  let open Result_syntax in
  match ctxt.status with
  | Ready ctxt -> Ok ctxt
  | Starting -> fail [Node_not_ready]

let get_status ctxt = ctxt.status

let get_tezos_node_cctxt ctxt = ctxt.tezos_node_cctxt

let get_dac_plugin ctxt =
  let open Result_syntax in
  match ctxt.status with
  | Ready {dac_plugin} -> Ok dac_plugin
  | Starting -> tzfail Node_not_ready

let get_page_store ctxt = ctxt.page_store

let get_node_store (type a) ctxt (access_mode : a Store_sigs.mode) :
    a Store.Irmin_store.t =
  match access_mode with
  | Store_sigs.Read_only -> Store.Irmin_store.readonly ctxt.node_store
  | Store_sigs.Read_write -> ctxt.node_store
