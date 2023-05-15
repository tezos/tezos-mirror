(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

type ready_ctxt = {
  cryptobox : Cryptobox.t;
  proto_parameters : Dal_plugin.proto_parameters;
  plugin : (module Dal_plugin.T);
}

type status = Ready of ready_ctxt | Starting

type t = {
  mutable status : status;
  config : Configuration.t;
  store : Store.node_store;
  tezos_node_cctxt : Client_context.full;
  neighbors_cctxts : Dal_node_client.cctxt list;
  committee_cache : Committee_cache.t;
  gs_worker : Gossipsub.Worker.t;
  transport_layer : Gossipsub.Transport_layer.t;
}

let init config store gs_worker transport_layer cctxt =
  let neighbors_cctxts =
    List.map
      (fun Configuration.{addr; port} ->
        let endpoint =
          Uri.of_string ("http://" ^ addr ^ ":" ^ string_of_int port)
        in
        Dal_node_client.make_unix_cctxt endpoint)
      config.Configuration.neighbors
  in
  {
    status = Starting;
    config;
    store;
    tezos_node_cctxt = cctxt;
    neighbors_cctxts;
    committee_cache =
      Committee_cache.create ~max_size:Constants.committee_cache_size;
    gs_worker;
    transport_layer;
  }

let set_ready ctxt plugin cryptobox proto_parameters =
  match ctxt.status with
  | Starting -> ctxt.status <- Ready {plugin; cryptobox; proto_parameters}
  | Ready _ -> raise Status_already_ready

type error += Node_not_ready

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.not.ready"
    ~title:"DAL Node not ready"
    ~description:"DAL node is starting. It's not ready to respond to RPCs."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "DAL node is starting. It's not ready to respond to RPCs.")
    Data_encoding.(unit)
    (function Node_not_ready -> Some () | _ -> None)
    (fun () -> Node_not_ready)

let get_ready ctxt =
  let open Result_syntax in
  match ctxt.status with
  | Ready ctxt -> Ok ctxt
  | Starting -> fail [Node_not_ready]

let get_config ctxt = ctxt.config

let get_status ctxt = ctxt.status

let get_store ctxt = ctxt.store

let get_tezos_node_cctxt ctxt = ctxt.tezos_node_cctxt

let get_neighbors_cctxts ctxt = ctxt.neighbors_cctxts

let fetch_committee ctxt ~level =
  let open Lwt_result_syntax in
  let {tezos_node_cctxt = cctxt; committee_cache = cache; _} = ctxt in
  match Committee_cache.find cache ~level with
  | Some committee -> return committee
  | None ->
      let*? {plugin = (module Plugin); _} = get_ready ctxt in
      let+ committee = Plugin.get_committee cctxt ~level in
      let committee =
        Tezos_crypto.Signature.Public_key_hash.Map.map
          (fun (start_index, offset) -> Committee_cache.{start_index; offset})
          committee
      in
      Committee_cache.add cache ~level ~committee ;
      committee

let fetch_assigned_shard_indices ctxt ~level ~pkh =
  let open Lwt_result_syntax in
  let+ committee = fetch_committee ctxt ~level in
  match Tezos_crypto.Signature.Public_key_hash.Map.find pkh committee with
  | None -> []
  | Some {start_index; offset} ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4540
         Consider returning some abstract representation of [(s, n)]
         instead of [int list] *)
      Stdlib.List.init offset (fun i -> start_index + i)
