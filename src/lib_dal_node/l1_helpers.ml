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

let fetch_info_from_l1 cctxt ~requested_info ~rpc =
  let open Lwt_syntax in
  let delay = 0.1 in
  let delay_max = 20.0 in
  let endpoint = Uri.to_string cctxt#base in
  let rec retry delay =
    let* r = rpc cctxt in
    match r with
    | Error
        [RPC_client_errors.(Request_failed {error = Connection_failed _; _})] ->
        let delay = min delay_max (delay *. 2.) in
        let* () =
          Event.emit_retry_fetching_info_from_l1
            ~endpoint
            ~delay
            ~requested_info
            ~event_level:(if delay < delay_max then `Notice else `Warning)
        in
        let* () = Lwt_unix.sleep delay in
        retry delay
    | Error err -> return_error err
    | Ok res ->
        let* () =
          Event.emit_fetched_l1_info_success ~endpoint ~requested_info
        in
        return_ok res
  in
  retry delay

let fetch_dal_config cctxt =
  fetch_info_from_l1
    cctxt
    ~rpc:Config_services.dal_config
    ~requested_info:"DAL config"

let fetch_l1_version_info cctxt =
  fetch_info_from_l1
    cctxt
    ~rpc:Version_services.version
    ~requested_info:"version info"

let fetch_l1_chain_id cctxt =
  fetch_info_from_l1
    cctxt
    ~rpc:(fun x -> Chain_services.chain_id x ())
    ~requested_info:"chain id"

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7851

   The function below infers the DAL network name based on the L1 chain name and
   the DAL node version.

   - For DAL node versions <= V22, the legacy "dal-sandbox" network name is used.
   - For versions >= V23, the new naming scheme "DAL_<L1_CHAIN_NAME>" is used.

   This ensures a smooth transition during the migration period.

   For the new naming scheme, the function queries the L1 node to retrieve its
   chain name and constructs the corresponding DAL network name by prefixing
   it with "DAL_".
*)
let infer_dal_network_name cctxt =
  let open Lwt_result_syntax in
  (* Now that V23 is released and V24 is in the pipeline, we can move to the
     second migration phase of https://gitlab.com/tezos/tezos/-/issues/7851:
     all upcoming versions (and master) should advertise only the new DAL node
     network naming scheme. "dal-sandbox" will no longer be advertised. *)
  let+ l1_version = fetch_l1_version_info cctxt in
  Format.sprintf "DAL_%s" (l1_version.network_version.chain_name :> string)
  |> Distributed_db_version.Name.of_string

let wait_for_l1_bootstrapped (cctxt : Rpc_context.t) =
  let open Lwt_result_syntax in
  let*! () = Event.emit_waiting_l1_node_bootstrapped () in
  let* stream, _stop = Monitor_services.bootstrapped cctxt in
  let*! () =
    Lwt_stream.iter_s (fun (_hash, _timestamp) -> Lwt.return_unit) stream
  in
  let*! () = Event.emit_l1_node_bootstrapped () in
  return_unit

let wait_for_block_with_plugin (cctxt : Rpc_context.t) =
  let open Lwt_result_syntax in
  let*! () = Event.emit_waiting_known_plugin () in
  let* stream, stop = Monitor_services.heads cctxt `Main in
  let rec wait_for_level () =
    let*! head_opt = Lwt_stream.get stream in
    match head_opt with
    | None -> failwith "Lost the connection with the L1 node"
    | Some (_hash, header) ->
        let* proto_plugins =
          Proto_plugins.get_supported_proto_plugins
            cctxt
            ~head_level:header.Block_header.shell.level
        in
        if Proto_plugins.has_plugins proto_plugins then
          let () = stop () in
          return (header, proto_plugins)
        else wait_for_level ()
  in
  wait_for_level ()
