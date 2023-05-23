(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

type error += Mode_not_supported of string

let () =
  register_error_kind
    `Permanent
    ~id:"dac.node.operating_mode_not_supported"
    ~title:"Operating mode not supported"
    ~description:"Operating mode not supported"
    ~pp:(fun ppf mode_string ->
      Format.fprintf ppf "DAC node cannot run in %s operating mode" mode_string)
    Data_encoding.(obj1 (req "mode" string))
    (function Mode_not_supported mode -> Some mode | _ -> None)
    (fun mode -> Mode_not_supported mode)

let daemonize handlers =
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
     Improve concurrent tasks by using workers *)
  let open Lwt_result_syntax in
  let* handlers = List.map_es (fun x -> x) handlers in
  let (_ : Lwt_exit.clean_up_callback_id) =
    (* close the stream when an exit signal is received *)
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        List.iter (fun (_, stopper) -> stopper ()) handlers ;
        Lwt.return_unit)
  in
  (let* _ = all (List.map fst handlers) in
   return_unit)
  |> lwt_map_error (List.fold_left (fun acc errs -> errs @ acc) [])

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3605
   Improve general architecture, handle L1 disconnection etc
*)
let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* (Configuration.
          {
            rpc_address;
            rpc_port;
            reveal_data_dir;
            mode = _;
            data_dir = _;
            allow_v1_api;
          } as config) =
    Configuration.load ~data_dir
  in
  let operating_mode_string = Configuration.mode_name config in
  let*! () = Event.(emit operating_mode operating_mode_string) in
  let* () = Page_store.ensure_reveal_data_dir_exists reveal_data_dir in
  let* ctxt = Node_context.init config cctxt in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4725
     Stop DAC node when in Legacy mode, if threshold is not reached. *)
  let* rpc_server =
    RPC_server.start ~rpc_address ~rpc_port ~allow_v1_api ctxt
  in
  let _ = RPC_server.install_finalizer rpc_server in
  let*! () = Event.(emit rpc_server_is_ready (rpc_address, rpc_port)) in
  (* Start daemon to resolve current protocol plugin *)
  let* () = daemonize [Handler.resolve_plugin_and_set_ready ctxt] in
  (* Start never-ending monitoring daemons. [coordinator_cctxt] is required to
     monitor new root hashes in legacy mode. *)
  let* handlers = Handler.handlers ctxt in
  daemonize handlers
