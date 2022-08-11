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

let resolve_plugin cctxt =
  let open Lwt_result_syntax in
  let* protocols =
    Tezos_shell_services.Chain_services.Blocks.protocols cctxt ()
  in
  return
  @@ Option.either
       (Dal_constants_plugin.get protocols.current_protocol)
       (Dal_constants_plugin.get protocols.next_protocol)

open Dal_cryptobox

type error += Cryptobox_initialisation_failed of string

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.cryptobox.initialisation_failed"
    ~title:"Cryptobox initialisation failed"
    ~description:"Unable to initialise the cryptobox parameters"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Unable to initialise the cryptobox parameters. Reason: %s"
        msg)
    Data_encoding.(obj1 (req "error" string))
    (function Cryptobox_initialisation_failed str -> Some str | _ -> None)
    (fun str -> Cryptobox_initialisation_failed str)

let init_cryptobox unsafe_srs cctxt (module Plugin : Dal_constants_plugin.T) =
  let open Lwt_result_syntax in
  let* parameters = Plugin.get_constants cctxt#chain cctxt#block cctxt in
  let* initialisation_parameters =
    if unsafe_srs then
      return
      @@ Cryptobox.Internal_for_tests.initialisation_parameters_from_slot_size
           ~slot_size:parameters.slot_size
    else
      let*? g1_path, g2_path = Tezos_base.Dal_srs.find_trusted_setup_files () in
      Cryptobox.initialisation_parameters_from_files ~g1_path ~g2_path
  in
  let*? () = Cryptobox.load_parameters initialisation_parameters in
  let* dal_constants =
    match Cryptobox.make parameters with
    | Ok cryptobox -> return cryptobox
    | Error (`Fail msg) -> fail [Cryptobox_initialisation_failed msg]
  in
  return @@ (dal_constants, parameters)

let daemonize cctxt handle =
  let open Lwt_result_syntax in
  let* t, stopper = Layer1.on_new_head cctxt handle in
  let (_ : Lwt_exit.clean_up_callback_id) =
    Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _exit_status ->
        stopper () ;
        Lwt.return_unit)
  in
  (* The no_cancel might not be necessary. Lwt_exit cancels the
     main promise upon receiving a signal or other form of interruption. The
     no_cancel renders this cancelation into a no-op.*)
  Lwt.no_cancel t

let run ~data_dir cctxt =
  let open Lwt_result_syntax in
  let*! () = Event.(emit starting_node) () in
  let* config = Configuration.load ~data_dir in
  let config = {config with data_dir} in
  let*! store = Store.init config in
  let ready = ref false in
  let*! () = Event.(emit layer1_node_tracking_started ()) in
  daemonize cctxt (fun (_hash, (_block_header : Tezos_base.Block_header.t)) ->
      (* Try to resolve the protocol plugin corresponding to the protocol of the
         targeted node *)
      if not !ready then
        let* plugin = resolve_plugin cctxt in
        match plugin with
        | Some plugin ->
            let (module Plugin : Dal_constants_plugin.T) = plugin in
            let*! () = Event.emit_protocol_plugin_resolved Plugin.Proto.hash in
            let* dal_constants, dal_parameters =
              init_cryptobox config.unsafe_srs cctxt plugin
            in
            let ctxt = Node_context.make config dal_constants dal_parameters in
            let* rpc_server = RPC_server.(start config (register ctxt store)) in
            let _ = RPC_server.install_finalizer rpc_server in
            let*! () =
              Event.(
                emit rpc_server_is_ready (config.rpc_addr, config.rpc_port))
            in
            let*! () = Event.(emit node_is_ready ()) in
            ready := true ;
            return_unit
        | None -> return_unit
      else
        (* If rpc and plugin are ready, there is nothing else to do.
           Future work will update this part of the code *)
        return_unit)
