(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let rec retry_on_disconnection (cctxt : #Protocol_client_context.full) f =
  let open Lwt_result_syntax in
  let*! result = f () in
  match result with
  | Ok () -> return_unit
  | Error (Baking_errors.Node_connection_lost :: _) ->
      let*! () =
        cctxt#warning
          "Lost connection with the node. Retrying to establish connection..."
      in
      (* Wait forever when the node stops responding... *)
      let* () =
        Client_confirmations.wait_for_bootstrapped
          ~retry:
            (Baking_scheduling.retry
               cctxt
               ~max_delay:10.
               ~delay:1.
               ~factor:1.5
               ~tries:max_int)
          cctxt
      in
      retry_on_disconnection cctxt f
  | Error err ->
      cctxt#error "Unexpected error: %a. Exiting..." pp_print_trace err

let await_protocol_start (cctxt : #Protocol_client_context.full) ~chain =
  let open Lwt_result_syntax in
  let*! () =
    cctxt#message "Waiting for protocol %s to start..." Protocol.name
  in
  Node_rpc.await_protocol_activation cctxt ~chain ()

module Baker = struct
  let run (cctxt : Protocol_client_context.full) ?minimal_fees
      ?minimal_nanotez_per_gas_unit ?minimal_nanotez_per_byte ?votes
      ?extra_operations ?dal_node_endpoint ?pre_emptive_forge_time ?force_apply
      ?context_path ?state_recorder ~chain ~keep_alive delegates =
    let open Lwt_result_syntax in
    let process () =
      let* user_activated_upgrades =
        Config_services.user_activated_upgrades cctxt
      in
      let* constants =
        let* chain_id =
          Shell_services.Chain.chain_id cctxt ~chain:cctxt#chain ()
        in
        Protocol.Alpha_services.Constants.all cctxt (`Hash chain_id, `Head 0)
      in
      let block_time_s =
        Int64.to_float
          (Protocol.Alpha_context.Period.to_seconds
             constants.parametric.minimal_block_delay)
      in
      let* pre_emptive_forge_time =
        match Option.map Q.to_float pre_emptive_forge_time with
        | Some t ->
            if t >= block_time_s then
              failwith
                "pre-emptive-forge-time must be less than current block time \
                 (<= %f seconds)"
                block_time_s
            else return t
        | None -> return (Float.mul 0.15 block_time_s)
      in
      let*! () =
        if pre_emptive_forge_time <> 0. then
          cctxt#message
            "pre-emptive-forge-time optimization set to %fs. Operation \
             inclusion window is ~%fs. Caution: Setting this too high may \
             result in reduced block proposal rewards."
            pre_emptive_forge_time
            (Float.sub block_time_s pre_emptive_forge_time)
        else Lwt.return_unit
      in
      let pre_emptive_forge_time =
        Time.System.Span.of_seconds_exn pre_emptive_forge_time
      in
      let config =
        Baking_configuration.make
          ?minimal_fees
          ?minimal_nanotez_per_gas_unit
          ?minimal_nanotez_per_byte
          ?votes
          ?extra_operations
          ?dal_node_endpoint
          ~pre_emptive_forge_time
          ?force_apply
          ?context_path
          ~user_activated_upgrades
          ?state_recorder
          ()
      in
      let*! () =
        cctxt#message
          "Baker v%a (%s) for %a started."
          Tezos_version.Version.pp
          Tezos_version_value.Current_git_info.octez_version
          Tezos_version_value.Current_git_info.abbreviated_commit_hash
          Protocol_hash.pp_short
          Protocol.hash
      in
      let canceler = Lwt_canceler.create () in
      let _ =
        Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
            let*! () = cctxt#message "Shutting down the baker..." in
            let*! _ = Lwt_canceler.cancel canceler in
            Lwt.return_unit)
      in
      let* () =
        let* dal_config = Node_rpc.fetch_dal_config cctxt in
        Cryptobox.Config.init_verifier_dal dal_config
      in
      let consumer = Protocol_logging.make_log_message_consumer () in
      Lifted_protocol.set_log_message_consumer consumer ;
      Baking_scheduling.run cctxt ~canceler ~chain ~constants config delegates
    in
    let* () =
      Client_confirmations.wait_for_bootstrapped
        ~retry:(Baking_scheduling.retry cctxt ~delay:1. ~factor:1.5 ~tries:5)
        cctxt
    in
    let* () = await_protocol_start cctxt ~chain in
    if keep_alive then retry_on_disconnection cctxt process else process ()
end

module Accuser = struct
  let run (cctxt : #Protocol_client_context.full) ~chain ~preserved_levels
      ~keep_alive =
    let open Lwt_result_syntax in
    let process () =
      let*! () =
        cctxt#message
          "Accuser v%a (%s) for %a started."
          Tezos_version.Version.pp
          Tezos_version_value.Current_git_info.octez_version
          Tezos_version_value.Current_git_info.abbreviated_commit_hash
          Protocol_hash.pp_short
          Protocol.hash
      in
      let* valid_blocks_stream, _ =
        Client_baking_blocks.monitor_applied_blocks
          ~next_protocols:(Some [Protocol.hash])
          cctxt
          ~chains:[chain]
          ()
      in
      let canceler = Lwt_canceler.create () in
      let _ =
        Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
            let*! () = cctxt#message "Shutting down the accuser..." in
            let*! _ = Lwt_canceler.cancel canceler in
            Lwt.return_unit)
      in
      Client_baking_denunciation.create
        cctxt
        ~canceler
        ~preserved_levels
        valid_blocks_stream
    in
    let* () =
      Client_confirmations.wait_for_bootstrapped
        ~retry:(Baking_scheduling.retry cctxt ~delay:1. ~factor:1.5 ~tries:5)
        cctxt
    in
    let* () = await_protocol_start cctxt ~chain in
    if keep_alive then retry_on_disconnection cctxt process else process ()
end

module VDF = struct
  let run (cctxt : Protocol_client_context.full) ~chain ~keep_alive =
    let open Lwt_result_syntax in
    let process () =
      let*! () =
        cctxt#message
          "VDF daemon v%a (%s) for %a started."
          Tezos_version.Version.pp
          Tezos_version_value.Current_git_info.octez_version
          Tezos_version_value.Current_git_info.abbreviated_commit_hash
          Protocol_hash.pp_short
          Protocol.hash
      in
      let* chain_id = Shell_services.Chain.chain_id cctxt ~chain () in
      let* constants =
        Protocol.Alpha_services.Constants.all cctxt (`Hash chain_id, `Head 0)
      in
      let canceler = Lwt_canceler.create () in
      let _ =
        Lwt_exit.register_clean_up_callback ~loc:__LOC__ (fun _ ->
            let*! () = cctxt#message "Shutting down the VDF daemon..." in
            let*! _ = Lwt_canceler.cancel canceler in
            Lwt.return_unit)
      in
      Baking_vdf.start_vdf_worker cctxt ~canceler constants chain
    in
    let* () =
      Client_confirmations.wait_for_bootstrapped
        ~retry:(Baking_scheduling.retry cctxt ~delay:1. ~factor:1.5 ~tries:5)
        cctxt
    in
    let* () = await_protocol_start cctxt ~chain in
    if keep_alive then retry_on_disconnection cctxt process else process ()
end
