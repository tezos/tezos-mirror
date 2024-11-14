(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let build_rpc_directory_for_rpc_process ~user_activated_upgrades
    ~user_activated_protocol_overrides ~dal_config dir =
  let open Lwt_result_syntax in
  let register endpoint f directory =
    Tezos_rpc.Directory.register directory endpoint f
  in
  dir
  |> register Config_services.Network.user_activated_upgrades (fun () () () ->
         return user_activated_upgrades)
  |> register
       Config_services.Network.user_activated_protocol_overrides
       (fun () () () -> return user_activated_protocol_overrides)
  |> register Config_services.Network.dal_config (fun () () () ->
         return dal_config)

let build_rpc_directory ~user_activated_upgrades
    ~user_activated_protocol_overrides ~dal_config ~mainchain_validator store =
  let open Lwt_result_syntax in
  let register endpoint f directory =
    Tezos_rpc.Directory.register directory endpoint f
  in
  build_rpc_directory_for_rpc_process
    ~user_activated_upgrades
    ~user_activated_protocol_overrides
    ~dal_config
    Tezos_rpc.Directory.empty
  |> register
       Config_services.History_mode_services.history_mode
       (fun () () () ->
         let chain_store = Store.main_chain_store store in
         let history_mode =
           match Store.Chain.history_mode chain_store with
           | (Full (Some _) | Rolling (Some _) | Archive) as history_mode ->
               history_mode
           | Full None -> Full (Some History_mode.default_additional_cycles)
           | Rolling None ->
               Rolling (Some History_mode.default_additional_cycles)
         in
         let*! head = Store.Chain.current_head chain_store in
         let*! protocol = Store.Block.protocol_hash_exn chain_store head in
         let*! blocks_preservation_cycles =
           let open Lwt_syntax in
           match history_mode with
           | Archive -> return_none
           | Rolling _ | Full _ -> (
               match Protocol_plugin.find_rpc protocol with
               | None -> return_none
               | Some (module RPC) ->
                   RPC.get_blocks_preservation_cycles ~get_context:(fun () ->
                       Store.Block.context_exn chain_store head))
         in
         return (history_mode, blocks_preservation_cycles))
  |> register Config_services.Logging.configure (fun () () configuration ->
         let open Lwt_result_syntax in
         let* () = Internal_event_unix.Configuration.reapply configuration in
         let* () =
           Chain_validator.reconfigure_event_logging
             mainchain_validator
             configuration
         in
         return_unit)
