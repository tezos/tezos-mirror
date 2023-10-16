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
  |> register Config_services.history_mode (fun () () () ->
         let chain_store = Store.main_chain_store store in
         return (Store.Chain.history_mode chain_store))
  |> register Config_services.Logging.configure (fun () () configuration ->
         let open Lwt_result_syntax in
         let* () = Internal_event_unix.Configuration.reapply configuration in
         let* () =
           Chain_validator.reconfigure_event_logging
             mainchain_validator
             configuration
         in
         return_unit)
