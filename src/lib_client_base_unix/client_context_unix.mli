(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

class unix_wallet :
  base_dir:string -> password_filename:string option -> Client_context.wallet

class unix_prompter : Client_context.prompter

class unix_logger : base_dir:string -> Client_context.printer

class unix_io_wallet :
  base_dir:string -> password_filename:string option -> Client_context.io_wallet

class unix_ui : Client_context.ui

class unix_full :
  base_dir:string
  -> chain:Shell_services.chain
  -> block:Shell_services.block
  -> confirmations:int option
  -> password_filename:string option
  -> rpc_config:Tezos_rpc_http_client_unix.RPC_client_unix.config
  -> verbose_rpc_error_diagnostics:bool
  -> Client_context.full

class unix_mockup :
  base_dir:string
  -> mem_only:bool
  -> mockup_env:Tezos_mockup_registration.Registration.mockup_environment
  -> chain_id:Chain_id.t
  -> rpc_context:Tezos_protocol_environment.rpc_context
  -> protocol_data:bytes
  -> Client_context.full

class unix_proxy :
  base_dir:string
  -> chain:Shell_services.chain
  -> block:Shell_services.block
  -> confirmations:int option
  -> password_filename:string option
  -> rpc_config:Tezos_rpc_http_client_unix.RPC_client_unix.config
  -> mode:Tezos_proxy.Proxy_services.mode
  -> proxy_env:Tezos_proxy.Registration.proxy_environment
  -> Client_context.full
