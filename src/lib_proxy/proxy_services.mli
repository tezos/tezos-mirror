(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Whether using the light mode or the proxy mode (remember that
    the light mode is a different instance of the proxy mode
    (see srcs/lib_proxy/README_LIGHT.md for documentation). *)
type mode = Light of Light.sources | Proxy

(** [build_directory printer rpc_context mode env] returns the directory
    of RPCs that is served locally by the proxy mode
    and the light mode. [printer] is used for logging. [rpc_context] is
    used to perform RPCs to distant endpoints. [mode] specifies whether
    to use the proxy mode or the light mode. [env] is a protocol-specific
    module used to create the context passed when executing a RPC. *)
val build_directory :
  Tezos_client_base.Client_context.printer ->
  RPC_context.json ->
  mode ->
  Registration.proxy_environment ->
  unit RPC_directory.t
