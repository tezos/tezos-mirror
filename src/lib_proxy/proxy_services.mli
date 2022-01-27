(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Exception used by the proxy mode when creation of the input environment
    (of the RPC handler) fails. This exception is used to temporarily escape
    from the monad, because at the point of throwing, the code is NOT in
    [tzresult Lwt.t]. As it's dealing with resto APIs it's in an [Lwt.t]-only
    monad. This exception is injected back in the tzresult [Lwt.t] monad at the
    point where it is caught (with [Lwt.catch]). *)
exception Rpc_dir_creation_failure of tztrace

(** Whether using the light mode or the proxy mode (remember that
    the light mode is a different instance of the proxy mode
    (see srcs/lib_proxy/README_LIGHT.md for documentation)
    and whether [tezos-client] or [tezos-proxy-server] is running. *)
type mode =
  | Light_client of Light.sources  (** [tezos-client --mode light] is running *)
  | Proxy_client  (** [tezos-client --mode proxy] is running *)
  | Proxy_server of int option
      (** [tezos-proxy-server] is running and the [int option] value
          is the value of argument [--sym-block-caching-time] *)

(** [build_directory printer rpc_context env mode] returns the directory
    of RPCs that is served locally by the client's light and proxy modes and
    by the proxy server. Parameters are:

    - [printer] is used for logging.
    - [rpc_context] is used to perform RPCs to distant endpoints.
    - [mode] specifies whether [tezos-client] (light or proxy mode)
      or [tezos-proxy-server] is running.
    - [env] is a protocol-specific module used to create the context passed when executing a RPC. *)
val build_directory :
  Tezos_client_base.Client_context.printer ->
  RPC_context.generic ->
  mode ->
  Registration.proxy_environment ->
  unit RPC_directory.t
