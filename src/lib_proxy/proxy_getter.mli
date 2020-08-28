(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type proxy_getter_input = {
  rpc_context : RPC_context.simple;
  chain : Tezos_shell_services.Block_services.chain;
  block : Tezos_shell_services.Block_services.block;
}

(** Converts a tree as returned by the RPC to a tree in terms of the
    client's context *)
val raw_context_to_tree :
  Tezos_shell_services.Block_services.raw_context ->
  Proxy_context.M.tree option

(** Module encapsulating the protocol-dependent functions:

    [do_rpc] performs
    the call to /chains/<chain_id>/blocks/<block>/context/raw/bytes/

    [split_key] potentially rewrites requests to retrieve
    a parent key of the actual key. This heuristic is also protocol-dependent.
  *)
module type PROTO_RPC = sig
  (** When about to do a RPC request, how to modify the request;
    to possibly request a parent tree of the key, to batch
    successive requests into one.

    For example when requesting baking_rights, there's the following
    sequence of requests (for carthage):

    v1;constants
    v1;first_level

    To speed things up, this function returns "v1" on any of these keys,
    to do a single request instead of two. This list is arbitrary. It was
    built by initial input from @klakplok and by experimenting.

    This function guarantees that if it returns Some(prefix, suffix) then
    prefix @@ suffix = key.
  *)
  val split_key :
    Proxy_context.M.key -> (Proxy_context.M.key * Proxy_context.M.key) option

  val do_rpc :
    proxy_getter_input ->
    Proxy_context.M.key ->
    Proxy_context.M.tree option tzresult Lwt.t
end

module type M = sig
  (** Whether the key is mapped to a directory *)
  val proxy_dir_mem :
    proxy_getter_input -> Proxy_context.M.key -> bool tzresult Lwt.t

  (** The value to which a key maps *)
  val proxy_get :
    proxy_getter_input ->
    Proxy_context.M.key ->
    Proxy_context.M.tree option tzresult Lwt.t

  (** Whether the key is mapped to a value *)
  val proxy_mem :
    proxy_getter_input -> Proxy_context.M.key -> bool tzresult Lwt.t
end

module Make (X : PROTO_RPC) : M
