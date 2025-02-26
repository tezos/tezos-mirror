(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** HTTP client context with automatic retries on connection errors *)

(** This class type extends [Tezos_rpc.Context.generic] with an additional
    method to access the endpoint URI. *)
class type t = object
  inherit Tezos_rpc.Context.generic

  (** [endpoint] returns the base URI of the RPC endpoint *)
  method endpoint : Uri.t
end

(** [ctxt ~timeout endpoint medias] creates a new HTTP client context that
    automatically retries failed RPC calls.

    When a connection error occurs, the client will retry with backoff, starting
    at 1 second and doubling up to a maximum of 30 seconds.

    @param timeout Maximum time in seconds to wait for each RPC call
    @param endpoint Base URI for the RPC endpoint
    @param medias List of supported media types
    @return A context object that handles retries automatically
*)
class ctxt : timeout:float -> Uri.t -> Media_type.t list -> t
