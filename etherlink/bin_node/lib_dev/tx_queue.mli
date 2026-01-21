(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** The Tx_queue is a worker allowing to batch raw transactions in a single
    [eth_sendRawTransaction] at a regular interval. It provides a non-blocking
    interface based on the use of callbacks. *)

(** [tx_container ~chain_family] is a pair [(start, container)] where
    [start ~config ~keep_alive ~timeout ~start_injector_worker ()] starts the
    worker, meaning it is possible to call {!inject}, {!confirm} and
    {!beacon}, [~start_injector_worker] starts a second worker that ensures
    batch are injected sequentially. And [container] is a wrapper of the
    Tx_queue to be compatible with the Tx_container signature for the services. *)
val tx_container :
  chain_family:'f L2_types.chain_family ->
  (config:Configuration.tx_queue ->
  keep_alive:bool ->
  timeout:float ->
  start_injector_worker:bool ->
  unit ->
  unit tzresult Lwt.t)
  * 'f Services_backend_sig.tx_container

(**/*)

module Internal_for_tests : sig
  module Address_nonce : sig
    type t

    val empty : start_size:int -> t

    val add :
      t ->
      addr:string ->
      next_nonce:Z.t ->
      nonce:'a ->
      add:(Nonce_bitset.t -> 'a -> Nonce_bitset.t tzresult) ->
      unit tzresult

    val find : t -> addr:string -> Nonce_bitset.t option

    val confirm_nonce :
      t -> addr:string -> nonce:'a -> next:('a -> Z.t) -> unit tzresult

    val remove :
      t ->
      addr:string ->
      nonce:'a ->
      rm:(Nonce_bitset.t -> 'a -> Nonce_bitset.t tzresult) ->
      unit tzresult

    val next_gap : t -> addr:string -> next_nonce:Z.t -> Z.t tzresult
  end
end
