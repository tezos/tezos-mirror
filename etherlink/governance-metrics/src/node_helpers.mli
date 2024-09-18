(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type header = {level : int}

module RPC : sig
  (** [chain_id base] returns the L1 chain identifier. *)
  val chain_id : Uri.t -> string tzresult Lwt.t

  (** [micheline_view ~chain_id ~contract ~view ~decode base] returns the decoded
      view of a smart contract. *)
  val micheline_view :
    chain_id:string ->
    contract:string ->
    view:string ->
    decode:(Tezos_micheline.Micheline_parser.node -> 'a tzresult) ->
    Uri.t ->
    'a tzresult Lwt.t

  (** [storage ~contract ~decode base] returns the decoded storage of a smart
      contract. *)
  val storage :
    contract:string ->
    decode:(Tezos_micheline.Micheline_parser.node -> 'a tzresult) ->
    Uri.t ->
    'a tzresult Lwt.t

  (** [monitor_heads ~process base] will monitor each head (streamed chunk) and
      apply [process] on the received chunk. *)
  val monitor_heads :
    process:(header -> unit tzresult Lwt.t) -> Uri.t -> unit tzresult Lwt.t
end
