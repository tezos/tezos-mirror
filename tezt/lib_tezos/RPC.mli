(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** In all RPCs, default [chain] is "main" and default [block] is "head". *)

(** Call RPC /network/connections if [peer_id] is [None].
    Call RPC /network/connections/[peer_id] otherwise. *)
val get_connections :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?peer_id:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/chain_id *)
val get_chain_id :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain] *)
val force_bootstrapped :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?bootstrapped:bool ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/checkpoint *)
val get_checkpoint :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /injection/block *)
val inject_block :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/header/protocol_data *)
val get_protocol_data :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  ?offset:int ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/operations *)
val get_operations :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chains/[chain]/mempool/pending_operations *)
val get_mempool_pending_operations :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/preapply/block *)
val preapply_block :
  ?node:Node.t ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

module Proto_alpha : sig
  include RPC_intf.COMMON_PROTOCOL

  module Contract : sig
    include RPC_intf.COMMON_PROTOCOL_CONTRACT
  end

  module Delegate : sig
    include RPC_intf.COMMON_PROTOCOL_DELEGATE

    (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/voting_power *)
    val get_voting_power :
      ?node:Node.t ->
      ?hooks:Process.hooks ->
      ?chain:string ->
      ?block:string ->
      pkh:string ->
      Client.t ->
      JSON.t Lwt.t

    (** Same as [get_voting_power], but do not wait for the process to exit. *)
    val spawn_get_voting_power :
      ?node:Node.t ->
      ?hooks:Process.hooks ->
      ?chain:string ->
      ?block:string ->
      pkh:string ->
      Client.t ->
      Process.t
  end

  module Votes : sig
    include RPC_intf.COMMON_PROTOCOL_VOTES

    (** Call RPC /chain/[chain]/blocks/[block]/votes/current_period *)
    val get_current_period :
      ?node:Node.t ->
      ?hooks:Process.hooks ->
      ?chain:string ->
      ?block:string ->
      Client.t ->
      JSON.t Lwt.t

    (** Call RPC /chain/[chain]/blocks/[block]/votes/successor_period *)
    val get_successor_period :
      ?node:Node.t ->
      ?hooks:Process.hooks ->
      ?chain:string ->
      ?block:string ->
      Client.t ->
      JSON.t Lwt.t

    (** Call RPC /chain/[chain]/blocks/[block]/votes/total_voting_power *)
    val get_total_voting_power :
      ?node:Node.t ->
      ?hooks:Process.hooks ->
      ?chain:string ->
      ?block:string ->
      Client.t ->
      JSON.t Lwt.t
  end
end

module Proto_007 : sig
  include RPC_intf.COMMON_PROTOCOL

  module Contract : sig
    include RPC_intf.COMMON_PROTOCOL_CONTRACT
  end

  module Delegate : sig
    include RPC_intf.COMMON_PROTOCOL_DELEGATE
  end

  module Votes : sig
    include RPC_intf.COMMON_PROTOCOL_VOTES
  end
end
