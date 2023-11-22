(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Tezos_rpc.Context

type chain_status =
  | Active_main of Chain_id.t
  | Active_test of {
      chain : Chain_id.t;
      protocol : Protocol_hash.t;
      expiration_date : Time.Protocol.t;
    }
  | Stopping of Chain_id.t

val bootstrapped :
  #streamed ->
  ((Block_hash.t * Time.Protocol.t) Lwt_stream.t * stopper) tzresult Lwt.t

(** Call RPC GET /monitor/validated_blocks

  - Default [chains] is [Main].
  - Default [protocols] is [[]].
  - Default [next_protocols] is [[]]. *)
val validated_blocks :
  #streamed ->
  ?chains:Chain_services.chain list ->
  ?protocols:Protocol_hash.t list ->
  ?next_protocols:Protocol_hash.t list ->
  unit ->
  ((Chain_id.t * Block_hash.t * Block_header.t * Operation.t list list)
   Lwt_stream.t
  * stopper)
  tzresult
  Lwt.t

(** Call RPC GET /monitor/applied_blocks

  - Default [chains] is [Main].
  - Default [protocols] is [[]].
  - Default [next_protocols] is [[]]. *)
val applied_blocks :
  #streamed ->
  ?chains:Chain_services.chain list ->
  ?protocols:Protocol_hash.t list ->
  ?next_protocols:Protocol_hash.t list ->
  unit ->
  ((Chain_id.t * Block_hash.t * Block_header.t * Operation.t list list)
   Lwt_stream.t
  * stopper)
  tzresult
  Lwt.t

val heads :
  #streamed ->
  ?protocols:Protocol_hash.t list ->
  ?next_protocols:Protocol_hash.t list ->
  Chain_services.chain ->
  ((Block_hash.t * Block_header.t) Lwt_stream.t * stopper) tzresult Lwt.t

val protocols :
  #streamed -> (Protocol_hash.t Lwt_stream.t * stopper) tzresult Lwt.t

val commit_hash : #simple -> string tzresult Lwt.t

val active_chains :
  #streamed -> (chain_status list Lwt_stream.t * stopper) tzresult Lwt.t

module S : sig
  val bootstrapped :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      Block_hash.t * Time.Protocol.t )
    Tezos_rpc.Service.t

  (** Define RPC GET /monitor/validated_blocks

  - Default [chains] is [Main].
  - Default [protocols] is [[]].
  - Default [next_protocols] is [[]]. *)
  val validated_blocks :
    ( [`GET],
      unit,
      unit,
      < chains : Chain_services.chain list
      ; next_protocols : Protocol_hash.t list
      ; protocols : Protocol_hash.t list >,
      unit,
      Chain_id.t * Block_hash.t * Block_header.t * Operation.t list list )
    Tezos_rpc.Service.t

  (** Define RPC GET /monitor/applied_blocks

  - Default [chains] is [Main].
  - Default [protocols] is [[]].
  - Default [next_protocols] is [[]]. *)
  val applied_blocks :
    ( [`GET],
      unit,
      unit,
      < chains : Chain_services.chain list
      ; next_protocols : Protocol_hash.t list
      ; protocols : Protocol_hash.t list >,
      unit,
      Chain_id.t * Block_hash.t * Block_header.t * Operation.t list list )
    Tezos_rpc.Service.t

  val heads :
    ( [`GET],
      unit,
      unit * Chain_services.chain,
      < protocols : Protocol_hash.t list
      ; next_protocols : Protocol_hash.t list >,
      unit,
      Block_hash.t * Block_header.t )
    Tezos_rpc.Service.t

  val protocols :
    ([`GET], unit, unit, unit, unit, Protocol_hash.t) Tezos_rpc.Service.t

  val commit_hash : ([`GET], unit, unit, unit, unit, string) Tezos_rpc.Service.t

  val active_chains :
    ([`GET], unit, unit, unit, unit, chain_status list) Tezos_rpc.Service.t
end
