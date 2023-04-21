(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

module S : sig
  (** Define RPC GET /monitor/root_hashes. *)
  val root_hashes :
    Dac_plugin.t ->
    ([`GET], unit, unit, unit, unit, Dac_plugin.hash) Tezos_rpc.Service.service

  (** Define RPC GET /monitor/certificate/{hex_root_hash}. *)
  val certificate :
    Dac_plugin.t ->
    ( [`GET],
      unit,
      unit * Dac_plugin.hash,
      unit,
      unit,
      Certificate_repr.t )
    Tezos_rpc.Service.service
end

(** [root_hashes streamed_cctxt dac_plugin] returns a stream of root hashes
    and a stopper for it.

    Stream is produced by calling RPC GET /monitor/root_hashes.
*)
val root_hashes :
  #Tezos_rpc.Context.streamed ->
  Dac_plugin.t ->
  (Dac_plugin.hash Lwt_stream.t * Tezos_rpc.Context.stopper)
  Error_monad.tzresult
  Lwt.t

val certificate :
  #Tezos_rpc.Context.streamed ->
  Dac_plugin.t ->
  Dac_plugin.hash ->
  (Certificate_repr.t Lwt_stream.t * Tezos_rpc.Context.stopper)
  Error_monad.tzresult
  Lwt.t
