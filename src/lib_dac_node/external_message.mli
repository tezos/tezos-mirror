(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech  <contact@trili.tech>                  *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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

(** External Message module. *)

type error += Could_not_serialize_rollup_external_message of string

type t = {
  root_hash : Dac_plugin.raw_hash;
  signature : Tezos_crypto.Aggregate_signature.signature;
  witnesses : Z.t;
}

(** Default implementation. *)
module Default : sig
  (** [make dac_plugin root_hash signature witnesses] returns the L1 external message
      as a byte sequence of the form [tag] ^ [root_hash] ^ [signature] ^ [witnesses].
  *)
  val make :
    Dac_plugin.t ->
    Dac_plugin.raw_hash ->
    Tezos_crypto.Aggregate_signature.signature ->
    Z.t ->
    bytes tzresult Lwt.t

  (** [of_bytes dac_hash_encoding payload] deserializes [payload] into a 
      [dac_message]. *)
  val of_bytes : Dac_plugin.raw_hash Data_encoding.t -> bytes -> t option
end
