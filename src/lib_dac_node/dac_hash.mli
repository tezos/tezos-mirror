(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

(** This module exposes a hash representation that abstracts over the protocol
    reveal hash type.
*)
type t

(** Operations of [Dac_hash.t] that relate to [Protocol.Sc_rollup_reveal_hash.t]. *)
module type Reveal_hash_mapper = sig
  (** Protocol reveal hash type typically coming from 
      [Protocol.Sc_rollup_reveal_hash.t].
  *)
  type reveal_hash

  (** Derives a [Dac_hash.t] from [reveal_hash]. *)
  val of_reveal_hash : reveal_hash -> t

  (** Derives a [reveal_hash] from [Dac_hash.t]. *)
  val to_reveal_hash : t -> reveal_hash

  val encoding : t Data_encoding.t
end

module Make (P : Dac_plugin.Protocol_reveal_hash) :
  Reveal_hash_mapper with type reveal_hash := P.t
