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
type t = bytes

module type Reveal_hash_mapper = sig
  type reveal_hash

  val of_reveal_hash : reveal_hash -> t

  val to_reveal_hash : t -> reveal_hash

  val encoding : t Data_encoding.t
end

module Make (H : Dac_plugin.Protocol_reveal_hash) = struct
  (* NB: Safe since failure case only occurs if [buffer_size] is given. *)
  let of_reveal_hash proto_reveal_hash =
    Data_encoding.Binary.to_bytes_exn H.encoding proto_reveal_hash

  (** NB: Safe since the [Dac_hash.t] could only be constructed from a
      [H.t]. *)
  let to_reveal_hash dac_hash =
    Data_encoding.Binary.of_bytes_exn H.encoding dac_hash

  let encoding = Data_encoding.conv to_reveal_hash of_reveal_hash H.encoding
end
