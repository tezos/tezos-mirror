(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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

module Hash : sig
  val prefix : string

  include S.HASH
end

type t = Hash.t

(** [in_memory_size event_addr] returns the number of bytes [event_addr]
    uses in RAM. *)
val in_memory_size : t -> Cache_memory_helpers.sint

(** [to_b58check addr] converts the event address [addr] to the Base58Check string representation *)
val to_b58check : t -> string

(** Pretty printer for contract events *)
val pp : Format.formatter -> t -> unit

(** [of_b58data data] tries to decode a contract event from a Base58 [data] and
    return [None] if conversion fails *)
val of_b58data : Base58.data -> t option

(** [of_b58check addr] tries to decode a contract event from a Base58Check string [addr] *)
val of_b58check : string -> t tzresult

(** [of_b58check_opt addr] tries to
    decode a contract event from a Base58Check string [addr]
    and return [None] if conversion fails *)
val of_b58check_opt : string -> t option
