(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type protocol = Nairobi | Oxford | Proto_alpha

(** [internal_message_kind] represent an internal message in a inbox. *)
type internal_message_kind =
  | Transfer (* Generic internal message. *)
  | Start_of_level
      (** Internal message put at the beginning of each inbox's level. *)
  | End_of_level  (** Internal message put at the end of each inbox's level. *)
  | Info_per_level
      (** Internal message containing the timestamp of the current block and the
          hash of the previous block. *)
  | Protocol_migration of protocol

(** A type representing messages from Layer 1 to Layer 2. Internal ones are
    originated from Layer 1 smart-contracts and external ones are messages from
    an external manager operation. Other messages represents non decodable tags.
    It aims to be future-proof and prevent discarding any new type of
    message.. *)
type t = Internal of internal_message_kind | External | Other

(** [from_raw_input input] takes a message produced by the L1 protocol and
    returns its kind. *)
val from_raw_input : string -> t

module Internal_for_tests : sig
  (** [to_binary_input kind input] returns the serialized representation of an
      [input] according to its [kind]. Internal message payloads and external
      message payloads are prefixed by their tag, and `Other` messages result in
      an exception. These messages are meant to be used in tests only, and does
      not give any guarantee on their validity according to the protocol's
      representation.

      @raise Failure on `Other` messages, and mismatches between kind and
        presence or absence of the input.
*)
  val to_binary_input : t -> string option -> string
end
