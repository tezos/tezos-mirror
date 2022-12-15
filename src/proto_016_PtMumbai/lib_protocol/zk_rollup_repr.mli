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

(** A ZK rollup has an address starting with "epx1".
    ZKRU addresses have a length of 20 bytes, which means
    that they have an injective encoding as BLS12-381 scalars.
*)
module Address : sig
  include S.HASH

  (** [from_nonce nonce] produces an address completely determined by
     an operation hash and an origination counter. *)
  val from_nonce : Origination_nonce.t -> t tzresult

  (** [encoded_size] is the number of bytes needed to represent an address. *)
  val encoded_size : int

  val of_b58data : Base58.data -> t option

  val prefix : string
end

type t = Address.t

(** [to_scalar address] returns the scalar corresponding to [address] *)
val to_scalar : t -> Zk_rollup_scalar.t

(** Description of a ZK rollup's pending list. *)
type pending_list =
  | Empty of {next_index : int64}
      (** Empty pending list but starting point will be [next_index]
          when adding to the list *)
  | Pending of {next_index : int64; length : int}
      (** Pending list with
          [(next_index - length) .. (next_index - 1)].
          [length] is encoded as a [uint16]. *)

val pending_list_encoding : pending_list Data_encoding.t

module Index : Storage_description.INDEX with type t = t

(** [in_memory_size zk_rollup] returns the number of bytes a [zk_rollup]
    address uses in RAM. *)
val in_memory_size : t -> Cache_memory_helpers.sint

module Internal_for_tests : sig
  val originated_zk_rollup : Origination_nonce.t -> Address.t
end
