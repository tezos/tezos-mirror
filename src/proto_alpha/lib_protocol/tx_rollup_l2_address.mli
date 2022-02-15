(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** This module introduces the types used to identify ticket holders
    within a transaction rollup. *)

(** The hash of a BLS public key is used as the primary identifier
    of ticket holders within a transaction rollup. *)
type t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

val to_b58check : t -> string

val of_b58check_opt : string -> t option

val of_b58check_exn : string -> t

val of_bytes_exn : bytes -> t

val of_bytes_opt : bytes -> t option

val compare : t -> t -> int

(** [of_bls_pk pk] computes the address of the L2 tickets holder
    authentified by [pk]. *)
val of_bls_pk : Bls_signature.pk -> t

(** [in_memory_size a] returns the number of bytes allocated in RAM for [a]. *)
val in_memory_size : t -> Cache_memory_helpers.sint

(** [size a] returns the number of bytes allocated in an inbox to store [a]. *)
val size : t -> int

module Indexable : sig
  type nonrec index = t Indexable.index

  type nonrec value = t Indexable.value

  type nonrec either = t Indexable.either

  val encoding : either Data_encoding.t

  val index_encoding : index Data_encoding.t

  val compare : either -> either -> int

  val compare_values : value -> value -> int

  val forget_value : value -> either

  val forget_index : index -> either

  val value : t -> value

  val index : int32 -> index tzresult

  val index_exn : int32 -> index

  val from_value : t -> either

  val from_index : int32 -> either tzresult

  val from_index_exn : int32 -> either

  val prepare_index :
    (t -> int32 tzresult Lwt.t) -> either -> index tzresult Lwt.t

  val prepare_value :
    (int32 -> t tzresult Lwt.t) -> either -> value tzresult Lwt.t

  val pp : Format.formatter -> either -> unit

  val size : either -> int

  val in_memory_size : either -> Cache_memory_helpers.sint

  type nonrec 'state t = ('state, t) Indexable.t
end
