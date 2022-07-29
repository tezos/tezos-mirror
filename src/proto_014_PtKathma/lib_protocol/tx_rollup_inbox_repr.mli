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

module Merkle : sig
  (** See {!Merkle_List} for the documentation of those functions. *)

  type tree

  type root

  type path

  val empty : tree

  val root : tree -> root

  val ( = ) : root -> root -> bool

  val compare : root -> root -> int

  val root_encoding : root Data_encoding.t

  val root_of_b58check_opt : string -> root option

  val pp_root : Format.formatter -> root -> unit

  val path_encoding : path Data_encoding.t

  val add_message : tree -> Tx_rollup_message_hash_repr.t -> tree

  val compute_path : Tx_rollup_message_hash_repr.t list -> int -> path tzresult

  val check_path :
    path -> int -> Tx_rollup_message_hash_repr.t -> root -> bool tzresult

  val path_depth : path -> int

  (** [merklize_list messages] construct a merkle root by build a
     tree, appending the [messages] one by one in the same order of
     the list and finally computing the root. *)
  val merklize_list : Tx_rollup_message_hash_repr.t list -> root
end

(** The view of an inbox: stores the [cumulated_size] in bytes for the
    inbox, the [inbox_length] ({i i.e.}, the number of messages), and
    the cumulative [hash] of the inbox contents. For newly created
    inboxes, the [hash] is initialized as an array 32 null
    byte. *)
type t = {inbox_length : int; cumulated_size : int; merkle_root : Merkle.root}

(** [size] is the number of bytes necessary to store an inbox in the
    layer-1 storage. *)
val size : Z.t

val ( = ) : t -> t -> bool

val encoding : t Data_encoding.t

val empty : t

val pp : Format.formatter -> t -> unit
