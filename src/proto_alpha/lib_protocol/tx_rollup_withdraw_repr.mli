(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** A [withdrawal] gives right to a L1 address [claimer] to
    retrieve the quantity [amount] of a ticket whose hash is [ticket_hash].
    Withdrawals result from layer-2-to-layer-1 transfers, and from
    failed layer-2 deposits.*)
type withdrawal = {
  claimer : Signature.Public_key_hash.t;
  ticket_hash : Ticket_hash_repr.t;
  amount : Tx_rollup_l2_qty.t;
}

type t = withdrawal

val encoding : t Data_encoding.t

module Merkle : sig
  (** See {Merkle_List} for the documentation of those functions. *)

  type tree

  type root

  type path

  val nil : tree

  val empty : root

  val root : tree -> root

  val ( = ) : root -> root -> bool

  val compare : root -> root -> int

  val root_encoding : root Data_encoding.t

  val root_of_b58check_opt : string -> root option

  val pp_root : Format.formatter -> root -> unit

  val path_encoding : path Data_encoding.t

  val compute_path : withdrawal list -> int -> path tzresult

  val check_path : path -> int -> withdrawal -> root -> bool tzresult

  val path_depth : path -> int

  (** [merklize_list messages] construct a merkle root by build a
      tree, appending the [messages] one by one in the same order of
      the list and finally computing the root. *)
  val merklize_list : withdrawal list -> root
end

(** [maximum_path_depth ~withdraw_count_limit] returns the maximum
    depth of a path, depending on the maximimum number of a withdraw in
    an inbox given by [message_count_limit]. *)
val maximum_path_depth : withdraw_count_limit:int -> int

(** [Withdrawal_accounting] provides an interface for the storage to
   account for which withdrawals (as identified by their index) have
   been consumed. *)
module Withdrawal_accounting : sig
  type t

  val encoding : t Data_encoding.t

  (** The state of withdrawal accounting where no
      withdrawals have been consumed. *)
  val empty : t

  (** [get l index] returns [true] if the withdrawal identified by
      [index] has been been consumed (as registered through
      {!Withdrawal_accounting.set}). Fails when [index] is negative. *)
  val get : t -> int -> bool tzresult

  (** [set l index] registers that the withdrawal identified by
      [index] has been consumed. Fails when [index] is negative. *)
  val set : t -> int -> t tzresult
end
