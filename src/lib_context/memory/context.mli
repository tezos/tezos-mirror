(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Implementation of Tezos context fully in memory. *)
module Make (Encoding : module type of Tezos_context_encoding.Context) : sig
  type index

  include Tezos_context_sigs.Context.S with type index := index

  val index : t -> index

  val exists : index -> Context_hash.t -> bool Lwt.t

  val checkout : index -> Context_hash.t -> t option Lwt.t

  val checkout_exn : index -> Context_hash.t -> t Lwt.t

  val hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t

  val commit :
    time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t Lwt.t

  val create : unit -> t

  val empty : t

  val encoding : t Data_encoding.t

  val get_protocol : t -> Protocol_hash.t Lwt.t

  val add_protocol : t -> Protocol_hash.t -> t Lwt.t

  (** Get the hash version used for the context *)
  val get_hash_version : t -> Context_hash.Version.t

  (** Set the hash version used for the context.  It may recalculate the hashes
    of the whole context, which can be a long process.
    Returns an [Error] if the hash version is unsupported. *)
  val set_hash_version : t -> Context_hash.Version.t -> t tzresult Lwt.t

  (** Exception raised by [find_tree] and [add_tree] when applied to shallow
    trees. It is exposed so that it can be catched by the proxy where such
    operations on shallow trees are expected. *)
  exception Context_dangling_hash of string

  val add_predecessor_block_metadata_hash :
    t -> Block_metadata_hash.t -> t Lwt.t

  val add_predecessor_ops_metadata_hash :
    t -> Operation_metadata_list_list_hash.t -> t Lwt.t

  val get_test_chain : t -> Test_chain_status.t Lwt.t

  val add_test_chain : t -> Test_chain_status.t -> t Lwt.t
end
