(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
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

(** Describes the different representations that can be stored persistently. *)

(** Storage. Each "table" is described under a different path. *)
type t

(** [load data_dir] loads the repository (stored in [data_dir]) that persists
    the various tables. If there no already resgistered store, a new one will
    be created. *)
val load : string -> t tzresult Lwt.t

(** After use [close data_dir], closes the storage. The daemon calls this function
    during the termination callback.*)
val close : string -> unit Lwt.t

(** {1 General Interfaces} *)

(** An interface that describes a generic Key-Value storage. *)
module type MAP = sig
  type nonrec t = t

  type key

  type value

  val mem : t -> key -> bool Lwt.t

  val find : t -> key -> value option Lwt.t

  val get : t -> key -> value tzresult Lwt.t

  val add : t -> key -> value -> unit tzresult Lwt.t

  val remove : t -> key -> unit tzresult Lwt.t
end

(** An interface that describes a generic value reference. *)
module type REF = sig
  type nonrec t = t

  type value

  val find : t -> value option Lwt.t

  val get : t -> value tzresult Lwt.t

  val set : t -> value -> unit tzresult Lwt.t
end

(** {1 Storages} *)

(** {2 References} *)

(** A persistent reference to the rollup origination block (initialized on first
   run). *)
module Rollup_origination : REF with type value = Block_hash.t * int32

(** A persistent reference cell that stores the hash of the head of the
    rollup. *)
module L2_head : REF with type value = L2block.hash

(** {2 Maps} *)

(** Persistent storage for mapping Tezos blocks to their L2 counterpart. *)
module Tezos_blocks :
  MAP with type key = Block_hash.t and type value = L2block.hash

(** Persistent storage for inboxes, indexed by an L2 block hash. Each block has
    a single inbox. *)
module Inboxes : MAP with type key = L2block.hash and type value = Inbox.t

(** Persistent storage for transaction L2 block headers. *)
module L2_blocks :
  MAP with type key = L2block.hash and type value = L2block.header

(** Persistent storage for associating an L2 block hash to each rollup level. *)
module Rollup_levels :
  MAP with type key = L2block.level and type value = L2block.hash
