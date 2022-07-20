(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(**
   Functions to manage slots storage.

   - writing a slot means splitting it in shards and store them on disk
   - reading a slot means rebuild it from the shards
   *)

(** [split_and_store cb_constants ts store slot] splits [slot] in shards, stores
    it onto the disk and returns the corresponding [slot_header], using
    [cb_constants] and trusted setup [ts] *)
val split_and_store :
  Cryptobox.t ->
  Cryptobox.srs ->
  Store.t ->
  Cryptobox.slot ->
  Cryptobox.slot_header tzresult Lwt.t

(** [get_shard store slot_header shard_id] gets the shard associated to
    [slot_header] at the range [shard_id] *)
val get_shard :
  Store.t -> Cryptobox.slot_header -> int -> Cryptobox.shard tzresult Lwt.t

(** [get_slot cb_constants store slot_header] fetches from disk the shards associated to
    [slot_header], gathers them, rebuilds and returns the [slot]. *)
val get_slot :
  Cryptobox.t ->
  Store.t ->
  Cryptobox.slot_header ->
  Cryptobox.slot tzresult Lwt.t

module Slot_header : sig
  type t = Cryptobox.commitment

  val to_b58check : t -> string

  val of_b58check_opt : string -> t option

  val rpc_arg : t RPC_arg.t
end

module Utils : sig
  (** [trim_x00 b] removes trailing '\000' at the end of a [b] and returns a new
      [bytes]. This function in needed to debug the fetching a slot and remove
      spurious uneeded data form it. *)
  val trim_x00 : bytes -> bytes

  (** [fill_x00 b] fills a bytes with '\000' to match
      [Cryptobox.Constants.slot_size] *)
  val fill_x00 : bytes -> bytes
end
