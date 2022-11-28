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

(** A shard storage on disk.

    To each stored slot is associated a directory named
    after the b58 encoding of the slot commitment. The shards are files in this
    directory, named after the shard index and whose content is a byte
    representation of the share. *)
type t

(** [init path] initiates a shard storage at [path] *)
val init : string -> t Lwt.t

(** [write_shards store commitment shards] stores the set [shards] on [store]
    associated with [commitment]. In case of IO error, [Io_error] is returned. *)
val write_shards :
  t ->
  Cryptobox.Commitment.t ->
  Cryptobox.share Cryptobox.IntMap.t ->
  unit tzresult Lwt.t

(** [read_shards ~share_size dal_constants store commitment] fetches the set of
    shards associated with [commitment] in [store]. The expected size of shards
    is given by [dal_constants]. In case of IO error, [Io_error] is returned. *)
val read_shards :
  share_size:int ->
  t ->
  Cryptobox.Commitment.t ->
  Cryptobox.share Cryptobox.IntMap.t tzresult Lwt.t

(** [read_shard ~share_size store commitment shard_id] fetches the shard
    associated to [commitment] in [store] with id [shard_id]. In case of IO
    error, [Io_error] is returned.*)
val read_shard :
  share_size:int ->
  t ->
  Cryptobox.Commitment.t ->
  int ->
  Cryptobox.shard tzresult Lwt.t

(** [read_shards_subset] has the same behavior as [read_shard] but fetches multiple
    shard. *)
val read_shards_subset :
  share_size:int ->
  t ->
  Cryptobox.Commitment.t ->
  int list ->
  Cryptobox.shard list tzresult Lwt.t
