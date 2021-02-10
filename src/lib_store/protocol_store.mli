(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Protocol store *)

(** The type for the protocol store. *)
type t

(** [mem pstore proto_hash] tests the existence of the protocol
    indexed by [proto_hash] in the store. *)
val mem : t -> Protocol_hash.t -> bool

(** [all pstore] returns the set of all stored protocols in [pstore]. *)
val all : t -> Protocol_hash.Set.t

(** [raw_store pstore proto_hash proto_bytes] stores on disk the
    protocol [proto_bytes] (encoded bytes) indexed as
    [proto_hash]. Returns [None] if the protocol already exists. *)
val raw_store : t -> Protocol_hash.t -> bytes -> Protocol_hash.t option Lwt.t

(** [store pstore proto_hash protocol] stores on disk the protocol
    [protocol] indexed as [proto_hash]. Returns [None] if the protocol
    already exists. *)
val store : t -> Protocol_hash.t -> Protocol.t -> Protocol_hash.t option Lwt.t

(** [read pstore proto_hash] reads from [pstore] and returns the
   protocol indexed by [proto_hash]. Returns [None] if the protocol
   cannot be read. *)
val read : t -> Protocol_hash.t -> Protocol.t option Lwt.t

(** [init store_dir] creates a store relatively to [store_dir] path
    or loads it if it already exists. *)
val init : [`Store_dir] Naming.directory -> t Lwt.t
