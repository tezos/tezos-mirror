(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Tezos Protocol Implementation - Typed storage builders.

    Contains functors used by [Storage] to create the structure on
    disk.

  See {!Make_subcontext}
 *)

open Storage_sigs

module Registered : REGISTER

module Ghost : REGISTER

(** Given a [Raw_context], return a new [Raw_context] that projects into
    a given subtree. Similar to a {i functional lens}.
 *)
module Make_subcontext (_ : REGISTER) (C : Raw_context.T) (_ : NAME) :
  Raw_context.T with type t = C.t

module Make_single_data_storage
    (_ : REGISTER)
    (C : Raw_context.T)
    (_ : NAME)
    (V : VALUE) : Single_data_storage with type t = C.t and type value = V.t

(** A type that can be serialized as a [string list], and used
    as a prefix in the typed datastore.

    Useful to implement storage of maps and sets.
 *)
module type INDEX = sig
  type t

  include Path_encoding.S with type t := t

  type 'a ipath

  val args : ('a, t, 'a ipath) Storage_description.args
end

module Pair (I1 : INDEX) (I2 : INDEX) : INDEX with type t = I1.t * I2.t

(** Create storage for a compound type. *)
module Make_data_set_storage (C : Raw_context.T) (I : INDEX) :
  Data_set_storage with type t = C.t and type elt = I.t

(** Like [Make_data_set_storage], adding tracking of storage cost. *)
module Make_carbonated_data_set_storage (C : Raw_context.T) (I : INDEX) :
  Carbonated_data_set_storage with type t = C.t and type elt = I.t

(** This functor creates storage for types with a notion of an index. *)
module Make_indexed_data_storage (C : Raw_context.T) (I : INDEX) (V : VALUE) :
  Indexed_data_storage with type t = C.t and type key = I.t and type value = V.t

(** Like [Make_indexed_data_storage], adding tracking of storage cost. *)
module Make_indexed_carbonated_data_storage
    (C : Raw_context.T)
    (I : INDEX)
    (V : VALUE) :
  Indexed_carbonated_data_storage
    with type t = C.t
     and type key = I.t
     and type value = V.t

module Make_indexed_data_snapshotable_storage
    (C : Raw_context.T)
    (Snapshot : INDEX)
    (I : INDEX)
    (V : VALUE) :
  Indexed_data_snapshotable_storage
    with type t = C.t
     and type snapshot = Snapshot.t
     and type key = I.t
     and type value = V.t

module Make_indexed_subcontext (C : Raw_context.T) (I : INDEX) :
  Indexed_raw_context
    with type t = C.t
     and type key = I.t
     and type 'a ipath = 'a I.ipath

module type WRAPPER = sig
  type t

  type key

  val wrap : t -> key

  val unwrap : key -> t option
end

module Wrap_indexed_data_storage
    (C : Indexed_data_storage)
    (K : WRAPPER with type key := C.key) :
  Indexed_data_storage
    with type t = C.t
     and type key = K.t
     and type value = C.value
