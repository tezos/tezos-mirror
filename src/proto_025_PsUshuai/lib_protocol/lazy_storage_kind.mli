(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
  Lazy_storage offers a unified interface for specific Michelson datatype that
  behave somewhat lazily, because they are intended to be quite big.
  Instead of serializing/deserializing the whole value to/from the storage,
  only an identifier is used. The identifier acts like a pointer.
  When using the value in a Michelson script, some part of it may be read from
  the storage, and a lightweight diff is computed.
  The diff is effectively applied to the storage at the end of the execution.

  This module defines the different kinds of lazy storages and their basic
  properties. See also [Lazy_storage_diff].

  Lazy storage types are:
   - Big_map
*)

(**
  Lazy storage ids are kept as abstract as possible to avoid mixing them up.

  Behind the scene they are [Z.t]s but, within the protocol, only [parse_data]/
  [unparse_data] are allowed convert from/to it.

  Temporary ids may be used to pass values between contracts that won't be kept
  longer than the lifetime of the operation.
  Behind the scene, temporary ids are negative [Z.t]s.
*)
module type ID = sig
  type t

  val compare : t -> t -> int

  val encoding : t Data_encoding.t

  val rpc_arg : t RPC_arg.arg

  (** Initial value for ids: zero. *)
  val init : t

  (** In the protocol, to be used in parse_data only *)
  val parse_z : Z.t -> t

  (** In the protocol, to be used in unparse_data only *)
  val unparse_to_z : t -> Z.t

  val next : t -> t

  val is_temp : t -> bool

  (* To be removed once legacy big map diff is removed: *)

  val of_legacy_USE_ONLY_IN_Legacy_big_map_diff : Z.t -> t

  val to_legacy_USE_ONLY_IN_Legacy_big_map_diff : t -> Z.t

  (* To be used in storage: *)

  include Path_encoding.S with type t := t
end

module Big_map : sig
  val title : string

  module Id : ID

  type alloc = {key_type : Script_repr.expr; value_type : Script_repr.expr}

  type update = {
    key : Script_repr.expr;
        (** The key is ignored by [apply_update] but is shown in the receipt,
            as specified in [print_big_map_diff]. *)
    key_hash : Script_expr_hash.t;
    value : Script_repr.expr option;
  }

  type updates = update list

  val alloc_encoding : alloc Data_encoding.t

  val updates_encoding : updates Data_encoding.t
end

module Sapling_state : sig
  val title : string

  module Id : ID

  type alloc = {memo_size : Sapling_repr.Memo_size.t}

  type updates = Sapling_repr.diff

  val alloc_encoding : alloc Data_encoding.t

  val updates_encoding : updates Data_encoding.t
end

(**
  Kinds of lazy storage.
  The GADT ensures operations are properly applied to the correct kind.

  ['id] the abstract type for the identifier of the kind.
  ['alloc] is the type used to construct a new value.
  ['updates] is the type used to update a value.
*)
type ('id, 'alloc, 'updates) t =
  | Big_map : (Big_map.Id.t, Big_map.alloc, Big_map.updates) t
  | Sapling_state :
      (Sapling_state.Id.t, Sapling_state.alloc, Sapling_state.updates) t

type ex = Ex_Kind : (_, _, _) t -> ex

val all : (int * ex) list

type (_, _) cmp = Eq : ('a, 'a) cmp | Neq

val equal :
  ('i1, 'a1, 'u1) t ->
  ('i2, 'a2, 'u2) t ->
  ('i1 * 'a1 * 'u1, 'i2 * 'a2 * 'u2) cmp

type ('i, 'a, 'u) kind = ('i, 'a, 'u) t

(**
  Type to manage temporary ids.
  Used only in the context.
*)
module Temp_ids : sig
  type t

  val init : t

  val fresh : ('i, 'a, 'u) kind -> t -> t * 'i

  val fold_s :
    ('i, 'a, 'u) kind -> ('acc -> 'i -> 'acc Lwt.t) -> t -> 'acc -> 'acc Lwt.t
end

module IdSet : sig
  type t

  type 'acc fold_f = {f : 'i 'a 'u. ('i, 'a, 'u) kind -> 'i -> 'acc -> 'acc}

  val empty : t

  val mem : ('i, 'a, 'u) kind -> 'i -> t -> bool

  val add : ('i, 'a, 'u) kind -> 'i -> t -> t

  val diff : t -> t -> t

  val fold : ('i, 'a, 'u) kind -> ('i -> 'acc -> 'acc) -> t -> 'acc -> 'acc

  val fold_all : 'acc fold_f -> t -> 'acc -> 'acc
end
