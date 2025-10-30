(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type level = {
  level : int32;
      (** The level of the block relative to genesis. This
      is also the Shell's notion of level. *)
  cycle : int32;
      (** The current cycle's number. Note that cycles are a protocol-specific
      notion. As a result, the cycle number starts at 0 with the first block of
      the first version of protocol alpha. *)
  cycle_position : int32;
      (** The current level of the block relative to the first block of the current
      cycle. *)
}

(** [convert_using_serialization ~dst ~src value] Conversion from one type with
    encoding [src] to another with encoding [dst], through serialization.
    Costly, but useful to build instances of a type when no builder is
    accessible. *)
val convert_using_serialization :
  name:string ->
  dst:'a Data_encoding.t ->
  src:'b Data_encoding.t ->
  'b ->
  'a tzresult

(** Imports the type Contract.t from Alpha_context. Not everything is imported
    from Alpha_context.Contract as most of it require a context, which we
    can't provide.
 *)
module Contract : sig
  type t = Tezlink_imports.Alpha_context.Contract.t

  val encoding : t Data_encoding.t

  val implicit_encoding : Signature.public_key_hash Data_encoding.t

  val of_b58check : string -> t tzresult

  val of_implicit : Signature.public_key_hash -> t

  val of_hex : string -> Signature.public_key_hash option
end

module Tez : sig
  include module type of Tezlink_imports.Alpha_context.Tez

  val of_string_exn : string -> t

  val to_mutez_z : t -> Z.t
end

module Operation : sig
  type t = {
    source : Signature.public_key_hash;
    first_counter : Z.t;
    length : int;
    op : Tezlink_imports.Alpha_context.packed_operation;
    raw : bytes;
    fee : Tez.t;
    gas_limit : Z.t;
  }

  val counter_to_z :
    Tezlink_imports.Alpha_context.Manager_counter.t -> Z.t tzresult

  val hash_operation : t -> Ethereum_types.hash

  val encoding : t Data_encoding.t

  val gas_limit_to_z : Tezlink_imports.Alpha_context.Gas.Arith.integral -> Z.t

  (** Underapproximation of the size of the smallest operation, defined as a
      Reveal of tz1 key, with zero fees/gas limit/storage limit/....*)
  val minimum_operation_size : int
end

module Manager = Tezlink_imports.Imported_protocol.Manager_repr
