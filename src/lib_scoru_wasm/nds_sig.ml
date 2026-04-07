(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  module Registry : sig
    type t

    val hash : t -> bytes tzresult

    val size : t -> int64 tzresult

    val resize : t -> int64 -> unit tzresult

    val copy_database : t -> src:int64 -> dst:int64 -> unit tzresult

    val move_database : t -> src:int64 -> dst:int64 -> unit tzresult

    val clear : t -> int64 -> unit tzresult
  end

  module Database : sig
    val exists : Registry.t -> db_index:int64 -> key:bytes -> bool tzresult

    val set :
      Registry.t -> db_index:int64 -> key:bytes -> value:bytes -> unit tzresult

    val write :
      Registry.t ->
      db_index:int64 ->
      key:bytes ->
      offset:int64 ->
      value:bytes ->
      int64 tzresult

    val read :
      Registry.t ->
      db_index:int64 ->
      key:bytes ->
      offset:int64 ->
      len:int64 ->
      bytes tzresult

    val value_length :
      Registry.t -> db_index:int64 -> key:bytes -> int64 tzresult

    val delete : Registry.t -> db_index:int64 -> key:bytes -> unit tzresult

    val hash : Registry.t -> db_index:int64 -> bytes tzresult
  end
end
