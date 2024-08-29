(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

module type VALUE = sig
  type t

  val name : string

  val encoding : t Data_encoding.t
end

module type S = sig
  type value

  type t

  val init : root_dir:string -> t tzresult Lwt.t

  val load : t -> value option tzresult Lwt.t

  val save : t -> value -> unit tzresult Lwt.t
end

module Make (Value : VALUE) : S with type value = Value.t = struct
  module KVS = Key_value_store

  type value = Value.t

  type nonrec t = (unit, unit, Value.t) KVS.t

  let init ~root_dir : t tzresult Lwt.t = KVS.init ~lru_size:1 ~root_dir

  let file_layout ~root_dir () =
    let filepath = Filename.concat root_dir Value.name in
    Key_value_store.layout
      ~encoding:Value.encoding
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:(fun () -> 0)
      ~number_of_keys_per_file:1
      ()

  let load t =
    let open Lwt_result_syntax in
    let* exists = KVS.value_exists t file_layout () () in
    if exists then
      let* res = KVS.read_value t file_layout () () in
      return_some res
    else return_none

  let save t value = KVS.write_value ~override:true t file_layout () () value
end
