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

  type ro

  type rw

  type _ t

  val init : root_dir:string -> rw t tzresult Lwt.t

  val init_readonly : root_dir:string -> ro t tzresult Lwt.t

  val load : _ t -> value option tzresult Lwt.t

  val save : rw t -> value -> unit tzresult Lwt.t
end

module Make (Value : VALUE) : S with type value = Value.t = struct
  module KVS = Key_value_store

  type value = Value.t

  type ro

  type rw

  (* Note: The readonly mode is safe as long as it is used with simple atomic
     values (such as integers representing levels). We assume that reads cannot
     observe partial writes of these small values, as they are typically written
     atomically by the OS or filesystem. This assumption may not hold for larger
     or more complex data structures if Read and Write operations are interleaved
     in different processes. The [Key_value_store] module only takes care of
     concurrent access within the same process. *)
  type _ t =
    | RO : (unit, unit, Value.t) KVS.Read.t -> ro t
    | RW : (unit, unit, Value.t) KVS.t -> rw t

  let init ~root_dir : rw t tzresult Lwt.t =
    let open Lwt_result_syntax in
    let* kvs = KVS.init ~lru_size:1 ~root_dir in
    return (RW kvs)

  let init_readonly ~root_dir : ro t tzresult Lwt.t =
    let open Lwt_result_syntax in
    let* kvs = KVS.Read.init ~lru_size:1 root_dir in
    return (RO kvs)

  let file_layout ~root_dir () =
    let filepath = Filename.concat root_dir Value.name in
    Key_value_store.layout
      ~encoding:Value.encoding
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:(fun () -> 0)
      ~number_of_keys_per_file:1
      ()

  let load : type a. a t -> value option tzresult Lwt.t =
    let open Lwt_result_syntax in
    function
    | RO kvs ->
        let* exists = KVS.Read.value_exists kvs file_layout () () in
        if exists then
          let* res = KVS.Read.read_value kvs file_layout () () in
          return_some res
        else return_none
    | RW kvs ->
        let* exists = KVS.value_exists kvs file_layout () () in
        if exists then
          let* res = KVS.read_value kvs file_layout () () in
          return_some res
        else return_none

  let save (RW kvs) value =
    KVS.write_value ~override:true kvs file_layout () () value
end
