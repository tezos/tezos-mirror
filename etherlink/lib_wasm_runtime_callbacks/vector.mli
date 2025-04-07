(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A subtree extracted from an EVM state which contains a durable value. *)
type t = private Irmin_context.tree

val empty : unit -> t Lwt.t

(** [get tree key] extracts a durable value from the durable storage of [tree].
    [create_if_absent] can be used to create said durable storage value if it
    does not already exist (defaults to [false]). *)
val get :
  ?create_if_absent:bool ->
  Irmin_context.tree ->
  string trace ->
  (t, Error_code.t) result Lwt.t

(** [load_all vec] will read the full contents of [vec] all at once. *)
val load_all : t -> string Lwt.t

val length : t -> (int, Error_code.t) result Lwt.t

(** [load_bytes vector ~offset ~num_bytes] will try to read [num_bytes] from
    [vector], starting from [offset].

    This function fails if [offset + num_bytes > length vector]. It does not
    fail, however, if [num_bytes] is greater than the IO limit of the WASM PVM
    ([load_bytes] can be used to read arbitrary subsets of the vector). *)
val load_bytes :
  t -> offset:int -> num_bytes:int -> (bytes, Error_code.t) result Lwt.t

(** [write_bytes vector offset bytes] will write the contents of [bytes] in
    [vector] starting at [offset]. The vector grows if [offset + Bytes.length
    bytes > length vector]. *)
val write_bytes : t -> int -> bytes -> (t, Error_code.t) result Lwt.t

(** [add_in_tree tree key vector] inserts in [tree] the durable storage value
    [vector] under [key] (in the durable storage). *)
val add_in_tree :
  Irmin_context.tree -> string trace -> t -> Irmin_context.tree Lwt.t
