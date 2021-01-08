(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** View over the context store, restricted to types, access and
    functional manipulation of an existing context. *)

(* Copy/paste of Environment_context_inttf.S *)

(** The type for database views. *)
type t

(** The type for database keys. *)
type key = string list

(** The type for database values. *)
type value = bytes

(** {2 Getters} *)

(** [mem t k] is true iff [k] is bound to a value in [t]. *)
val mem : t -> key -> bool Lwt.t

(** [mem_tree t k] is like {!mem} but for trees. *)
val mem_tree : t -> key -> bool Lwt.t

(** [find t k] is [Some v] if [k] is bound to [v] in [t] and [None]
      otherwise. *)
val find : t -> key -> value option Lwt.t

(** {2 Setters} *)

(** [add t k v] is the database view where [k] is bound to [v] and
      is similar to [t] for other keys. *)
val add : t -> key -> value -> t Lwt.t

(** [remove c k] removes any values and trees bound to [k] in [c]. *)
val remove : t -> key -> t Lwt.t

(** {2 Misc} *)

(** [copy] returns None if the [from] key is not bound *)
val copy : t -> from:key -> to_:key -> t option Lwt.t

type key_or_dir = [`Key of key | `Dir of key]

val fold : t -> key -> init:'a -> f:(key_or_dir -> 'a -> 'a Lwt.t) -> 'a Lwt.t

val keys : t -> key -> key list Lwt.t

val fold_keys : t -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

val register_resolver :
  'a Base58.encoding -> (t -> string -> 'a list Lwt.t) -> unit

val complete : t -> string -> string list Lwt.t
