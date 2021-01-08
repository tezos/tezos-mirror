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

(** All context manipulation functions. This signature is included
    as-is for direct context accesses, and used in {!Storage_functors}
    to provide restricted views to the context. *)

module type T = sig
  type root_context

  type t

  type key

  type value

  type context = t

  (** Tells if the key is already defined as a value. *)
  val mem : context -> key -> bool Lwt.t

  (** Tells if the key is already defined as a directory. *)
  val dir_mem : context -> key -> bool Lwt.t

  (** Retrieve the value from the storage bucket ; returns a
      {!Storage_error Missing_key} if the key is not set. *)
  val get : context -> key -> value tzresult Lwt.t

  (** Retrieves the value from the storage bucket ; returns [None] if
      the data is not initialized. *)
  val get_option : context -> key -> value option Lwt.t

  (** Allocates the storage bucket and initializes it ; returns a
      {!Storage_error Existing_key} if the bucket exists. *)
  val init : context -> key -> value -> context tzresult Lwt.t

  (** Updates the content of the bucket ; returns a {!Storage_error
      Missing_key} if the value does not exists. *)
  val set : context -> key -> value -> context tzresult Lwt.t

  (** Allocates the data and initializes it with a value ; just
      updates it if the bucket exists. *)
  val init_set : context -> key -> value -> context Lwt.t

  (** When the value is [Some v], allocates the data and initializes
      it with [v] ; just updates it if the bucket exists. When the
      value is [None], delete the storage bucket when the value ; does
      nothing if the bucket does not exists. *)
  val set_option : context -> key -> value option -> context Lwt.t

  (** Delete the storage bucket ; returns a {!Storage_error
      Missing_key} if the bucket does not exists. *)
  val delete : context -> key -> context tzresult Lwt.t

  (** Removes the storage bucket and its contents ; does nothing if the
      bucket does not exists. *)
  val remove : context -> key -> context Lwt.t

  (** Recursively removes all the storage buckets and contents ; does
      nothing if no bucket exists. *)
  val remove_rec : context -> key -> context Lwt.t

  val copy : context -> from:key -> to_:key -> context tzresult Lwt.t

  (** Iterator on all the items of a given directory. *)
  val fold :
    context ->
    key ->
    init:'a ->
    f:([`Key of key | `Dir of key] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  (** Recursively list all subkeys of a given key. *)
  val keys : context -> key -> key list Lwt.t

  (** Recursive iterator on all the subkeys of a given key. *)
  val fold_keys :
    context -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

  (** Internally used in {!Storage_functors} to escape from a view. *)
  val project : context -> root_context

  (** Internally used in {!Storage_functors} to retrieve a full key
      from partial key relative a view. *)
  val absolute_key : context -> key -> key

  (** Raised if block gas quota is exhausted during gas
     consumption. *)
  type error += Block_quota_exceeded

  (** Raised if operation gas quota is exhausted during gas
     consumption. *)
  type error += Operation_quota_exceeded

  (** Internally used in {!Storage_functors} to consume gas from
      within a view. May raise {!Block_quota_exceeded} or
      {!Operation_quota_exceeded}. *)
  val consume_gas : context -> Gas_limit_repr.cost -> context tzresult

  (** Check if consume_gas will fail *)
  val check_enough_gas : context -> Gas_limit_repr.cost -> unit tzresult

  val description : context Storage_description.t
end
