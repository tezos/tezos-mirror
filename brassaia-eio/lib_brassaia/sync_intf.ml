(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  (** {1 Native Synchronization} *)

  (** Type type for store handles. *)
  type db

  (** The type for store heads. *)
  type commit

  (** The type for remote status. *)
  type status = [`Empty | `Head of commit]

  (** The type for commit info. *)
  type info

  (** [status_t db] is the value type for {!status} of remote [db]. *)
  val status_t : db -> status Type.t

  (** [pp_status] pretty-prints return statuses. *)
  val pp_status : status Fmt.t

  (** [fetch t ?depth r] populate the local store [t] with objects from the
      remote store [r], using [t]'s current branch. The [depth] parameter limits
      the history depth. Return [`Empty] if either the local or remote store do
      not have a valid head. *)
  val fetch : db -> ?depth:int -> Remote.t -> (status, [`Msg of string]) result

  (** Same as {!fetch} but raise [Invalid_argument] if either the local or
      remote store do not have a valid head. *)
  val fetch_exn : db -> ?depth:int -> Remote.t -> status

  (** The type for pull errors. *)
  type pull_error = [`Msg of string | Merge.conflict]

  (** [pp_pull_error] pretty-prints pull errors. *)
  val pp_pull_error : pull_error Fmt.t

  (** [pull t ?depth r s] is similar to {{!Sync.fetch} fetch} but it also
      updates [t]'s current branch. [s] is the update strategy:

      - [`Merge] uses [Head.merge]. Can return a conflict.
      - [`Set] uses [S.Head.set]. *)
  val pull :
    db ->
    ?depth:int ->
    Remote.t ->
    [`Merge of unit -> info | `Set] ->
    (status, pull_error) result

  (** Same as {!pull} but raise [Invalid_arg] in case of conflict. *)
  val pull_exn :
    db -> ?depth:int -> Remote.t -> [`Merge of unit -> info | `Set] -> status

  (** The type for push errors. *)
  type push_error = [`Msg of string | `Detached_head]

  (** [pp_push_error] pretty-prints push errors. *)
  val pp_push_error : push_error Fmt.t

  (** [push t ?depth r] populates the remote store [r] with objects from the
      current store [t], using [t]'s current branch. If [b] is [t]'s current
      branch, [push] also updates the head of [b] in [r] to be the same as in
      [t].

      {b Note:} {e Git} semantics is to update [b] only if the new head if more
      recent. This is not the case in {e Brassaia}. *)
  val push : db -> ?depth:int -> Remote.t -> (status, push_error) result

  (** Same as {!push} but raise [Invalid_argument] if an error happens. *)
  val push_exn : db -> ?depth:int -> Remote.t -> status
end

module type Sigs = sig
  module type S = S

  val remote_store :
    (module Store.Generic_key.S with type t = 'a) -> 'a -> Remote.t

  module Make (X : Store.Generic_key.S) :
    S with type db = X.t and type commit = X.commit and type info = X.info
end
