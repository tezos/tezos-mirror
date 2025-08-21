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

(** {1 Commit Info} *)

module type S = sig
  type author = string [@@deriving brassaia]

  type message = string [@@deriving brassaia]

  (** The type for commit info. *)
  type t [@@deriving brassaia]

  (** [encoding] is the data_encoding for {!type-t}. *)
  val encoding : t Data_encoding.t

  (** Create a new commit info. *)
  val init : ?author:author -> ?message:message -> int64 -> t

  (** [date t] is [t]'s commit date.

      The date provided by the user when calling the {!v} function. Rounding
      [Unix.gettimeofday ()] (when available) is a good value for such date. On
      more esoteric platforms, any monotonic counter is a fine value as well. On
      the Git backend, the date is translated into the commit {e Date} field and
      is expected to be the number of POSIX seconds (thus not counting leap
      seconds) since the Epoch. *)
  val date : t -> int64

  (** [author t] is [t]'s commit author.

      The author identifies the entity (human, unikernel, process, thread, etc)
      performing an operation. For the Git backend, this will be directly
      translated into the {e Author} field. *)
  val author : t -> author

  (** [message t] is [t]'s commit message. *)
  val message : t -> message

  (** The empty commit info. *)
  val empty : t

  (** {1 Info Functions} *)

  (** Alias for functions which can build commit info. *)
  type f = unit -> t

  (** The empty info function. [none ()] is [empty] *)
  val none : f
end

module type Sigs = sig
  module type S = S

  module Default : S

  type default = Default.t
end
