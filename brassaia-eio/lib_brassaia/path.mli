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

(** Tree path handling. *)

(** {1 Path} *)

(** Type type for path's steps. *)
type step = string
[@@deriving brassaia ~pp ~compare ~to_bin_string ~of_bin_string]

(** The type for path values. *)
type t = step list [@@deriving brassaia ~pp ~compare]

(** The empty path. *)
val empty : t

(** Creates a path from a list of steps. *)
val init : step list -> t

(** Check if the path is empty. *)
val is_empty : t -> bool

(** Prepends a step to the path. *)
val cons : step -> t -> t

(** Append a step to the path. *)
val rcons : t -> step -> t

(** Deconstructs the first element of the path. Return [None] if the path is
      empty. *)
val decons : t -> (step * t) option

(** Deconstructs the last element of the path. Return [None] if the path is
      empty. *)
val rdecons : t -> (t * step) option

(** [map t f] maps [f] over all steps of [t]. *)
val map : t -> (step -> 'a) -> 'a list

(** {1 Value Types} *)

(** [t] is the value type for {!type-t}. *)
val t : t Type.t

(** [encoding] is the data_encoding for {!type-t}. *)
val encoding : t Data_encoding.t

(** [step_t] is the value type for {!step}. *)
val step_t : step Type.t

(** [step_encoding] is the data_encoding for {!type-step}. *)
val step_encoding : step Data_encoding.t
