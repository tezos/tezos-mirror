(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
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

(** This API has the same semantics as that of [List]. *)

type +'a t

val empty : _ t
val cons : 'a -> 'a t -> 'a t
val map : f:('a -> 'b) -> 'a t -> 'b t
val iter : f:('a -> unit) -> 'a t -> unit
val exists : f:('a -> bool) -> 'a t -> bool
val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
val to_array : 'a t -> 'a array
val find_map : f:('a -> 'b option) -> 'a t -> 'b option
val fold_left : f:('acc -> 'a -> 'acc) -> init:'acc -> 'a t -> 'acc
