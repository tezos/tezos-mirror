(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

(** {1 Brassaia Unix utilities}

    This module provides utilities for Unix applications. *)

module Info = Info.Make

(** [info fmt ()] creates a fresh commit info, with the {{!Brassaia.Info.S.date}
    date} set to [Unix.gettimeoday ()] and the {{!Brassaia.Info.S.author} author}
    built using [Unix.gethostname()] and [Unix.getpid()] if [author] is not
    provided. *)
val info :
  ?author:string ->
  ('a, Format.formatter, unit, unit -> Brassaia.Info.default) format4 ->
  'a
