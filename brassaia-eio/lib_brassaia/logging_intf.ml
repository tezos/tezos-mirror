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

module type Clock = sig
  (** A monotonic time source. See {!Mtime_clock} for an OS-dependent
      implementation. *)

  type counter

  val counter : unit -> counter

  val count : counter -> Mtime.span
end

module type Sigs = sig
  (** {!Logs} tags attached to the log entries emitted by Brassaia: *)

  module Source_code_position : sig
    (** The type of iclusive ranges of source code positions, as generated by
        the OCaml {!val-__POS__} macro. The 4-tuple components are 'file
        name', 'line number', 'column start' and 'column end' respectively. *)
    type t = string * int * int * int

    val pp : t Fmt.t

    val tag : t Logs.Tag.def
  end

  module type Clock = Clock

  (** A default {!Logs} reporter that is sensitive to the logs tags above. *)
  val reporter :
    ?filter_src:(Logs.src -> bool) ->
    ?prefix:string ->
    (module Clock) ->
    Logs.reporter

  (** [to_string_exn encoding v] returns [v] as a string
      according to [encoding] *)
  val to_string_exn : 'a Data_encoding.encoding -> 'a -> string
end
