(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

open! Import

module type S = sig
  (** An abstraction on top of the index library that exposes an API that better
      fits the brassaia-pack use case. *)

  type t
  type key
  type value = int63 * int * Pack_value.Kind.t

  include
    Brassaia_index.Index.S
      with type value := value
       and type t := t
       and type key := key

  module Io = Io.Unix

  val init_exn :
    ?flush_callback:(unit -> unit) ->
    ?fresh:bool ->
    ?readonly:bool ->
    ?throttle:[ `Block_writes | `Overcommit_memory ] ->
    ?lru_size:int ->
    log_size:int ->
    string ->
    t

  type create_error := [ `Index_failure of string | `Io_misc of Io.misc_error ]

  type write_error :=
    [ `Index_failure of string | `Io_misc of Io.misc_error | `Ro_not_allowed ]

  val init :
    ?flush_callback:(unit -> unit) ->
    ?fresh:bool ->
    ?readonly:bool ->
    ?throttle:[ `Block_writes | `Overcommit_memory ] ->
    ?lru_size:int ->
    log_size:int ->
    string ->
    (t, [> create_error ]) result

  val reload : t -> (unit, [> write_error ]) result
  val close : t -> (unit, [> write_error ]) result
  val close_exn : t -> unit
  val flush : t -> with_fsync:bool -> (unit, [> write_error ]) result
  val find : t -> key -> value option
  val add : ?overcommit:bool -> t -> key -> value -> unit
  val merge : t -> unit
  val mem : t -> key -> bool
  val iter : (key -> value -> unit) -> t -> unit
  val filter : t -> (key * value -> bool) -> unit
  val try_merge : t -> unit

  module Stats = Brassaia_index.Index.Stats
  module Key : Brassaia_index.Index.Key.S with type t = key
end

module type Sigs = sig
  module type S = S

  module Make (K : Brassaia.Hash.S) : S with type key = K.t
end
