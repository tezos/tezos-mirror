(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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
  module File_manager : File_manager.S
  module Async : Async_intf.Sigs
  module Dict : Dict.S with module File_manager = File_manager
  module Dispatcher : Dispatcher.S with module File_manager = File_manager

  type hash
  type key = hash Pack_key.t [@@deriving brassaia]

  module Hash : sig
    val hash_size : int
  end

  module Contents_store : sig
    type 'a t

    val purge_lru : 'a t -> unit
  end

  module Node_value : sig
    type t

    val pred :
      t ->
      (Path.step option * [ `Contents of key | `Inode of key | `Node of key ])
      list
  end

  module Node_store : sig
    type 'a t

    val init :
      config:Brassaia.Backend.Conf.t ->
      file_manager:File_manager.t ->
      dict:Dict.t ->
      dispatcher:Dispatcher.t ->
      lru:Lru.t option ->
      read t

    val unsafe_find :
      check_integrity:bool -> [< read ] t -> key -> Node_value.t option

    val key_of_offset : [< read ] t -> int63 -> key
    val unsafe_find_no_prefetch : 'a t -> key -> Node_value.t option
    val purge_lru : 'a t -> unit
    val get_offset : 'a t -> key -> int63
    val get_length : 'a t -> key -> int
  end

  module Commit_value : sig
    type t

    val node : t -> key
    val parents : t -> key list
  end

  module Commit_store :
    Pack_store.S
      with type value = Commit_value.t
       and type key = key
       and type file_manager = File_manager.t
       and type dict = Dict.t
       and type dispatcher = Dispatcher.t
       and type hash = hash
end
