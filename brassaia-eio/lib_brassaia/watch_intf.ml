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
  (** {1 Watch Helpers} *)

  (** The type for store keys. *)
  type key

  (** The type for store values. *)
  type value

  (** The type for watch handlers. *)
  type watch

  (** The type for watch state. *)
  type t

  (** [stats t] is a tuple [(k,a)] represeting watch stats. [k] is the number of
      single key watchers for the store [t] and [a] the number of global
      watchers for [t]. *)
  val stats : t -> int * int

  (** Notify all listeners in the given watch state that a key has changed, with
      the new value associated to this key. [None] means the key has been
      removed. *)
  val notify : t -> key -> value option -> unit

  (** Creates a watch state. *)
  val create : unit -> t

  (** Clear all register listeners in the given watch state. *)
  val clear : t -> unit

  (** Watch a given key for changes. More efficient than {!val-watch}. *)
  val watch_key : t -> key -> ?init:value -> (value Diff.t -> unit) -> watch

  (** Add a watch handler. To watch a specific key, use {!watch_key} which is
      more efficient. *)
  val watch :
    t -> ?init:(key * value) list -> (key -> value Diff.t -> unit) -> watch

  (** Remove a watch handler. *)
  val unwatch : t -> watch -> unit

  (** Register a thread looking for changes in the given directory and return a
      function to stop watching and free up resources. *)
  val listen_dir :
    t ->
    string ->
    key:(string -> key option) ->
    value:(key -> value option) ->
    unit ->
    unit
end

module type Sigs = sig
  (** The signature for watch helpers. *)
  module type S = S

  (** [workers ()] is the number of background worker threads managing event
      notification currently active. *)
  val workers : unit -> int

  (** The type for watch hooks. *)
  type hook = int -> string -> (string -> unit) -> unit -> unit

  (** [none] is the hooks which asserts false. *)
  val none : hook

  (** A terrible hack that will need fixed... *)
  val set_watch_switch : Eio.Switch.t -> unit

  (** Register a function which looks for file changes in a directory and return
      a function to stop watching. It is probably best to use
      {!Brassaia_watcher.hook} there. By default, it uses {!none}. *)
  val set_listen_dir_hook : hook -> unit

  (** [Make] builds an implementation of watch helpers. *)
  module Make (K : Type.S) (V : Type.S) :
    S with type key = K.t and type value = V.t
end
