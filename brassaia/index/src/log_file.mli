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

open! Import

module Make (IO : Io.S) (Key : Data.Key) (Value : Data.Value) : sig
  module Entry : Data.Entry.S with type key := Key.t and type value := Value.t

  type t
  type key := Key.t
  type value := Value.t

  val create : IO.t -> t
  (** [create io] constructs a write-ahead log from an IO handle referencing an
      unordered sequence of (binary encoded) [key * value] bindings. The
      bindings are read into memory, and any subsequent {!replace} operations
      are reflected on disk. *)

  val close : t -> unit

  (** {2 Hashtable API} *)

  val cardinal : t -> int
  val find : t -> key -> value
  val replace : t -> key -> value -> unit
  val iter : t -> f:(Entry.t -> unit) -> unit
  val fold : t -> f:('acc -> Entry.t -> 'acc) -> init:'acc -> 'acc

  val to_sorted_seq : t -> Entry.t Seq.t
  (** [to_sorted_seq t] is the sequence of all entries in [t], sorted by
      [Entry.compare]. Modifying [t] while consuming the sequence results in
      undefined behaviour. *)

  (** {2 Low-level API} *)

  val io : t -> IO.t
  (** [io t] is [t]'s underlying IO handle. Any updates to the file will not be
      reflected in the in-memory mirror of its state. *)

  val sync_entries : min:int63 -> t -> unit
  (** [sync_entries ~min t] loads the entries from [log]'s IO into memory,
      starting from offset [min]. *)

  val flush : ?no_callback:unit -> with_fsync:bool -> t -> unit
  (** [flush t] is [IO.flush (io t)]. *)

  val reload : t -> unit
  (** [reload t] clears [t]'s in-memory state and reloads all bindings from the
      underlying IO hande. *)

  val clear :
    generation:int63 -> ?hook:(unit -> unit) -> reopen:bool -> t -> unit
  (** [clear t] clears both [t]'s in-memory state and its underlying IO handle.
      The flags are passed to [IO.clear]. *)
end
