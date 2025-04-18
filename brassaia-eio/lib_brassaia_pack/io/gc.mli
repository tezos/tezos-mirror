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

(** [Make] returns a module that can manage GC processes. *)
module Make
    (Args : Gc_args.S) : sig
  module Args : Gc_args.S

  (** A running GC process. *)
  type t

  (** Creates and starts a new GC process. *)
  val init_and_start :
    root:string ->
    lower_root:string option ->
    output:[`External of string | `Root] ->
    generation:int ->
    unlink:bool ->
    dispatcher:Args.Dispatcher.t ->
    file_manager:Args.File_Manager.t ->
    contents:read Args.Contents_store.t ->
    node:read Args.Node_store.t ->
    commit:read Args.Commit_store.t ->
    Args.key ->
    (t, [> `Gc_disallowed of string]) result

  (** [finalise ~wait t] returns the state of the GC process.

      If [wait = true], the call will block until GC finishes. *)
  val finalise :
    wait:bool ->
    t ->
    ([> `Running | `Finalised of Stats.Latest_gc.stats], Io_errors.t) result

  (** Attaches a callback to the GC process, which will be called when the GC
      finalises. *)
  val on_finalise :
    t -> ((Stats.Latest_gc.stats, Io_errors.t) result -> unit) -> unit

  val cancel : t -> bool

  (** Waits for the current gc to finish and returns immediately without
      swapping the files and doing the other finalisation steps from [finalise].
      Returns the [gced] status to create a fresh control file for the snapshot. *)
  val finalise_without_swap : t -> Control_file_intf.Payload.Upper.Latest.gced
end
with module Args = Args
