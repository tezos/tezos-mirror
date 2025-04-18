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

module Brassaia = Brassaia_eio.Brassaia

module type S = sig
  (** Basic abstraction for a worker. *)

  (** A task *)
  type t

  type outcome = [`Success | `Cancelled | `Failure of string]
  [@@deriving brassaia]

  type status = [outcome | `Running] [@@deriving brassaia]

  (** Start a task. *)
  val async : (unit -> unit) -> t

  (** If running, wait for a task to finish and return its outcome.

      If not running, return the oucome of the task. *)
  val await : t -> [> outcome]

  (** If running, refresh the status of the task, without blocking.

      If not running, return the oucome of the task. *)
  val status : t -> [> status]

  (** If running, cancel the task and return [true].

      If not running, do nothing and return [false]. *)
  val cancel : t -> bool
end
