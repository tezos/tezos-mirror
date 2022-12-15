(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** This module type is the minimal API a storage backend has to
    implement to be compatible with the [Tx_rollup] layer-2
    implementation.

    In a nutshell, the [Tx_rollup] only needs a simple key-value
    store, where both keys and values are raw bytes buffers. We build
    a type-safe abstraction on top of this simple (but potentially
    unsafe) interface in [Tx_rollup_l2_context]. *)
module type STORAGE = sig
  (** The state of the storage.

      The API adopts a functional paradigm, where the [set] function
      returns a new state for the storage, and where it should be
      possible to reuse a previous state. *)
  type t

  (** The monad of the storage backend. *)
  type 'a m

  (** The necessary monadic operators the monad of the storage backend
      is required to provide. *)
  module Syntax : sig
    val ( let+ ) : 'a m -> ('a -> 'b) -> 'b m

    val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m

    (** [fail err] shortcuts the current computation by raising an
        error.

        Said error can be handled with the [catch] combinator. *)
    val fail : error -> 'a m

    (** [catch p k h] tries to executes the monadic computation [p].
        If [p] terminates without an error, then its result is passed
        to the continuation [k]. On the contrary, if an error [err] is
        raised, it is passed to the error handler [h]. *)
    val catch : 'a m -> ('a -> 'b m) -> (error -> 'b m) -> 'b m

    (** [return x] is the simplest computation inside the monad [m] which simply
        computes [x] and nothing else. *)
    val return : 'a -> 'a m

    (** [list_fold_left_m f] is a monadic version of [List.fold_left
        f], wherein [f] is not a pure computation, but a computation
        in the monad [m]. *)
    val list_fold_left_m : ('a -> 'b -> 'a m) -> 'a -> 'b list -> 'a m
  end

  (** [get storage key] returns the value stored in [storage] for
      [key], if it exists. Returns [None] if it does not. *)
  val get : t -> bytes -> bytes option m

  (** [set storage key] computes a new state for the storage wherein
      the value associated to [key] is [value].

      [storage] is expected to remain usable and consistent even after
      the execution of [set]. *)
  val set : t -> bytes -> bytes -> t m

  (** [remove storage key] removes [key] from the [storage]. *)
  val remove : t -> bytes -> t m
end
