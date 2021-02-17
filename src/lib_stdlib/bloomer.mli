(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** A probabilistic set implementation in a fixed memory buffer, with
    an optional best-effort cleanup mechanism. The bigger the memory
    buffer, the less false positive outcomes of [mem]. *)
type 'a t

(** [create ~hash ~hashes ~index_bits ~countdown_bits] creates an
    initially empty generational bloom filter. The number of
    generations is [2^countdown_bits]. The filter is an array of
    [2^index_bits] countdown cells, each of size [countdown_bits].
    The resulting filter takes [2^index_bits * countdown_bits] bits
    in memory. The hash function must return enough bytes to take
    [hashes] indexes of [index_bits] bits.

    When a value is [add]ed, its [hash] is split into [hashes] chunks of
    [index_bits], that are used as indexes in the filter's countdown
    array. These countdown cells are then set to their maximum value,
    that is [2^countdown_bits-1].

    The value will remain a [mem]ber for as long as all these cells
    are above zero, which in the most optimistic case (where no
    collision occur) is until [countdown] has been called
    [2^countdown_bits-1] times. An exception is if [clear] is called,
    in which case it is certain to disappear, as all other values. *)
val create :
  hash:('a -> bytes) ->
  hashes:int ->
  index_bits:int ->
  countdown_bits:int ->
  'a t

(** Checks if the value is still considered in the set (see {!create}). *)
val mem : 'a t -> 'a -> bool

(** Adds a member to the set (see {!create}). *)
val add : 'a t -> 'a -> unit

(** Force remove an element, which may remove others in case of collisions. *)
val rem : 'a t -> 'a -> unit

(** Decrements all the countdowns cells of the set (see {!create}). *)
val countdown : 'a t -> unit

(** Clears the entire set. *)
val clear : 'a t -> unit

(** Percentage (in the [0;1] interval) of cells which are nonzero. *)
val fill_percentage : 'a t -> float

(** Histogram of life expectancies (measured in number of countdowns to 0). *)
val life_expectancy_histogram : 'a t -> int array
