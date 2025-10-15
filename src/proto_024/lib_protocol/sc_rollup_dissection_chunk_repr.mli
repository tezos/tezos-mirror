(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Sc_rollup_repr

(** A dissection chunk is made of an optional state hash, and a tick count. *)
type t = {state_hash : State_hash.t option; tick : Sc_rollup_tick_repr.t}

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

val default_check_sections_number :
  default_number_of_sections:int ->
  number_of_sections:int ->
  dist:Z.t ->
  unit tzresult

(** We check firstly that [dissection] is the correct length. It must
    be [default_number_of_sections] values long, unless the distance
    between [start_tick] and [stop_tick] is too small to make this
    possible, in which case it should be as long as possible. (If the
    distance is one we fail immediately as there is no possible legal
    dissection).

    Then we check that [dissection] starts at the correct tick and
    state (specified by [start_chunk]), and that it ends at
    [stop_chunk], at the correct tick and with a different state to
    the current dissection.

    Finally, we check that [dissection] is well formed: it has
    correctly ordered the ticks, and it begins with a real hash of the
    form [Some s] not a [None] state. Note that we have to allow the
    possibility of multiple [None] states because the restrictions on
    dissection shape (which are necessary to prevent a 'linear-time
    game' attack) will mean that sometimes the honest play is a
    dissection with multiple [None] states. *)
val default_check :
  section_maximum_size:Z.t ->
  check_sections_number:
    (default_number_of_sections:int ->
    number_of_sections:int ->
    dist:Z.t ->
    unit tzresult) ->
  default_number_of_sections:int ->
  start_chunk:t ->
  stop_chunk:t ->
  t list ->
  unit tzresult

type error +=
  | Dissection_number_of_sections_mismatch of {expected : Z.t; given : Z.t}
        (** There are more or less than the expected number of sections in the
          given dissection. *)
  | Dissection_invalid_number_of_sections of Z.t
        (** There are less than two sections in the given dissection, which is
          not valid. *)
  | Dissection_start_hash_mismatch of {
      expected : Sc_rollup_repr.State_hash.t option;
      given : Sc_rollup_repr.State_hash.t option;
    }
        (** The given start hash in a dissection is [None] or doesn't match the
          expected one.*)
  | Dissection_stop_hash_mismatch of Sc_rollup_repr.State_hash.t option
        (** The given stop state hash in a dissection should not match the last
          hash of the section being refuted. *)
  | Dissection_edge_ticks_mismatch of {
      dissection_start_tick : Sc_rollup_tick_repr.t;
      dissection_stop_tick : Sc_rollup_tick_repr.t;
      chunk_start_tick : Sc_rollup_tick_repr.t;
      chunk_stop_tick : Sc_rollup_tick_repr.t;
    }
        (** The given dissection's edge ticks don't match the edge ticks of the
          section being refuted. *)
  | Dissection_ticks_not_increasing
        (** Invalid provided dissection because ticks are not increasing between
          two successive sections. *)
  | Dissection_invalid_distribution of Z.t
        (** Invalid provided dissection because ticks split is not well balanced
          across sections *)
  | Dissection_invalid_successive_states_shape
        (** A dissection cannot have a section with no state hash after another
          section with some state hash. *)
