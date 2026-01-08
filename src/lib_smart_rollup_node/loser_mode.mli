(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** A list of failures. *)
type t

type dal_parameters = {
  number_of_slots : int64;
  attestation_lag : int64;
  slot_size : int64;
  page_size : int64;
}

(** DAL page selector and payload-forging strategy.

    Optional fields act as wildcards:
    - [None] means "match any value" for that dimension.
      Example:
        { published_level = None; slot_index = Some 3; page_index = None; _ }
      matches every page of slot index 3 at any level.

    Fields:
    - [published_level]: L1 level at which the slot was published (or [None]).
    - [slot_index]: Index of the target slot at that level (or [None]).
    - [page_index]: Index of the target page within the slot (or [None]).
    - [page_payload_strategy]: How the faulty/losing node derives the payload
      from the honest page:
        - [`Alter]: Mutate the bytes of an existing [Some payload].
                    No effect if the honest payload is [None].
        - [`Flip]: Toggle presence: [None] -> [Some bytes],
                   [Some _] -> [None]. *)
type dal_page = {
  published_level : int32 option;
  slot_index : int option;
  page_index : int option;
  page_payload_strategy : [`Alter | `Flip];
}

val encoding : t Data_encoding.t

(** [no_failures] are planned. *)
val no_failures : t

(** [make s] parses a list of integers separated by spaces that is a
   periodic sequence of triple [level message_index message_tick]
   representing a failure that the rollup node is supposed to make.
   This function returns [None] if the input string is not syntactically
   correct. *)
val make : string -> t option

(** [is_failure failures ~level ~message_index] returns [message_ticks]
   where a failure is supposed to happen at the point
   of the rollup node processing of a given inbox [level], a given
   [message_index] and for all [message_ticks]. Ticks are sorted by
   increasing order. *)
val is_failure : t -> level:int -> message_index:int -> int64 list

val is_invalid_dal_parameters : t -> dal_parameters option

(** Decide whether to corrupt a DAL page and, if so, how.

    Given the current [published_level], [slot_index], [page_index], [page_size],
    the honest page payload ([honest_payload]), and a failure plan [t], this
    function checks whether an [Invalid_dal_page] rule matches (wildcards allowed
    via [None] in the rule). If no rule applies, returns [Either.left ()].

    If a rule applies, returns [Either.right forged]:
    - [forged = None]    -> payload is removed (flip from [Some _] to [None]).
    - [forged = Some bs] -> payload is replaced with [bs].

    See {!dal_page} above for forge strategies. *)
val is_invalid_dal_page :
  published_level:int32 ->
  slot_index:int ->
  page_index:int ->
  page_size:int ->
  honest_payload:bytes option ->
  t ->
  (unit, bytes option) Either.t
