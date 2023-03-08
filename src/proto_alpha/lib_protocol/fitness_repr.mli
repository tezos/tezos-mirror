(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type error +=
  | (* `Permanent *) Invalid_fitness
  | (* `Permanent *) Wrong_fitness
  | (* `Permanent *) Outdated_fitness
  | (* `Permanent *)
      Locked_round_not_less_than_round of {
      round : Round_repr.t;
      locked_round : Round_repr.t;
    }

type t

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

val create :
  level:Raw_level_repr.t ->
  locked_round:Round_repr.t option ->
  predecessor_round:Round_repr.t ->
  round:Round_repr.t ->
  t tzresult

val create_without_locked_round :
  level:Raw_level_repr.t ->
  predecessor_round:Round_repr.t ->
  round:Round_repr.t ->
  t

val to_raw : t -> Fitness.t

(** Returns the corresponding protocol fitness if the shell fitness has
    the expected version, given by
    Constants_repr.fitness_version_number. If the fitness' version is
    from a previous protocol version, then it raises an "outdated
    fitness" error. If the fitness version is higher then
    it raises an "invalid fitness" error. *)
val from_raw : Fitness.t -> t tzresult

(** Returns the round from a raw fitness. If the fitness is from a
    previous protocol, the returned value will be Round.zero. *)
val round_from_raw : Fitness.t -> Round_repr.t tzresult

(** Returns the predecessor round from a raw fitness. If the fitness
   is from a previous protocol, the returned value will be Round.zero. *)
val predecessor_round_from_raw : Fitness.t -> Round_repr.t tzresult

(** Returns the locked round from a raw fitness. If the fitness is
    from a previous version, the returned value will be None. *)
val locked_round_from_raw : Fitness.t -> Round_repr.t option tzresult

(** Validate only the part of the fitness for which information are
    available during begin_application *)
val check_except_locked_round :
  t -> level:Raw_level_repr.t -> predecessor_round:Round_repr.t -> unit tzresult

(** Validate the locked_round component of the fitness, which could
    not be validated during begin_application. *)
val check_locked_round :
  fitness_locked_round:Round_repr.t option ->
  locked_round:Round_repr.t option ->
  unit tzresult

val level : t -> Raw_level_repr.t

val round : t -> Round_repr.t

val locked_round : t -> Round_repr.t option

val predecessor_round : t -> Round_repr.t

(**/**)

module Internal_for_tests : sig
  (** uses a lexicographic order relation for [level, locked_round,
     -predecessor_round, round] *)
  val compare : t -> t -> int
end
