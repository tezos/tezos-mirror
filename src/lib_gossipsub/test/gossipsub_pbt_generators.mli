(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Test_gossipsub_shared
open GS
module M = QCheck2.Gen

(** { 2 Type definitions } *)

type 'a t := 'a QCheck2.Gen.t

(** The type of inputs to the gossipsub automaton. *)
type input =
  | Add_peer of GS.add_peer (* case 0 *)
  | Remove_peer of GS.remove_peer (* case 1 *)
  | Ihave of GS.ihave (* case 2 *)
  | Iwant of GS.iwant (* case 3 *)
  | Graft of GS.graft (* case 4 *)
  | Prune of GS.prune (* case 5 *)
  | Publish of GS.publish (* case 6 *)
  | Heartbeat (* case 7 *)
  | Join of GS.join (* case 8 *)
  | Leave of GS.leave (* case 9 *)
  | Subscribe of GS.subscribe (* case 10 *)
  | Unsubscribe of GS.unsubscribe (* case 11 *)

(** The type of outputs of the gossipsub automaton, wrapped for convenience. *)
type output = O : _ GS.output -> output

(** An [event] is either an input for the gossipsub automaton, or some time elapsing. *)
type event = Input of input | Elapse of int

(** A [transition] is a quadruple [(i, s, s', o)] corresponding to an
      automaton transition from state [s] to state [s'] under input [i],
      producing output [o].  *)
type transition = private {
  t : Time.t;
  i : input;
  s : GS.state;
  s' : GS.state;
  o : output;
}

(** A [trace] is a sequence of transitions. *)
type trace = transition list

(** { 2 Printers } *)

val pp_input : Format.formatter -> input -> unit

val pp_trace :
  ?pp_state:(Format.formatter -> state -> unit) ->
  ?pp_state':(Format.formatter -> state -> unit) ->
  ?pp_output:(Format.formatter -> output -> unit) ->
  unit ->
  Format.formatter ->
  trace ->
  unit

(** {2 Input generation helpers.} *)

val add_peer : gen_peer:Peer.t t -> add_peer t

val remove_peer : gen_peer:Peer.t t -> remove_peer t

val ihave :
  gen_peer:Peer.t t ->
  gen_topic:Topic.t t ->
  gen_message_id:Message_id.t t ->
  gen_msg_count:int t ->
  ihave t

val iwant :
  gen_peer:Peer.t t ->
  gen_message_id:Message_id.t t ->
  gen_msg_count:int t ->
  GS.iwant t

val graft : gen_peer:Peer.t t -> gen_topic:Topic.t t -> GS.graft t

val prune :
  gen_peer:Peer.t t ->
  gen_topic:Topic.t t ->
  gen_span:span t ->
  int ->
  GS.prune t

val publish :
  gen_peer:Peer.t t ->
  gen_topic:Topic.t t ->
  gen_message_id:Message_id.t t ->
  gen_message:message t ->
  GS.publish t

val join : gen_topic:Topic.t t -> GS.join t

val leave : gen_topic:Topic.t t -> GS.leave t

val subscribe : gen_topic:Topic.t t -> gen_peer:Peer.t t -> GS.subscribe t

val unsubscribe : gen_topic:Topic.t t -> gen_peer:Peer.t t -> GS.unsubscribe t

(** We fuzz the automaton by composing basic sequences of inputs, called {e fragments}.
    Fragments can be composed sequentially, but most importantly
    we can model concurrent interactions with the automaton by generating interleavings
    of fragments. *)
module Fragment : sig
  (** [t] is the type of fragments, modelled as {!event} sequence generators. *)
  type t

  (** [of_list inputs] injects a deterministic sequence of events as a fragment. *)
  val of_list : input list -> t

  (** [of_input_gen gen f] creates a fragment generated using
      [gen] mapped to an list of {!input} through [f]. *)
  val of_input_gen : 'a M.t -> ('a -> input list) -> t

  (** [tck] is a basic event that increments the time of one unit. *)
  val tick : t

  (** [repeat n f] corresponds to the sequential composition of [f] n times. *)
  val repeat : int -> t -> t

  (** [repeat_at_most n f] corresponds to the sequential composition of [f]
      at most n times. *)
  val repeat_at_most : int -> t -> t

  (** [f @% f'] is the concatenation of [f] and [f']. *)
  val ( @% ) : t -> t -> t

  (** [interleave fs] is a generator for interleavings of the fragments in [fs]. *)
  val interleave : t list -> t

  (** [fork n f] is equivalent to [interleave [f; f; ...; f]] with a list
      of [n] copies of [f]. *)
  val fork : int -> t -> t

  (** [fork_at_most n f] is [fork n' f] with [n'] generated between [0] and
      [n] (inclusive). *)
  val fork_at_most : int -> t -> t
end

(** [run s0 f] constructs a {!trace} generator by running the gossipsub automaton on inputs
    generated from [f]. *)
val run : GS.state -> Fragment.t -> trace t

(** [check_fold step inv trace] folds the predicate [step] with initial invariant
    [inv] on [trace]. If the [step] fails with [Error e], [check_predicate] returns [Error (e, prefix)]
    where [prefix] is the prefix of [trace] up to and including the {!transition} which failed. *)
val check_fold :
  (transition -> 'inv -> ('inv, 'e) result) ->
  'inv ->
  trace ->
  (unit, 'e * trace) result

(** [check_final f trace] applies [f] to the last state and the last output in [trace]. *)
val check_final :
  (GS.state -> output -> (unit, 'a) result) -> trace -> (unit, 'a) result
