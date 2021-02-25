(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** {2 Consensus heuristic} *)

(** The consensus heuristic handles a heuristic to decide a consensus
   over one data. This data is intented to be asked to a remote
   peer. The set of a data and a peer is called a [candidate].

    The consensus heuristic is parameterized by two values:

    - [expected] which is the number of [expected] candidates
   associated to [expected] different peers.

    - [threshold] which is the required number of candidates to
   achieve a consensus.

   A precondition for the heuristic is that [0 <= threshold <=
   expected < 2 * threshold] to ensure a unique consensus value. If
   this precondition is not met the [create] function raises
   [invalid_argument].  *)

(** The internal state of the heuristic. *)
type 'a t

type 'a state =
  | Consensus of 'a
  | No_consensus of ('a * int) list
  | Need_more_candidates

(** The semantics of the heuristic is the following (assuming each
   candidate using a different peer):

   - If the heuristic received less than [threshold] candidates then
   its state is [Need_more_candidates]

   - If the heuristic received more than [expected] candidates and one
   candidate did not reach the [threshold] it returns [No_consensus]
   with the different candidates received and theirs
   occurences. Otherwise, it returns [Consensus] with the candidate.

   - Otherwise, Either a candidate has reached the [threshold] and the
   state of the heuristic is [Consensus] with the candidate, or the
   state is [Need_more_candidates].

   If the heuristic receives several updates associated to the same
   peer, only the last updated is remembered.  *)

(** [create compare expected threshold ()] initializes the consensus
   heuristic. [compare] is used internally to create a map of
   candidates and to sort the candidates for the list of [candidates]
   when the function [get_state] returns [No_consensus candidates]. *)
val create :
  ?compare:('a -> 'a -> int) -> expected:int -> threshold:int -> unit -> 'a t

(** [get_state heuristic] returns the current state of the heuristic
   according to the semantics given above. *)
val get_state : 'a t -> 'a state

(** [update heuristic (peer, candidate)] updates the heuristic with
   the candidate [(peer, data)]. If a data was already registered for
   [peer], then it is erased by the update. It is the responsability
   of the caller given a peer to track the best candidate. *)
val update : 'a t -> P2p_peer.Id.t * 'a -> unit

(** {2 Consensus heuristic worker} *)

(** This worker implements a memoisation mechanism for the consensus
   heuristic with an expiry date mechanism. The worker also handles a
   hook mechanism to be executed on values found by the consensus
   heuristic. *)
module Worker : sig
  type 'a t

  (** [create expire_time job restart_delay] creates the internal
     state of the worker.

   - [expire] is the expiration time for the last consensus found.

   - [job] is the task to be run and returns the state of the
     heuristic when the task is over.

   - [restart_delay] is a time delay between two calls to [job] if the
     [job] returned either [Need_more_candidates] or
     [No_consensus]. *)
  val create :
    expire_time:Ptime.Span.t ->
    job:(unit -> 'a state Lwt.t) ->
    restart_delay:Ptime.Span.t ->
    'a t

  (** [wait worker] waits for a consensus value given by [job]
     associated to the worker. If the job associated to the heuristic
     returns [Need_more_candidates] or [No_consensus], the heuristic
     is run again. *)
  val wait : 'a t -> 'a Lwt.t

  (** [on_next_consensus worker hook] registers a hook to be executed
     on the next consensus value or the current one if there is a
     valid consensus (i.e., [Lwt.state (wait worker)] = [Lwt.Return
     hash]).

     Hooks are executed in the same order they are registered. *)
  val on_next_consensus : 'a t -> ('a -> unit) -> unit

  (** [on_all_consensus worker hook] registers a hook to be executed
     on every future consensus value as well as the current one if
     there is a valid consensus (i.e., [Lwt.state (wait worker)] =
     [Lwt.Return hash]).

     It is guaranteed that a hook is executed exactly once for each
     time a consensus is reached. More precisely, between two
     executions of the same hook, there are at most [expire_time] time
     elapsed. These hooks are executed after the ones registered with
     [on_next_consensus].

     Hooks are executed in the same order they are registered. *)
  val on_all_consensus : 'a t -> ('a -> unit) -> unit

  (** [cancel worker] cancels the current task of the worker. All
     hooks are removed. If there is any promises returned by [wait]
     which were in pending states, it is canceled. *)
  val cancel : 'a t -> unit
end
