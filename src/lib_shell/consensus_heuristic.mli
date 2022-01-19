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

(** A consensus heuristic is a mechanism by which a node attempts to find a
    data that its network of peers generally agrees upon. Typically, the node
    attempts to find out what the p2p network as a whole agrees is the head of
    the chain.

    Note that, by nature, this problem does not necessarily have one exact
    solution. This is a heuristic, it makes a "best guess" based on information
    available locally.

    The consensus heuristic is given {e candidates}: tuples of the form [p,d]
    where [d] is the data and [p] is a peer id indicating the provenance of the
    data. Typically, a candidate indicates what a peer considers to be the head
    of the chain.

    The consensus heuristic is parametrized by two values:

    - {e expected} is the number of candidates that the heuristic keeps
      internally,
    - {e threshold} is the number of candidates that need to agree on their data
      for a consensus to be reached.

    A precondition for the heuristic is that
    [0 <= threshold <= expected < 2 * threshold]
    to ensure a unique consensus value.
*)

(** ['a t] is the abstract internal state for the consensus heuristic mechanism.
    ['a] is the type of the data that needs to be agreed upon.

    Note that these values are intended to be short-lived, one shot.
    Specifically, these values are not meant to be used over a long period of
    time during which there is churn in the set of p2p neighbors. *)
type 'a t

(** The three different states that the heuristic can be in.

   - If the heuristic received less than {e expected} candidates and no
     candidates' data has {e threshold} occurrences, then its state is
     [Need_more_candidates].

   - If the heuristic received {e expected} (or more) candidates but no
     candidates' data has {e threshold} occurrences, its state is
     [No_consensus]. The payload for the constructor lists all the candidates'
     data with the number of their occurrences.

   - Otherwise there is some candidates' data that has {e threshold} occurrences
     or more and the state is [Consensus]. *)
type 'a state =
  | Consensus of 'a
  | No_consensus of ('a * int) list
  | Need_more_candidates

(** [create ~compare ~expected ~threshold ()] is a fresh heuristic state.

    [compare] is used internally to instantiate a map of candidates. It must be
    a total order over the type of data, but the specific of the order has no
    effect on the consensus.

    @raise Invalid_argument if not
    [0 <= threshold <= expected < 2 * threshold]
*)
val create :
  ?compare:('a -> 'a -> int) -> expected:int -> threshold:int -> unit -> 'a t

(** [get_state heuristic] returns the current {!state} of the heuristic
    according to the semantics given above. *)
val get_state : 'a t -> 'a state

(** [update heuristic (peer, data)] updates the heuristic with
    the candidate [(peer, data)]. If a data was already registered for
    [peer], then it is replaced by the update. It is the responsibility
    of the caller given a peer to update better and better data (for the
    caller's notion of "better"). *)
val update : 'a t -> P2p_peer.Id.t * 'a -> unit

(** {2 Consensus heuristic worker} *)

(** This worker implements a memoisation mechanism for the consensus
    heuristic with an expiry date mechanism. The worker also handles a
    hook mechanism to be executed on values found by the consensus
    heuristic. *)
module Worker : sig
  (** A worker for a consensus over ['a]. *)
  type 'a t

  (** [create ~expire_time ~job ~restart_delay] creates a worker.

      - [expire] is a span of time during which a found consensus is memoised.
        After this span of time has elapsed, the found consensus is invalidated
        and a new consensus will be sought the next time it is queried.

      - [job] is the task the worker runs to seek a consensus. It is the
        responsibility of the caller to assemble [job] using the functions
        {!create}, {!update}, and {!get_state} above.

      - [restart_delay] is a span of time that elapses between a non-successful
        run of [job] (a run that returns [No_consensus] or
        [Need_more_candidates]), and the next run. The delay leaves some time to
        elapse so that the network data has time to evolve before attempting to
        find a consensus again.

      Note: Remember that the base heuristic states are meant to be short-lived.
      They are meant to be used as one-shot. Consequently, when you assemble a
      function for [job], it should create a heuristic state, update it, and
      query it. You should {e NOT} share a single heuristic state for multiple
      runs of [job]. *)
  val create :
    expire_time:Ptime.Span.t ->
    job:(unit -> 'a state Lwt.t) ->
    restart_delay:Ptime.Span.t ->
    'a t

  (** [wait worker] is a promise that resolves the next time the worker finds a
      consensus; i.e., the next time the worker's [job] returns [Consensus].

      If the worker has found a consensus less than [expire] time ago, the
      promise is already resolved with the found consensus.

      If the worker is not currently seeking a consensus (and it doesn't have a
      currently memoised consensus), [job] is called again to seek one. *)
  val wait : 'a t -> 'a Lwt.t

  (** [on_next_consensus worker hook] registers a hook to be executed
      on the next consensus found by the worker or the current one if there is a
      non-expired consensus (i.e.,
      [Lwt.state (wait worker) = Lwt.Return hash]).

      Hooks are executed in the same order they are registered. *)
  val on_next_consensus : 'a t -> ('a -> unit) -> unit

  (** [on_all_consensus worker hook] registers a hook to be executed
      on every future consensus found by the worker as well as the current one
      if there is a valid consensus (i.e.,
      [Lwt.state (wait worker) = Lwt.Return hash]).

      It is guaranteed that a hook is executed exactly once for each
      time a consensus is reached. More precisely, between two
      executions of the same hook, there is at least one execution of [job].

      Hooks are executed in the same order they are registered. *)
  val on_all_consensus : 'a t -> ('a -> unit) -> unit

  (** [cancel worker] cancels the current task of the worker. All
      hooks are removed. If there are any pending promises returned by [wait],
      they are canceled. *)
  val cancel : 'a t -> unit
end
