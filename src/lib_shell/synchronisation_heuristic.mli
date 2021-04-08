(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** {2 Synchronisation heuristic} *)

(** The synchronisation heuristic module handles a heuristic to decide
   whether a node is synchronized with respect to its peers. This
   heuristic is parameterized by two variables:

    - [threshold] is the number of peers to take into account for the
   heuristic

    - [latency] is the timestamp drift (in seconds) expected for the
   [threshold] best candidates (see below). *)

type status = Chain_validator_worker_state.Event.synchronisation_status =
  | Synchronised of {is_chain_stuck : bool}
  | Not_synchronised

(** A node is either [Not_synchronised] or [Synchronised]. If the node
   is [Synchronised] the chain may be stuck (last block validated was
   a while ago). The heuristic needs to be updated every time a block
   is validated or if a peer sends a block which is already known as
   valid. In the following, we denote such a block as a
   `candidate`. The heuristic maintains a set of candidates such that
   there is at most one candidate per peer. Given a peer, the
   heuristic always keeps the most recent candidate.

    The heuristic works as follows:

    If [t.threshold] is negative then [get_state t] always returns
   [Not_synchronised].

    If [t.threshold] is 0 then [get_state t] always returns
   [Synchronised {is_chain_stuck=false}].

    Otherwise:

    - The state is [Synchronised {is_chain_stuck = false}] if the set
   of candidates that are more recent than ``latency`` seconds from
   now has a cardinal equal or greater to ``threshold``.

    - The state is [Synchronised {is_chain_stuck = true}] if all the
   following statements are respected:

    1. [threshold > 1]

    2. The ``threshold`` most recent candidates have the same
   timestamp.

    3. There is no candidate more than ``latency`` seconds from now

    - The state is [Not_synchronised] otherwise.

    Notice that if [threshold] is 1, the state is either [Synchronised
   {is_chain_stuck=false}] or [Not_synchronised].

    This heuristic should be used with a small [threshold]: Between 2
   and 10. Other values should be used with care and are mostly here
   for testing or debugging purpose.

    Following the {e separation of concerns} principle, the heuristic
   exports two modules:

    - The [Core] module contains all the logic behind the heuristic
   described above and can be used as such.

    - The [Bootstrapping] module provides facility to register
   callbacks when the status of the heuristic changes, and in
   particular defines a [bootstrapped] flag which is set to [true] if
   the heuristic was synchronised at least once.

  *)

(** {2 Core}

    This module is [Lwt] agnostic and implements all the logic
    behind the synchronisation heuristic.  *)
module Core : sig
  (** Internal state of the bootstrap_heuristic *)
  type t

  (** [create ~threshold ~latency ()] initializes the heuristic with these two
      parameters *)
  val create : threshold:int -> latency:int -> t

  (** [update t (timestamp, peer)] updates [t] according to the
      heuristic above. The [timestamp] should come from a valid block,
      and the [peer] should be the one who sent the block. *)
  val update : t -> Time.Protocol.t * P2p_peer.Id.t -> unit

  (** [get_status t] is the current status according of the bootstrap
      heuristic described above. *)
  val get_status : t -> status
end

(** {2 Bootstrapping }

    This module inherits from [Core] and handle a bootstrapped flag as
    well as [Lwt] callbacks when the status of heuristic or the
    bootstrap flag changes.

    The bootstrap flag is set to [true] in two cases:

    - When the core heuristic status is [Synchronised] for the first
      time

    - When [force_bootstrapped state b] is called with [b = true]

    Only the function [force_bootstrapped] can set the boolean flag to
    [false]. *)
module Bootstrapping : sig
  type t

  (** [create ?when_bootstrapped_changes ?when_status_changes ~threshold ~latency ()]
      is a wrapper around [Core.create ~threshold ~latency ()].

      - [when_status_changes] is a callback called immediately when the status
        changes (as a result of some calls to {!update}) and when the status is
        first set (as a result of calling {!activate}).

      - [when_bootstrapped_changes] is a callback called immediately when the
        bootstrap flagged changes (as a result of some calls to {!update} or to
        {!force_bootstrapped}) or when the flag is first set (as a result of
        calling {!activate}). *)
  val create :
    ?when_bootstrapped_changes:(bool -> unit Lwt.t) ->
    ?when_status_changes:(status -> unit Lwt.t) ->
    threshold:int ->
    latency:int ->
    unit ->
    t

  (** [activate state] activates the heuristics. It sets the status and the
      bootstrap flag to their initial values (which depend on the initial value
      of [threshold]) and calls the [when_status_changes] and
      [when_bootstrapped_changes] callbacks with these initial values.

      This function is intended to be used once, after the call to [create]
      and before the first call to [update].  *)
  val activate : t -> unit Lwt.t

  (** [update state candidate] is a wrapper around [Core.update]. If
      an [update] changes the status of the core heuristic, the
      [when_status_changes] callback is called. Similarly, if an [update]
      changes the bootstrap flag, the [when_bootstrapped_changes] callback is
      called. *)
  val update : t -> Time.Protocol.t * P2p_peer.Id.t -> unit Lwt.t

  (** [get_status state] is a wrapper around [Core.get_status]. *)
  val get_status : t -> status

  (** [is_bootstrapped state] returns the value of the bootstrap flag. *)
  val is_bootstrapped : t -> bool

  (** [force_bootstrapped state b] forces the status of the bootstrap flag. If
      the value of the flag changes as a result, the callback
      [when_bootstrapped_changes] is called. *)
  val force_bootstrapped : t -> bool -> unit Lwt.t

  (** [bootstrapped state] is a promise that becomes fulfilled when the current
      bootstrap flag becomes [true]. If the bootstrap flag is already [true],
      the promise is already resolved. *)
  val bootstrapped : t -> unit Lwt.t
end
