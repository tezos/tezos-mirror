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

type status = Synchronised of {is_chain_stuck : bool} | Not_synchronised

(** A node is either [Not_synchronised] or [Synchronised]. If the node
   is [Synchronised] the chain may be stuck (last block validated was
   a while ago). The heuristic needs to be updated every time a block
   is validated or if a peer sends a block which is already known as
   valid. In the following, we denote such a block as a
   `candidate`. The heuristic maintains a set of candidates such that
   there is at most one candidate per peer. Given a peer, the heuristic
   always keeps the most recent candidate.

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
   for testing or debugging purpose. *)

(** internal state of the bootstrap_heuristic *)
type t

(** [update t (timestamp, peer)] updates [t] according the
   heuristic above. The [timestamp] should come from a valid block,
   and the [peer] should be the one who sent the block. *)
val update : t -> Time.Protocol.t * P2p_peer.Id.t -> unit

(** [get_status t] gives the current status according of the bootstrap
   heuristic described above. *)
val get_status : t -> status

(** [create ~threshold ~sync_latency] initializes the heuristic with
   these two parameters *)
val create : threshold:int -> latency:int -> t
