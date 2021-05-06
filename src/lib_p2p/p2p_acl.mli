(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module implements four Access Control Lists:

   - IP greylist is a set of banned IP addresses automatically added
   by the P2P layer.

   - [peer_id] greylist is a set of banned peers ids automatically
   added by the P2P layer.

   - IP blacklist is a set of IP addresses manually added by the node
   admin.

   - peers blacklist is a set of peers ids manually added by the node
   admin.

   IP greylists use a time based GC to periodically remove entries
   from the table, while [peer_id] greylists are built using an LRU
   cache, where the least-recently grey-listed peer is evicted from
   the table when adding a new banned peer to a full cache. Other
   tables are user defined and static.

*)

type t

(** [create ~peer_id_size ~ip_size] is a set of four ACLs (see above)
   with the peer_id greylist being a LRU cache of size [peer_id_size]
   and the IP address greylist a bloom filter of size [ip_size]
   (expressed in KiB). Elements are (probabilistically) kept in the
   bloom filter for [ip_cleanup_delay], the cleanup happens in a
   discrete way in sixteen steps. *)
val create :
  peer_id_size:int -> ip_size:int -> ip_cleanup_delay:Time.System.Span.t -> t

(** [banned_addr t addr] is [true] if [addr] is blacklisted or
    greylisted. *)
val banned_addr : t -> P2p_addr.t -> bool

(** [unban_addr t addr] remove the address from both the blacklist
    of banned addresses and the greylist of addresses *)
val unban_addr : t -> P2p_addr.t -> unit

(** [banned_peer t peer_id] is [true] if peer with id [peer_id] is
    blacklisted or greylisted. *)
val banned_peer : t -> P2p_peer.Id.t -> bool

(** [unban_peer t peer] remove the peer from both the blacklist
    of banned peers and the greylist of peers *)
val unban_peer : t -> P2p_peer.Id.t -> unit

(** [clear t] clears all four ACLs. *)
val clear : t -> unit

module IPGreylist : sig
  (** [add t addr] adds [addr] to the address greylist. *)
  val add : t -> P2p_addr.t -> unit

  (** [clear t] removes all address greylistings. *)
  val clear : t -> unit

  (** [gc t] removes some address greylistings (the oldest have a
     higher probability to be removed, yet due to the underlying
     probabilistic structure, recent greylistings can be dropped). *)
  val gc : t -> unit

  (** [mem t addr] tells if [addr] is greylisted, but may return a
     false positive due to the underlying probabilistic structure. *)
  val mem : t -> P2p_addr.t -> bool

  (** [fill_percentage t] returns the percentage (in the [0,1] interval)
      of bloom filter cells which are nonzero. *)
  val fill_percentage : t -> float

  (** [life_expectancy_histogram t] returns the life expectancy
      distribution of cells (measured in [gc] calls) stored as
      an array: the value at index [k] stores the number
      of cells that are [k] calls to [gc] away from being
      removed from the greylist. *)
  val life_expectancy_histogram : t -> int array

  (** [list t] if [list_not_reliable_since t] returns [None], returns the list
      of currently greylisted IPs. Else it return the list of all ips greylisted
      since `list_not_reliable_since` *)
  val list : t -> P2p_addr.t list

  (* The list returned by [list t] can be overflowed. When it
     has been overflowed, it is no more reliable. The date of the first
     overflow is memorized to indicate that it is no more reliable. This is
     reset by calling [P2p_acl.clear t]. *)
  val list_not_reliable_since : t -> Time.System.t option
end

module IPBlacklist : sig
  val add : t -> P2p_addr.t -> unit

  val mem : t -> P2p_addr.t -> bool
end

module PeerBlacklist : sig
  val add : t -> P2p_peer.Id.t -> unit

  val mem : t -> P2p_peer.Id.t -> bool
end

module PeerGreylist : sig
  (** [add t peer] adds [peer] to the peer greylist. It might
     potentially evict the least-recently greylisted peer, if the grey
     list is full. If [peer] was already in the list, it will become
     the most-recently greylisted, thus ensuring it is not evicted
     unfairly soon. *)
  val add : t -> P2p_peer.Id.t -> unit

  (** [mem t peer] returns true iff [peer] is greylisted.*)
  val mem : t -> P2p_peer.Id.t -> bool

  (** [list t]: return the list peers added to the greylist *)
  val list : t -> P2p_peer.Id.t list
end

(** / *)

module PeerFIFOCache : Ringo.CACHE_SET with type elt = P2p_peer.Id.t

module IpTable : Hashtbl.S with type key = Ipaddr.V6.t
