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

module PeerFIFOCache : Ringo.CACHE_SET with type elt = P2p_peer.Id.t =
  (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise
     : Ringo.SET_MAKER)
    (P2p_peer.Id)

module IPV6 = struct
  type t = Ipaddr.V6.t

  let hash = Hashtbl.hash

  let equal x y = Ipaddr.V6.compare x y = 0
end

(* This module is used to store a bounded number of IPs that are greylisted.
   It is only used to create a friendly interface for the user to get a list
   of greylisted IPs. It is not possible to extract this list from the
   PatriciaTree structure below. So we are forced to duplicate this information.
   This is different from the module above that is used to keep the list of
   greylisted peers *)
module IpFIFOCache : Ringo.CACHE_SET with type elt = IPV6.t =
  (val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise
     : Ringo.SET_MAKER)
    (IPV6)

module IpTable = Hashtbl.Make (struct
  type t = Ipaddr.V6.t

  let hash = Hashtbl.hash

  let equal x y = Ipaddr.V6.compare x y = 0
end)

type greylist_t = {
  (* The set is used to have a compact store for IPs *)
  bloomer : Ipaddr.V6.t Bloomer.t;
  (* The fifo is a bounded store to provide a friendly list of the greylisted
     IPs to the user. See comment at the declaration of IpFIFOCache. *)
  fifo : IpFIFOCache.t;
  (* Since the fifo is bounded, it can be overflowed. When it has been
     overflowed, it is no more reliable. The date of the first overflow is
     memorized to indicate that the fifo is no more reliable. This is reset by
     calling clear. *)
  mutable fifo_not_reliable_since : Ptime.t option;
}

type t = {
  greylist_ips : greylist_t;
  greylist_peers : PeerFIFOCache.t;
  banned_ips : unit IpTable.t;
  banned_peers : unit P2p_peer.Table.t;
}

let create ~peer_id_size ~ip_size ~ip_cleanup_delay =
  if peer_id_size <= 0 then
    invalid_arg "P2p_acl.create: bad value for peer_id_size" ;
  if ip_size <= 0 then invalid_arg "P2p_acl.create: bad value for ip_size" ;
  let bloomer =
    Bloomer.create (* 512KiB *)
      ~hash:(fun x -> Blake2B.(to_bytes (hash_string [Ipaddr.V6.to_octets x])))
      ~hashes:5 (* fixed, good for reasonable values of [ip_size] *)
      ~countdown_bits:
        4 (* 16 steps to 0, fixed discrete split of the cleanup delay *)
      ~index_bits:(Bits.numbits (ip_size * 8 * 1024 (* to bits *) / 4))
  in
  let delay = Time.System.Span.multiply_exn (1. /. 16.) ip_cleanup_delay in
  let rec cleanup_loop () =
    let open Lwt_syntax in
    let* () = Systime_os.sleep delay in
    Bloomer.countdown bloomer ;
    cleanup_loop ()
  in
  let rec cleanup_start () =
    Lwt.dont_wait cleanup_loop (fun exc ->
        Format.eprintf "Exception caught: %s\n%!" (Printexc.to_string exc) ;
        Format.eprintf "Resetting bloomer to an ok state\n%!" ;
        Bloomer.clear bloomer ;
        cleanup_start ())
  in
  cleanup_start () ;
  {
    greylist_ips =
      {
        bloomer;
        fifo = IpFIFOCache.create ip_size;
        fifo_not_reliable_since = None;
      };
    greylist_peers = PeerFIFOCache.create peer_id_size;
    banned_ips = IpTable.create 53;
    banned_peers = P2p_peer.Table.create ~random:true 53;
  }

(* Check if an ip is banned. Priority is given to the static
   banned_ips blacklist, then to the greylist *)
let banned_addr acl addr =
  IpTable.mem acl.banned_ips addr || Bloomer.mem acl.greylist_ips.bloomer addr

let unban_addr acl addr =
  IpTable.remove acl.banned_ips addr ;
  Bloomer.rem acl.greylist_ips.bloomer addr ;
  IpFIFOCache.remove acl.greylist_ips.fifo addr

(* Check if [peer_id] is in either of the banned/greylisted
   collections. Caveat Emptor: it might be possible for a peer to have
   its ip address banned, but not its ID. *)
let banned_peer acl peer_id =
  P2p_peer.Table.mem acl.banned_peers peer_id
  || PeerFIFOCache.mem acl.greylist_peers peer_id

let unban_peer acl peer_id =
  P2p_peer.Table.remove acl.banned_peers peer_id ;
  PeerFIFOCache.remove acl.greylist_peers peer_id

let clear acl =
  Bloomer.clear acl.greylist_ips.bloomer ;
  IpFIFOCache.clear acl.greylist_ips.fifo ;
  (* After a clear the fifo is reliable. *)
  acl.greylist_ips.fifo_not_reliable_since <- None ;
  P2p_peer.Table.clear acl.banned_peers ;
  IpTable.clear acl.banned_ips ;
  PeerFIFOCache.clear acl.greylist_peers

module IPGreylist = struct
  let add acl addr =
    Bloomer.add acl.greylist_ips.bloomer addr ;
    (* If the fifo is full before adding the IP, it is overflowed and then no
       more reliable. Since we want the first date we do not update the date if
       there is already one. *)
    if
      Option.is_none acl.greylist_ips.fifo_not_reliable_since
      && IpFIFOCache.length acl.greylist_ips.fifo
         = IpFIFOCache.capacity acl.greylist_ips.fifo
    then acl.greylist_ips.fifo_not_reliable_since <- Some (Ptime_clock.now ()) ;
    IpFIFOCache.add acl.greylist_ips.fifo addr

  let mem acl addr = Bloomer.mem acl.greylist_ips.bloomer addr

  let clear acl = Bloomer.clear acl.greylist_ips.bloomer

  (* The GC operation works only on the greylisted addresses
     set. Peers are evicted from the cache following LRU semantics,
     i.e. the least recently greylisted, when adding a new (distinct)
     peer to the greylist. If an address is removed by the GC from the
     acl.greylist set, it could potentially persist in the acl.peers
     set until more peers are banned.

     The fifo is filtered after the GC has operated on the IpSet.*)
  let gc acl =
    let filter_fifo f fifo =
      let to_remove =
        IpFIFOCache.fold
          (fun elt acc -> if f elt then acc else elt :: acc)
          fifo
          []
      in
      List.iter (fun elt -> IpFIFOCache.remove fifo elt) to_remove
    in
    Bloomer.countdown acl.greylist_ips.bloomer ;
    filter_fifo (Bloomer.mem acl.greylist_ips.bloomer) acl.greylist_ips.fifo

  let fill_percentage acl = Bloomer.fill_percentage acl.greylist_ips.bloomer

  let life_expectancy_histogram acl =
    Bloomer.life_expectancy_histogram acl.greylist_ips.bloomer

  let list acl =
    IpFIFOCache.fold (fun e acc -> e :: acc) acl.greylist_ips.fifo []

  let list_not_reliable_since acl = acl.greylist_ips.fifo_not_reliable_since
end

module IPBlacklist = struct
  let add acl addr = IpTable.add acl.banned_ips addr ()

  let mem acl addr = IpTable.mem acl.banned_ips addr
end

module PeerBlacklist = struct
  let add acl addr = P2p_peer.Table.add acl.banned_peers addr ()

  let mem acl addr = P2p_peer.Table.mem acl.banned_peers addr
end

module PeerGreylist = struct
  let add acl peer_id = PeerFIFOCache.add acl.greylist_peers peer_id

  let mem acl peer_id = PeerFIFOCache.mem acl.greylist_peers peer_id

  let list acl =
    PeerFIFOCache.fold (fun e acc -> e :: acc) acl.greylist_peers []
end
