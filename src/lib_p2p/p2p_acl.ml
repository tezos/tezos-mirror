(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
  ( val Ringo.set_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise
      : Ringo.SET_MAKER )
    (P2p_peer.Id)

module IpTable = Hashtbl.Make (struct
  type t = Ipaddr.V6.t

  let hash = Hashtbl.hash

  let equal x y = Ipaddr.V6.compare x y = 0
end)

type t = {
  mutable greylist_ips : Ipaddr.V6.t Bloomer.t;
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
    Systime_os.sleep delay
    >>= fun () -> Bloomer.countdown bloomer ; cleanup_loop ()
  in
  let rec cleanup_start () =
    Lwt_utils.dont_wait
      (fun exc ->
        Format.eprintf "Exception caught: %s\n%!" (Printexc.to_string exc) ;
        Format.eprintf "Resetting bloomer to an ok state\n%!" ;
        Bloomer.clear bloomer ;
        cleanup_start ())
      cleanup_loop
  in
  cleanup_start () ;
  {
    greylist_ips = bloomer;
    greylist_peers = PeerFIFOCache.create peer_id_size;
    banned_ips = IpTable.create 53;
    banned_peers = P2p_peer.Table.create ~random:true 53;
  }

(* Check if an ip is banned. Priority is given to the static
   banned_ips blacklist, then to the greylist *)
let banned_addr acl addr =
  IpTable.mem acl.banned_ips addr || Bloomer.mem acl.greylist_ips addr

let unban_addr acl addr =
  IpTable.remove acl.banned_ips addr ;
  Bloomer.rem acl.greylist_ips addr

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
  Bloomer.clear acl.greylist_ips ;
  P2p_peer.Table.clear acl.banned_peers ;
  IpTable.clear acl.banned_ips ;
  PeerFIFOCache.clear acl.greylist_peers

module IPGreylist = struct
  let add acl addr = Bloomer.add acl.greylist_ips addr

  let mem acl addr = Bloomer.mem acl.greylist_ips addr

  let clear acl = Bloomer.clear acl.greylist_ips

  (* The GC operation works only on the greylisted addresses
     set. If an address is removed by the GC from the
     acl.greylist_ips set, it could potentially persist in the
     acl.greylist_peers set until more peers are banned. *)
  let gc acl = Bloomer.countdown acl.greylist_ips

  let fill_percentage acl = Bloomer.fill_percentage acl.greylist_ips

  let life_expectancy_histogram acl =
    Bloomer.life_expectancy_histogram acl.greylist_ips

  let encoding = Data_encoding.(list P2p_addr.encoding)
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
end
