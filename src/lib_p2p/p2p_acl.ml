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

module PeerLRUCache : Ring.TABLE with type v = P2p_peer.Id.t = struct
  module Cache = Lru_cache.Make (P2p_peer.Id.Table)

  type t = unit Cache.t

  type v = Cache.key

  let create capacity = Cache.create ~capacity

  let add cache peer_id = Cache.push cache peer_id ()

  let mem cache peer_id = Cache.is_cached cache peer_id

  let remove cache peer_id = Cache.remove cache peer_id

  let clear cache = Cache.clear cache

  let elements cache = Cache.bindings cache |> List.map fst
end

module PatriciaTree (V : HashPtree.Value) = struct
  module M = HashPtree.Make_BE_int2_64 (V)
  module Bits = HashPtree.Int2_64

  type t = M.t

  let empty = M.empty

  let key_of_ipv6 ip = Ipaddr.V6.to_int64 ip

  let key_to_ipv6 key = Ipaddr.V6.of_int64 key

  let key_mask_of_ipv6_prefix p =
    let ip = Ipaddr.V6.Prefix.network p in
    let mask = Ipaddr.V6.Prefix.netmask p in
    (key_of_ipv6 ip, key_of_ipv6 mask)

  let remove key t = M.remove (key_of_ipv6 key) t

  let remove_prefix prefix t =
    let (key, mask) = key_mask_of_ipv6_prefix prefix in
    M.remove_prefix key mask t

  let add_prefix prefix value t =
    let (key, mask) = key_mask_of_ipv6_prefix prefix in
    M.add (fun _ v -> v) ~key ~value ~mask t

  let add key value t =
    let key = key_of_ipv6 key in
    M.add (fun _ v -> v) ~key ~value t

  let mem key t = M.mem (key_of_ipv6 key) t

  let key_mask_to_prefix key mask =
    Ipaddr.V6.Prefix.of_netmask (key_to_ipv6 mask) (key_to_ipv6 key)

  let fold f t acc =
    let f key mask value acc =
      let prefix = key_mask_to_prefix key mask in
      f prefix value acc
    in
    M.fold f t acc

  let pp ppf t =
    let lst = fold (fun p _ l -> p :: l) t [] in
    Format.fprintf
      ppf
      "@[<2>[%a]@]"
      Format.(
        pp_print_list
          ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
          Ipaddr.V6.Prefix.pp)
      lst
end

(* patricia trees using IpV6 addresses as keys *)
module IpSet = struct
  include PatriciaTree (Time.System)

  let remove_old t ~older_than =
    let module MI = struct
      type result = Time.System.t

      let default = Ptime.max

      let map _t _key value = value

      let reduce _t left right = Time.System.(min left right)
    end in
    let module MR = M.Map_Reduce (MI) in
    MR.filter (fun addtime -> Time.System.(older_than <= addtime)) t
end

module IpTable = Hashtbl.Make (struct
  type t = Ipaddr.V6.t

  let hash = Hashtbl.hash

  let equal x y = Ipaddr.V6.compare x y = 0
end)

type t = {
  mutable greylist_ips : IpSet.t;
  greylist_peers : PeerLRUCache.t;
  banned_ips : unit IpTable.t;
  banned_peers : unit P2p_peer.Table.t;
}

let create size =
  {
    greylist_ips = IpSet.empty;
    greylist_peers = PeerLRUCache.create size;
    banned_ips = IpTable.create 53;
    banned_peers = P2p_peer.Table.create 53;
  }

(* Check if an ip is banned. Priority is given to the static
   banned_ips blacklist, then to the greylist *)
let banned_addr acl addr =
  IpTable.mem acl.banned_ips addr || IpSet.mem addr acl.greylist_ips

let unban_addr acl addr =
  IpTable.remove acl.banned_ips addr ;
  acl.greylist_ips <- IpSet.remove addr acl.greylist_ips

(* Check if [peer_id] is in either of the banned/greylisted
   collections. Caveat Emptor: it might be possible for a peer to have
   its ip adress banned, but not its ID. *)
let banned_peer acl peer_id =
  P2p_peer.Table.mem acl.banned_peers peer_id
  || PeerLRUCache.mem acl.greylist_peers peer_id

let unban_peer acl peer_id =
  P2p_peer.Table.remove acl.banned_peers peer_id ;
  PeerLRUCache.remove acl.greylist_peers peer_id

let clear acl =
  acl.greylist_ips <- IpSet.empty ;
  P2p_peer.Table.clear acl.banned_peers ;
  IpTable.clear acl.banned_ips ;
  PeerLRUCache.clear acl.greylist_peers

module IPGreylist = struct
  let add acl addr time =
    acl.greylist_ips <- IpSet.add addr time acl.greylist_ips

  let mem acl addr = IpSet.mem addr acl.greylist_ips

  (* The GC operation works only on the greylisted addresses
     set. Peers are evicted from the cache following LRU semantics,
     i.e. the least recently greylisted, when adding a new (distinct)
     peer to the greylist. If an address is removed by the GC from the
     acl.greylist set, it could potentially persist in the acl.peers
     set until more peers are banned. *)
  let remove_old acl ~older_than =
    acl.greylist_ips <- IpSet.remove_old acl.greylist_ips ~older_than

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
  let add acl peer_id = PeerLRUCache.add acl.greylist_peers peer_id

  let mem acl peer_id = PeerLRUCache.mem acl.greylist_peers peer_id
end
