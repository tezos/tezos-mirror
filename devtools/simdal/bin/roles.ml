type attester_index = Attester_index of int [@@ocaml.unboxed]

type node_index = Node_index of int [@@ocaml.unboxed]

(** The type of roles. *)
type t =
  | Producer of {slot : int; relayed : int Seq.t}
  | Attester of {
      index : attester_index;
      shards : Interval.t;
      relayed : int Seq.t;
    }
  | Consumer of {slot : int; relayed : int Seq.t}

type cfg = {
  nproducers : int;  (** Number of slot producers in the network *)
  nrelayed_producers : int;
      (** Each producer relays [nrelayed_producers] pseudo-random slots.
            These extra relayed slots are generated deterministically
            from the slot index of each slot producer. *)
  nattesters : int;  (** Number of attesters in the network *)
  min_relayed_shards : int;
      (** Minimum number of shards to relay, including that assigned to the attester. *)
  nconsumers : int;  (** Number of slot consumers in the network *)
  nrelayed_consumers : int;
      (** Each consumer relays [nrelayed_consumers] pseudo-random slots.
            These extra relayed slots are generated deterministically
            from the slot index of each slot producer. *)
  shard_assignment : (attester_index, Interval.t) Hashtbl.t;
      (** Shard interval assigned to each attester *)
  reweighted_shard_dist : int Stats.Gen.t;
      (** Shard distribution such that the shard owner distribution
            is uniform over owners. *)
}

let realistic_cfg ?(flat_stake = false) ?(nproducers = 256) ?(nconsumers = 256)
    ?(nrelayed_producers = 0) ?(min_relayed_shards = 0)
    ?(nrelayed_consumers = 0) () =
  let shards =
    Array.to_seq
      (if flat_stake then Stake.uniform_shard_assignment
      else Stake.mainnet_shard_assignment)
  in
  let shard_assignment =
    shards
    |> Seq.map (fun (i, bounds) -> (Attester_index i, bounds))
    |> Hashtbl.of_seq
  in
  let weights =
    shards
    |> Seq.map (fun (_, itv) ->
           let len = Interval.len itv in
           Interval.to_seq itv
           |> Seq.map (fun shard -> (shard, 1. /. float len)))
    |> Seq.concat |> Array.of_seq
  in
  let reweighted_shard_dist = Stats.Gen.categorical weights in
  {
    nproducers;
    nrelayed_producers;
    nattesters = Hashtbl.length shard_assignment;
    min_relayed_shards;
    nconsumers;
    nrelayed_consumers;
    shard_assignment;
    reweighted_shard_dist;
  }

let count cfg = cfg.nproducers + cfg.nattesters + cfg.nconsumers

let attesters_count cfg = cfg.nattesters

(* This function is correct because of the two following facts:
   - the [Stake.shards] list is in decreasing order of stake
   - In [realistic_cfg], [shard_assignment] stores shards in order,
     starting from 0 *)
let largest_attester_index cfg = Node_index cfg.nproducers

let median_attester_index cfg =
  let (Node_index i) = largest_attester_index cfg in
  Node_index (i + (cfg.nattesters / 2))

let smallest_attester_index cfg =
  let (Node_index i) = largest_attester_index cfg in
  Node_index (i + (cfg.nattesters - 1))

let producer_for_slot cfg slot =
  assert (slot >= 0 && slot < cfg.nproducers) ;
  Node_index slot

let consumer_for_slot cfg slot =
  assert (slot >= 0 && slot < cfg.nproducers) ;
  Node_index (cfg.nproducers + cfg.nattesters + slot)

let attester_to_node_index cfg (Node_index i) =
  if i >= cfg.nattesters then invalid_arg "attester_index" ;
  Node_index (i + cfg.nproducers)

let is_producer cfg (Node_index i) = i < cfg.nproducers

let is_attester cfg (Node_index i) =
  i >= cfg.nproducers && i < cfg.nproducers + cfg.nattesters

let hash_orbit_modulo p i =
  Seq.unfold
    (fun i ->
      let state = Hashtbl.hash i in
      let res = state mod p in
      Some (res, state))
    i

let role_of_index cfg (Node_index i as index) =
  if i < 0 || i >= count cfg then None
  else if is_producer cfg index then
    let slot = i in
    let relayed =
      (* We want distinct but deterministic sequences of relayed slots
         resp. for producers and consumers, so we seed the initial element
         with a string. *)
      let init = Hashtbl.hash (slot, "producer") in
      hash_orbit_modulo cfg.nproducers init |> Seq.take cfg.nrelayed_producers
    in
    Some (Producer {slot; relayed})
  else if is_attester cfg index then
    let index = i - cfg.nproducers in
    let att_index = Attester_index index in
    match Hashtbl.find_opt cfg.shard_assignment att_index with
    | None ->
        Format.kasprintf
          failwith
          "role_of_index: attester index %d not assigned shards"
          index
    | Some shards ->
        let relayed =
          (* Note that if we're unlucky, the extra relayed shards might already be present in [shards]. *)
          let relayed_count = cfg.min_relayed_shards in
          let init = index in
          hash_orbit_modulo 2048 init |> Seq.take relayed_count
        in
        Some (Attester {index = att_index; shards; relayed})
  else
    let slot = i - cfg.nproducers - cfg.nattesters in
    let relayed =
      (* We want distinct but deterministic sequences of relayed slots
         resp. for producers and consumers, so we seed the initial element
         with a string. *)
      let init = Hashtbl.hash (slot, "consumer") in
      hash_orbit_modulo cfg.nproducers init |> Seq.take cfg.nrelayed_producers
    in
    Some (Consumer {slot; relayed})

let index_of_role cfg role =
  match role with
  | Producer {slot; relayed = _} -> Node_index slot
  | Attester {index = Attester_index i; shards = _; relayed = _} ->
      Node_index (i + cfg.nproducers)
  | Consumer {slot; relayed = _} ->
      Node_index (slot + cfg.nproducers + cfg.nattesters)

let shards_of_attester_index cfg att_index =
  Hashtbl.find_opt cfg.shard_assignment att_index

let shards_of_attester cfg index =
  match role_of_index cfg index with
  | None -> None
  | Some r -> (
      match r with
      | Producer _ -> None
      | Attester {shards; _} -> Some shards
      | Consumer _ -> None)

let relayed_shards_of_attester cfg index =
  match role_of_index cfg index with
  | None -> None
  | Some r -> (
      match r with
      | Producer _ -> None
      | Attester {shards; relayed; _} ->
          Some (Seq.append (Interval.to_seq shards) relayed)
      | Consumer _ -> None)

let fold_range start stop f acc =
  let r = ref acc in
  for i = start to stop do
    r := f i !r
  done ;
  !r

let fold_producers cfg = fold_range 0 (cfg.nproducers - 1)

let fold_attesters cfg =
  fold_range cfg.nproducers (cfg.nproducers + cfg.nattesters - 1)

let fold_consumers {nproducers; nattesters; nconsumers; _} =
  fold_range (nproducers + nattesters) (nproducers + nattesters + nconsumers - 1)
