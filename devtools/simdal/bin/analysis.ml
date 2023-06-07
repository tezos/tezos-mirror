module Tree = Simdal.Sampler.Network_stats_helpers.Tree

type tree = Tree.t

type subgraph_predicate = Simdal.G.vertex -> bool

type 'a t = {
  pre_shard_iteration : Simdal.Sampler.state -> unit;
  sample :
    Simdal.Sampler.state -> shard:int -> subgraph_predicate -> tree list -> unit;
  post_shard_iteration : Simdal.Sampler.state -> unit;
  post_simulation : unit -> 'a;
}

let void =
  {
    pre_shard_iteration = (fun _ -> ());
    sample = (fun _ ~shard:_ _ _ -> ());
    post_shard_iteration = (fun _ -> ());
    post_simulation = (fun () -> ());
  }

let postcompose f a =
  {a with post_simulation = (fun () -> f (a.post_simulation ()))}

let proportion_of_attesters_in_neighbourhood id cfg =
  let atts = ref 0 in
  let total = ref 0 in
  {
    void with
    pre_shard_iteration =
      (fun state ->
        Simdal.G.iter_pred
          (fun v ->
            incr total ;
            if Roles.is_attester cfg (Node_index v) then incr atts else ())
          (Simdal.Sampler.graph state)
          id);
  }

let receives_enough_shards ~receiver ~target_shard_count ~nshards cfg opts =
  let not_enough_shards = ref 0 in
  let fill_rate = ref 0.0 in
  let shards_received = ref 0 in
  let assignment =
    match Roles.role_of_index cfg receiver with
    | None -> assert false
    | Some (Producer _) ->
        invalid_arg "receives_enough_shards: receiver is a producer"
    | Some (Attester {shards; _}) -> shards
    | Some (Consumer _) -> Interval.{lo = 0; hi = nshards - 1}
  in
  {
    pre_shard_iteration = (fun _state -> shards_received := 0);
    sample =
      (fun _state ~shard _subgraph trees ->
        match trees with
        | [] -> failwith "Analysis.receives_enough_shards, sample"
        | tree :: _ ->
            (* All spanning trees are equivalent from the POV of shard reception,
               we only look at the first one *)
            let (Node_index receiver) = receiver in
            if Tree.mem_vertex tree receiver && Interval.mem shard assignment
            then incr shards_received
            else ());
    post_shard_iteration =
      (fun _state ->
        let shards_received = float !shards_received in
        fill_rate := !fill_rate +. shards_received ;
        if shards_received < target_shard_count then incr not_enough_shards);
    post_simulation =
      (fun () ->
        (* probability that the receiver fails *)
        let att_fail = float !not_enough_shards /. float opts.Cmdline.ngraphs in
        (* average shard fill rate for largest receiver *)
        let att_rate =
          !fill_rate /. (float opts.Cmdline.ngraphs *. target_shard_count)
        in
        (att_fail, att_rate));
  }

let slot_confirmed ~max_shards_required ~thresholds ~nshards (cfg : Roles.cfg) =
  let nattesters = Roles.attesters_count cfg in
  let shards_received = Array.make nattesters 0 in
  let shards_confirmed_dist = ref [] in
  {
    pre_shard_iteration =
      (fun _state -> Array.fill shards_received 0 nattesters 0);
    sample =
      (fun _state ~shard _subgraph trees ->
        match trees with
        | [] -> failwith "Analysis.receives_enough_shards, sample"
        | tree :: _ ->
            (* For each vertex in the subnetwork which is an attester, if
               the [shard] is in the attester assignment, increment the count
               of received shards. *)
            Tree.iter_vertices tree (fun v ->
                match Roles.role_of_index cfg (Node_index v) with
                | None -> assert false
                | Some (Attester {index = Attester_index i; shards; _}) ->
                    if Interval.mem shard shards then
                      shards_received.(i) <- shards_received.(i) + 1
                | _ -> ()));
    post_shard_iteration =
      (fun _state ->
        let shards_confirmed = ref 0 in
        for i = 0 to nattesters - 1 do
          match Roles.shards_of_attester_index cfg (Roles.Attester_index i) with
          | None -> assert false
          | Some itv ->
              let required_count =
                Int.min max_shards_required (Interval.len itv)
              in
              if shards_received.(i) >= required_count then
                shards_confirmed := !shards_confirmed + shards_received.(i)
        done ;
        shards_confirmed_dist := !shards_confirmed :: !shards_confirmed_dist);
    post_simulation =
      (fun () ->
        List.map
          (fun threshold ->
            let ratio_dist =
              List.map
                (fun confirmed -> float confirmed /. float nshards)
                !shards_confirmed_dist
            in
            let success_count =
              List.fold_left
                (fun acc ratio -> if ratio >= threshold then acc + 1 else acc)
                0
                ratio_dist
            in
            ( threshold,
              `Ratio_dist ratio_dist,
              `Confirm_p (float success_count /. float (List.length ratio_dist))
            ))
          thresholds);
  }

let estimate_bandwidth rng_state (opts : Cmdline.options) cfg =
  let open Simdal in
  let samples = ref [] in
  let bwth_stats = ref (Sampler.create_bandwidth_stats ()) in
  {
    pre_shard_iteration =
      (fun _state -> bwth_stats := Sampler.create_bandwidth_stats ());
    sample =
      (fun state ~shard:_ subgraph_predicate trees ->
        Sampler.estimate_bandwidth
          ~state
          ~subgraph_predicate
          ~counters:!bwth_stats
          ~spanning_trees:trees
          rng_state);
    post_shard_iteration = (fun _state -> samples := !bwth_stats :: !samples);
    post_simulation =
      (fun () ->
        let module T = G.Vertex_table in
        let nsamples = List.length !samples in
        assert (opts.ngraphs = nsamples) ;
        let vcount = Roles.count cfg in
        let in_table = Array.make_matrix vcount nsamples 0.0 in
        let out_table = Array.make_matrix vcount nsamples 0.0 in
        List.iteri
          (fun sample_index {Sampler.incoming; outgoing} ->
            T.iter
              (fun v count -> in_table.(v).(sample_index) <- !count)
              incoming ;
            T.iter
              (fun v count -> out_table.(v).(sample_index) <- !count)
              outgoing)
          !samples ;
        let stats table =
          Array.to_seq table |> Seq.map Stats_record.of_arr |> Array.of_seq
        in
        (stats in_table, stats out_table));
  }
