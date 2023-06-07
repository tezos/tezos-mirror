open Simdal
module R = Roles

let sf = Format.asprintf

(** {2 Commandline parameters } *)

let parameters = Cmdline.parse_options ()

let () =
  if Sys.file_exists parameters.Cmdline.data_root then ()
  else (
    Format.printf "Creating directory %s@." parameters.Cmdline.data_root ;
    Sys.mkdir parameters.Cmdline.data_root 0o755)

let log_fmtr, close =
  let file =
    Filename.concat
      parameters.data_root
      (Format.asprintf "log_%d.txt" (Unix.getpid ()))
  in
  let oc = open_out file in
  let fmtr = Format.formatter_of_out_channel oc in
  (fmtr, fun () -> close_out oc)

let when_verbose k = if parameters.verbose then k () else ()

let () =
  Format.fprintf log_fmtr "Parameters:@;%a@." Cmdline.pp_options parameters

let pid = Unix.getpid ()

(** {2 RNG init } *)

let rng_state = Random.State.make_self_init ()

(** {2 Preparing configurations } *)

(** [all_slots ~nslots ~shards] returns the set of topics corresponding
    to the cartesian product of the slots [0] to [nslots - 1] with [shards].  *)
let all_slots ~nslots ~shards =
  Seq.map
    (fun shard -> Seq.init nslots (fun slot -> Topic.{slot; shard}))
    shards
  |> Seq.concat |> Topic.Set.of_seq

(** [all_shards ~nshards ~slots] returns the set of topics corresponding
    to the cartesian product of the shards [0] to [2048 - 1] with [slots].  *)
let all_shards ~nshards ~slots =
  let all_shards_for_slot slot =
    Seq.init nshards (fun shard -> Topic.{slot; shard})
  in
  Seq.map all_shards_for_slot slots |> Seq.concat |> Topic.Set.of_seq

(** [tripartite_configuration] generates a configuration with slot producers,
    attesters and slot consumers. *)
let tripartite_configuration () =
  let nslots = 256 in
  let nshards = 2048 in
  let nproducers = nslots in
  let nconsumers = if parameters.Cmdline.with_consumers then nslots else 0 in
  let cfg =
    Roles.realistic_cfg
      ~flat_stake:parameters.Cmdline.flat_stake
      ~nproducers:nslots
      ~nconsumers
      ~min_relayed_shards:parameters.Cmdline.min_relayed_shards
      ()
  in
  let nattesters = Roles.attesters_count cfg in
  Format.printf
    "producers: %d, attesters: %d, consumers: %d@."
    nproducers
    nattesters
    nconsumers ;
  let extract name opt =
    match opt with
    | None -> Format.kasprintf failwith "Argument %s is mandatory" name
    | Some x -> x
  in
  let prod_bounds = extract "--prod-deg" parameters.prod_deg in
  let att_bounds = extract "--att-deg" parameters.att_deg in
  let cons_bounds = extract "--cons-deg" parameters.cons_deg in
  Format.fprintf log_fmtr "producer bounds: %a@." Interval.pp prod_bounds ;
  Format.fprintf log_fmtr "attester bounds: %a@." Interval.pp att_bounds ;
  Format.fprintf log_fmtr "consumer bounds: %a@." Interval.pp cons_bounds ;
  (*
    TODO: check consistency of options
    let att_outgoing_conns = (nattester * (att_deg - min_relay)) in
    check att_outgoing_conns = prod_conns + cons_conns
  *)
  let topic_function =
    Helpers.memoize (fun i ->
        match Roles.role_of_index cfg (Node_index i) with
        | None -> failwith "topic_function: invalid index"
        | Some r -> (
            match r with
            | R.Producer {slot; relayed} ->
                all_shards ~nshards ~slots:(Seq.cons slot relayed)
            | R.Attester {shards; relayed; index = _} ->
                all_slots
                  ~nslots
                  ~shards:(Seq.append (Interval.to_seq shards) relayed)
            | R.Consumer {slot; relayed} ->
                all_shards ~nshards ~slots:(Seq.cons slot relayed)))
  in
  let state =
    Sampler.create_empty
      (Roles.count cfg)
      ~kind:topic_function
      ~compat:(fun set1 set2 -> not (Topic.Set.disjoint set1 set2))
      ~bounds:(fun i ->
        match Roles.role_of_index cfg (Node_index i) with
        | None -> failwith "topic_function: invalid index"
        | Some r -> (
            match r with
            | R.Producer _ ->
                let itv = prod_bounds in
                (itv.lo, itv.hi)
            | R.Attester {index = _; _} ->
                let itv = att_bounds in
                (itv.lo, itv.hi)
            | R.Consumer _ ->
                let itv = cons_bounds in
                (itv.lo, itv.hi)))
  in
  (state, cfg, topic_function)

let g, cfg, topic_function = Helpers.chrono "init" tripartite_configuration

(* The value used for [burn_in] has currently to be tweaked by hand:
   one should burn samples until the error reaches a stable steady state.
   The current value should be a safe upper bound. *)
let after_burn_in =
  Helpers.chrono "burn-in" (fun () ->
      Sampler.network
        ~verbosity:`Silent
        ~initial:g
        ~burn_in:(10 * Sampler.edge_count g)
        rng_state)

let bandwidth_data cfg (in_stats : Stats_record.t array)
    (out_stats : Stats_record.t array) =
  let message_size = parameters.Cmdline.message_kb in
  let push stats i acc = Stats_record.rescale message_size stats.(i) :: acc in
  let of_rev_list l = Array.of_list (List.rev l) in
  let producer_incoming =
    R.fold_producers cfg (push in_stats) [] |> of_rev_list
  in
  let attester_incoming =
    R.fold_attesters cfg (push in_stats) [] |> of_rev_list
  in
  let consumer_incoming =
    R.fold_consumers cfg (push in_stats) [] |> of_rev_list
  in
  let producer_outgoing =
    R.fold_producers cfg (push out_stats) [] |> of_rev_list
  in
  let attester_outgoing =
    R.fold_attesters cfg (push out_stats) [] |> of_rev_list
  in
  let consumer_outgoing =
    R.fold_consumers cfg (push out_stats) [] |> of_rev_list
  in
  ( producer_incoming,
    producer_outgoing,
    attester_incoming,
    attester_outgoing,
    consumer_incoming,
    consumer_outgoing )

let pp_degree_stats_hum (state : Sampler.state) =
  let graph = Sampler.graph state in
  let cons_deg i acc = float (G.out_degree graph i) :: acc in
  let producer_degrees =
    R.fold_producers cfg cons_deg [] |> Array.of_list |> Stats_record.of_arr
  in
  let attester_degrees =
    R.fold_attesters cfg cons_deg [] |> Array.of_list |> Stats_record.of_arr
  in
  let consumer_degrees =
    R.fold_consumers cfg cons_deg [] |> Array.of_list |> Stats_record.of_arr
  in
  Helpers.pp_mat
    Format.pp_print_float
    (fun fmtr x ->
      Format.pp_print_string
        fmtr
        (match x with
        | `avg -> "avg"
        | `std -> "std"
        | `min -> "min"
        | `max -> "max"))
    (fun fmtr x ->
      Format.pp_print_string
        fmtr
        (match x with
        | `prod_degree -> "prod-degree"
        | `att_degree -> "att-degree"
        | `cons_degree -> "cons-degree"))
    ~header:[|`avg; `std; `min; `max|]
    ~rows:[|`prod_degree; `att_degree; `cons_degree|]
    (fun r c ->
      match r with
      | `prod_degree -> Stats_record.proj producer_degrees c
      | `att_degree -> Stats_record.proj attester_degrees c
      | `cons_degree -> Stats_record.proj consumer_degrees c)
    Format.std_formatter
    ()

let pp_propagation_state_hum fmtr name fail rate =
  Helpers.pp_mat
    ~no_row_label:true
    Format.pp_print_float
    (fun fmtr x ->
      match x with
      | `failure -> Format.fprintf fmtr "P(%s fails)" name
      | `fill_rate -> Format.fprintf fmtr "%s shards ratio" name)
    (fun _fmtr () -> ())
    ~header:[|`failure; `fill_rate|]
    ~rows:[|()|]
    (fun () c -> match c with `failure -> fail | `fill_rate -> rate)
    fmtr
    ()

let () =
  let full_chain =
    Seq.of_dispenser (fun () -> Some (after_burn_in rng_state))
  in
  let chain =
    Seq.zip full_chain (Seq.ints 0)
    |> Seq.filter (fun (_, i) ->
           i mod parameters.Cmdline.autocorrelation_drop = 0)
    |> Seq.map fst
  in
  let dat name = Filename.concat parameters.Cmdline.data_root (name ^ ".dat") in
  (* 0 is the first slot producer (see [tripartite_configuration]) *)
  let (Node_index source) = Roles.producer_for_slot cfg 0 in
  let attester_receives_enough_shards attester =
    let attester_shards =
      Roles.shards_of_attester cfg attester
      |> Option.get |> Interval.len |> float
    in
    let threshold =
      Float.min attester_shards (2048. /. parameters.Cmdline.redundancy)
    in
    Analysis.receives_enough_shards
      ~receiver:attester
      ~target_shard_count:threshold
      ~nshards:2048
      cfg
      parameters
    |> Analysis.postcompose (fun (fail, rate) ->
           let (Node_index attester) = attester in
           pp_propagation_state_hum
             log_fmtr
             (Format.asprintf "att_%d(stake=%.1f)" attester attester_shards)
             fail
             rate ;
           let filename = Format.kasprintf dat "attester_%d_%d" attester pid in
           Dat.(
             make
               filename
               [|"prod_deg"; "r"; "shards"; "failp"; "fill_rate"|]
               [|
                 [|
                   Option.get parameters.prod_deg |> Interval.mid |> float;
                   float cfg.min_relayed_shards;
                   attester_shards;
                   fail;
                   rate;
                 |];
               |]
             |> write))
  in
  let consumer_receives_enough_shards =
    Analysis.receives_enough_shards
      ~receiver:(Roles.consumer_for_slot cfg 0)
      ~target_shard_count:(2048. /. parameters.Cmdline.redundancy)
      ~nshards:2048
      cfg
      parameters
    |> Analysis.postcompose (fun (fail, rate) ->
           pp_propagation_state_hum
             log_fmtr
             (Format.asprintf "consumer(slot=%d)" 0)
             fail
             rate ;
           let filename = Format.kasprintf dat "consumer_%d" pid in
           Dat.(
             make
               filename
               [|"prod_deg"; "r"; "failp"; "fill_rate"|]
               [|
                 [|
                   Option.get parameters.prod_deg |> Interval.mid |> float;
                   float cfg.min_relayed_shards;
                   fail;
                   rate;
                 |];
               |]
             |> write))
  in
  let estimate_bandwidth =
    Analysis.estimate_bandwidth rng_state parameters cfg
    |> Analysis.postcompose (fun (in_stats, out_stats) ->
           let pi, po, ai, ao, ci, co = bandwidth_data cfg in_stats out_stats in
           (* pp_bandwidth_stats_hum log_fmtr data ; *)
           let stats =
             let vecs =
               [
                 (pi, "producer_in");
                 (po, "producer_out");
                 (ai, "attester_in");
                 (ao, "attester_out");
                 (ci, "consumer_in");
                 (co, "consumer_out");
               ]
             in
             vecs |> List.to_seq
             |> Seq.flat_map (fun (v, title) ->
                    Array.to_seqi v
                    |> Seq.flat_map
                         (fun (i, {Stats_record.avg; std; min; max}) ->
                           List.to_seq
                             [
                               (avg, sf "%s_%d_avg" title i);
                               (std, sf "%s_%d_std" title i);
                               (min, sf "%s_%d_min" title i);
                               (max, sf "%s_%d_max" title i);
                             ]))
             |> List.of_seq
           in
           let row =
             [
               ( Option.get parameters.prod_deg |> Interval.mid |> float,
                 "prod_deg" );
               (float cfg.nproducers, "nproducers");
               (float cfg.nattesters, "nattesters");
               (float cfg.nconsumers, "nconsumers");
               (float cfg.min_relayed_shards, "r");
             ]
             @ stats
           in
           let row, header = List.split row in
           let filename = Format.kasprintf dat "bandwidth_%d" pid in
           Dat.(
             make filename (Array.of_list header) [|Array.of_list row|] |> write))
  in
  let slot_confirmed =
    let max_shards_required =
      int_of_float (2048. /. parameters.Cmdline.redundancy)
    in
    Analysis.slot_confirmed
      ~max_shards_required
      ~thresholds:(List.init 9 (fun i -> float (i + 1) /. 10.))
      ~nshards:2048
      cfg
    |> Analysis.postcompose (fun results ->
           let header =
             [|
               "prod_deg";
               "nproducers";
               "nattesters";
               "nconsumers";
               "r";
               "threshold";
               "confirmp";
             |]
           in
           let rows =
             List.map
               (fun (threshold, _, `Confirm_p p) ->
                 [|
                   Option.get parameters.prod_deg |> Interval.mid |> float;
                   float cfg.nproducers;
                   float cfg.nattesters;
                   float cfg.nconsumers;
                   float cfg.min_relayed_shards;
                   threshold;
                   p;
                 |])
               results
             |> Array.of_list
           in
           Dat.(
             make (Format.kasprintf dat "slot_confirmation_%d" pid) header rows
             |> write))
  in
  let analyses =
    List.concat
      [
        (if List.mem "bandwidth" parameters.Cmdline.analysis then
         [estimate_bandwidth]
        else []);
        (if List.mem "consumer" parameters.Cmdline.analysis then
         [consumer_receives_enough_shards]
        else []);
        (if List.mem "shards" parameters.Cmdline.analysis then
         [
           attester_receives_enough_shards (Roles.largest_attester_index cfg);
           attester_receives_enough_shards (Roles.median_attester_index cfg);
           attester_receives_enough_shards (Roles.smallest_attester_index cfg);
         ]
        else []);
        (if List.mem "confirmation" parameters.Cmdline.analysis then
         [slot_confirmed]
        else []);
      ]
  in

  let c = ref 0 in

  let module Tree = Sampler.Network_stats_helpers.Tree in
  Seq.take parameters.Cmdline.ngraphs chain
  |> Seq.iter (fun state ->
         when_verbose (fun () ->
             Format.printf "-------------\n" ;
             Format.printf "error: %f\n" (Sampler.error state) ;
             Format.printf "degree stats:\n" ;
             pp_degree_stats_hum state) ;

         List.iter (fun a -> a.Analysis.pre_shard_iteration state) analyses ;

         for shard = 0 to 2047 do
           let subgraph_predicate v =
             Topic.Set.mem {shard; slot = 0} (topic_function v)
           in
           let spanning_trees =
             Sampler.uniform_spanning_trees
               ~graph:(Sampler.graph state)
               ~source
               ~subgraph_predicate
               rng_state
           in
           let trees =
             Seq.take parameters.Cmdline.nroutings spanning_trees |> List.of_seq
           in
           List.iter
             (fun a -> a.Analysis.sample state ~shard subgraph_predicate trees)
             analyses
         done ;

         List.iter (fun a -> a.Analysis.post_shard_iteration state) analyses ;

         incr c ;
         Format.printf "\r%d/%d%!" !c parameters.Cmdline.ngraphs) ;

  List.iter (fun a -> a.Analysis.post_simulation ()) analyses ;

  close ()
