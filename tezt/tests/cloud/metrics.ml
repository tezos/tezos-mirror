(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type public_key_hash = PKH of string

type commitment_info = {commitment : string; publisher_pkh : string}

type dal_status =
  | With_DAL of Z.t
  | Without_DAL
  | Out_of_committee
  | Expected_to_DAL_attest

type per_level_info = {
  level : int;
  published_commitments : (int, commitment_info) Hashtbl.t;
  baker_dal_statuses : (public_key_hash, dal_status) Hashtbl.t;
  attested_commitments : Z.t;
  etherlink_operator_balance_sum : Tez.t;
  echo_rollup_fetched_data : (int, int) Hashtbl.t;
}

type t = {
  level_first_commitment_published : int option;
  level_first_commitment_attested : int option;
  total_published_commitments : int;
  total_published_commitments_per_slot : (int, int) Hashtbl.t;
  (* A hash table mapping slot indices to their total number of
     published commitments. *)
  expected_published_commitments : int;
  total_attested_commitments : int;
  total_attested_commitments_per_slot : (int, int) Hashtbl.t;
  (* A hash table mapping slot indices to their total number of
     attested commitments. *)
  ratio_published_commitments : float;
  ratio_attested_commitments : float;
  ratio_published_commitments_last_level : float;
  ratio_attested_commitments_per_baker :
    (public_key_hash, Baker_helpers.per_baker_dal_summary) Hashtbl.t;
  etherlink_operator_balance_sum : Tez.t;
  total_echo_rollup_unattested_slots : int;
  total_echo_rollup_fetched_data_size : int;
  last_echo_rollup_fetched_data_size : int;
}

let default =
  {
    level_first_commitment_published = None;
    level_first_commitment_attested = None;
    total_published_commitments = 0;
    total_published_commitments_per_slot = Hashtbl.create 32;
    expected_published_commitments = 0;
    total_attested_commitments = 0;
    total_attested_commitments_per_slot = Hashtbl.create 32;
    ratio_published_commitments = 0.;
    ratio_attested_commitments = 0.;
    ratio_published_commitments_last_level = 0.;
    ratio_attested_commitments_per_baker = Hashtbl.create 0;
    etherlink_operator_balance_sum = Tez.zero;
    total_echo_rollup_unattested_slots = 0;
    total_echo_rollup_fetched_data_size = 0;
    last_echo_rollup_fetched_data_size = 0;
  }

let aliases =
  Hashtbl.create
    50 (* mapping from baker addresses to their Tzkt aliases (if known)*)

let merge_aliases =
  Option.iter (fun new_aliases ->
      Hashtbl.iter
        (fun key alias -> Hashtbl.replace aliases key alias)
        new_aliases)

let pp_slot_metrics fmt xs =
  let open Format in
  fprintf
    fmt
    "[ %a ]"
    (pp_print_list
       (fun fmt (x, y) -> fprintf fmt "(%d -> %d)" x y)
       ~pp_sep:(fun fmt () -> fprintf fmt "; "))
    (List.of_seq xs
    |> List.filter (fun (_, n) -> n > 0)
    (* Sorting the list per slot index increasing order. *)
    |> List.sort (fun (idx1, _) (idx2, _) -> Int.compare idx1 idx2))

let pp ~bakers
    {
      level_first_commitment_published;
      level_first_commitment_attested;
      total_published_commitments;
      total_published_commitments_per_slot;
      expected_published_commitments;
      total_attested_commitments;
      total_attested_commitments_per_slot;
      ratio_published_commitments;
      ratio_attested_commitments;
      ratio_published_commitments_last_level;
      ratio_attested_commitments_per_baker;
      etherlink_operator_balance_sum;
      total_echo_rollup_unattested_slots;
      total_echo_rollup_fetched_data_size;
      last_echo_rollup_fetched_data_size;
    } =
  let pp_ratio fmt (num, div) =
    if div = 0 then Format.fprintf fmt "Not a number: %d/0" num
    else Format.fprintf fmt "%.2f" (float_of_int num *. 100. /. float_of_int div)
  in
  (match level_first_commitment_published with
  | None -> ()
  | Some level_first_commitment_published ->
      Log.info
        "First commitment published level: %d"
        level_first_commitment_published) ;
  (match level_first_commitment_attested with
  | None -> ()
  | Some level_first_commitment_attested ->
      Log.info
        "First commitment attested level: %d"
        level_first_commitment_attested) ;
  Log.info "Total published commitments: %d" total_published_commitments ;
  Log.info "Expected published commitments: %d" expected_published_commitments ;
  Log.info "Total attested commitments: %d" total_attested_commitments ;
  Log.info "Ratio published commitments: %f" ratio_published_commitments ;
  Log.info "Ratio attested commitments: %f" ratio_attested_commitments ;
  Log.info
    "Ratio published commitments last level: %f"
    ratio_published_commitments_last_level ;
  List.iter
    (fun Baker_helpers.{accounts; stake; baker; _} ->
      let baker_name = Agnostic_baker.name baker in
      List.iter
        (fun account ->
          let pkh = account.Baker_helpers.delegate.public_key_hash in
          match
            Hashtbl.find_opt ratio_attested_commitments_per_baker (PKH pkh)
          with
          | None -> Log.info "We lack information about %s" pkh
          | Some {attestable_slots; attested_slots; _} ->
              let alias =
                Hashtbl.find_opt aliases account.delegate.public_key_hash
                |> Option.value ~default:account.delegate.public_key_hash
              in
              Log.info
                "%s: Ratio for %s (with stake %d): %a"
                baker_name
                alias
                stake
                pp_ratio
                (attested_slots, attestable_slots))
        accounts)
    bakers ;
  Log.info
    "Sum of balances of the Etherlink operator: %s tez"
    (Tez.to_string etherlink_operator_balance_sum) ;
  Log.info
    "DAL slots: total published commitments per slot (<slot index> -> \
     <published commit.>).@.%a"
    pp_slot_metrics
    (Hashtbl.to_seq total_published_commitments_per_slot) ;
  Log.info
    "DAL slots: total attested commitments per slot (<slot index> -> <attested \
     commit.>).@.%a"
    pp_slot_metrics
    (Hashtbl.to_seq total_attested_commitments_per_slot) ;
  Log.info
    "Echo rollup slots: total unattested slots: %d"
    total_echo_rollup_unattested_slots ;
  Log.info
    "Echo rollup slots: total fetched data: %d"
    total_echo_rollup_fetched_data_size ;
  Log.info
    "Echo rollup slots: last fetched data: %d"
    last_echo_rollup_fetched_data_size

let push ~versions ~cloud
    {
      level_first_commitment_published = _;
      level_first_commitment_attested = _;
      total_published_commitments;
      total_published_commitments_per_slot;
      expected_published_commitments;
      total_attested_commitments;
      total_attested_commitments_per_slot;
      ratio_published_commitments;
      ratio_attested_commitments;
      ratio_published_commitments_last_level;
      ratio_attested_commitments_per_baker;
      etherlink_operator_balance_sum;
      total_echo_rollup_unattested_slots;
      total_echo_rollup_fetched_data_size;
      last_echo_rollup_fetched_data_size;
    } =
  let get_labels public_key_hash =
    let alias =
      Hashtbl.find_opt aliases public_key_hash
      |> Option.map (fun alias -> [("alias", alias)])
      |> Option.value ~default:[]
    in
    let version =
      Hashtbl.find_opt versions public_key_hash
      |> Option.map (fun version -> [("version", version)])
      |> Option.value ~default:[]
    in
    [("attester", public_key_hash)] @ alias @ version
  in
  let push_attested ~labels value =
    Cloud.push_metric
      cloud
      ~help:"Number of attested commitments per baker"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_dal_commitments_attested"
      (float_of_int value)
  in
  let push_attestable ~labels value =
    Cloud.push_metric
      cloud
      ~help:
        "Number of attestable commitments per baker (ie published when the \
         baker is in the DAL committee at attestation level)"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_dal_commitments_attestable"
      (float_of_int value)
  in
  let push_dal_attestation_sent ~labels = function
    | None -> ()
    | Some value ->
        Cloud.push_metric
          cloud
          ~help:
            "Did the baker sent a DAL attestation when they had the \
             opportunity to"
          ~typ:`Gauge
          ~labels
          ~name:"tezt_dal_attestation_sent"
          (if value then 1. else 0.)
  in
  let push_metric_out_attestation_sent ~labels () =
    Cloud.push_metric
      cloud
      ~help:"The baker sent an attestation while out of the DAL committee"
      ~typ:`Gauge
      ~labels
      ~name:"tezt_attestation_sent_when_out_of_dal_committee"
      1.
  in
  Hashtbl.iter
    (fun (PKH public_key_hash)
         Baker_helpers.
           {
             attested_slots;
             attestable_slots;
             in_committee;
             attestation_with_dal;
           }
       ->
      if in_committee then (
        let labels = get_labels public_key_hash in
        push_attested ~labels attested_slots ;
        push_attestable ~labels attestable_slots ;
        push_dal_attestation_sent ~labels attestation_with_dal)
      else
        let labels = get_labels public_key_hash in
        push_metric_out_attestation_sent ~labels ())
    ratio_attested_commitments_per_baker ;
  Hashtbl.iter
    (fun slot_index value ->
      let labels = [("slot_index", string_of_int slot_index)] in
      Cloud.push_metric
        cloud
        ~help:"Total published commitments per slot"
        ~typ:`Counter
        ~labels
        ~name:"tezt_total_published_commitments_per_slot"
        (float value))
    total_published_commitments_per_slot ;
  Hashtbl.iter
    (fun slot_index value ->
      let labels = [("slot_index", string_of_int slot_index)] in
      Cloud.push_metric
        cloud
        ~help:"Total attested commitments per slot"
        ~typ:`Counter
        ~labels
        ~name:"tezt_total_attested_commitments_per_slot"
        (float value))
    total_attested_commitments_per_slot ;
  Cloud.push_metric
    cloud
    ~help:"Ratio between the number of published and expected commitments"
    ~typ:`Gauge
    ~name:"tezt_dal_commitments_ratio"
    ~labels:[("kind", "published")]
    ratio_published_commitments ;
  Cloud.push_metric
    cloud
    ~help:"Ratio between the number of attested and expected commitments"
    ~typ:`Gauge
    ~name:"tezt_dal_commitments_ratio"
    ~labels:[("kind", "attested")]
    ratio_attested_commitments ;
  Cloud.push_metric
    cloud
    ~help:
      "Ratio between the number of attested and expected commitments per level"
    ~typ:`Gauge
    ~name:"tezt_dal_commitments_ratio"
    ~labels:[("kind", "published_last_level")]
    ratio_published_commitments_last_level ;
  Cloud.push_metric
    cloud
    ~help:"Number of commitments expected to be published"
    ~typ:`Counter
    ~name:"tezt_dal_commitments_total"
    ~labels:[("kind", "expected")]
    (float_of_int expected_published_commitments) ;
  Cloud.push_metric
    cloud
    ~help:"Number of published commitments "
    ~typ:`Counter
    ~name:"tezt_dal_commitments_total"
    ~labels:[("kind", "published")]
    (float_of_int total_published_commitments) ;
  Cloud.push_metric
    cloud
    ~help:"Number of attested commitments"
    ~typ:`Counter
    ~name:"tezt_dal_commitments_total"
    ~labels:[("kind", "attested")]
    (float_of_int total_attested_commitments) ;
  Cloud.push_metric
    cloud
    ~help:"Sum of the balances of the etherlink operator"
    ~typ:`Gauge
    ~name:"tezt_etherlink_operator_balance_total"
    (Tez.to_float etherlink_operator_balance_sum) ;
  Cloud.push_metric
    cloud
    ~help:"Number of slots unattested from the echo rollup perspective"
    ~typ:`Gauge
    ~labels:[("kind", "unattested")]
    ~name:"tezt_total_echo_rollup_unattested_slots"
    (float_of_int total_echo_rollup_unattested_slots) ;
  Cloud.push_metric
    cloud
    ~help:"Total size of data fetched by the echo rollup"
    ~typ:`Gauge
    ~labels:[("kind", "data")]
    ~name:"tezt_total_echo_rollup_fetched_data"
    (float_of_int total_echo_rollup_fetched_data_size) ;
  Cloud.push_metric
    cloud
    ~help:"Last size of data fetched by the echo rollup"
    ~typ:`Gauge
    ~labels:[("kind", "data")]
    ~name:"tezt_last_echo_rollup_fetched_data"
    (float_of_int last_echo_rollup_fetched_data_size)

let published_level_of_attested_level ~attestation_lag level =
  level - attestation_lag

let update_level_first_commitment_published per_level_info metrics =
  match metrics.level_first_commitment_published with
  | None ->
      if Hashtbl.length per_level_info.published_commitments > 0 then
        Some per_level_info.level
      else None
  | Some l -> Some l

let update_level_first_commitment_attested ~first_level ~attestation_lag
    per_level_info metrics =
  match metrics.level_first_commitment_attested with
  | None ->
      if
        Z.popcount per_level_info.attested_commitments > 0
        && per_level_info.level >= first_level + attestation_lag
      then Some per_level_info.level
      else None
  | Some l -> Some l

let update_total_published_commitments per_level_info metrics =
  metrics.total_published_commitments
  + Hashtbl.length per_level_info.published_commitments

let update_expected_published_commitments ~dal_node_producers ~number_of_slots
    metrics =
  match metrics.level_first_commitment_published with
  | None -> 0
  | Some _ ->
      (* -1 since we are looking at level n operation submitted at the previous
         level. *)
      let producers = min (List.length dal_node_producers) number_of_slots in
      metrics.expected_published_commitments + producers

let update_total_attested_commitments per_level_info metrics =
  metrics.total_attested_commitments
  + Z.popcount per_level_info.attested_commitments

let update_ratio_published_commitments metrics =
  if metrics.expected_published_commitments = 0 then 0.
  else
    float_of_int metrics.total_published_commitments
    *. 100.
    /. float_of_int metrics.expected_published_commitments

let update_ratio_published_commitments_last_level ~dal_node_producers
    ~number_of_slots per_level_info metrics =
  match metrics.level_first_commitment_published with
  | None -> 0.
  | Some _ ->
      let producers = min (List.length dal_node_producers) number_of_slots in
      if producers = 0 then 100.
      else
        float_of_int (Hashtbl.length per_level_info.published_commitments)
        *. 100. /. float_of_int producers

let update_ratio_attested_commitments ~first_level ~infos ~attestation_lag
    per_level_info metrics =
  let published_level =
    published_level_of_attested_level ~attestation_lag per_level_info.level
  in
  if published_level <= first_level then (
    Log.warn
      "Unable to retrieve information for published level %d because it \
       precedes the earliest available level (%d)."
      published_level
      first_level ;
    metrics.ratio_attested_commitments)
  else
    match Hashtbl.find_opt infos published_level with
    | None ->
        Log.warn
          "Unexpected error: The level %d is missing in the infos table"
          published_level ;
        metrics.ratio_attested_commitments
    | Some old_per_level_info ->
        let n = Hashtbl.length old_per_level_info.published_commitments in
        if n = 0 then metrics.ratio_attested_commitments
        else
          float (Z.popcount per_level_info.attested_commitments)
          *. 100. /. float n

let update_published_and_attested_commitments_per_slot ~first_level ~infos
    ~number_of_slots ~attestation_lag per_level_info
    total_published_commitments_per_slot total_attested_commitments_per_slot =
  let published_level =
    published_level_of_attested_level ~attestation_lag per_level_info.level
  in
  if published_level <= first_level then (
    Log.warn
      "Unable to retrieve information for published level %d because it \
       precedes the earliest available level (%d)."
      published_level
      first_level ;
    (total_published_commitments_per_slot, total_attested_commitments_per_slot))
  else
    match Hashtbl.find_opt infos published_level with
    | None ->
        Log.warn
          "Unexpected error: The level %d is missing in the infos table"
          published_level ;
        ( total_published_commitments_per_slot,
          total_attested_commitments_per_slot )
    | Some old_per_level_info ->
        let published_commitments = old_per_level_info.published_commitments in
        for slot_index = 0 to pred number_of_slots do
          let is_published = Hashtbl.mem published_commitments slot_index in
          let total_published_commitments =
            Option.value
              ~default:0
              (Hashtbl.find_opt total_published_commitments_per_slot slot_index)
          in
          let new_total_published_commitments =
            if is_published then succ total_published_commitments
            else total_published_commitments
          in
          Hashtbl.replace
            total_published_commitments_per_slot
            slot_index
            new_total_published_commitments ;
          (* per_level_info.attested_commitments is a binary
             sequence of length parameters.number_of_slots
             (e.g. '00111111001110100010101011100101').
             For each index i:
             - 1 indicates the slot has been attested
             - 0 indicates the slot has not been attested. *)
          let is_attested =
            Z.testbit per_level_info.attested_commitments slot_index
          in
          let total_attested_commitments =
            Option.value
              ~default:0
              (Hashtbl.find_opt total_attested_commitments_per_slot slot_index)
          in
          let new_total_attested_commitments =
            if is_attested then succ total_attested_commitments
            else total_attested_commitments
          in
          Hashtbl.replace
            total_attested_commitments_per_slot
            slot_index
            new_total_attested_commitments
        done ;
        ( total_published_commitments_per_slot,
          total_attested_commitments_per_slot )

let update_ratio_attested_commitments_per_baker ~first_level ~infos
    ~attestation_lag per_level_info =
  let default () = Hashtbl.create 0 in
  let published_level =
    published_level_of_attested_level ~attestation_lag per_level_info.level
  in
  if published_level <= first_level then (
    Log.warn
      "Unable to retrieve information for published level %d because it \
       precedes the earliest available level (%d)."
      published_level
      first_level ;
    default ())
  else
    match Hashtbl.find_opt infos published_level with
    | None ->
        Log.warn
          "Unexpected error: The level %d is missing in the infos table"
          published_level ;
        default ()
    | Some published_level_info ->
        (* Retrieves the number of published commitments *)
        let attestable_slots =
          Hashtbl.length published_level_info.published_commitments
        in
        let table =
          Hashtbl.(create (length per_level_info.baker_dal_statuses))
        in
        Hashtbl.to_seq per_level_info.baker_dal_statuses
        |> Seq.map (fun (public_key_hash, status) ->
               ( public_key_hash,
                 match status with
                 (* The baker is in the DAL committee and sent an attestation_with_dal. *)
                 | With_DAL attestation_bitset ->
                     Baker_helpers.
                       {
                         attestable_slots;
                         attested_slots = Z.popcount attestation_bitset;
                         in_committee = true;
                         attestation_with_dal = Some true;
                       }
                 (* The baker is out of the DAL committee and sent an attestation. *)
                 | Out_of_committee ->
                     {
                       attestable_slots;
                       attested_slots = 0;
                       in_committee = false;
                       attestation_with_dal = None;
                     }
                 (* The baker is in the DAL committee but sent an attestation without DAL. *)
                 | Without_DAL ->
                     {
                       attestable_slots;
                       attested_slots = 0;
                       in_committee = true;
                       attestation_with_dal = Some false;
                     }
                 (* The baker is in the DAL committee but sent no attestations. *)
                 | Expected_to_DAL_attest ->
                     {
                       attestable_slots;
                       attested_slots = 0;
                       in_committee = true;
                       attestation_with_dal = None;
                     } ))
        |> Hashtbl.add_seq table ;
        table

let update_echo_rollup_metrics infos_per_level metrics =
  let data_size, unattested_slots =
    Hashtbl.fold
      (fun _ v (total_size, unattested) ->
        (total_size + v, if v = 0 then unattested + 1 else unattested))
      infos_per_level.echo_rollup_fetched_data
      (0, 0)
  in
  ( metrics.total_echo_rollup_unattested_slots + unattested_slots,
    metrics.total_echo_rollup_fetched_data_size + data_size,
    data_size )

let get ~first_level ~attestation_lag ~dal_node_producers ~number_of_slots
    ~infos infos_per_level metrics =
  let level_first_commitment_published =
    update_level_first_commitment_published infos_per_level metrics
  in
  let level_first_commitment_attested =
    update_level_first_commitment_attested
      ~first_level
      ~attestation_lag
      infos_per_level
      metrics
  in
  (* Metrics below depends on the new value for the metrics above. *)
  let metrics =
    {
      metrics with
      level_first_commitment_attested;
      level_first_commitment_published;
    }
  in
  let total_published_commitments =
    update_total_published_commitments infos_per_level metrics
  in
  let expected_published_commitments =
    update_expected_published_commitments
      ~dal_node_producers
      ~number_of_slots
      metrics
  in
  let ratio_published_commitments_last_level =
    update_ratio_published_commitments_last_level
      ~dal_node_producers
      ~number_of_slots
      infos_per_level
      metrics
  in
  let total_attested_commitments =
    update_total_attested_commitments infos_per_level metrics
  in
  (* Metrics below depends on the new value for the metrics above. *)
  let metrics =
    {
      metrics with
      level_first_commitment_attested;
      level_first_commitment_published;
      total_published_commitments;
      expected_published_commitments;
      total_attested_commitments;
      ratio_published_commitments_last_level;
    }
  in
  let ratio_published_commitments =
    update_ratio_published_commitments metrics
  in
  let ratio_attested_commitments =
    update_ratio_attested_commitments
      ~first_level
      ~infos
      ~attestation_lag
      infos_per_level
      metrics
  in
  let ratio_attested_commitments_per_baker =
    update_ratio_attested_commitments_per_baker
      ~first_level
      ~infos
      ~attestation_lag
      infos_per_level
  in
  let total_published_commitments_per_slot, total_attested_commitments_per_slot
      =
    update_published_and_attested_commitments_per_slot
      ~first_level
      ~infos
      ~number_of_slots
      ~attestation_lag
      infos_per_level
      metrics.total_published_commitments_per_slot
      metrics.total_attested_commitments_per_slot
  in
  let ( total_echo_rollup_unattested_slots,
        total_echo_rollup_fetched_data_size,
        last_echo_rollup_fetched_data_size ) =
    update_echo_rollup_metrics infos_per_level metrics
  in
  {
    level_first_commitment_published;
    level_first_commitment_attested;
    total_published_commitments;
    total_published_commitments_per_slot;
    expected_published_commitments;
    total_attested_commitments;
    total_attested_commitments_per_slot;
    ratio_published_commitments;
    ratio_attested_commitments;
    ratio_published_commitments_last_level;
    ratio_attested_commitments_per_baker;
    etherlink_operator_balance_sum =
      infos_per_level.etherlink_operator_balance_sum;
    total_echo_rollup_unattested_slots;
    total_echo_rollup_fetched_data_size;
    last_echo_rollup_fetched_data_size;
  }
