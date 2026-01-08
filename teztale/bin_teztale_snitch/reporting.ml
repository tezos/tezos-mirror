(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Data

(** [reached_at threshold timestamps power]

    @param threshold percentage of the total endorsing power to reach
    @param timestamps Public key hash (PKH) associated to the time node received their operation
    @param power PKH associated with the number of slot granted

    @return the time when the threshold as been reached.
*)
let reached_at (threshold : float)
    (reception : Ptime.t Signature.Public_key_hash.Map.t)
    (attestingPower : int Signature.Public_key_hash.Map.t) =
  let delegates =
    Signature.Public_key_hash.Map.bindings reception
    |> List.sort (fun (_, r1) (_, r2) -> Ptime.compare r1 r2)
  in
  let numberOfSlots =
    Signature.Public_key_hash.Map.fold (fun _ -> ( + )) attestingPower 0
  in
  let threshold =
    Float.to_int (ceil (float_of_int numberOfSlots *. threshold))
  in
  let rec loop acc = function
    | [] -> None
    | (delegate, reception) :: tl ->
        let p =
          Signature.Public_key_hash.Map.find delegate attestingPower
          |> Option.value ~default:0
        in
        let acc = acc + p in
        if acc >= threshold then Some reception else loop acc tl
  in
  loop 0 delegates

type 'a partitioned = {preattestation : 'a; attestation : 'a}

(** [times kind ops round]

    @param kind either [Attestation] or [Preattestation]
    @param ops list of received operations used to look for timestamp
    @param round the round concerned

    Associate the reception time of an operation to the corresponding delegate.
    If multiple sources are available, the minimum time is selected.

    Note that it is likely to lead to false results unless you filter out
    [ops] according to their [source] because it mixes times from
    different teztale archivers.

    Let's imagine the worst kind of error that can be produced:

    Archiver A1 receives attestation from delegate D1 at T1
    Archiver A2 receives attestation from delegate D2 at T2

    The times reported by this function, using these data, would be
    [ (D1, T1) ; (D2, T2) ] that could lead to a consensus reached at T2
    whereas in reality, no node has received both attestation,
    so none reached quorum.

    Filter your input data if you want to avoid this.
*)
let times (kind : Consensus_ops.operation_kind)
    (ops : Delegate_operations.t list) (round : int32) :
    Ptime.t Signature.Public_key_hash.Map.t =
  List.fold_left
    (fun acc (o : Delegate_operations.t) ->
      let time =
        List.find_map
          (fun (o : Delegate_operations.operation) ->
            if o.kind <> kind || Option.value ~default:0l o.round <> round then
              None
            else
              match o.mempool_inclusion with
              | [] -> None
              | hd :: tl ->
                  List.fold_left
                    (fun m (x : Delegate_operations.reception) ->
                      Time.System.min m x.reception_time)
                    hd.reception_time
                    tl
                  |> Option.some)
          o.operations
      in
      match time with
      | None -> acc
      | Some t -> Signature.Public_key_hash.Map.add o.delegate t acc)
    Signature.Public_key_hash.Map.empty
    ops

(** Generic type, used by both report creation and theshold definition *)
type ('level, 'round, 'tm, 'delay, 'rate, 'int) report0 = {
  level : 'level;
  timestamp : 'tm;
  round : 'round;
  validation_delay : 'delay;
  application_delay : 'delay;
  delay_66 : 'delay;
  delay_90 : 'delay;
  delay_66_pre : 'delay;
  delay_90_pre : 'delay;
  delay_validation_pqc : 'delay;
  delay_validation_qc : 'delay;
  delay_pqc_qc : 'delay;
  attestation_rate : 'rate;
  pre_attestation_only : 'int;
  attestation_only : 'int;
}

type report =
  ( int32,
    int32,
    Time.Protocol.t,
    float option,
    float option,
    int option )
  report0

type thresholds =
  ( unit,
    int32 option,
    int32 option,
    float option,
    float option,
    int option )
  report0

(** [filter_ops ~round ops] filters out all operation that are not concerning round [round] *)
let filter_ops :
    round:int32 -> Delegate_operations.t list -> Delegate_operations.t list =
 fun ~round ops ->
  List.filter_map
    (fun (e : Delegate_operations.t) ->
      let operations =
        List.filter
          (fun (o : Delegate_operations.operation) ->
            Option.value ~default:0l o.round = round)
          e.operations
      in
      match operations with
      | [] -> None
      | operations -> Some {e with operations})
    ops

(** [(pre_attestation_only, attestation_only, included) = ops_consistency ops]
    [pre_attestation_only]: number of delegates that pre-attested but did not attest
    [attestation_only]: number of delegates that attested but did not pre-attest
    [included]: number of attested slots fullfiled that was included in the next block
*)
let ops_consistency ops =
  let open Delegate_operations in
  List.fold_left
    (fun (pa, a, r)
         ({operations; attesting_power; _} : Delegate_operations.t)
       ->
      let r =
        if
          List.exists
            (fun o ->
              o.kind = Consensus_ops.Attestation && o.block_inclusion <> [])
            operations
        then r + attesting_power
        else r
      in
      let pa, a =
        let exists k =
          List.exists
            (fun (oo : Delegate_operations.operation) -> oo.kind = k)
            operations
        in
        match
          (exists Consensus_ops.Preattestation, exists Consensus_ops.Attestation)
        with
        | true, false -> (pa + 1, a)
        | false, true -> (pa, a + 1)
        | _ -> (pa, a)
      in
      (pa, a, r))
    (0, 0, 0)
    ops

(** [block_report level block ops distribution attestingPower]
    Compute report for a given block
*)
let block_report (level : int32) (block : Block.t)
    (ops : Delegate_operations.t list)
    (distribution : Ptime.t Signature.Public_key_hash.Map.t partitioned)
    (attestingPower : int Signature.Public_key_hash.Map.t) : report =
  let round = block.round in
  let open Data in
  let timestamp = block.timestamp in
  let timestamp_f = Int64.to_float (Time.Protocol.to_seconds block.timestamp) in
  let min getter =
    match List.filter_map getter block.reception_times with
    | [] -> None
    | hd :: tl ->
        List.fold_left Time.System.min hd tl |> Ptime.to_float_s |> Option.some
  in
  let validation_time = min (fun r -> r.Block.validation_time) in
  let application_time = min (fun r -> r.Block.application_time) in
  let delay_with timestamp = Option.map (fun t -> Float.sub t timestamp) in
  let validation_delay = delay_with timestamp_f validation_time in
  let application_delay =
    Option.bind validation_time (fun t -> delay_with t application_time)
  in
  let timeToReach threshold reception =
    Option.map
      (fun t -> Ptime.to_float_s t -. timestamp_f)
      (reached_at threshold reception attestingPower)
  in
  let delay_66 = timeToReach 0.66 distribution.attestation in
  let delay_90 = timeToReach 0.90 distribution.attestation in
  let delay_66_pre = timeToReach 0.66 distribution.preattestation in
  let delay_90_pre = timeToReach 0.90 distribution.preattestation in
  let delay_validation_pqc =
    Option.bind validation_delay (fun t -> delay_with t delay_66_pre)
  in
  let delay_validation_qc =
    Option.bind validation_delay (fun t -> delay_with t delay_66)
  in
  let delay_pqc_qc =
    Option.bind delay_66_pre (fun t -> delay_with t delay_66)
  in
  let ops = filter_ops ~round ops in
  let pre_attestation_only, attestation_only, attestation_rate0 =
    ops_consistency ops
  in
  let attestation_rate =
    match
      Signature.Public_key_hash.Map.fold (fun _ -> ( + )) attestingPower 0
    with
    | 0 -> None
    | total_attestingPower ->
        Some
          (float_of_int attestation_rate0 /. float_of_int total_attestingPower)
  in
  {
    level;
    timestamp;
    round;
    validation_delay;
    application_delay;
    delay_66;
    delay_90;
    delay_66_pre;
    delay_90_pre;
    delay_validation_pqc;
    delay_validation_qc;
    delay_pqc_qc;
    attestation_rate;
    pre_attestation_only =
      (if pre_attestation_only = 0 then None else Some pre_attestation_only);
    attestation_only =
      (if attestation_only = 0 then None else Some attestation_only);
  }
