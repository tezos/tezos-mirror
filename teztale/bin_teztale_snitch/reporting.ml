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
        let acc' = acc + p in
        if threshold >= acc && threshold <= acc then Some reception
        else loop acc' tl
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
type ('level, 'round, 'tm, 'delay) report0 = {
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
}

type report = (int32, int32, Time.Protocol.t, float option) report0

type thresholds = (unit, int32 option, int32 option, float option) report0

(** [block_report level block distribution attestingPower]
    Compute report for a given block
*)
let block_report (level : int32) (block : Block.t)
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
  }
