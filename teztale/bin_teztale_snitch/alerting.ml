(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let sf = Format.asprintf

(** [Some hd @? tl] is [hd::tl], [None @? tl] is [tl] *)
let ( @? ) hd tl = match hd with Some hd -> hd :: tl | None -> tl

type alert =
  | Latest_canonical_block of int32
  | Timestamp of int32
  | Round of int32
  | Validation_delay of float
  | Application_delay of float
  | Delay_66 of float
  | Delay_90 of float
  | Delay_66_pre of float
  | Delay_90_pre of float
  | Delay_validation_pqc of float
  | Delay_validation_qc of float
  | Delay_pqc_qc of float
  | Attestation_rate of float
  | Pre_attestation_only of int
  | Attestation_only of int
  | No_data

let strings_of_alert =
  let msg fmt name v = (name, fmt v) in
  let int32 = msg Int32.to_string in
  let float = msg string_of_float in
  let int = msg string_of_int in
  function
  | Latest_canonical_block t -> int32 "Last canonical block" t
  | Timestamp t -> int32 "Time between blocks" t
  | Round t -> int32 "Round number" t
  | Validation_delay t -> float "Validation delay" t
  | Application_delay t -> float "Application delay" t
  | Delay_66 t -> float "Delay until 66% attestations reached" t
  | Delay_90 t -> float "Delay until 90% attestations reached" t
  | Delay_66_pre t -> float "Delay until 66% pre-attestations reached" t
  | Delay_90_pre t -> float "Delay until 90% pre-attestations reached" t
  | Delay_validation_pqc t -> float "Delay between validation and PQC" t
  | Delay_validation_qc t -> float "Delay between validation and QC" t
  | Delay_pqc_qc t -> float "Delay between PQC and QC" t
  | Attestation_rate t -> float "Attestation rate" t
  | Pre_attestation_only t ->
      int "Number of delegates that pre-attested but didn't attest" t
  | Attestation_only t ->
      int "Number of delegates that attested but didn't pre-attest" t
  | No_data -> ("No data available in teztale", "")

(** [gen_alerts ?prev r t]
    Produce alerts from a report and some thresholds.
    [?prev] is the report from previous level,
    used for computing the delay between two consecutive blocks.
*)
let gen_alerts ?prev r t =
  let open Reporting in
  let alert ?(op = ( >= )) fmt v t : alert option =
    Option.bind t @@ fun t ->
    match v with Some v when op v t -> Some (fmt v) | _ -> None
  in
  let timestamp =
    Option.map
      (fun prev ->
        Time.Protocol.diff r.timestamp prev.timestamp |> Int64.to_int32)
      prev
  in
  ( r.level,
    alert (fun x -> Timestamp x) timestamp t.timestamp
    @? alert (fun x -> Round x) (r.round |> Option.some) t.round
    @? alert (fun x -> Validation_delay x) r.validation_delay t.validation_delay
    @? alert
         (fun x -> Application_delay x)
         r.application_delay
         t.application_delay
    @? alert (fun x -> Delay_66 x) r.delay_66 t.delay_66
    @? alert (fun x -> Delay_90 x) r.delay_90 t.delay_90
    @? alert (fun x -> Delay_66_pre x) r.delay_66_pre t.delay_66_pre
    @? alert (fun x -> Delay_90_pre x) r.delay_90_pre t.delay_90_pre
    @? alert
         (fun x -> Delay_validation_pqc x)
         r.delay_validation_pqc
         t.delay_validation_pqc
    @? alert
         (fun x -> Delay_validation_qc x)
         r.delay_validation_qc
         t.delay_validation_qc
    @? alert (fun x -> Delay_pqc_qc x) r.delay_pqc_qc t.delay_pqc_qc
    @? alert
         ~op:( <= )
         (fun x -> Attestation_rate x)
         r.attestation_rate
         t.attestation_rate
    @? alert
         (fun x -> Pre_attestation_only x)
         r.pre_attestation_only
         t.pre_attestation_only
    @? alert (fun x -> Attestation_only x) r.attestation_only t.attestation_only
    @? [] )

(** [print_alerts alerts]
    Format human readable alerts print them on stdout. *)
let print_alerts alerts =
  let alerts =
    List.map
      (fun (level, alerts) -> (level, List.map strings_of_alert alerts))
      alerts
  in
  let alerts = List.sort compare alerts in
  alerts
  |> List.map (fun (level, alerts) ->
         List.map
           (fun (field, threshold) -> sf "\t%s: %s" field threshold)
           alerts
         |> String.concat "\n" |> sf "%ld:\n%s\n" level)
  |> String.concat "\n"
  |> Lwt_io.printf "----------\n%s----------\n"
