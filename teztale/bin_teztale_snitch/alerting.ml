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

type vigie = Slack of string * string

(* FIXME: Find a way not to include preview in message *)
let _tzkt network level = sf "https://%s.tzkt.io/%ld" network level

let teztale network level =
  sf
    "https://nomadic-labs.gitlab.io/teztale-dataviz/Level#server=https%%253A%%252F%%252Fteztale-%s.obs.nomadic-labs.cloud%%252F&block=%ld"
    network
    level

(** [slack_blocks ~source ~network ~level alerts]
    Format alerts as javascript object to be sent via slack API.
*)
let slack_blocks ~source ~network ~level alerts =
  let divider = `O [("type", `String "divider")] in
  let block typ txt = `O [("type", `String typ); ("text", txt)] in
  let mrkdwn text = block "mrkdwn" (`String text) in
  let section text = block "section" text in
  let header =
    let src = Option.(map (sf " (%s)") source |> value ~default:"") in
    sf "*%s%s / %ld*" network src level |> mrkdwn |> section
  in
  let report =
    List.map
      (function
        | name, "" -> sf "%s" name | name, value -> sf "%s: %s" name value)
      alerts
    |> String.concat "\n" |> sf "```%s```" |> mrkdwn |> section
  in
  let links = sf "<%s|Teztale>" (teztale network level) |> mrkdwn |> section in
  [divider; header; report; links; divider]

(** [slack token body]
    [body] is a javascript object that contains [channel] and [block] fields
    to be used with the slack API.
*)
let slack token body =
  let open Lwt.Syntax in
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  let headers =
    Cohttp.Header.add_authorization headers (`Other (sf "Bearer %s" token))
  in
  let uri = Uri.of_string "https://slack.com/api/chat.postMessage" in
  let body = `String (Ezjsonm.value_to_string body) in
  let* _res, out = Cohttp_lwt_unix.Client.post ~body ~headers uri in
  let* _ = Cohttp_lwt.Body.to_string out in
  Lwt.return_unit

(** [send_alerts ~source ~network alerting alerts]
    Format human readable alerts and send them,
    or print of stdout if no alerting endpoint is provided.
*)
let send_alerts ~source ~network alerting alerts =
  let alerts =
    List.map
      (fun (level, alerts) -> (level, List.map strings_of_alert alerts))
      alerts
  in
  let alerts = List.sort compare alerts in
  if [] = alerting then
    alerts
    |> List.map (fun (level, alerts) ->
           List.map
             (fun (field, threshold) -> sf "\t%s: %s" field threshold)
             alerts
           |> String.concat "\n" |> sf "%ld:\n%s\n" level)
    |> String.concat "\n"
    |> Lwt_io.printf "----------\n%s----------\n"
  else
    let msg channel =
      List.map
        (fun (level, alerts) -> slack_blocks ~source ~network ~level alerts)
        alerts
      |> List.flatten
      |> fun blocks -> `O [("channel", `String channel); ("blocks", `A blocks)]
    in
    Lwt_list.iter_p
      (fun (Slack (channel, token)) -> slack token (msg channel))
      alerting
