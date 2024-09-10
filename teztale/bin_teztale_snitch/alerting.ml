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
  | No_data

let strings_of_alert =
  let msg fmt name v = (name, fmt v) in
  let int32 = msg Int32.to_string in
  function
  | Latest_canonical_block t -> int32 "Last canonical block" t
  | Timestamp t -> int32 "Time between blocks" t
  | Round t -> int32 "Round number" t
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
