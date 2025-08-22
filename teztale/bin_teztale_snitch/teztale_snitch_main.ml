(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Lwt_syntax
open Data
open Reporting
open Alerting

let now () = Time.System.(now () |> to_protocol)

type ctx = {
  network : string;
  endpoint : string;
  source : string option;
  refresh_rate : int;
  thresholds : thresholds;
  mutable now : Time.Protocol.t;
  mutable latest_report : (int32 * report) option;
  mutable latest_alerts : (int32 * alert list) list;
}

(* Utility function used by the [get_*] functions *)
let get (path : string) encoding (endpoint : string) =
  let endpoint = Uri.of_string endpoint in
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  let uri = Uri.with_path endpoint (Uri.path endpoint ^ path) in
  let* _resp, out = Cohttp_lwt_unix.Client.get ~headers uri in
  let* body = Cohttp_lwt.Body.to_string out in
  Ezjsonm.from_string body |> Data_encoding.Json.destruct encoding |> Lwt.return

let get_available start stop =
  let encoding = Data_encoding.(list (tup3 int32 int32 bool)) in
  get (sf "%ld-%ld/available.json" start stop) encoding

let get_head = get "head.json" head_encoding

let get_data_range start stop =
  get (sf "%ld-%ld.json" start stop) batch_encoding

let gen_report level (block : Block.t) ops =
  let attesting_power =
    List.fold_left
      (fun acc (x : Delegate_operations.t) ->
        Signature.Public_key_hash.Map.add x.delegate x.attesting_power acc)
      Signature.Public_key_hash.Map.empty
      ops
  in
  let delays =
    let preattestation = times Consensus_ops.Preattestation ops block.round in
    let attestation = times Consensus_ops.Attestation ops block.round in
    {attestation; preattestation}
  in
  block_report level block ops delays attesting_power

(** Filter data, compute report (use it to update context) and compute alert for
    this report. *)
let report ctx canonical ({level; data} : batch_item) =
  match
    (* We don't process the HEAD level, so any examinated block should have a
       successor (and then be marked as canonical). *)
    List.find
      (fun (b : Block.t) ->
        List.exists (fun (l, r) -> l = level && r = b.round) canonical)
      data.blocks
  with
  | None -> (ctx, Some (level, [No_data]))
  | Some block ->
      let ops = data.delegate_operations in
      let report = gen_report level (block : Block.t) ops in
      let prev =
        (* It is only relevant to use the previous report if it was the previous level *)
        match ctx.latest_report with
        | Some (latest, prev) when latest = Int32.pred level -> Some prev
        | _ -> None
      in
      let alerts = gen_alerts ?prev report ctx.thresholds in
      let latest_report = Some (level, report) in
      let ctx = {ctx with latest_report} in
      (ctx, if [] = snd alerts then None else Some alerts)

(** Remove all the measures that come from an archiver other than [source] *)
let filter_source source data =
  List.map
    (fun {
           level;
           data = {missing_blocks; delegate_operations; blocks; cycle_info};
         }
       ->
      let missing_blocks =
        missing_blocks
        |> List.filter_map @@ fun b ->
           if List.exists (( = ) source) b.sources then
             Some {b with sources = [source]}
           else None
      in
      let delegate_operations =
        let open Delegate_operations in
        delegate_operations
        |> List.map @@ fun o ->
           let operations =
             o.operations
             |> List.map @@ fun o ->
                let mempool_inclusion =
                  o.mempool_inclusion
                  |> List.filter (fun i -> i.source = source)
                in
                {o with mempool_inclusion}
           in
           {o with operations}
      in
      let blocks =
        let open Block in
        blocks
        |> List.map @@ fun b ->
           let reception_times =
             b.reception_times |> List.filter @@ fun r -> r.source = source
           in
           {b with reception_times}
      in
      let data = {missing_blocks; delegate_operations; blocks; cycle_info} in
      {level; data})
    data

(** Fetch data, filter according to [ctx.source] and generate associated report(s) (using {!val:report}) *)
let fetch_and_report endpoint ctx =
  let* {level = lst} = get_head endpoint in
  let now = now () in
  let ctx = {ctx with now} in
  let lst =
    (* We don't report for HEAD because data are likely to be very incomplete *)
    Int32.pred lst
  in
  let fst =
    (* Some interesting values are computed comparing current level with the previous one.
       If latest report is a level in the past, fetch all of level between latest en present (included).
       Otherwise, fetch present level and the previous one. *)
    match ctx.latest_report with
    | Some (latest, _) when latest < lst -> latest
    | _ -> Int32.pred lst
  in
  let data = get_data_range fst lst endpoint in
  let* available = get_available fst lst endpoint in
  let canonical =
    List.filter_map (fun (l, r, c) -> if c then Some (l, r) else None) available
  in
  let* data =
    match ctx.source with
    | None -> data
    | Some source -> Lwt.map (filter_source source) data
  in
  List.sort (fun (a : batch_item) b -> compare a.level b.level) data
  |> List.fold_left
       (fun (ctx, acc) data ->
         match report ctx canonical data with
         | ctx, None -> (ctx, acc)
         | ctx, Some alerts -> (ctx, alerts :: acc))
       (ctx, [])
  |> Lwt.return

(** Filter out alerts that have already been reported.
    For a given level, if the new alert batch has at least one item not present
    in last sent alerts, the whole batch is kept in order to be sent. *)
let filter_alerts ctx alerts =
  List.filter_map
    (fun (lvl, alerts) ->
      match List.find (fun (lvl', _) -> lvl' = lvl) ctx.latest_alerts with
      | None -> Some (lvl, alerts)
      | Some (_, latest_alerts) ->
          if List.for_all (fun a -> Stdlib.List.mem a latest_alerts) alerts then
            None
          else Some (lvl, alerts))
    alerts

(** Generate reports and raise alerts according to [ctx].
    This function never returns and loops every [ctx.refresh_rate] seconds. *)
let report_loop ~network ~endpoint ~refresh_rate ~(thresholds : thresholds)
    ~alerting ~source : 'a =
  let rec loop ctx0 =
    let* ctx, alerts = fetch_and_report endpoint ctx0 in
    let alerts = List.sort (fun a b -> compare b a) alerts in
    let alerts =
      match (thresholds.timestamp, ctx.latest_report) with
      | Some t, Some (_, {timestamp; level; _}) ->
          let diff = Time.Protocol.diff ctx.now timestamp |> Int64.to_int32 in
          if diff >= Int32.mul 2l t then
            let alert = Latest_canonical_block diff in
            match alerts with
            | (l, a) :: tl when l = level -> (l, alert :: a) :: tl
            | alerts -> (level, [alert]) :: alerts
          else alerts
      | _ -> alerts
    in
    let* () =
      match filter_alerts ctx alerts with
      | [] -> Lwt.return_unit
      | alerts ->
          Alerting.send_alerts
            ~source:ctx.source
            ~network:ctx.network
            alerting
            alerts
    in
    let ctx = {ctx with latest_alerts = alerts} in
    let delay =
      let now = now () in
      Time.Protocol.(diff now ctx.now |> Int64.to_int) + ctx.refresh_rate
    in
    Unix.sleep (max delay 0) ;
    loop ctx
  in
  loop
    {
      network;
      endpoint;
      refresh_rate;
      source;
      now = Time.Protocol.epoch;
      thresholds;
      latest_report = None;
      latest_alerts = [];
    }

let section_thresholds = "THRESHOLDS"

let section_required = "REQUIRED OPTIONS"

let alert_cmd =
  let open Cmdliner in
  let t ?(lower = false) f c =
    let doc =
      "Threshold that will trigger an alert if " ^ f ^ " is "
      ^ (if lower then "lower" else "higher")
      ^ " or equal to it."
    in
    Arg.(
      value
      & opt (some c) None
      & info ["threshold." ^ f] ~doc ~docs:section_thresholds)
  in
  let timestamp = t "timestamp" Arg.int32 in
  let round = t "round" Arg.int32 in
  let validation_delay = t "validation_delay" Arg.float in
  let application_delay = t "application_delay" Arg.float in
  let delay_66 = t "delay_66" Arg.float in
  let delay_90 = t "delay_90" Arg.float in
  let delay_66_pre = t "delay_66_pre" Arg.float in
  let delay_90_pre = t "delay_90_pre" Arg.float in
  let delay_validation_pqc = t "delay_validation_pqc" Arg.float in
  let delay_validation_qc = t "delay_validation_qc" Arg.float in
  let delay_pqc_qc = t "delay_pqc_qc" Arg.float in
  let attestation_rate = t ~lower:true "attestation_rate" Arg.float in
  let pre_attestation_only = t "pre_attestation_only" Arg.int in
  let attestation_only = t "attestation_only" Arg.int in
  let format timestamp round validation_delay application_delay delay_66
      delay_90 delay_66_pre delay_90_pre delay_validation_pqc
      delay_validation_qc delay_pqc_qc attestation_rate pre_attestation_only
      attestation_only =
    {
      level = ();
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
      pre_attestation_only;
      attestation_only;
    }
  in
  let alerts =
    let doc = "A given endpoint to send alerts" in
    let arg =
      Arg.(
        value
        & opt_all (t2 ~sep:' ' string string) []
        & info ["slack"] ~docv:"CHANNEL TOKEN" ~doc)
    in
    Term.(
      const (List.map (fun (channel, token) -> Slack (channel, token))) $ arg)
  in
  let thresholds =
    Term.(
      const format $ timestamp $ round $ validation_delay $ application_delay
      $ delay_66 $ delay_90 $ delay_66_pre $ delay_90_pre $ delay_validation_pqc
      $ delay_validation_qc $ delay_pqc_qc $ attestation_rate
      $ pre_attestation_only $ attestation_only)
  in
  let url =
    let doc = "The teztale server URL" in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"URL" ~doc)
  in
  let refresh_rate =
    let doc =
      "Time in seconds expected by the chain between two consecutive levels"
    in
    Arg.(
      required
      & opt (some int) None
      & info ["delay"] ~docv:"SECONDS" ~doc ~docs:section_required)
  in
  let network =
    let doc = "Name of the network monitored" in
    Arg.(
      required
      & opt (some string) None
      & info ["network"] ~doc ~docs:section_required)
  in
  let source =
    let doc =
      "Name of the source you want to select for analysis. Using multiple may \
       lead to wrong alerts."
    in
    Arg.(value & opt (some string) None & info ["source"] ~doc)
  in
  let action url network refresh_rate thresholds alerting source =
    let endpoint =
      if String.ends_with ~suffix:"/" url then url else url ^ "/"
    in
    Lwt_main.run
      (report_loop
         ~refresh_rate
         ~endpoint
         ~network
         ~thresholds
         ~alerting
         ~source)
    |> exit
  in
  let term =
    Term.(
      const action $ url $ network $ refresh_rate $ thresholds $ alerts $ source)
  in
  let doc =
    "Continuously compute alerts and warn callbacks if at least one threshold \
     is passed. If no callback is defined, print message on stdout."
  in
  let section_synopsis = "SYNOPSIS" in
  let paragraph_synopsis =
    "octez-teztale-snitch alert [--network=NETWORK] [--delay=SECONDS] \
     [THRESHOLD]… [OPTION]… URL"
  in
  let man =
    [
      `S section_synopsis;
      `P paragraph_synopsis;
      `S Manpage.s_arguments;
      `S section_required;
      `P "Required options declared as named options to make the CLI readable";
      `S Manpage.s_options;
      `S section_thresholds;
      `P "Thresholds used to trigger an alert";
    ]
  in
  let info = Cmd.info "alert" ~doc ~man in
  Cmd.v info term

let main =
  let open Cmdliner in
  let version = Tezos_version_value.Bin_version.octez_version_string in
  let doc = "Monitor report chain health using data from teztale server" in
  let section_synopsis = "SYNOPSIS" in
  let paragraph_synopsis = "octez-teztale-snitch [COMMAND]" in
  let man =
    [
      `S section_synopsis;
      `P paragraph_synopsis;
      `S Manpage.s_commands;
      `S Manpage.s_bugs;
      `P "You can open an issue on: https://gitlab.com/tezos/tezos/-/issues";
    ]
  in
  let info = Cmd.info Sys.executable_name ~version ~doc ~man in
  let default = Term.(ret @@ const @@ `Help (`Pager, None)) in
  Cmd.group ~default info [alert_cmd]

let () = exit (Cmdliner.Cmd.eval' main)
