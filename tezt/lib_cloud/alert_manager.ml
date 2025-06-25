(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)
let section = "Alert_manager"

type receiver =
  | Slack of {
      name : string;
      channel : string;
          (** Slack channel to send notifications to, use '#' for public channel. *)
      api_url : string;  (** Slack notification configuration. *)
      bot_token : string option;
      text : string option; (* Template of message title shown in slack *)
      title : string option; (* Template of message body shown in slack *)
    }
  | Null

type route = {
  group_wait : Duration.t option;
      (** How long to wait before sending a notification about new
          alerts. If omitted, child routes inherit the group_wait of
          the parent route. *)
  group_interval : Duration.t option;
      (** How long to wait before sending notification about new
          alerts for a group. If omitted, child routes inherit the
          group_interval of the parent route. *)
  repeat_interval : Duration.t option;
      (** Minimum time interval between sending two notifications
          about the same alert. If omitted, child routes inherit the
          repeat_interval of the parent route. The [repeat_interval]
          value should be a multiple of [group_interval]. *)
  receiver : receiver;
}

let route ?group_wait ?group_interval ?repeat_interval receiver =
  let group_wait = Option.map Duration.of_string group_wait in
  let group_interval = Option.map Duration.of_string group_interval in
  let repeat_interval = Option.map Duration.of_string repeat_interval in
  {group_wait; group_interval; repeat_interval; receiver}

type alert = {alert : Prometheus.alert; route : route option}

let alert ?route alert = {alert; route}

let slack_webhook_receiver ?channel ~name ~api_url ?title ?text () =
  let channel =
    match channel with
    | None -> {|{{ range .Alerts }}{{ .Annotations.description }}{{ end }}|}
    | Some channel -> channel
  in
  Slack {name; channel; api_url; bot_token = None; title; text}

(* https://prometheus.io/docs/alerting/latest/configuration/#slack_config *)
let slack_bottoken_receiver ~name ~channel ~bot_token ?title ?text () =
  Slack
    {
      name;
      channel;
      api_url = "https://slack.com/api/chat.postMessage";
      bot_token = Some bot_token;
      title;
      text;
    }

let null_receiver = Null

let name_of_receiver = function
  | Slack {name; _} -> name
  | Null -> "null-receiver"

let default_route =
  {
    group_wait = None;
    group_interval = None;
    repeat_interval = None;
    receiver = null_receiver;
  }

type t = {
  configuration_file : string;
  mutable routes : (Prometheus.alert * route) list;
  receivers : receiver list;
  default_receiver : receiver;
}

let jingoo_receiver_template receiver =
  let open Jingoo.Jg_types in
  let config_template = function
    | Null -> []
    | Slack {name = _; channel; api_url; bot_token; title; text} ->
        [
          ( "config",
            Tobj
              ([
                 ("type", Tstr "slack");
                 ("api_url", Tstr api_url);
                 ("channel", Tstr channel);
               ]
              @ Option.fold ~none:[] ~some:(fun v -> [("title", Tstr v)]) title
              @ Option.fold ~none:[] ~some:(fun v -> [("text", Tstr v)]) text
              @
              match bot_token with
              | None -> []
              | Some bot_token ->
                  [
                    ( "http_config",
                      Tobj
                        [
                          ("type", Tstr "Bearer");
                          ("credentials", Tstr bot_token);
                        ] );
                  ]) );
        ]
  in
  Tobj ([("name", Tstr (name_of_receiver receiver))] @ config_template receiver)

let jingoo_routes_template routes =
  let open Jingoo.Jg_types in
  let simple_route group_wait group_interval repeat_interval receiver =
    [("receiver_name", Tstr (name_of_receiver receiver))]
    @ Option.fold
        ~none:[]
        ~some:(fun group_wait ->
          [("group_wait", Tstr (Duration.to_string group_wait))])
        group_wait
    @ Option.fold
        ~none:[]
        ~some:(fun group_interval ->
          [("group_interval", Tstr (Duration.to_string group_interval))])
        group_interval
    @ Option.fold
        ~none:[]
        ~some:(fun repeat_interval ->
          [("repeat_interval", Tstr (Duration.to_string repeat_interval))])
        repeat_interval
  in
  let jingoo_route_template (alert, route) =
    let args =
      simple_route
        route.group_wait
        route.group_interval
        route.repeat_interval
        route.receiver
    in
    Tobj ([("alert_name", Tstr (Prometheus.name_of_alert alert))] @ args)
  in
  Tobj
    [
      (* We can't use default as it is a keyword in Jingoo. *)
      ( "fallback",
        Tobj
          (simple_route
             default_route.group_wait
             default_route.group_interval
             default_route.repeat_interval
             default_route.receiver) );
      ("routes", Tlist (List.map jingoo_route_template routes));
    ]

let routes_of_alerts alerts =
  alerts
  |> List.filter_map (fun alert ->
         Option.map (fun route -> (alert.alert, route)) alert.route)
  |> List.sort_uniq compare

let receivers_of_alerts alerts =
  alerts
  |> List.filter_map (fun alert -> alert.route)
  |> List.map (fun route -> route.receiver)
  |> List.sort_uniq compare

let jingoo_configuration_template t =
  let receivers = null_receiver :: t.receivers in
  let receivers =
    receivers @ List.map (fun (_, route) -> route.receiver) t.routes
  in
  let receivers = List.sort_uniq compare receivers in
  let open Jingoo.Jg_types in
  [
    ("receivers", Tlist (List.map jingoo_receiver_template receivers));
    ("route", jingoo_routes_template t.routes);
  ]

let write_configuration t =
  jingoo_configuration_template t
  |> List.iter (fun (str, value) ->
         Log.info "%s:%a" str Jingoo.Jg_types.pp_tvalue value) ;
  let content =
    Jingoo.Jg_template.from_file
      Path.alert_manager_configuration
      ~models:(jingoo_configuration_template t)
  in
  with_open_out t.configuration_file (fun oc ->
      Stdlib.seek_out oc 0 ;
      output_string oc content)

let run ?(default_receiver = null_receiver) alerts =
  let alert_manager_configuration_directory = Path.tmp_dir // "alert_manager" in
  let* () = Process.run "mkdir" ["-p"; alert_manager_configuration_directory] in
  let configuration_file =
    alert_manager_configuration_directory // "alert_manager.yml"
  in
  Log.info "Alert_manager: run with configuration file %s" configuration_file ;
  let routes = routes_of_alerts alerts in
  let receivers =
    List.sort_uniq compare (default_receiver :: receivers_of_alerts alerts)
  in
  let t = {configuration_file; routes; default_receiver; receivers} in
  match receivers with
  | [Null] -> Lwt.return_none
  | _ ->
      write_configuration t ;
      let* () =
        Lwt.catch
          (fun () ->
            Process.run
              "docker"
              [
                "run";
                "-v";
                "/tmp/alert_manager:/tmp/alert_manager";
                "--rm";
                "-d";
                "--network";
                "host";
                "--name";
                "alert-manager";
                "-p";
                "9093-9093";
                "prom/alertmanager:latest";
                "--config.file";
                configuration_file;
              ])
          (fun exn ->
            Log.error "Alert_manager:%s" (Printexc.to_string exn) ;
            Lwt.return_unit)
      in
      Lwt.return_some t

let reload t =
  Log.info "%s: reloading" section ;
  write_configuration t ;
  Lwt.catch
    (fun () ->
      Process.run "curl" ["-X"; "POST"; "http://127.0.0.1:9093/-/reload"])
    (fun exn ->
      Log.error
        "%s: Could not reload alert_manager, curl returned: %s"
        section
        (Printexc.to_string exn) ;
      Lwt.return_unit)

let shutdown () =
  Log.info "%s: shutting down" section ;
  let* () =
    Lwt.catch
      (fun () -> Docker.kill "alert-manager" |> Process.check)
      (fun exn ->
        Log.error
          "Could not shutdown alert_manager properly: %s"
          (Printexc.to_string exn) ;
        Lwt.return_unit)
  in
  Lwt.return_unit

let add_alert t ~alert =
  let {alert; route = route'} = alert in
  let route' = Option.value ~default:(route t.default_receiver) route' in
  let {receiver; _} = route' in
  Log.info
    "%s: adding alert with receiver: %s"
    section
    (name_of_receiver receiver) ;
  t.routes <- List.sort_uniq compare ((alert, route') :: t.routes) ;
  reload t
