(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Alert manager configuration and runner. *)

type alert = {
  name : string;
  expr : string;
  for_ : string option;
  description : string option;
  summary : string option;
  severity : [`Critical | `Warning | `Info | `None];
}

let alert ~expr ~severity ?for_ ?description ?summary name =
  {name; expr; for_; description; summary; severity}

let name_of_alert {name; _} = name

let severity_to_string = function
  | `Critical -> "critical"
  | `Warning -> "warning"
  | `Info -> "info"
  | `None -> "none"

let alert_template alert =
  let open Jingoo.Jg_types in
  let payload =
    [
      ("name", Tstr alert.name);
      ("expr", Tstr alert.expr);
      ("severity", Tstr (severity_to_string alert.severity));
    ]
    @ Option.fold
        ~none:[]
        ~some:(fun v -> [("description", Tstr v)])
        alert.description
    @ Option.fold ~none:[] ~some:(fun v -> [("summary", Tstr v)]) alert.summary
  in
  Tobj payload

module Duration : sig
  type t

  val v : string -> t

  val j : t -> Jingoo.Jg_types.tvalue
end = struct
  (** Smart constructor for wait and interval duration. *)
  type t = Duration of string [@@unboxed]

  (** Time units available are the following:
        ms - miliseconds
        s  - seconds
        m  - minutes
        h  - hours
        d  - days
        w  - weeks
        y  - years. *)
  let v s =
    let valid_duration_re =
      (* https://prometheus.io/docs/alerting/latest/configuration/ *)
      Re.Pcre.regexp
        "((([0-9]+)y)?(([0-9]+)w)?(([0-9]+)d)?(([0-9]+)h)?(([0-9]+)m)?(([0-9]+)s)?(([0-9]+)ms)?|0)"
    in
    if not (Re.Pcre.pmatch ~rex:valid_duration_re s) then
      failwith (Format.sprintf "alert-manager: invalid duration format: %S." s) ;
    Duration s

  let j (Duration duration) = Jingoo.Jg_types.box_string duration
end

(** Set and map used to ensure key uniqueness. *)

module SSet = Set.Make (String)
module SMap = Map.Make (String)

(** Map used to index groups by id. *)
module IMap = Map.Make (Int)

(** Configuration type for Alert manager. *)
type config = {
  global : string SMap.t;  (** Global settings as key-value pairs. *)
  default_group : group;
  groups : group IMap.t;  (** Routing that defines how alerts are handled. *)
  receivers : receiver_config SMap.t;
      (** Receivers that can handle notifications. *)
}

and group = {
  identifier : group_id;  (** Metadata. *)
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
  alert_names : SSet.t;  (** Alert names to match against. *)
  receiver_name : string;  (** Name of the receiver to handle matched alerts. *)
}

(** Group identifier consists of:
    - a [group_number] for simple identifiers.
    - a [group_name] for debugging purpose.
  *)
and group_id = {group_name : string; group_number : int}

(** Receiver configurations *)
and receiver_config =
  | Slack of {
      channel : string;
          (** Slack channel to send notifications to, use '#' for public channel. *)
      title : string;  (** Title of the notification message. *)
      text : string;  (** Content of the notification message. *)
    }  (** Slack notification configuration. *)

let add_global ~k ~v t =
  match SMap.find_opt k t.global with
  | Some v' ->
      if v <> v' then
        failwith
          (Format.sprintf
             "alert-manager: global key '%s' is already bound to '%s' in the \
              alert manage configuration, cannot bind it to '%s'."
             k
             v'
             v)
      else t
  | None -> {t with global = SMap.add k v t.global}

let add_globals kvs t =
  List.fold_left (fun t (k, v) -> add_global ~k ~v t) t kvs

type receiver = {name : string; config : receiver_config}

let slack_receiver ~name ~channel ~title ~text =
  {name; config = Slack {channel; title; text}}

let add_receiver receiver t =
  match SMap.find_opt receiver.name t.receivers with
  | Some v ->
      if receiver.config <> v then
        failwith
          (Format.sprintf
             "alert-manager: receiver %s already registered with a different \
              configuration."
             receiver.name)
      else t
  | None ->
      {t with receivers = SMap.add receiver.name receiver.config t.receivers}

let add_group group t =
  {
    t with
    groups =
      (match IMap.find_opt group.identifier.group_number t.groups with
      | None -> IMap.add group.identifier.group_number group t.groups
      | Some v ->
          if v.identifier.group_number <> group.identifier.group_number then
            failwith
              (Format.sprintf
                 "alert-manager: the group '%s' already exists in the alert \
                  manager configuration."
                 group.identifier.group_name)
          else t.groups);
  }

let with_group group f t =
  {
    t with
    groups =
      (match IMap.find_opt group.identifier.group_number t.groups with
      | None ->
          failwith
            (Format.sprintf
               "alert-manager: the group '%s' does not exist in the alert \
                manager configuration."
               group.identifier.group_name)
      | Some x -> IMap.add group.identifier.group_number (f x) t.groups);
  }

let group =
  let id = ref (-1) in
  fun ?group_wait ?group_interval ?repeat_interval group_name receiver ->
    incr id ;
    {
      identifier = {group_name; group_number = !id};
      group_wait = Option.map Duration.v group_wait;
      group_interval = Option.map Duration.v group_interval;
      repeat_interval = Option.map Duration.v repeat_interval;
      alert_names = SSet.empty;
      receiver_name = receiver.name;
    }

let add_group_alert alert group =
  {group with alert_names = SSet.add (name_of_alert alert) group.alert_names}

let add_group_alerts alerts group =
  List.fold_left (Fun.flip add_group_alert) group alerts

(** [default_group] with default duration values as found in
      https://prometheus.io/docs/alerting/latest/configuration *)
let default_group =
  {
    identifier = {group_name = "default"; group_number = -1};
    group_wait = Some (Duration.v "30s");
    group_interval = Some (Duration.v "5m");
    repeat_interval = Some (Duration.v "4h");
    alert_names = SSet.empty;
    receiver_name = "default";
  }

let config_zero =
  {
    global = SMap.empty;
    default_group;
    groups = IMap.empty;
    receivers = SMap.empty;
  }

module Config = struct
  (** Jingoo template generation. *)

  module J = struct
    open Jingoo.Jg_types

    let rec template {global; groups; default_group; receivers} =
      let receivers = SMap.bindings receivers in
      [
        ("globals", global_j global);
        ("default_group", group_j default_group);
        ("groups", groups_j groups);
        ("receivers", box_list (List.map receiver_j receivers));
      ]

    and global_j global =
      box_list
      @@ List.map
           (fun (k, v) -> box_obj [("k", box_string k); ("v", box_string v)])
           (SMap.bindings global)

    and groups_j groups =
      box_list @@ List.map (fun x -> group_j (snd x)) (IMap.bindings groups)

    and group_j
        {
          identifier = _;
          group_wait;
          group_interval;
          repeat_interval;
          alert_names;
          receiver_name;
        } =
      let alert_names_s =
        let elements = SSet.elements alert_names in
        Format.sprintf "(%s)" (String.concat "|" elements)
      in
      let duration_helper name x =
        Option.fold ~none:[] ~some:(fun v -> [(name, Duration.j v)]) x
      in
      let payload =
        [("alertname", box_string alert_names_s)]
        @ duration_helper "group_wait" group_wait
        @ duration_helper "group_interval" group_interval
        @ duration_helper "repeat_interval" repeat_interval
        @ [("receiver", box_string receiver_name)]
      in
      box_obj payload

    and receiver_j (name, config) =
      let payload =
        [("name", box_string name); ("config", receiver_config_j config)]
      in
      box_obj payload

    and receiver_config_j = function
      | Slack {channel; title; text} ->
          let type_ = box_string "slack" in
          let data =
            box_obj
              [
                ("channel", box_string channel);
                ("title", box_string title);
                ("text", box_string text);
              ]
          in
          box_obj [("type", type_); ("data", data)]
  end

  (** [write t target] writes the jingoo generated file using the
      template [Path.alert_manager_configuration] and the config [t]
      at path [target]. *)
  let write t target =
    let models = J.template t in
    let content =
      Jingoo.Jg_template.from_file
        ~env:{Jingoo.Jg_types.std_env with autoescape = false}
        Path.alert_manager_configuration
        ~models
    in
    with_open_out target (fun oc ->
        Stdlib.seek_out oc 0 ;
        output_string oc content)
end

module Handler = struct
  type t = {name : string; setup : config -> config}

  let make ~name ~setup = {name; setup}
end

module Collection = struct
  type t = {
    alert_registery : (string, alert) Hashtbl.t;
    handler_registery : (string, Handler.t) Hashtbl.t;
  }

  let init () : t =
    {alert_registery = Hashtbl.create 16; handler_registery = Hashtbl.create 16}

  let register_alert alert t =
    Hashtbl.add t.alert_registery (name_of_alert alert) alert

  let register_handler handler t =
    Hashtbl.add t.handler_registery handler.Handler.name handler

  let find_handler handler_name t =
    Hashtbl.find_opt t.handler_registery handler_name

  let alerts t = List.of_seq @@ Hashtbl.to_seq_values t.alert_registery

  let handlers t = List.of_seq @@ Hashtbl.to_seq_values t.handler_registery
end

type t = {config : config; config_file : string; collection : Collection.t}

let config_from_alert_handlers config collection alert_handlers =
  List.fold_left
    (fun config alert_handler ->
      match Collection.find_handler alert_handler collection with
      | None ->
          failwith
            (Format.sprintf
               "alert-manager: alert handler '%s' not found in the alert \
                manager collection."
               alert_handler)
      | Some h -> h.setup config)
    config
    alert_handlers

let run collection alert_handlers =
  let alert_manager_configuration_directory =
    Filename.get_temp_dir_name () // "alert_manager"
  in
  let* () = Process.run "mkdir" ["-p"; alert_manager_configuration_directory] in
  let alert_manager_configuration_file =
    alert_manager_configuration_directory // "alert_manager.yml"
  in
  let config =
    config_from_alert_handlers config_zero collection alert_handlers
  in
  Config.write config alert_manager_configuration_file ;
  let t =
    {config; config_file = alert_manager_configuration_file; collection}
  in
  let* () =
    Process.run
      "docker"
      [
        "run";
        "-d";
        "-v";
        "/tmp/alert_manager:/tmp/alert_manager";
        "--rm";
        "--network";
        "host";
        "--name";
        "alert-manager";
        "-p";
        "9093-9093";
        "prom/alertmanager:latest";
        "--config.file";
        alert_manager_configuration_file;
      ]
  in
  Lwt.return t

let shutdown () =
  let* () = Docker.kill "alert-manager" |> Process.check in
  Lwt.return_unit
