(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

type type_ =
  | Unspecified
  | Count of {interval : Ptime.Span.t}
  | Rate of {interval : Ptime.Span.t}
  | Gauge

type point = {timestamp : Ptime.t; value : float}

type origin = {
  metric_type : int32 option;
  product : int32 option;
  service : int32 option;
}

type metadata = {origin : origin option}

type resource = {name : string; type_ : string}

type series_item = {
  metric : string;
  metadata : metadata option;
  points : point list;
  resources : resource list;
  source_type_name : string option;
  tags : string list;
  type_ : type_;
  unit : string option;
}

type metric = series_item list

let int_int64 = Data_encoding.conv Int64.of_int Int64.to_int Data_encoding.int64

let span_encoding =
  let open Data_encoding in
  conv
    (fun d -> Ptime.Span.to_int_s d |> Option.get)
    Ptime.Span.of_int_s
    int_int64

let timestamp_encoding =
  let open Data_encoding in
  conv_with_guard
    (fun t -> Ptime.to_float_s t |> int_of_float)
    (fun s ->
      Ptime.of_float_s (float_of_int s)
      |> Option.to_result ~none:"Invalid timestamp")
    int_int64

let type_encoding =
  let open Data_encoding in
  conv
    (function
      | Unspecified -> (0, None)
      | Count {interval} -> (1, Some interval)
      | Rate {interval} -> (2, Some interval)
      | Gauge -> (3, None))
    (function
      | 0, None -> Unspecified
      | 1, Some interval -> Count {interval}
      | 2, Some interval -> Rate {interval}
      | 3, None -> Gauge
      | _ -> assert false)
  @@ obj2 (req "type" (ranged_int 0 3)) (opt "interval" span_encoding)

let point_encoding =
  let open Data_encoding in
  conv
    (fun {timestamp; value} -> (timestamp, value))
    (fun (timestamp, value) -> {timestamp; value})
  @@ obj2 (req "timestamp" timestamp_encoding) (req "value" float)

let origin_encoding =
  let open Data_encoding in
  conv
    (fun {metric_type; product; service} -> (metric_type, product, service))
    (fun (metric_type, product, service) -> {metric_type; product; service})
  @@ obj3 (opt "metric_type" int32) (opt "product" int32) (opt "service" int32)

let metatada_encoding =
  let open Data_encoding in
  conv (fun {origin} -> origin) (fun origin -> {origin})
  @@ obj1 (opt "origin" origin_encoding)

let resource_encoding =
  let open Data_encoding in
  conv (fun {name; type_} -> (name, type_)) (fun (name, type_) -> {name; type_})
  @@ obj2 (req "name" string) (req "type" string)

let series_item_encoding =
  let open Data_encoding in
  conv
    (fun {
           metric;
           metadata;
           points;
           resources;
           source_type_name;
           tags;
           type_;
           unit;
         }
       ->
      ( type_,
        (metric, metadata, points, resources, source_type_name, tags, unit) ))
    (fun ( type_,
           (metric, metadata, points, resources, source_type_name, tags, unit)
         )
       ->
      {metric; metadata; points; resources; source_type_name; tags; type_; unit})
  @@ merge_objs type_encoding
  @@ obj7
       (req "metric" string)
       (opt "metadata" metatada_encoding)
       (req "points" (list point_encoding))
       (dft "resources" (list resource_encoding) [])
       (opt "source_type_name" string)
       (dft "tags" (list string) [])
       (opt "unit" string)

let metric_encoding =
  let open Data_encoding in
  obj1 (req "series" (list series_item_encoding))

(** Send metrics to datadog.  *)
let send ?site ?api_key ?runner (metric : metric) =
  let api_key =
    [
      api_key;
      Sys.getenv_opt "DD_API_KEY" (* Standard datadog env variable *);
      Sys.getenv_opt "DATADOG_API_KEY" (* Variable used in tezos gitlab *);
    ]
    |> List.find_map Fun.id
    |> function
    | Some s -> s
    | None ->
        failwith
          "No Datadog API key configured to send metrics. Use environment \
           variable DD_API_KEY or DATADOG_API_KEY"
  in
  let site =
    [
      site;
      Sys.getenv_opt "DD_SITE" (* Standard datadog env variable *);
      Sys.getenv_opt "DATADOG_SITE" (* Variable used in tezos gitlab *);
    ]
    |> List.find_map Fun.id
    |> Option.value ~default:"datadoghq.eu"
  in
  let data =
    JSON.encode
    @@ JSON.annotate ~origin:"datadog_metric"
    @@ Data_encoding.Json.construct metric_encoding metric
  in
  let process =
    Process.spawn
      ?runner
      ~log_command:false (* Don't leak API key *)
      "curl"
      [
        "-f";
        "-v";
        "-X";
        "POST";
        sf "https://api.%s/api/v2/series" site;
        "-H";
        "Accept: application/json";
        "-H";
        "Content-Type: application/json";
        "-H";
        sf "DD-API-KEY: %s" api_key;
        "-d";
        data;
      ]
  in
  Log.debug
    "Sending metric to Datadog: %s"
    data
    ~prefix:(Process.name process)
    ~color:Log.Color.bold ;
  Runnable.{value = process; run = Process.check}
