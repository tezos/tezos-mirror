(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type config = {
  url : Uri.t;
  api_token : string;
  data_source : string;
  timeout : float;
}

let config_of_json json =
  {
    url = JSON.(json |-> "url" |> as_string |> Uri.of_string);
    api_token = JSON.(json |-> "api_token" |> as_string);
    data_source = JSON.(json |-> "data_source" |> as_string);
    timeout =
      JSON.(json |-> "timeout" |> as_float_opt |> Option.value ~default:20.);
  }

type duration =
  | Seconds of int
  | Minutes of int
  | Hours of int
  | Days of int
  | Weeks of int
  | Month of int
  | Years of int

let string_of_duration =
  let f value unit = string_of_int value ^ unit in
  function
  | Seconds x -> f x "s"
  | Minutes x -> f x "m"
  | Hours x -> f x "h"
  | Days x -> f x "d"
  | Weeks x -> f x "w"
  | Month x -> f x "M"
  | Years x -> f x "y"

type yaxis = {format : string; label : string option}

type graph = {
  title : string;
  description : string;
  queries : InfluxDB.select list;
  interval : duration option;
  yaxis_1 : yaxis option;
  yaxis_2 : yaxis option;
}

type panel = Row of string | Graph of graph

(* We use the [uid] and not the [id] because the [uid] can be stable
   between Grafana installs. *)
type dashboard = {
  uid : string;
  title : string;
  description : string;
  panels : panel list;
}

let encode_target (query : InfluxDB.select) : JSON.u =
  `O
    [
      ("query", `String (InfluxDB.show_select ~grafana:true query));
      ("rawQuery", `Bool true);
      ("resultFormat", `String "time_series");
    ]

let encode_yaxis = function
  | None -> `O [("show", `Bool false)]
  | Some {format; label} ->
      `O
        (("format", `String format)
         ::
         (match label with
         | None -> []
         | Some label -> [("label", `String label)])
        @ [("show", `Bool true)])

let encode_panel config y panel : JSON.u =
  match panel with
  | Row title ->
      `O
        [
          ("type", `String "row");
          ("title", `String title);
          ( "gridPos",
            `O
              [
                ("h", `Float 1.);
                ("w", `Float 24.);
                ("x", `Float 0.);
                ( "y",
                  `Float
                    (let value = float !y in
                     y := !y + 1 ;
                     value) );
              ] );
        ]
  | Graph {title; description; queries; interval; yaxis_1; yaxis_2} ->
      let interval =
        Option.map
          (fun i -> ("interval", `String (string_of_duration i)))
          interval
        |> Option.to_list
      in
      `O
        (interval
        @ [
            ("type", `String "timeseries");
            ("datasource", `String config.data_source);
            ("title", `String title);
            ("description", `String description);
            ( "gridPos",
              `O
                [
                  ("h", `Float 8.);
                  ("w", `Float 24.);
                  ("x", `Float 0.);
                  ( "y",
                    `Float
                      (let value = float !y in
                       y := !y + 8 ;
                       value) );
                ] );
            ("targets", `A (List.map encode_target queries));
            ("yaxes", `A [encode_yaxis yaxis_1; encode_yaxis yaxis_2]);
            ( "fieldConfig",
              `O
                [
                  ( "defaults",
                    `O
                      [
                        ( "custom",
                          `O
                            [
                              ("drawStyle", `String "line");
                              ("lineInterpolation", `String "linear");
                              ("showPoints", `String "always");
                              ("pointSize", `Float 5.);
                              ("spanNulls", `Bool true);
                              ("lineWidth", `Float 1.);
                              ("fillOpacity", `Float 10.);
                              ("axisSoftMin", `Float 0.);
                            ] );
                        ("unit", `String "s");
                      ] );
                ] );
          ])

let encode_dashboard config {uid; title; description; panels} : JSON.u =
  `O
    [
      ("uid", `String uid);
      ("title", `String title);
      ("description", `String description);
      ("panels", `A (List.map (encode_panel config (ref 0)) panels));
    ]

let make_url {url; _} path =
  let path =
    let base_path = Uri.path url in
    if base_path <> "" && base_path.[String.length base_path - 1] = '/' then
      base_path ^ path
    else base_path ^ "/" ^ path
  in
  Uri.with_path url path

let with_timeout {timeout; _} p =
  let timeout =
    let* () = Lwt_unix.sleep timeout in
    failwith "timeout"
  in
  Lwt.pick [p; timeout]

let uid_rex = rex "[a-zA-Z0-9._-]{1,128}"

type http_request = {
  uri : Uri.t;
  meth : Cohttp.Code.meth;
  headers : Cohttp.Header.t;
  body : Cohttp_lwt.Body.t option;
}

let http_call request config =
  with_timeout config
  @@ Cohttp_lwt_unix.Client.call
       ~headers:request.headers
       ?body:request.body
       request.meth
       request.uri

let string_of_http_request request =
  let* body =
    Option.map
      (fun body ->
        Lwt.map (fun s -> ", Body: " ^ s) @@ Cohttp_lwt.Body.to_string body)
      request.body
    |> Option.value ~default:(Lwt.return "")
  in
  return
  @@ Format.sprintf
       "Uri: %s, Method: %s, Headers: [%s]%s"
       (Uri.to_string request.uri)
       (Cohttp.Code.string_of_method request.meth)
       (Cohttp.Header.to_string request.headers)
       body

let handle_http_error resp_status resp_body request =
  let* body = Cohttp_lwt.Body.to_string resp_body in
  let* req = string_of_http_request request in
  failwith
  @@ sf
       "Grafana responded with %s - %s for request (%s) "
       (Cohttp.Code.string_of_status resp_status)
       body
       req

let update_dashboard config dashboard =
  if dashboard.uid =~! uid_rex then
    invalid_arg
      (sf
         "Grafana.update_dashboard: invalid UID: %s (must match: %s)"
         dashboard.uid
         (show_rex uid_rex)) ;
  let authorization = ("Authorization", "Bearer " ^ config.api_token) in
  (* Delete so that we don't care about versions. *)
  let* () =
    let delete_request =
      {
        uri = make_url config ("dashboards/uid/" ^ dashboard.uid);
        meth = `DELETE;
        headers = Cohttp.Header.of_list [authorization];
        body = None;
      }
    in
    let* (response, body) = http_call delete_request config in
    match response.status with
    | #Cohttp.Code.success_status | `Not_found ->
        Cohttp_lwt.Body.drain_body body
    | status -> handle_http_error status body delete_request
  in
  (* (Re-)create dashboard. *)
  let body =
    `O [("dashboard", encode_dashboard config dashboard)] |> JSON.encode_u
  in
  let create_request =
    {
      uri = make_url config "dashboards/db";
      meth = `POST;
      headers =
        Cohttp.Header.of_list
        @@ [("Content-Type", "application/json"); authorization];
      body = Option.some @@ Cohttp_lwt.Body.of_string body;
    }
  in
  let* (response, body) = http_call create_request config in
  match response.status with
  | #Cohttp.Code.success_status -> Cohttp_lwt.Body.drain_body body
  | status -> handle_http_error status body create_request

let where_clause_of_tag (tag_name, tag_label) =
  InfluxDB.Tag (tag_name, EQ, tag_label)

let where_clause_of_tags hd tail =
  List.fold_left
    (fun clause tag -> InfluxDB.And (clause, where_clause_of_tag tag))
    (where_clause_of_tag hd)
    tail

let simple_query ?(tags = []) ~measurement ~field ~test () =
  let where_clause =
    InfluxDB.And (Grafana_time_filter, where_clause_of_tags ("test", test) tags)
  in
  InfluxDB.select
    [Function (MEAN, Field field)]
    ~from:(Measurement measurement)
    ~where:where_clause
    ~group_by:(Time {interval = Grafana_interval; tag = None; fill = None})

let simple_graph ?title ?(description = "") ?(yaxis_format = "s") ?tags
    ?interval ~measurement ~field ~test () =
  let title = Option.value title ~default:measurement in
  Graph
    {
      title;
      description;
      queries = [simple_query ?tags ~measurement ~field ~test ()];
      interval;
      yaxis_1 = Some {format = yaxis_format; label = Some field};
      yaxis_2 = None;
    }

let graphs_per_tags ?title ?(description = "") ?(yaxis_format = "s") ?interval
    ~measurement ~field ~test ~tags () =
  let title = Option.value title ~default:measurement in
  let queries =
    List.map
      (fun tag -> simple_query ~tags:[tag] ~measurement ~field ~test ())
      tags
  in
  Graph
    {
      title;
      description;
      queries;
      interval;
      yaxis_1 = Some {format = yaxis_format; label = Some field};
      yaxis_2 = None;
    }
