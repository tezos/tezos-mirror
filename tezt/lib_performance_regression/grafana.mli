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

(** Create Grafana dashboards. *)

(** Grafana configuration.

    [url] is the base URL of the Grafana API.
    [api_token] is the bearer token to use in the Authorization header.
    [data_source] is the name of the InfluxDB data source configured in Grafana. *)
type config = {
  url : Uri.t;
  api_token : string;
  data_source : string;
  timeout : float;
}

(** Read a Grafana configuration from a JSON value. *)
val config_of_json : JSON.t -> config

(** Y axes for Grafana panels.

    [format] is the unit of the axis.
    A typical example is ["s"], meaning "seconds".
    See Grafana documentation for other possible values. *)
type yaxis = {format : string; label : string option}

(** Grafana graph panels.

    Queries should use [Grafana_time_filter] in their WHERE clause
    and [Grafana_interval] in a GROUP BY time clause to reduce the size of the query.
    The GROUP BY time clause should usually also contain a FILL clause to make
    continuous graphs instead of bunches of dots.

    Example query:
    [
      InfluxDB.select
        [Function (MEAN, Field "duration")]
        ~from:(Measurement "rpc")
        ~where:Grafana_time_filter
        ~group_by:(Time {interval = Grafana_interval; tag = None; fill = None})
    ] *)
type graph = {
  title : string;
  description : string;
  queries : InfluxDB.select list;
  yaxis_1 : yaxis option;
  yaxis_2 : yaxis option;
}

(** Grafana panels.

    Rows are horizontal separators between graphs, with a title. *)
type panel = Row of string | Graph of graph

(** Grafana dashboards.

    [uid] is a unique identifier of your choosing.
    It will be used in the URL of the dashboard.
    It must be composed of between 1 and 128 alphanumeric characters, dashes, periods
    or underscores. *)
type dashboard = {
  uid : string;
  title : string;
  description : string;
  panels : panel list;
}

(** Create or update a dashboard.

    If the dashboard already exists, it is deleted first.
    All version history is lost.

    @raise Invalid_arg if the dashboard UID is invalid. *)
val update_dashboard : config -> dashboard -> unit Lwt.t

(** Make a simple SELECT query for a graph panel.

    Usage: [simple_query ~tags:[("tag1", "value1"), ("tag2", "value2")] measurement field]

    Default [tags] is an empty list.

    This returns the following query:
    [
      SELECT MEAN(field)
      FROM measurement
      WHERE $timeFilter AND tag1 = value1 AND tag2 = value2
      GROUP BY time($__interval) fill(previous)
    ] *)
val simple_query :
  ?tags:(InfluxDB.tag * string) list ->
  InfluxDB.measurement ->
  InfluxDB.field ->
  InfluxDB.select

(** Make a graph panel from a simple query.

    The query is built using {!simple_query}.

    Default [title] is the measurement name.
    Default [description] is [""].
    Default [yaxis_format] is [s] (seconds).
    Default [tags] is an empty list. *)
val simple_graph :
  ?title:string ->
  ?description:string ->
  ?yaxis_format:string ->
  ?tags:(InfluxDB.tag * string) list ->
  InfluxDB.measurement ->
  InfluxDB.field ->
  panel
