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

(** Send and retrieve data points from InfluxDB. *)

(** InfluxDB configuration.

    [url] is the base URL of the InfluxDB API.
    [database] is the name of the InfluxDB database to read from and write to.
    [username] and [password] are the credentials for this database.

    [measurement_prefix] is prepended to all measurement names when sending data points
    and in SELECT queries.

    [tags] is added to the tags of all data points when sending data points. *)
type config =
  | V1_8 of {
      url : Uri.t;
      database : string;
      username : string;
      password : string;
      measurement_prefix : string;
      tags : (string * string) list;
      timeout : float;
    }

(** Read an InfluxDB configuration from a JSON value. *)
val config_of_json : JSON.t -> config

(** Measurement names for data points. *)
type measurement = string

(** Tag names for data points. *)
type tag = string

(** Field names for data points. *)
type field = string

(** Field values for data points. *)
type field_value = Float of float | String of string

(** Unix timestamps as returned by [Unix.gettimeofday]. *)
type timestamp = float

(** Data points (see function [data_point]). *)
type data_point = private {
  measurement : measurement;
  tags : (tag * string) list;
  first_field : field * field_value;
  other_fields : (field * field_value) list;
  timestamp : timestamp;
}

(** Create a data point.

    Usage: [data_point measurement key_value]

    Data points are composed of:
    - a name [measurement];
    - a possibly-empty list of [tags] to complete the name;
    - a non-empty list of [(key, value)] fields, composed of [key_value] and [other_fields];
    - a UNIX [timestamp] (default value is now).
    See the documentation of InfluxDB for more information about those components.

    @raise Invalid_arg if one of the tags, tag values, fields, field values,
    or measurement contains a newline character. *)
val data_point :
  ?tags:(tag * string) list ->
  ?other_fields:(field * field_value) list ->
  ?timestamp:timestamp ->
  measurement ->
  field * field_value ->
  data_point

(** Add a tag to a data point.

    @raise Invalid_arg if the tag or the value contains a newline character. *)
val add_tag : tag -> string -> data_point -> data_point

(** Convert a data point to a string for display purposes. *)
val show_data_point : data_point -> string

(** Push data points to InfluxDB.

    Return [Error message] if the data could not be sent, where [message] is
    a human-readable error message. *)
val write : config -> data_point list -> unit Lwt.t

(** {2 InfluxQL} *)

(** This section provides a [select] type that is an AST of InfluxQL SELECT queries,
    and a function to perform those queries.

    See the documentation of InfluxQL here:
    https://docs.influxdata.com/influxdb/v1.8/query_language/explore-data *)

(** Time interval specifications for InfluxQL queries.

    [Grafana_interval] denotes [$__interval], a placeholder that Grafana
    replaces with an interval which is proportional to the selected time window size.
    Use this in a [GROUP BY] clause. Example:
    [~group_by: (Time {interval = Grafana_interval; tag = None; fill = Some Previous})].
    If you try to actually {!query} a [select] with this, [query] raises [Invalid_arg]. *)
type time_interval =
  | Ns of int  (** nanoseconds *)
  | U of int  (** microseconds *)
  | Ms of int  (** milliseconds *)
  | S of int  (** seconds *)
  | M of int  (** minutes *)
  | H of int  (** hours *)
  | D of int  (** days *)
  | W of int  (** weeks *)
  | Grafana_interval

(** Functions for InfluxQL SELECT queries. *)
type func =
  | COUNT
  | DISTINCT
  | INTEGRAL of time_interval
  | MEAN
  | MEDIAN
  | MODE
  | SPREAD
  | STDDEV
  | SUM

(** Get the name of the column for a given function in query results. *)
val column_name_of_func : func -> string

(** Arguments of functions for InfluxQL SELECT queries. *)
type argument = All | Field of string

(** Columns to retrieve using InfluxQL SELECT queries. *)
type column =
  | All
  | Field of string
  | Tag of string
  | Function of func * argument

(** Operators for tag comparisons in WHERE clauses. *)
type tag_operator = EQ | NE

(** Operators for field comparisons in WHERE clauses. *)
type field_operator = EQ | NE | GT | GE | LT | LE

(** WHERE clauses of InfluxQL SELECT queries.

    [Grafana_time_filter] denotes [$timeFilter], a placeholder that Grafana
    replaces with a predicate on [time] which denotes the time window
    that the user selected in Grafana. If you try to actually {!query}
    a [select] with this, [query] raises [Invalid_arg]. *)
type where =
  | Tag of string * tag_operator * string
  | Field of string * field_operator * field_value
  | Or of where * where
  | And of where * where
  | Grafana_time_filter

(** Fill argument of GROUP BY time clauses. *)
type fill = Value of float | Linear | F_none | Null | Previous

(** GROUP BY clauses of InfluxQL SELECT queries. *)
type group_by =
  | All_tags
  | Tags of tag list
  | Time of {interval : time_interval; tag : tag option; fill : fill option}

(** ORDER BY clauses of InfluxQL SELECT queries. *)
type order_by = Time_desc

(** InfluxQL SELECT queries. *)
type select = {
  columns : column list;
  from : from;
  where : where option;
  group_by : group_by option;
  order_by : order_by option;
  limit : int option;
  slimit : int option;
}

(** FROM clauses of InfluxQL SELECT queries. *)
and from = Measurement of string | Select of select

(** Make an InfluxQL SELECT query. *)
val select :
  from:from ->
  ?where:where ->
  ?group_by:group_by ->
  ?order_by:order_by ->
  ?limit:int ->
  ?slimit:int ->
  column list ->
  select

(** Convert a SELECT query to a string for display purposes.

    If [grafana] is [true], allow [Grafana_time_filter] and [Grafana_interval].
    Default is [false].

    @raise Invalid_arg if [grafana] is [false] and the query contains
    [Grafana_time_filter] or [Grafana_interval]. *)
val show_select : ?grafana:bool -> select -> string

(** Prepend the measurement of the innermost SELECT query with the configured prefix.

    This is automatically done by {!query}. *)
val prefix_measurement : config -> select -> select

(** Data points returned when making a SELECT query. *)
type result_data_point

(** Convert a data point returned by a SELECT query to a human-readable string. *)
val show_result_data_point : result_data_point -> string

(** Perform a SELECT query.

    @raise Invalid_arg if the query contains [Grafana_time_filter] or [Grafana_interval]. *)
val query : config -> select -> result_data_point list list Lwt.t

(** Get a value from a data point in a query result.

    Example: [get "count" JSON.as_int] *)
val get : string -> (JSON.t -> 'a) -> result_data_point -> 'a

(** Convert results from a SELECT query into a string for debugging purposes. *)
val show_query_result : result_data_point list list -> string
