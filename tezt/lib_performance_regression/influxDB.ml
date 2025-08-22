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

let nanosecond_precision = 1_000_000_000

let with_buffer size f =
  let buffer = Buffer.create size in
  f buffer ;
  Buffer.contents buffer

type credentials = {username : string; password : string}

type config =
  | V1_8 of {
      url : Uri.t;
      database : string;
      credentials : credentials option;
      measurement_prefix : string;
      tags : (string * string) list;
      timeout : float;
    }

let credentials_of_json json =
  {
    username = JSON.(json |-> "username" |> as_string);
    password = JSON.(json |-> "password" |> as_string);
  }

let config_of_json json =
  V1_8
    {
      url = JSON.(json |-> "url" |> as_string |> Uri.of_string);
      database = JSON.(json |-> "database" |> as_string);
      credentials =
        JSON.(
          json |-> "credentials" |> as_opt |> Option.map credentials_of_json);
      measurement_prefix =
        JSON.(
          json |-> "measurement_prefix" |> as_string_opt
          |> Option.value ~default:"tezt_");
      tags =
        JSON.(
          json |-> "tags" |> as_object
          |> List.map (fun (name, value) -> (name, as_string value)));
      timeout =
        JSON.(json |-> "timeout" |> as_float_opt |> Option.value ~default:20.);
    }

type measurement = string

type tag = string

type field = string

type field_value = Float of float | String of string

type timestamp = float

type data_point = {
  measurement : measurement;
  tags : (tag * string) list;
  first_field : field * field_value;
  other_fields : (field * field_value) list;
  timestamp : timestamp;
}

let check_no_newline ~f ~in_ value =
  if String.contains value '\n' then
    invalid_arg (Printf.sprintf "InfluxDB.%s: newline character in %s" f in_)

let data_point ?(tags = []) ?(other_fields = [])
    ?(timestamp = Unix.gettimeofday ()) measurement first_field =
  let check_nl = check_no_newline ~f:"data_point" in
  let check_tag (tag, value) =
    check_nl ~in_:"tag name" tag ;
    check_nl ~in_:"tag value" value
  in
  List.iter check_tag tags ;
  let check_field_value (name, value) =
    check_nl ~in_:"field name" name ;
    match value with
    | Float _ -> ()
    | String value -> check_nl ~in_:"field value" value
  in
  check_field_value first_field ;
  List.iter check_field_value other_fields ;
  check_nl ~in_:"measurement name" measurement ;
  {measurement; tags; first_field; other_fields; timestamp}

let add_tag tag value data_point =
  let check_nl = check_no_newline ~f:"add_tag" in
  check_nl ~in_:"tag name" tag ;
  check_nl ~in_:"tag value" value ;
  {data_point with tags = (tag, value) :: data_point.tags}

let make_url (V1_8 {url; database; credentials; _}) path =
  let creds_as_uri_params =
    match credentials with
    | Some {username; password} -> [("u", username); ("p", password)]
    | None -> []
  in
  let url =
    let path =
      let base_path = Uri.path url in
      if base_path <> "" && base_path.[String.length base_path - 1] = '/' then
        base_path ^ path
      else base_path ^ "/" ^ path
    in
    Uri.with_path url path
  in
  Uri.add_query_params' url @@ (("db", database) :: creds_as_uri_params)

(* https://docs.influxdata.com/influxdb/v1.8/write_protocols/line_protocol_reference *)
module Line_protocol = struct
  let write_measurement buffer s =
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\\' -> Buffer.add_string buffer "\\\\"
      | ',' -> Buffer.add_string buffer "\\,"
      | ' ' -> Buffer.add_string buffer "\\ "
      | c -> Buffer.add_char buffer c
    done

  let write_key buffer s =
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\\' -> Buffer.add_string buffer "\\\\"
      | ',' -> Buffer.add_string buffer "\\,"
      | '=' -> Buffer.add_string buffer "\\="
      | ' ' -> Buffer.add_string buffer "\\ "
      | c -> Buffer.add_char buffer c
    done

  let write_tag_value buffer s =
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\n' -> invalid_arg "Long_test.write_tag_value: newline character"
      | '\\' -> Buffer.add_string buffer "\\\\"
      | ',' -> Buffer.add_string buffer "\\,"
      | '=' -> Buffer.add_string buffer "\\="
      | ' ' -> Buffer.add_string buffer "\\ "
      | c -> Buffer.add_char buffer c
    done

  let write_field_value_string buffer s =
    Buffer.add_char buffer '\"' ;
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\n' ->
          invalid_arg "Long_test.write_field_value_string: newline character"
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '"' -> Buffer.add_string buffer "\\\""
      | c -> Buffer.add_char buffer c
    done ;
    Buffer.add_char buffer '\"'

  let write_field_value_float buffer f =
    Buffer.add_string buffer (string_of_float f)

  let write_field_value buffer = function
    | String s -> write_field_value_string buffer s
    | Float f -> write_field_value_float buffer f

  (* Encode using the line protocol of InfluxDB. *)
  let write_data_point buffer precision
      {
        measurement;
        tags;
        first_field = first_field_key, first_field_value;
        other_fields;
        timestamp;
      } =
    let char = Buffer.add_char buffer in
    write_measurement buffer measurement ;
    List.iter
      (fun (key, value) ->
        char ',' ;
        write_key buffer key ;
        char '=' ;
        write_tag_value buffer value)
      tags ;
    char ' ' ;
    write_key buffer first_field_key ;
    char '=' ;
    write_field_value buffer first_field_value ;
    List.iter
      (fun (key, value) ->
        char ',' ;
        write_key buffer key ;
        char '=' ;
        write_field_value buffer value)
      other_fields ;
    char ' ' ;
    Buffer.add_string
      buffer
      (Int64.to_string (Int64.of_float (timestamp *. float precision)))

  let write_data_points buffer data_points =
    List.iteri
      (fun i data_point ->
        if i > 0 then Buffer.add_char buffer '\n' ;
        write_data_point buffer nanosecond_precision data_point)
      data_points
end

let show_data_point data_point =
  with_buffer 128 @@ fun buffer ->
  Line_protocol.write_data_point buffer nanosecond_precision data_point

let with_timeout (V1_8 {timeout; _}) p =
  let timeout =
    let* () = Lwt_unix.sleep timeout in
    failwith "timeout"
  in
  Lwt.pick [p; timeout]

let write (V1_8 cfg as config) data_points =
  let data_points =
    List.map
      (fun data_point ->
        {
          data_point with
          measurement = cfg.measurement_prefix ^ data_point.measurement;
          tags = cfg.tags @ data_point.tags;
        })
      data_points
  in
  let body =
    `String
      ( with_buffer 256 @@ fun buffer ->
        Line_protocol.write_data_points buffer data_points )
  in
  let* response, body =
    with_timeout config
    @@ Cohttp_lwt_unix.Client.call ~body `POST (make_url config "write")
  in
  match response.status with
  | #Cohttp.Code.success_status -> Cohttp_lwt.Body.drain_body body
  | status ->
      let* body = Cohttp_lwt.Body.to_string body in
      failwith
        (sf
           "InfluxDB responded with %s - %s"
           (Cohttp.Code.string_of_status status)
           body)

type time_interval =
  | Ns of int
  | U of int
  | Ms of int
  | S of int
  | M of int
  | H of int
  | D of int
  | W of int
  | Grafana_interval

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

let column_name_of_func = function
  | COUNT -> "count"
  | DISTINCT -> "distinct"
  | INTEGRAL _ -> "integral"
  | MEAN -> "mean"
  | MEDIAN -> "median"
  | MODE -> "mode"
  | SPREAD -> "spread"
  | STDDEV -> "stddev"
  | SUM -> "sum"

type argument = All | Field of string

type column =
  | All
  | Field of string
  | Tag of string
  | Function of func * argument

type tag_operator = EQ | NE

type field_operator = EQ | NE | GT | GE | LT | LE

type where =
  | Tag of string * tag_operator * string
  | Field of string * field_operator * field_value
  | Or of where * where
  | And of where * where
  | Grafana_time_filter

type fill = Value of float | Linear | F_none | Null | Previous

type group_by =
  | All_tags
  | Tags of tag list
  | Time of {interval : time_interval; tag : tag option; fill : fill option}

type order_by = Time_desc

type select = {
  columns : column list;
  from : from;
  where : where option;
  group_by : group_by option;
  order_by : order_by option;
  limit : int option;
  slimit : int option;
}

and from = Measurement of string | Select of select

let select ~from ?where ?group_by ?order_by ?limit ?slimit columns =
  {columns; from; where; group_by; order_by; limit; slimit}

let rec write_select ~grafana buffer
    {columns; from; where; group_by; order_by; limit; slimit} =
  let s = Buffer.add_string buffer in
  let c = Buffer.add_char buffer in
  let int i = s (string_of_int i) in
  let id s =
    c '"' ;
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\n' -> invalid_arg "InfluxDB_identifier: newline character"
      | '"' -> Buffer.add_string buffer "\\\""
      | c -> Buffer.add_char buffer c
    done ;
    c '"'
  in
  let comma_list write_item = function
    | [] -> ()
    | [item] -> write_item item
    | head :: tail ->
        write_item head ;
        List.iter
          (fun item ->
            s ", " ;
            write_item item)
          tail
  in
  let write_time_interval = function
    | Ns x ->
        int x ;
        s "ns"
    | U x ->
        int x ;
        c 'u'
    | Ms x ->
        int x ;
        s "ms"
    | S x ->
        int x ;
        c 's'
    | M x ->
        int x ;
        c 'm'
    | H x ->
        int x ;
        c 'h'
    | D x ->
        int x ;
        c 'd'
    | W x ->
        int x ;
        c 'w'
    | Grafana_interval ->
        if not grafana then
          invalid_arg "cannot perform queries that use Grafana_interval" ;
        s "$__interval"
  in
  let write_string_value s =
    c '\'' ;
    for i = 0 to String.length s - 1 do
      match s.[i] with
      | '\n' -> invalid_arg "string value contains a newline character"
      | '\'' ->
          (* I couldn't find, in the documentation, whether we could escape single quotes. *)
          invalid_arg "string value contains a single quote character"
      | c -> Buffer.add_char buffer c
    done ;
    c '\''
  in
  s "SELECT " ;
  let write_column (column : column) =
    match column with
    | All -> c '*'
    | Field f ->
        id f ;
        s "::field"
    | Tag t ->
        id t ;
        s "::tag"
    | Function (f, arg) ->
        (match f with
        | COUNT -> s "COUNT("
        | DISTINCT -> s "DISTINCT("
        | INTEGRAL _ -> s "INTEGRAL("
        | MEAN -> s "MEAN("
        | MEDIAN -> s "MEDIAN("
        | MODE -> s "MODE("
        | SPREAD -> s "SPREAD("
        | STDDEV -> s "STDDEV("
        | SUM -> s "SUM(") ;
        (match arg with All -> c '*' | Field f -> id f) ;
        (match f with
        | INTEGRAL t ->
            s ", " ;
            write_time_interval t
        | _ -> ()) ;
        c ')'
  in
  comma_list write_column columns ;
  s " FROM " ;
  (match from with
  | Measurement m -> id m
  | Select s ->
      c '(' ;
      write_select ~grafana buffer s ;
      c ')') ;
  let rec write_predicate ~parentheses (predicate : where) =
    match predicate with
    | Tag (tag, operator, value) ->
        id tag ;
        (match operator with EQ -> s " = " | NE -> s " <> ") ;
        write_string_value value
    | Field (field, operator, value) -> (
        id field ;
        (match operator with
        | EQ -> s " = "
        | NE -> s " <> "
        | GT -> s " > "
        | GE -> s " >= "
        | LT -> s " < "
        | LE -> s " <= ") ;
        match value with
        | Float f -> s (string_of_float f)
        | String s -> write_string_value s)
    | Or (a, b) ->
        if parentheses then c '(' ;
        write_predicate ~parentheses:true a ;
        s " OR " ;
        write_predicate ~parentheses:true b ;
        if parentheses then c ')'
    | And (a, b) ->
        if parentheses then c '(' ;
        write_predicate ~parentheses:true a ;
        s " AND " ;
        write_predicate ~parentheses:true b ;
        if parentheses then c ')'
    | Grafana_time_filter ->
        if not grafana then
          invalid_arg "cannot perform queries that use Grafana_time_filter" ;
        s "$timeFilter"
  in
  let write_where where =
    s " WHERE " ;
    write_predicate where
  in
  Option.iter (write_where ~parentheses:false) where ;
  let write_group_by group_by =
    s " GROUP BY " ;
    match group_by with
    | All_tags -> c '*'
    | Tags list -> comma_list id list
    | Time {interval; tag; fill} ->
        s "time(" ;
        write_time_interval interval ;
        c ')' ;
        Option.iter
          (fun tag ->
            s ", " ;
            id tag)
          tag ;
        let write_fill (fill : fill) =
          s " fill(" ;
          (match fill with
          | Value f -> s (string_of_float f)
          | Linear -> s "linear"
          | F_none -> s "none"
          | Null -> s "null"
          | Previous -> s "previous") ;
          c ')'
        in
        Option.iter write_fill fill
  in
  Option.iter write_group_by group_by ;
  (match order_by with None -> () | Some Time_desc -> s " ORDER BY time DESC") ;
  (match limit with
  | None -> ()
  | Some i ->
      s " LIMIT " ;
      int i) ;
  (match slimit with
  | None -> ()
  | Some i ->
      s " SLIMIT " ;
      int i) ;
  ()

let show_select ?(grafana = false) select =
  with_buffer 256 @@ fun buffer -> write_select ~grafana buffer select

let rec prefix_measurement (V1_8 {measurement_prefix; _} as config) select =
  let from =
    match select.from with
    | Measurement m -> Measurement (measurement_prefix ^ m)
    | Select s -> Select (prefix_measurement config s)
  in
  {select with from}

(* Example response from the documentation of InfluxQL:
   {
       "results": [
           {
               "statement_id": 0,
               "series": [
                   {
                       "name": "cpu_load_short",
                       "columns": [
                           "time",
                           "value"
                       ],
                       "values": [
                           [
                               "2015-01-29T21:55:43.702900257Z",
                               2
                           ],
                           [
                               "2015-01-29T21:55:43.702900257Z",
                               0.55
                           ],
                           [
                               "2015-06-11T20:46:02Z",
                               0.64
                           ]
                       ]
                   }
               ]
           }
       ]
   }
*)

type result_data_point = JSON.t String_map.t

let show_result_data_point (data : result_data_point) =
  JSON.encode_u
    (`O
       (String_map.bindings data
       |> List.map (fun (k, v) -> (k, JSON.unannotate v))))

type query_result_series = {name : string; values : result_data_point list}

let as_query_result_series json =
  let columns = JSON.(json |-> "columns" |> as_list |> List.map as_string) in
  let as_map json =
    let values = JSON.as_list json in
    match List.combine columns values with
    | exception Invalid_argument _ ->
        failwith
          (sf
             "invalid InfluxDB response: a value contains %d columns instead \
              of the expected %d"
             (List.length values)
             (List.length columns))
    | combined_list ->
        List.fold_left
          (fun acc (k, v) -> String_map.add k v acc)
          String_map.empty
          combined_list
  in
  let values = JSON.(json |-> "values" |> as_list |> List.map as_map) in
  {name = JSON.(json |-> "name" |> as_string); values}

type query_result = {statement_id : int; series : query_result_series list}

let as_query_result json =
  {
    statement_id = JSON.(json |-> "statement_id" |> as_int);
    series =
      JSON.(json |-> "series" |> as_list |> List.map as_query_result_series);
  }

let as_query_results json =
  JSON.(json |-> "results" |> as_list |> List.map as_query_result)

let raw_query config select =
  let select = prefix_measurement config select in
  let query = show_select select in
  let url = Uri.add_query_param' (make_url config "query") ("q", query) in
  let* response, body =
    with_timeout config @@ Cohttp_lwt_unix.Client.call `GET url
  in
  let* body = Cohttp_lwt.Body.to_string body in
  match response.status with
  | #Cohttp.Code.success_status -> (
      let json = JSON.parse ~origin:"InfluxDB query response" body in
      let results = json |> as_query_results in
      match results with
      | [] -> JSON.error json "InfluxDB response contains no results"
      | [result] -> return (List.map (fun x -> x.values) result.series)
      | _ :: _ :: _ ->
          JSON.error json "InfluxDB response contains multiple results")
  | status ->
      failwith
        (sf
           "InfluxDB responded with %s - %s"
           (Cohttp.Code.string_of_status status)
           body)

let get column convert_json (result_data_point : result_data_point) =
  (* TODO: in the error message, list existing columns, and also log the query? *)
  match String_map.find_opt column result_data_point with
  | None -> failwith (sf "missing column in InfluxDB response: %s" column)
  | Some json -> convert_json json

(* Wrapper over [raw_query] to handle nested queries with aggregation functions,
   as it looks like they are not working correctly in InfluxDB (at least in v1.8). *)
let query config select =
  let supported_aggregate_functions =
    let exception Not_supported in
    let supported_aggregate_function = function
      | Function (COUNT, _) -> `count
      | Function (MEAN, Field f) -> `mean f
      | Function (MEDIAN, Field f) -> `median f
      | Function (STDDEV, Field f) -> `stddev f
      | _ -> raise Not_supported
    in
    try Some (List.map supported_aggregate_function select.columns)
    with Not_supported -> None
  in
  match (select.from, supported_aggregate_functions) with
  | Select sub_query, Some functions ->
      let* sub_query_results = raw_query config sub_query in
      let aggregate (results : result_data_point list) : result_data_point =
        let get_field field = List.map (get field JSON.as_float) results in
        let compute_function acc = function
          | `count ->
              String_map.add
                (column_name_of_func COUNT)
                (float (List.length results))
                acc
          | `mean field ->
              String_map.add
                (column_name_of_func MEAN)
                (Statistics.mean (get_field field))
                acc
          | `median field ->
              String_map.add
                (column_name_of_func MEDIAN)
                (Statistics.median (get_field field))
                acc
          | `stddev field ->
              String_map.add
                (column_name_of_func STDDEV)
                (Statistics.stddev (get_field field))
                acc
        in
        let floats =
          List.fold_left compute_function String_map.empty functions
        in
        String_map.map
          (fun float ->
            JSON.annotate ~origin:"InfluxDB.query aggregator" (`Float float))
          floats
      in
      return (List.map aggregate sub_query_results |> List.map (fun x -> [x]))
  | _ -> raw_query config select

let show_query_result = function
  | [] -> "No results."
  | result ->
      with_buffer 1024 @@ fun buffer ->
      let add_series i series =
        Buffer.add_string buffer (sf "Result series #%d:" i) ;
        let add_data_point i data_point =
          Buffer.add_char buffer '\n' ;
          Buffer.add_string
            buffer
            (sf "#%d: %s" i (show_result_data_point data_point))
        in
        List.iteri add_data_point series
      in
      List.iteri add_series result
