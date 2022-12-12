(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Prometheus

let sc_rollup_node_registry = CollectorRegistry.create ()

module Cohttp (Server : Cohttp_lwt.S.Server) = struct
  let callback _conn req _body =
    let open Cohttp in
    let open Lwt_syntax in
    let uri = Request.uri req in
    match (Request.meth req, Uri.path uri) with
    | `GET, "/metrics" ->
        let* data = CollectorRegistry.(collect sc_rollup_node_registry) in
        let body =
          Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data
        in
        let headers =
          Header.init_with "Content-Type" "text/plain; version=0.0.4"
        in
        Server.respond_string ~status:`OK ~headers ~body ()
    | _ -> Server.respond_error ~status:`Bad_request ~body:"Bad request" ()
end

module Metrics_server = Cohttp (Cohttp_lwt_unix.Server)

let metrics_serve metrics_addr =
  let open Lwt_result_syntax in
  match metrics_addr with
  | Some metrics_addr ->
      let* addrs =
        Octez_node_config.Config_file.resolve_metrics_addrs
          ~default_metrics_port:Configuration.default_metrics_port
          metrics_addr
      in
      let*! () =
        List.iter_p
          (fun (addr, port) ->
            let host = Ipaddr.V6.to_string addr in
            let*! () = Event.starting_metrics_server ~host ~port in
            let*! ctx = Conduit_lwt_unix.init ~src:host () in
            let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
            let mode = `TCP (`Port port) in
            let callback = Metrics_server.callback in
            Cohttp_lwt_unix.Server.create
              ~ctx
              ~mode
              (Cohttp_lwt_unix.Server.make ~callback ()))
          addrs
      in
      return_unit
  | None -> return_unit

let metric_type_to_string = function
  | Counter -> "Counter"
  | Gauge -> "Gauge"
  | Summary -> "Summary"
  | Histogram -> "Histogram"

let pp_label_names fmt =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
    (fun fmt v -> Format.fprintf fmt "%a" LabelName.pp v)
    fmt

let print_csv_metrics ppf metrics =
  let open Prometheus in
  Format.fprintf ppf "@[<v>Name,Type,Description,Labels" ;
  List.iter
    (fun (v, _) ->
      Format.fprintf
        ppf
        "@,@[%a@],%s,\"%s\",%a"
        MetricName.pp
        v.MetricInfo.name
        (metric_type_to_string v.MetricInfo.metric_type)
        v.MetricInfo.help
        pp_label_names
        v.MetricInfo.label_names)
    (Prometheus.MetricFamilyMap.to_list metrics) ;
  Format.fprintf ppf "@]@."
