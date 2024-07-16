open! Astring
open Prometheus
open Prometheus_app

open Lwt.Infix

let test_metrics () =
  let registry = CollectorRegistry.create () in
  let requests =
    let label_names = ["method"; "path"] in
    Counter.v_labels ~label_names ~registry ~help:"Requests" ~namespace:"dkci" ~subsystem:"tests" "requests" in
  let m = Counter.v ~registry ~help:"Test \\counter:\n1" "tests" in
  Counter.inc_one m;
  let get_index = Counter.labels requests ["GET"; "\"\\-\n"] in
  let post_login = Counter.labels requests ["POST"; "/login"] in
  Counter.inc get_index 5.;
  Counter.inc post_login 2.;
  let post_login2 = Counter.labels requests ["POST"; "/login"] in
  Counter.inc_one post_login2;
  CollectorRegistry.collect registry >|= fun collected ->
  let output = Fmt.to_to_string TextFormat_0_0_4.output collected in
  Alcotest.(check string) "Text output"
    "#HELP dkci_tests_requests Requests\n\
     #TYPE dkci_tests_requests counter\n\
     dkci_tests_requests{method=\"GET\", path=\"\\\"\\\\-\\n\"} 5.000000\n\
     dkci_tests_requests{method=\"POST\", path=\"/login\"} 3.000000\n\
     #HELP tests Test \\\\counter:\\n1\n\
     #TYPE tests counter\n\
     tests 1.000000\n\
    "
    output

let test_lwt_collectors () =
  let registry = CollectorRegistry.create () in
  let register_counter ~name ~help value =
    let metric_info = {
      MetricInfo.name = MetricName.v name;
      metric_type = Counter;
      help;
      label_names = []
    }
    in
    let collector () =
      Lwt.pause () >|= fun () ->
      LabelSetMap.singleton [] [Prometheus.Sample_set.sample value]
    in
    CollectorRegistry.register_lwt registry metric_info collector
  in
  (* Test register_lwt *)
  register_counter ~name:"counter_1" ~help:"The first counter" 1.0;
  register_counter ~name:"counter_2" ~help:"The second counter" 2.0;
  CollectorRegistry.collect registry >|= fun collected ->
  let output = Fmt.to_to_string TextFormat_0_0_4.output collected in
  Alcotest.(check string) "Text output"
    "#HELP counter_1 The first counter\n\
     #TYPE counter_1 counter\n\
     counter_1 1.000000\n\
     #HELP counter_2 The second counter\n\
     #TYPE counter_2 counter\n\
     counter_2 2.000000\n"
    output

module Buckets = struct
  let spec = Histogram_spec.of_list [0.25; 0.5]
end

module H = Histogram (Buckets)

let test_histogram () =
  let registry = CollectorRegistry.create () in
  let requests =
    let label_names = ["method"; "path"] in
    H.v_labels ~label_names ~registry ~help:"Requests" ~namespace:"dkci" ~subsystem:"tests" "requests" in
  let foo = H.labels requests ["GET"; "/foo"] in
  let bar = H.labels requests ["PUT"; "/bar"] in
  H.observe foo 0.12;
  H.observe bar 0.33;
  CollectorRegistry.collect registry >|= fun collected ->
  let output = Fmt.to_to_string TextFormat_0_0_4.output collected in
  Alcotest.(check string) "Text output"
    "#HELP dkci_tests_requests Requests\n\
     #TYPE dkci_tests_requests histogram\n\
     dkci_tests_requests_sum{method=\"GET\", path=\"/foo\"} 0.120000\n\
     dkci_tests_requests_count{method=\"GET\", path=\"/foo\"} 1.000000\n\
     dkci_tests_requests_bucket{le=\"+Inf\", method=\"GET\", path=\"/foo\"} 1.000000\n\
     dkci_tests_requests_bucket{le=\"0.500000\", method=\"GET\", path=\"/foo\"} 1.000000\n\
     dkci_tests_requests_bucket{le=\"0.250000\", method=\"GET\", path=\"/foo\"} 1.000000\n\
     dkci_tests_requests_sum{method=\"PUT\", path=\"/bar\"} 0.330000\n\
     dkci_tests_requests_count{method=\"PUT\", path=\"/bar\"} 1.000000\n\
     dkci_tests_requests_bucket{le=\"+Inf\", method=\"PUT\", path=\"/bar\"} 1.000000\n\
     dkci_tests_requests_bucket{le=\"0.500000\", method=\"PUT\", path=\"/bar\"} 1.000000\n\
     dkci_tests_requests_bucket{le=\"0.250000\", method=\"PUT\", path=\"/bar\"} 0.000000\n\
    "
    output

(* "^[a-zA-Z_][a-zA-Z0-9_]*$" *)
let valid_labels = [
  "_";
  "a";
  "aA0b1B9c8C7z6Z5y4Y3x2X1";
  "_______";
]

let invalid_labels = [
  "";
  "1";
  "a bad label";
]

let check_valid_label label () =
  let _l: LabelName.t = LabelName.v label in
  ()

let check_invalid_label label () =
  let valid =
    try
      let _l: LabelName.t = LabelName.v label in
      true
    with _ -> false in
  if valid then
    failwith (label ^ " should be an invalid label")

let test_valid_labels_set = List.map (fun label ->
  label, `Quick, fun () -> Lwt.return @@ check_valid_label label ()
) valid_labels

let test_invalid_labels_set = List.map (fun label ->
  label, `Quick, fun () -> Lwt.return @@ check_invalid_label label ()
) invalid_labels

let check_valid_metric metric () =
  let _m: MetricName.t = MetricName.v metric in
  ()

let check_invalid_metric metric () =
  let valid =
    try
      let _m: MetricName.t = MetricName.v metric in
      true
    with _ -> false in
  if valid then
    failwith (metric ^ " should be an invalid metric")

(* "^[a-zA-Z_:][a-zA-Z0-9_:]*$"  *)
let valid_metrics = [
  "_";
  ":";
  "aA0b1B9c8C7z6Z5y4Y3x:2X1";
  ":::::::";
]

let invalid_metrics = [
  "";
  "1";
  "a bad metric";
]

let test_valid_metrics_set = List.map (fun metric ->
  metric, `Quick, fun () -> Lwt.return @@ check_valid_metric metric ()
) valid_metrics

let test_invalid_metrics_set = List.map (fun metric ->
  metric, `Quick, fun () -> Lwt.return @@ check_invalid_metric metric ()
) invalid_metrics

let test_set = [
  "Metrics", `Quick, test_metrics;
  "Lwt collectors",`Quick, test_lwt_collectors;
  "Histogram", `Quick, test_histogram;
]

let () =
  Lwt_main.run @@ Alcotest_lwt.run "prometheus" [
    "main", test_set;
    "valid_labels", test_valid_labels_set;
    "invalid_labels", test_invalid_labels_set;
    "valid_metrics", test_valid_metrics_set;
    "invalid_metrics", test_invalid_metrics_set;
  ]
