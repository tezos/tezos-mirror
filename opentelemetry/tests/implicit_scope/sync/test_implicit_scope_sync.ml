open Alcotest
module Otel = Opentelemetry

let spans_emitted : Otel.Proto.Trace.resource_spans list ref = ref []

module Test_backend = struct
  open Otel.Collector
  open Otel.Proto
  include Noop_backend

  let record_emitted_spans (l : Trace.resource_spans list) ~ret =
    spans_emitted := l @ !spans_emitted;
    ret ()

  let send_trace : Trace.resource_spans list sender =
    { send = record_emitted_spans }
end

let with_test_backend f =
  (* uncomment for eprintf debugging: *)
  (* let module Debug_and_test_backend = Otel.Collector.Debug_backend (Test_backend) in
     let backend = (module Debug_and_test_backend : Otel.Collector.BACKEND) in *)
  let backend = (module Test_backend : Otel.Collector.BACKEND) in
  Otel.Collector.with_setup_debug_backend backend () f

let bytes_to_hex = Otel.Util_.bytes_to_hex

let test_stack_based_implicit_scope () =
  let run () =
    Otel.Trace.with_ "first trace" @@ fun _scope ->
    Thread.delay 0.2;
    Otel.Trace.with_ "second trace" @@ fun _scope ->
    Thread.delay 0.2;
    Otel.Trace.with_ "third trace" @@ fun _scope ->
    Thread.delay 0.2;
    ()
  in
  with_test_backend @@ fun () ->
  (* start *)
  run ();
  check' int ~msg:"count of spans emitted"
    ~actual:(List.length !spans_emitted)
    ~expected:3;
  let open Otel.Proto.Trace in
  let f prev_span_id { scope_spans; _ } =
    Format.printf "\n%a@\n" (Format.pp_print_list pp_scope_spans) scope_spans;
    check' int ~msg:"count of scope_spans in emitted span"
      ~actual:(List.length scope_spans) ~expected:1;
    let { scope; spans; _ } = List.hd scope_spans in
    check' bool ~msg:"scope exists in emitted span"
      ~actual:(Option.is_some scope) ~expected:true;
    check' int ~msg:"count of spans in scope_span" ~actual:(List.length spans)
      ~expected:1;
    let { name; trace_id; span_id; parent_span_id; _ } = List.hd spans in
    Printf.printf
      "name='%s' trace_id='%s' span_id='%s' parent_span_id='%s' \
       prev_span_id='%s'\n"
      name (bytes_to_hex trace_id) (bytes_to_hex span_id)
      (bytes_to_hex parent_span_id)
      (bytes_to_hex prev_span_id);
    check' string ~msg:"previous span is parent"
      ~actual:(bytes_to_hex parent_span_id)
      ~expected:(bytes_to_hex prev_span_id);
    span_id
  in
  List.fold_left f (Bytes.of_string "") !spans_emitted |> ignore

let suite =
  [
    test_case "stack-based implicit scope" `Quick
      test_stack_based_implicit_scope;
  ]

let () = Alcotest.run "implicit scope" [ "sync", suite ]
