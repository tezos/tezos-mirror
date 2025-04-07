open Opentelemetry

let pp_traceparent fmt (trace_id, parent_id) =
  let open Format in
  fprintf fmt "trace_id:%S parent_id:%S" (Trace_id.to_hex trace_id)
    (Span_id.to_hex parent_id)

let test_of_value str =
  let open Format in
  printf "@[<v 2>Trace_context.Traceparent.of_value %S:@ %a@]@." str
    (pp_print_result
       ~ok:(fun fmt (trace_id, parent_id) ->
         fprintf fmt "Ok %a" pp_traceparent (trace_id, parent_id))
       ~error:(fun fmt msg -> fprintf fmt "Error %S" msg))
    (Trace_context.Traceparent.of_value str)

let () = test_of_value "xx"

let () = test_of_value "00"

let () = test_of_value "00-xxxx"

let () = test_of_value "00-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

let () = test_of_value "00-0123456789abcdef0123456789abcdef"

let () = test_of_value "00-0123456789abcdef0123456789abcdef-xxxx"

let () = test_of_value "00-0123456789abcdef0123456789abcdef-xxxxxxxxxxxxxxxx"

let () = test_of_value "00-0123456789abcdef0123456789abcdef-0123456789abcdef"

let () = test_of_value "00-0123456789abcdef0123456789abcdef-0123456789abcdef-"

let () = test_of_value "00-0123456789abcdef0123456789abcdef-0123456789abcdef-00"

let () = test_of_value "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"

let () = test_of_value "03-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"

let () = test_of_value "00-ohnonohex7b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"

let () = test_of_value "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aazzzzzzb7-01"

let () = print_endline ""

let test_to_value trace_id parent_id =
  let open Format in
  printf "@[<v 2>Trace_context.Traceparent.to_value %a:@ %S@]@." pp_traceparent
    (trace_id, parent_id)
    (Trace_context.Traceparent.to_value ~trace_id ~parent_id ())

let () =
  test_to_value
    (Trace_id.of_hex "4bf92f3577b34da6a3ce929d0e0e4736")
    (Span_id.of_hex "00f067aa0ba902b7")
