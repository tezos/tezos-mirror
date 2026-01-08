(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let initialize ?unique_identifier service_name =
  let service_name =
    match unique_identifier with
    | None -> service_name
    | Some id -> Format.sprintf "%s-%s" service_name id
  in
  Opentelemetry.Globals.service_name := service_name ;
  Opentelemetry.GC_metrics.basic_setup () ;
  Ambient_context.set_storage_provider (Ambient_context_lwt.storage ()) ;
  Opentelemetry_client_cohttp_lwt.setup ()

let op_hash_to_trace_id op_hash =
  (* A trace id must be of 16 bytes. We use the first 16 bytes of an operation
     hash to make it replicable and unique enough. *)
  Bytes.sub (Tezos_crypto.Hashed.Operation_hash.to_bytes op_hash) 0 16
  |> Opentelemetry.Trace_id.of_bytes

let add_event ?attrs name =
  Opentelemetry.(
    Scope.get_ambient_scope ()
    |> Option.iter @@ fun scope ->
       Scope.add_event scope @@ fun () -> Event.make ?attrs name)

let add_attribute attrs id value =
  match attrs with None -> [(id, value)] | Some attrs -> (id, value) :: attrs

let trace ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind ?trace_id
    ?parent ?scope ?links name k =
  Opentelemetry.Trace.with_
    ?force_new_trace_id
    ?trace_state
    ?service_name
    ?attrs
    ?kind
    ?trace_id
    ?parent
    ?scope
    ?links
    name
  @@ fun _scope -> k ()

let trace_operation op ?attrs =
  let op_hash =
    match op with `Operation op -> Operation.hash op | `Hash oph -> oph
  in
  let attrs =
    add_attribute
      attrs
      "operation_hash"
      (`String (Tezos_crypto.Hashed.Operation_hash.to_b58check op_hash))
  in
  trace ~trace_id:(op_hash_to_trace_id op_hash) ~attrs

let update_scope s f =
  match s with
  | Some s ->
      Opentelemetry_ambient_context.with_binding
        Opentelemetry.Scope.ambient_scope_key
        s
        f
  | None -> f ()

type config = {service_name : string; verbosity : Profiler.verbosity}

type state = State

type (_, _) Profiler.kind +=
  | Opentelemetry_profiler : (config, state) Profiler.kind

module Driver : Profiler.DRIVER with type config = config = struct
  type nonrec config = config

  type nonrec state = state

  let kind = Opentelemetry_profiler

  let encoding_case =
    Data_encoding.(
      case
        Json_only
        ~title:"opentelemetry"
        ~description:"Opentelemetry driver"
        (constant "opentelemetry")
        (function Profiler.View Opentelemetry_profiler -> Some () | _ -> None)
        (fun () -> Profiler.View Opentelemetry_profiler))

  let create _ = State

  let time ~cpu:_ _ = Simple_profiler.time ~cpu:None ()

  let record ~cpu:_ _ _ _ = ()

  let aggregate ~cpu:_ _ _ _ = ()

  let stop _ = ()

  let stamp ~cpu:_ _ _ _ = ()

  let mark _ _ _ = ()

  let span ~cpu:_ _ _ _ _ = ()

  let inc _ _ = ()

  let report ~cpu:_ _ = None

  let close _ = ()
end

let opentelemetry : config Profiler.driver =
  (module Driver : Profiler.DRIVER with type config = config)

let instance_maker driver ~verbosity ~directory:_ ~name =
  Profiler.instance driver {verbosity; service_name = name}

let () =
  Profiler_instance.register_backend
    ["opentelemetry"]
    instance_maker
    opentelemetry
