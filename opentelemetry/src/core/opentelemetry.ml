(** Opentelemetry types and instrumentation *)

open struct
  let spf = Printf.sprintf

  module Ambient_context = Opentelemetry_ambient_context
end

module Lock = Lock
(** Global lock. *)

module Rand_bytes = Rand_bytes
(** Generation of random identifiers. *)

module AList = AList
(** Atomic list, for internal usage
    @since 0.7 *)

(** {2 Wire format} *)

module Proto = Opentelemetry_proto
(** Protobuf types.

   This is mostly useful internally. Users should not need to touch it. *)

(** {2 Timestamps} *)

(** Unix timestamp.

    These timestamps measure time since the Unix epoch (jan 1, 1970) UTC
    in nanoseconds. *)
module Timestamp_ns = struct
  type t = int64

  let ns_in_a_day = Int64.(mul 1_000_000_000L (of_int (24 * 3600)))

  (** Current unix timestamp in nanoseconds *)
  let[@inline] now_unix_ns () : t =
    let span = Ptime_clock.now () |> Ptime.to_span in
    let d, ps = Ptime.Span.to_d_ps span in
    let d = Int64.(mul (of_int d) ns_in_a_day) in
    let ns = Int64.(div ps 1_000L) in
    Int64.(add d ns)
end

(** {2 Interface to data collector} *)

(** Collector types

    These types are used by backend implementations, to send events to
    collectors such as Jaeger.

    Note: most users will not need to touch this module *)
module Collector = struct
  open Opentelemetry_proto

  type 'msg sender = { send: 'a. 'msg -> ret:(unit -> 'a) -> 'a }
  (** Sender interface for a message of type [msg].
      Inspired from Logs' reporter
      (see {{:https://erratique.ch/software/logs/doc/Logs/index.html#sync} its doc})
      but without [over] as it doesn't make much sense in presence
      of batching.

      The [ret] callback is used to return the desired type (unit, or
      a Lwt promise, or anything else) once the event has been transferred
      to the backend.
      It doesn't mean the event has been collected yet, it
      could sit in a batch queue for a little while.
  *)

  (** Collector client interface. *)
  module type BACKEND = sig
    val send_trace : Trace.resource_spans list sender

    val send_metrics : Metrics.resource_metrics list sender

    val send_logs : Logs.resource_logs list sender

    val signal_emit_gc_metrics : unit -> unit
    (** Signal the backend that it should emit GC metrics when it has the
        chance. This should be installed in a GC alarm or another form
        of regular trigger. *)

    val tick : unit -> unit
    (** Should be called regularly for background processing,
        timeout checks, etc. *)

    val set_on_tick_callbacks : (unit -> unit) AList.t -> unit
    (** Give the collector the list of callbacks to be executed
        when [tick()] is called. Each such callback should be short and
        reentrant. Depending on the collector's implementation, it might be
        called from a thread that is not the one that called [on_tick]. *)

    val cleanup : unit -> unit
  end

  type backend = (module BACKEND)

  module Noop_backend : BACKEND = struct
    let noop_sender _ ~ret = ret ()

    let send_trace : Trace.resource_spans list sender = { send = noop_sender }

    let send_metrics : Metrics.resource_metrics list sender =
      { send = noop_sender }

    let send_logs : Logs.resource_logs list sender = { send = noop_sender }

    let signal_emit_gc_metrics () = ()

    let tick () = ()

    let set_on_tick_callbacks _cbs = ()

    let cleanup () = ()
  end

  module Debug_backend (B : BACKEND) : BACKEND = struct
    open Proto

    let send_trace : Trace.resource_spans list sender =
      {
        send =
          (fun l ~ret ->
            Format.eprintf "SPANS: %a@."
              (Format.pp_print_list Trace.pp_resource_spans)
              l;
            B.send_trace.send l ~ret);
      }

    let send_metrics : Metrics.resource_metrics list sender =
      {
        send =
          (fun l ~ret ->
            Format.eprintf "METRICS: %a@."
              (Format.pp_print_list Metrics.pp_resource_metrics)
              l;
            B.send_metrics.send l ~ret);
      }

    let send_logs : Logs.resource_logs list sender =
      {
        send =
          (fun l ~ret ->
            Format.eprintf "LOGS: %a@."
              (Format.pp_print_list Logs.pp_resource_logs)
              l;
            B.send_logs.send l ~ret);
      }

    let signal_emit_gc_metrics () = B.signal_emit_gc_metrics ()

    let tick () = B.tick ()

    let set_on_tick_callbacks cbs = B.set_on_tick_callbacks cbs

    let cleanup () = B.cleanup ()
  end

  let debug_backend : backend = (module Debug_backend (Noop_backend))

  (* hidden *)
  open struct
    let on_tick_cbs_ = AList.make ()

    let backend : backend option ref = ref None
  end

  (** Set collector backend *)
  let set_backend (b : backend) : unit =
    let (module B) = b in
    B.set_on_tick_callbacks on_tick_cbs_;
    backend := Some b

  (** Is there a configured backend? *)
  let[@inline] has_backend () : bool = !backend != None

  (** Current backend, if any *)
  let[@inline] get_backend () : backend option = !backend

  let send_trace (l : Trace.resource_spans list) ~ret =
    match !backend with
    | None -> ret ()
    | Some (module B) -> B.send_trace.send l ~ret

  let send_metrics (l : Metrics.resource_metrics list) ~ret =
    match !backend with
    | None -> ret ()
    | Some (module B) -> B.send_metrics.send l ~ret

  let send_logs (l : Logs.resource_logs list) ~ret =
    match !backend with
    | None -> ret ()
    | Some (module B) -> B.send_logs.send l ~ret

  let[@inline] rand_bytes_16 () = !Rand_bytes.rand_bytes_16 ()

  let[@inline] rand_bytes_8 () = !Rand_bytes.rand_bytes_8 ()

  let[@inline] on_tick f = AList.add on_tick_cbs_ f

  (** Do background work. Call this regularly if the collector doesn't
      already have a ticker thread or internal timer. *)
  let tick () =
    match !backend with
    | None -> ()
    | Some (module B) -> B.tick ()

  let with_setup_debug_backend b ?(enable = true) () f =
    let (module B : BACKEND) = b in
    if enable then (
      set_backend b;
      Fun.protect ~finally:B.cleanup f
    ) else
      f ()
end

(**/**)

module Util_ = struct
  let int_to_hex (i : int) =
    if i < 10 then
      Char.chr (i + Char.code '0')
    else
      Char.chr (i - 10 + Char.code 'a')

  let bytes_to_hex_into b res off : unit =
    for i = 0 to Bytes.length b - 1 do
      let n = Char.code (Bytes.get b i) in
      Bytes.set res ((2 * i) + off) (int_to_hex ((n land 0xf0) lsr 4));
      Bytes.set res ((2 * i) + 1 + off) (int_to_hex (n land 0x0f))
    done

  let bytes_to_hex (b : bytes) : string =
    let res = Bytes.create (2 * Bytes.length b) in
    bytes_to_hex_into b res 0;
    Bytes.unsafe_to_string res

  let int_of_hex = function
    | '0' .. '9' as c -> Char.code c - Char.code '0'
    | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
    | c -> raise (Invalid_argument (spf "invalid hex char: %C" c))

  let bytes_of_hex_substring (s : string) off len =
    if len mod 2 <> 0 then
      raise (Invalid_argument "hex sequence must be of even length");
    let res = Bytes.make (len / 2) '\x00' in
    for i = 0 to (len / 2) - 1 do
      let n1 = int_of_hex (String.get s (off + (2 * i))) in
      let n2 = int_of_hex (String.get s (off + (2 * i) + 1)) in
      let n = (n1 lsl 4) lor n2 in
      Bytes.set res i (Char.chr n)
    done;
    res

  let bytes_of_hex (s : string) : bytes =
    bytes_of_hex_substring s 0 (String.length s)

  let bytes_non_zero (self : bytes) : bool =
    try
      for i = 0 to Bytes.length self - 1 do
        if Char.code (Bytes.unsafe_get self i) <> 0 then raise_notrace Exit
      done;
      false
    with Exit -> true
end

(**/**)

(** {2 Identifiers} *)

(** Trace ID.

    This 16 bytes identifier is shared by all spans in one trace. *)
module Trace_id : sig
  type t

  val create : unit -> t

  val dummy : t

  val pp : Format.formatter -> t -> unit

  val is_valid : t -> bool

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t

  val to_hex : t -> string

  val to_hex_into : t -> bytes -> int -> unit

  val of_hex : string -> t

  val of_hex_substring : string -> int -> t
end = struct
  type t = bytes

  let to_bytes self = self

  let dummy : t = Bytes.make 16 '\x00'

  let create () : t =
    let b = Collector.rand_bytes_16 () in
    assert (Bytes.length b = 16);
    (* make sure the identifier is not all 0, which is a dummy identifier. *)
    Bytes.set b 0 (Char.unsafe_chr (Char.code (Bytes.get b 0) lor 1));
    b

  let of_bytes b =
    if Bytes.length b = 16 then
      b
    else
      raise (Invalid_argument "trace ID must be 16 bytes in length")

  let is_valid = Util_.bytes_non_zero

  let to_hex = Util_.bytes_to_hex

  let to_hex_into = Util_.bytes_to_hex_into

  let[@inline] of_hex s = of_bytes (Util_.bytes_of_hex s)

  let[@inline] of_hex_substring s off =
    of_bytes (Util_.bytes_of_hex_substring s off 32)

  let pp fmt t = Format.fprintf fmt "%s" (to_hex t)
end

(** Hmap key to carry around a {!Trace_id.t}, to remember what the current
    trace is.
    @since 0.8 *)
let k_trace_id : Trace_id.t Hmap.key = Hmap.Key.create ()

(** Unique ID of a span. *)
module Span_id : sig
  type t

  val create : unit -> t

  val dummy : t

  val pp : Format.formatter -> t -> unit

  val is_valid : t -> bool

  val to_bytes : t -> bytes

  val of_bytes : bytes -> t

  val to_hex : t -> string

  val to_hex_into : t -> bytes -> int -> unit

  val of_hex : string -> t

  val of_hex_substring : string -> int -> t
end = struct
  type t = bytes

  let to_bytes self = self

  let dummy : t = Bytes.make 8 '\x00'

  let create () : t =
    let b = Collector.rand_bytes_8 () in
    assert (Bytes.length b = 8);
    (* make sure the identifier is not all 0, which is a dummy identifier. *)
    Bytes.set b 0 (Char.unsafe_chr (Char.code (Bytes.get b 0) lor 1));
    b

  let is_valid = Util_.bytes_non_zero

  let of_bytes b =
    if Bytes.length b = 8 then
      b
    else
      raise (Invalid_argument "span IDs must be 8 bytes in length")

  let to_hex = Util_.bytes_to_hex

  let to_hex_into = Util_.bytes_to_hex_into

  let[@inline] of_hex s = of_bytes (Util_.bytes_of_hex s)

  let[@inline] of_hex_substring s off =
    of_bytes (Util_.bytes_of_hex_substring s off 16)

  let pp fmt t = Format.fprintf fmt "%s" (to_hex t)
end

(** Span context. This bundles up a trace ID and parent ID.

    {{: https://opentelemetry.io/docs/specs/otel/trace/api/#spancontext} https://opentelemetry.io/docs/specs/otel/trace/api/#spancontext}
    @since 0.7 *)
module Span_ctx : sig
  type t

  val make : trace_id:Trace_id.t -> parent_id:Span_id.t -> unit -> t

  val dummy : t
  (** Invalid span context, to be used as a placeholder *)

  val is_valid : t -> bool

  val trace_id : t -> Trace_id.t

  val parent_id : t -> Span_id.t

  val is_remote : t -> bool

  val to_w3c_trace_context : t -> bytes

  val of_w3c_trace_context : bytes -> (t, string) result

  val of_w3c_trace_context_exn : bytes -> t
  (** @raise Invalid_argument if parsing failed *)
end = struct
  (* TODO: trace flags *)
  (* TODO: trace state *)

  type t = {
    trace_id: Trace_id.t;
    parent_id: Span_id.t;
    is_remote: bool;
  }

  let dummy =
    { trace_id = Trace_id.dummy; parent_id = Span_id.dummy; is_remote = false }

  let make ~trace_id ~parent_id () : t =
    { trace_id; parent_id; is_remote = false }

  let[@inline] is_valid self =
    Trace_id.is_valid self.trace_id && Span_id.is_valid self.parent_id

  let[@inline] is_remote self = self.is_remote

  let[@inline] trace_id self = self.trace_id

  let[@inline] parent_id self = self.parent_id

  let to_w3c_trace_context (self : t) : bytes =
    let bs = Bytes.create 55 in
    Bytes.set bs 0 '0';
    Bytes.set bs 1 '0';
    Bytes.set bs 2 '-';
    Trace_id.to_hex_into self.trace_id bs 3;
    (* +32 *)
    Bytes.set bs (3 + 32) '-';
    Span_id.to_hex_into self.parent_id bs 36;
    (* +16 *)
    Bytes.set bs 52 '-';
    Bytes.set bs 53 '0';
    Bytes.set bs 54 '0';
    bs

  let of_w3c_trace_context bs : _ result =
    try
      if Bytes.length bs <> 55 then invalid_arg "trace context must be 55 bytes";
      (match int_of_string_opt (Bytes.sub_string bs 0 2) with
      | Some 0 -> ()
      | Some n -> invalid_arg @@ spf "version is %d, expected 0" n
      | None -> invalid_arg "expected 2-digit version");
      if Bytes.get bs 2 <> '-' then invalid_arg "expected '-' before trace_id";
      let trace_id =
        try Trace_id.of_hex_substring (Bytes.unsafe_to_string bs) 3
        with Invalid_argument msg -> invalid_arg (spf "in trace id: %s" msg)
      in
      if Bytes.get bs (3 + 32) <> '-' then
        invalid_arg "expected '-' before parent_id";
      let parent_id =
        try Span_id.of_hex_substring (Bytes.unsafe_to_string bs) 36
        with Invalid_argument msg -> invalid_arg (spf "in span id: %s" msg)
      in
      if Bytes.get bs 52 <> '-' then invalid_arg "expected '-' after parent_id";

      (* ignore flags *)
      Ok { trace_id; parent_id; is_remote = true }
    with Invalid_argument msg -> Error msg

  let of_w3c_trace_context_exn bs =
    match of_w3c_trace_context bs with
    | Ok t -> t
    | Error msg -> invalid_arg @@ spf "invalid w3c trace context: %s" msg
end

(** Hmap key to carry around a {!Span_ctx.t}, e.g. to remember what the current
    parent span is.
    @since 0.8 *)
let k_span_ctx : Span_ctx.t Hmap.key = Hmap.Key.create ()

(** {2 Attributes and conventions} *)

(** Semantic conventions

    {{: https://opentelemetry.io/docs/specs/semconv/} https://opentelemetry.io/docs/specs/semconv/} *)
module Conventions = struct
  module Attributes = struct
    module Process = struct
      module Runtime = struct
        let name = "process.runtime.name"

        let version = "process.runtime.version"

        let description = "process.runtime.description"
      end
    end

    (** https://opentelemetry.io/docs/specs/semconv/attributes-registry/code/ *)
    module Code = struct
      (** Int *)
      let column = "code.column"

      let filepath = "code.filepath"

      let function_ = "code.function"

      (** int *)
      let line = "code.lineno"

      let namespace = "code.namespace"

      let stacktrace = "code.stacktrace"
    end

    module Service = struct
      let name = "service.name"

      let namespace = "service.namespace"

      let instance_id = "service.instance.id"

      let version = "service.version"
    end

    module HTTP = struct
      let error_type = "error.type"

      let request_method = "http.request.method"

      let route = "http.route"

      let url_full = "url.full"

      (** HTTP status code, int *)
      let response_status_code = "http.response.status_code"

      let server_address = "server.address"

      let server_port = "server.port"

      (** http or https *)
      let url_scheme = "url.scheme"
    end

    (** https://github.com/open-telemetry/semantic-conventions/blob/main/docs/resource/host.md *)
    module Host = struct
      let id = "host.id"

      let name = "host.name"

      let type_ = "host.type"

      let arch = "host.arch"

      let ip = "host.ip"

      let mac = "host.mac"

      let image_id = "host.image.id"

      let image_name = "host.image.name"

      let image_version = "host.image.version"
    end
  end

  module Metrics = struct
    module Process = struct
      module Runtime = struct
        module Ocaml = struct
          module GC = struct
            let compactions = "process.runtime.ocaml.gc.compactions"

            let major_collections = "process.runtime.ocaml.gc.major_collections"

            let major_heap = "process.runtime.ocaml.gc.major_heap"

            let minor_allocated = "process.runtime.ocaml.gc.minor_allocated"

            let minor_collections = "process.runtime.ocaml.gc.minor_collections"
          end
        end
      end
    end

    (** https://opentelemetry.io/docs/specs/semconv/http/ *)
    module HTTP = struct
      module Server = struct
        let request_duration = "http.server.request.duration"

        let active_requests = "http.server.active_requests"

        (** Histogram *)
        let request_body_size = "http.server.request.body.size"

        (** Histogram *)
        let response_body_size = "http.server.response.body.size"
      end

      module Client = struct
        let request_duration = "http.client.request.duration"

        (** Histogram *)
        let request_body_size = "http.client.request.body.size"

        (** Histogram *)
        let response_body_size = "http.client.response.body.size"
      end
    end
  end
end

type value =
  [ `Int of int
  | `String of string
  | `Bool of bool
  | `Float of float
  | `None
  ]
(** A value in a key/value attribute *)

type key_value = string * value

open struct
  let _conv_value =
    let open Proto.Common in
    function
    | `Int i -> Some (Int_value (Int64.of_int i))
    | `String s -> Some (String_value s)
    | `Bool b -> Some (Bool_value b)
    | `Float f -> Some (Double_value f)
    | `None -> None

  let _conv_key_value (k, v) =
    let open Proto.Common in
    let value = _conv_value v in
    default_key_value ~key:k ~value ()
end

(** {2 Global settings} *)

(** Process-wide metadata, environment variables, etc. *)
module Globals = struct
  open Proto.Common

  (** Main service name metadata *)
  let service_name = ref "unknown_service"

  (** Namespace for the service *)
  let service_namespace = ref None

  (** Unique identifier for the service *)
  let service_instance_id = ref None

  let instrumentation_library =
    default_instrumentation_scope ~version:"%%VERSION_NUM%%" ~name:"ocaml-otel"
      ()

  (** Global attributes, initially set
      via OTEL_RESOURCE_ATTRIBUTES and modifiable
      by the user code. They will be attached to each outgoing metrics/traces. *)
  let global_attributes : key_value list ref =
    let parse_pair s =
      match String.split_on_char '=' s with
      | [ a; b ] -> default_key_value ~key:a ~value:(Some (String_value b)) ()
      | _ -> failwith (Printf.sprintf "invalid attribute: %S" s)
    in
    ref
    @@
    try
      Sys.getenv "OTEL_RESOURCE_ATTRIBUTES"
      |> String.split_on_char ',' |> List.map parse_pair
    with _ -> []

  (** Add a global attribute *)
  let add_global_attribute (key : string) (v : value) : unit =
    global_attributes := _conv_key_value (key, v) :: !global_attributes

  (* add global attributes to this list *)
  let merge_global_attributes_ into : _ list =
    let not_redundant kv = List.for_all (fun kv' -> kv.key <> kv'.key) into in
    List.rev_append (List.filter not_redundant !global_attributes) into

  (** Default span kind in {!Span.create}.
      This will be used in all spans that do not specify [~kind] explicitly;
      it is set to "internal", following directions from the [.proto] file.
      It can be convenient to set "client" or "server" uniformly in here.
      @since 0.4 *)
  let default_span_kind = ref Proto.Trace.Span_kind_internal

  let mk_attributes ?(service_name = !service_name) ?(attrs = []) () : _ list =
    let l = List.map _conv_key_value attrs in
    let l =
      default_key_value ~key:Conventions.Attributes.Service.name
        ~value:(Some (String_value service_name)) ()
      :: l
    in
    let l =
      match !service_instance_id with
      | None -> l
      | Some v ->
        default_key_value ~key:Conventions.Attributes.Service.instance_id
          ~value:(Some (String_value v)) ()
        :: l
    in
    let l =
      match !service_namespace with
      | None -> l
      | Some v ->
        default_key_value ~key:Conventions.Attributes.Service.namespace
          ~value:(Some (String_value v)) ()
        :: l
    in
    l |> merge_global_attributes_
end

(** {2 Traces and Spans} *)

(** Events.

    Events occur at a given time and can carry attributes. They always
    belong in a span. *)
module Event : sig
  open Proto.Trace

  type t = span_event

  val make :
    ?time_unix_nano:Timestamp_ns.t -> ?attrs:key_value list -> string -> t
end = struct
  open Proto.Trace

  type t = span_event

  let make ?(time_unix_nano = Timestamp_ns.now_unix_ns ()) ?(attrs = [])
      (name : string) : t =
    let attrs = List.map _conv_key_value attrs in
    default_span_event ~time_unix_nano ~name ~attributes:attrs ()
end

(** {2 Scopes} *)

(** Scopes.

    A scope is a trace ID and the span ID of the currently active span.
*)
module Scope = struct
  type t = {
    trace_id: Trace_id.t;
    span_id: Span_id.t;
    mutable events: Event.t list;
    mutable attrs: key_value list;
  }

  (** Turn the scope into a span context *)
  let[@inline] to_span_ctx (self : t) : Span_ctx.t =
    Span_ctx.make ~trace_id:self.trace_id ~parent_id:self.span_id ()

  (** Add an event to the scope. It will be aggregated into the span.

      Note that this takes a function that produces an event, and will only
      call it if there is an instrumentation backend. *)
  let[@inline] add_event (scope : t) (ev : unit -> Event.t) : unit =
    if Collector.has_backend () then scope.events <- ev () :: scope.events

  (** Add an attr to the scope. It will be aggregated into the span.

      Note that this takes a function that produces attributes, and will only
      call it if there is an instrumentation backend. *)
  let[@inline] add_attrs (scope : t) (attrs : unit -> key_value list) : unit =
    if Collector.has_backend () then
      scope.attrs <- List.rev_append (attrs ()) scope.attrs

  (** The opaque key necessary to access/set the ambient scope with
      {!Ambient_context}. *)
  let ambient_scope_key : t Ambient_context.key = Ambient_context.create_key ()

  (** Obtain current scope from {!Ambient_context}, if available. *)
  let get_ambient_scope ?scope () : t option =
    match scope with
    | Some _ -> scope
    | None -> Ambient_context.get ambient_scope_key

  (** [with_ambient_scope sc thunk] calls [thunk()] in a context where [sc] is
      the (thread|continuation)-local scope, then reverts to the previous local
      scope, if any.

      @see <https://github.com/ELLIOTTCABLE/ocaml-ambient-context> ambient-context docs *)
  let[@inline] with_ambient_scope (sc : t) (f : unit -> 'a) : 'a =
    Ambient_context.with_binding ambient_scope_key sc (fun _ -> f ())
end

(** {2 Traces} *)

(** Span Link

   A pointer from the current span to another span in the same trace or in a
   different trace. For example, this can be used in batching operations,
   where a single batch handler processes multiple requests from different
   traces or when the handler receives a request from a different project.
*)
module Span_link : sig
  open Proto.Trace

  type t = span_link

  val make :
    trace_id:Trace_id.t ->
    span_id:Span_id.t ->
    ?trace_state:string ->
    ?attrs:key_value list ->
    ?dropped_attributes_count:int ->
    unit ->
    t

  val of_span_ctx : ?attrs:key_value list -> Span_ctx.t -> t
end = struct
  open Proto.Trace

  type t = span_link

  let make ~trace_id ~span_id ?trace_state ?(attrs = [])
      ?dropped_attributes_count () : t =
    let attributes = List.map _conv_key_value attrs in
    let dropped_attributes_count =
      Option.map Int32.of_int dropped_attributes_count
    in
    default_span_link
      ~trace_id:(Trace_id.to_bytes trace_id)
      ~span_id:(Span_id.to_bytes span_id) ?trace_state ~attributes
      ?dropped_attributes_count ()

  let[@inline] of_span_ctx ?attrs ctx : t =
    make ~trace_id:(Span_ctx.trace_id ctx) ~span_id:(Span_ctx.parent_id ctx)
      ?attrs ()
end

(** Spans.

    A Span is the workhorse of traces, it indicates an operation that
    took place over a given span of time (indicated by start_time and end_time)
    as part of a hierarchical trace. All spans in a given trace are bound by
    the use of the same {!Trace_id.t}. *)
module Span : sig
  open Proto.Trace

  type t = span

  type id = Span_id.t

  type nonrec kind = span_span_kind =
    | Span_kind_unspecified
    | Span_kind_internal
    | Span_kind_server
    | Span_kind_client
    | Span_kind_producer
    | Span_kind_consumer

  type nonrec status_code = status_status_code =
    | Status_code_unset
    | Status_code_ok
    | Status_code_error

  type nonrec status = status = {
    message: string;
    code: status_code;
  }

  val id : t -> Span_id.t

  type key_value =
    string
    * [ `Int of int
      | `String of string
      | `Bool of bool
      | `Float of float
      | `None
      ]

  val create :
    ?kind:kind ->
    ?id:id ->
    ?trace_state:string ->
    ?attrs:key_value list ->
    ?events:Event.t list ->
    ?status:status ->
    trace_id:Trace_id.t ->
    ?parent:id ->
    ?links:Span_link.t list ->
    start_time:Timestamp_ns.t ->
    end_time:Timestamp_ns.t ->
    string ->
    t * id
  (** [create ~trace_id name] creates a new span with its unique ID.
        @param trace_id the trace this belongs to
        @param parent parent span, if any
        @param links list of links to other spans, each with their trace state
        (see {{: https://www.w3.org/TR/trace-context/#tracestate-header} w3.org}) *)
end = struct
  open Proto.Trace

  type t = span

  type id = Span_id.t

  type nonrec kind = span_span_kind =
    | Span_kind_unspecified
    | Span_kind_internal
    | Span_kind_server
    | Span_kind_client
    | Span_kind_producer
    | Span_kind_consumer

  type key_value =
    string
    * [ `Int of int
      | `String of string
      | `Bool of bool
      | `Float of float
      | `None
      ]

  type nonrec status_code = status_status_code =
    | Status_code_unset
    | Status_code_ok
    | Status_code_error

  type nonrec status = status = {
    message: string;
    code: status_code;
  }

  let id self = Span_id.of_bytes self.span_id

  let create ?(kind = !Globals.default_span_kind) ?(id = Span_id.create ())
      ?trace_state ?(attrs = []) ?(events = []) ?status ~trace_id ?parent
      ?(links = []) ~start_time ~end_time name : t * id =
    let trace_id = Trace_id.to_bytes trace_id in
    let parent_span_id = Option.map Span_id.to_bytes parent in
    let attributes = List.map _conv_key_value attrs in
    let span =
      default_span ~trace_id ?parent_span_id ~span_id:(Span_id.to_bytes id)
        ~attributes ~events ?trace_state ~status ~kind ~name ~links
        ~start_time_unix_nano:start_time ~end_time_unix_nano:end_time ()
    in
    span, id
end

(** Traces.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal} the spec} *)
module Trace = struct
  open Proto.Trace

  type span = Span.t

  let make_resource_spans ?service_name ?attrs spans =
    let ils =
      default_scope_spans ~scope:(Some Globals.instrumentation_library) ~spans
        ()
    in
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    default_resource_spans ~resource:(Some resource) ~scope_spans:[ ils ] ()

  (** Sync emitter.

      This instructs the collector to forward
      the spans to some backend at a later point.

      {b NOTE} be careful not to call this inside a Gc alarm, as it can
      cause deadlocks. *)
  let emit ?service_name ?attrs (spans : span list) : unit =
    let rs = make_resource_spans ?service_name ?attrs spans in
    Collector.send_trace [ rs ] ~ret:(fun () -> ())

  type scope = Scope.t = {
    trace_id: Trace_id.t;
    span_id: Span_id.t;
    mutable events: Event.t list;
    mutable attrs: Span.key_value list;
  }
  [@@deprecated "use Scope.t"]

  let add_event = Scope.add_event [@@deprecated "use Scope.add_event"]

  let add_attrs = Scope.add_attrs [@@deprecated "use Scope.add_attrs"]

  let with_' ?(force_new_trace_id = false) ?trace_state ?service_name
      ?(attrs : (string * [< value ]) list = []) ?kind ?trace_id ?parent ?scope
      ?links name cb =
    let scope =
      if force_new_trace_id then
        None
      else
        Scope.get_ambient_scope ?scope ()
    in
    let trace_id =
      match trace_id, scope with
      | _ when force_new_trace_id -> Trace_id.create ()
      | Some trace_id, _ -> trace_id
      | None, Some scope -> scope.trace_id
      | None, None -> Trace_id.create ()
    in
    let parent =
      match parent, scope with
      | _ when force_new_trace_id -> None
      | Some span_id, _ -> Some span_id
      | None, Some scope -> Some scope.span_id
      | None, None -> None
    in
    let start_time = Timestamp_ns.now_unix_ns () in
    let span_id = Span_id.create () in
    let scope = { trace_id; span_id; events = []; attrs } in
    (* called once we're done, to emit a span *)
    let finally res =
      let status =
        match res with
        | Ok () -> default_status ~code:Status_code_ok ()
        | Error (e, bt) ->
          (* add backtrace *)
          add_event scope (fun () ->
              Event.make "error"
                ~attrs:
                  [ "backtrace", `String (Printexc.raw_backtrace_to_string bt) ]);
          default_status ~code:Status_code_error ~message:e ()
      in
      let span, _ =
        (* TODO: should the attrs passed to with_ go on the Span
           (in Span.create) or on the ResourceSpan (in emit)?
           (question also applies to Opentelemetry_lwt.Trace.with) *)
        Span.create ?kind ~trace_id ?parent ?links ~id:span_id ?trace_state
          ~attrs:scope.attrs ~events:scope.events ~start_time
          ~end_time:(Timestamp_ns.now_unix_ns ())
          ~status name
      in
      emit ?service_name [ span ]
    in
    let thunk () =
      (* set global scope in this thread *)
      Scope.with_ambient_scope scope @@ fun () -> cb scope
    in
    thunk, finally

  (** Sync span guard.

      Notably, this includes {e implicit} scope-tracking: if called without a
      [~scope] argument (or [~parent]/[~trace_id]), it will check in the
      {!Ambient_context} for a surrounding environment, and use that as the
      scope. Similarly, it uses {!Scope.with_ambient_scope} to {e set} a new
      scope in the ambient context, so that any logically-nested calls to
      {!with_} will use this span as their parent.

      {b NOTE} be careful not to call this inside a Gc alarm, as it can
      cause deadlocks.

      @param force_new_trace_id if true (default false), the span will not use a
      ambient scope, the [~scope] argument, nor [~trace_id], but will instead
      always create fresh identifiers for this span *)

  let with_ ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind
      ?trace_id ?parent ?scope ?links name (cb : Scope.t -> 'a) : 'a =
    let thunk, finally =
      with_' ?force_new_trace_id ?trace_state ?service_name ?attrs ?kind
        ?trace_id ?parent ?scope ?links name cb
    in

    try
      let rv = thunk () in
      finally (Ok ());
      rv
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      finally (Error (Printexc.to_string e, bt));
      raise e
end

(** {2 Metrics} *)

(** Metrics.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#metric-signal} the spec} *)
module Metrics = struct
  open Proto
  open Proto.Metrics

  type t = Metrics.metric
  (** A single metric, measuring some time-varying quantity or statistical
      distribution. It is composed of one or more data points that have
      precise values and time stamps. Each distinct metric should have a
      distinct name. *)

  open struct
    let _program_start = Timestamp_ns.now_unix_ns ()
  end

  (** Number data point, as a float *)
  let float ?(start_time_unix_nano = _program_start)
      ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = []) (d : float) :
      number_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_number_data_point ~start_time_unix_nano ~time_unix_nano:now
      ~attributes ~value:(As_double d) ()

  (** Number data point, as an int *)
  let int ?(start_time_unix_nano = _program_start)
      ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = []) (i : int) :
      number_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_number_data_point ~start_time_unix_nano ~time_unix_nano:now
      ~attributes
      ~value:(As_int (Int64.of_int i))
      ()

  (** Aggregation of a scalar metric, always with the current value *)
  let gauge ~name ?description ?unit_ (l : number_data_point list) : t =
    let data = Gauge (default_gauge ~data_points:l ()) in
    default_metric ~name ?description ?unit_ ~data ()

  type aggregation_temporality = Metrics.aggregation_temporality =
    | Aggregation_temporality_unspecified
    | Aggregation_temporality_delta
    | Aggregation_temporality_cumulative

  (** Sum of all reported measurements over a time interval *)
  let sum ~name ?description ?unit_
      ?(aggregation_temporality = Aggregation_temporality_cumulative)
      ?is_monotonic (l : number_data_point list) : t =
    let data =
      Sum (default_sum ~data_points:l ?is_monotonic ~aggregation_temporality ())
    in
    default_metric ~name ?description ?unit_ ~data ()

  (** Histogram data
      @param count number of values in population (non negative)
      @param sum sum of values in population (0 if count is 0)
      @param bucket_counts count value of histogram for each bucket. Sum of
      the counts must be equal to [count].
      length must be [1+length explicit_bounds]
      @param explicit_bounds strictly increasing list of bounds for the buckets *)
  let histogram_data_point ?(start_time_unix_nano = _program_start)
      ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = []) ?(exemplars = [])
      ?(explicit_bounds = []) ?sum ~bucket_counts ~count () :
      histogram_data_point =
    let attributes = attrs |> List.map _conv_key_value in
    default_histogram_data_point ~start_time_unix_nano ~time_unix_nano:now
      ~attributes ~exemplars ~bucket_counts ~explicit_bounds ~count ?sum ()

  let histogram ~name ?description ?unit_ ?aggregation_temporality
      (l : histogram_data_point list) : t =
    let data =
      Histogram (default_histogram ~data_points:l ?aggregation_temporality ())
    in
    default_metric ~name ?description ?unit_ ~data ()

  (* TODO: exponential history *)
  (* TODO: summary *)
  (* TODO: exemplar *)

  (** Aggregate metrics into a {!Proto.Metrics.resource_metrics} *)
  let make_resource_metrics ?service_name ?attrs (l : t list) : resource_metrics
      =
    let lm =
      default_scope_metrics ~scope:(Some Globals.instrumentation_library)
        ~metrics:l ()
    in
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    default_resource_metrics ~scope_metrics:[ lm ] ~resource:(Some resource) ()

  (** Emit some metrics to the collector (sync). This blocks until
      the backend has pushed the metrics into some internal queue, or
      discarded them.

      {b NOTE} be careful not to call this inside a Gc alarm, as it can
      cause deadlocks.
      *)
  let emit ?attrs (l : t list) : unit =
    let rm = make_resource_metrics ?attrs l in
    Collector.send_metrics [ rm ] ~ret:ignore
end

(** A set of callbacks that produce metrics when called.
    The metrics are automatically called regularly.

    This allows applications to register metrics callbacks from various points
    in the program (or even in libraries), and not worry about setting
    alarms/intervals to emit them. *)
module Metrics_callbacks = struct
  open struct
    let cbs_ : (unit -> Metrics.t list) list ref = ref []
  end

  (** [register f] adds the callback [f] to the list.

      [f] will be called at unspecified times and is expected to return
      a list of metrics. It might be called regularly by the backend,
      in particular (but not only) when {!Collector.tick} is called. *)
  let register f : unit =
    if !cbs_ = [] then
      (* make sure we call [f] (and others) at each tick *)
      Collector.on_tick (fun () ->
          let m = List.map (fun f -> f ()) !cbs_ |> List.flatten in
          Metrics.emit m);
    cbs_ := f :: !cbs_
end

(** {2 Logs} *)

(** Logs.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#log-signal} the spec} *)
module Logs = struct
  open Opentelemetry_proto
  open Logs

  type t = log_record

  (** Severity level of a log event *)
  type severity = Logs.severity_number =
    | Severity_number_unspecified
    | Severity_number_trace
    | Severity_number_trace2
    | Severity_number_trace3
    | Severity_number_trace4
    | Severity_number_debug
    | Severity_number_debug2
    | Severity_number_debug3
    | Severity_number_debug4
    | Severity_number_info
    | Severity_number_info2
    | Severity_number_info3
    | Severity_number_info4
    | Severity_number_warn
    | Severity_number_warn2
    | Severity_number_warn3
    | Severity_number_warn4
    | Severity_number_error
    | Severity_number_error2
    | Severity_number_error3
    | Severity_number_error4
    | Severity_number_fatal
    | Severity_number_fatal2
    | Severity_number_fatal3
    | Severity_number_fatal4

  let pp_severity = Logs.pp_severity_number

  type flags = Logs.log_record_flags =
    | Log_record_flags_do_not_use
    | Log_record_flags_trace_flags_mask

  let pp_flags = Logs.pp_log_record_flags

  (** Make a single log entry *)
  let make ?time ?(observed_time_unix_nano = Timestamp_ns.now_unix_ns ())
      ?severity ?log_level ?flags ?trace_id ?span_id (body : value) : t =
    let time_unix_nano =
      match time with
      | None -> observed_time_unix_nano
      | Some t -> t
    in
    let trace_id = Option.map Trace_id.to_bytes trace_id in
    let span_id = Option.map Span_id.to_bytes span_id in
    let body = _conv_value body in
    default_log_record ~time_unix_nano ~observed_time_unix_nano
      ?severity_number:severity ?severity_text:log_level ?flags ?trace_id
      ?span_id ~body ()

  (** Make a log entry whose body is a string *)
  let make_str ?time ?observed_time_unix_nano ?severity ?log_level ?flags
      ?trace_id ?span_id (body : string) : t =
    make ?time ?observed_time_unix_nano ?severity ?log_level ?flags ?trace_id
      ?span_id (`String body)

  (** Make a log entry with format *)
  let make_strf ?time ?observed_time_unix_nano ?severity ?log_level ?flags
      ?trace_id ?span_id fmt =
    Format.kasprintf
      (fun bod ->
        make_str ?time ?observed_time_unix_nano ?severity ?log_level ?flags
          ?trace_id ?span_id bod)
      fmt

  (** Emit logs.

    This instructs the collector to send the logs to some backend at
    a later date.
    {b NOTE} be careful not to call this inside a Gc alarm, as it can
    cause deadlocks. *)
  let emit ?service_name ?attrs (l : t list) : unit =
    let attributes = Globals.mk_attributes ?service_name ?attrs () in
    let resource = Proto.Resource.default_resource ~attributes () in
    let ll =
      default_scope_logs ~scope:(Some Globals.instrumentation_library)
        ~log_records:l ()
    in
    let rl =
      default_resource_logs ~resource:(Some resource) ~scope_logs:[ ll ] ()
    in
    Collector.send_logs [ rl ] ~ret:ignore
end

(** {2 Utils} *)

(** Implementation of the W3C Trace Context spec

    https://www.w3.org/TR/trace-context/
*)
module Trace_context = struct
  (** The traceparent header
      https://www.w3.org/TR/trace-context/#traceparent-header
  *)
  module Traceparent = struct
    let name = "traceparent"

    (** Parse the value of the traceparent header.

        The values are of the form:

        {[
        {version}-{trace_id}-{parent_id}-{flags}
        ]}

        For example:

        {[ 00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01 ]}

        [{flags}] are currently ignored.
    *)
    let of_value str : (Trace_id.t * Span_id.t, string) result =
      match Span_ctx.of_w3c_trace_context (Bytes.unsafe_of_string str) with
      | Ok sp -> Ok (Span_ctx.trace_id sp, Span_ctx.parent_id sp)
      | Error _ as e -> e

    let to_value ~(trace_id : Trace_id.t) ~(parent_id : Span_id.t) () : string =
      let span_ctx = Span_ctx.make ~trace_id ~parent_id () in
      Bytes.unsafe_to_string @@ Span_ctx.to_w3c_trace_context span_ctx
  end
end

(** Export GC metrics.

    These metrics are emitted after each GC collection. *)
module GC_metrics : sig
  val basic_setup : unit -> unit
  (** Setup a hook that will emit GC statistics on every tick (assuming
      a ticker thread) *)

  val get_runtime_attributes : unit -> Span.key_value list
  (** Get OCaml name and version runtime attributes *)

  val get_metrics : unit -> Metrics.t list
  (** Get a few metrics from the current state of the GC *)
end = struct
  (** See https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/resource/semantic_conventions/process.md#process-runtimes *)
  let runtime_attributes =
    lazy
      Conventions.Attributes.
        [
          Process.Runtime.name, `String "ocaml";
          Process.Runtime.version, `String Sys.ocaml_version;
        ]

  let get_runtime_attributes () = Lazy.force runtime_attributes

  let basic_setup () =
    let on_tick () =
      match Collector.get_backend () with
      | None -> ()
      | Some (module C) -> C.signal_emit_gc_metrics ()
    in
    Collector.on_tick on_tick

  let bytes_per_word = Sys.word_size / 8

  let word_to_bytes n = n * bytes_per_word

  let word_to_bytes_f n = n *. float bytes_per_word

  let get_metrics () : Metrics.t list =
    let gc = Gc.quick_stat () in
    let now = Timestamp_ns.now_unix_ns () in
    let open Metrics in
    let open Conventions.Metrics in
    [
      gauge ~name:Process.Runtime.Ocaml.GC.major_heap ~unit_:"B"
        [ int ~now (word_to_bytes gc.Gc.heap_words) ];
      sum ~name:Process.Runtime.Ocaml.GC.minor_allocated
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true ~unit_:"B"
        [ float ~now (word_to_bytes_f gc.Gc.minor_words) ];
      sum ~name:Process.Runtime.Ocaml.GC.minor_collections
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~now gc.Gc.minor_collections ];
      sum ~name:Process.Runtime.Ocaml.GC.major_collections
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~now gc.Gc.major_collections ];
      sum ~name:Process.Runtime.Ocaml.GC.compactions
        ~aggregation_temporality:Metrics.Aggregation_temporality_cumulative
        ~is_monotonic:true
        [ int ~now gc.Gc.compactions ];
    ]
end
