(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/6953

   Make the sequencer node resilient to rollup node disconnect.

   RPC failures makes the sequencer stop or maybe fails to parse
   specific element.
*)

open Tezos_rpc
open Path

type error += Connection_error of Uri.t | Timeout of float * Uri.t

let () =
  register_error_kind
    `Temporary
    ~id:"evm_node_dev_lost_connection"
    ~title:"Lost connection with node"
    ~description:"The EVM node cannot communicate with the node."
    ~pp:(fun ppf url ->
      Format.fprintf
        ppf
        "The EVM node cannot communicate with the node when calling %s"
        url)
    Data_encoding.(obj1 (req "url" string))
    (function Connection_error url -> Some (Uri.to_string url) | _ -> None)
    (fun url -> Connection_error (Uri.of_string url)) ;
  register_error_kind
    `Temporary
    ~id:"evm_node_timeout"
    ~title:"Timeout when calling an RPC"
    ~description:"An RPC called by the EVM node timed out."
    ~pp:(fun ppf (t, url) ->
      Format.fprintf ppf "The RPC %s timed out after %fs" url t)
    Data_encoding.(obj2 (req "timeout" float) (req "url" string))
    (function Timeout (t, u) -> Some (t, Uri.to_string u) | _ -> None)
    (fun (t, u) -> Timeout (t, Uri.of_string u))

let is_connection_error trace =
  let open RPC_client_errors in
  TzTrace.fold
    (fun yes error ->
      yes
      ||
      match error with
      | Request_failed
          {
            error =
              ( Connection_failed _
              | Unexpected_status_code
                  {
                    code = `Bad_gateway | `Gateway_timeout | `Service_unavailable;
                    _;
                  } );
            _;
          } ->
          true
      | Timeout _ -> true
      | _ -> false)
    false
    trace

let smart_rollup_address :
    ([`GET], unit, unit, unit, unit, bytes) Service.service =
  Service.get_service
    ~description:"Smart rollup address"
    ~query:Query.empty
    ~output:(Data_encoding.Fixed.bytes 20)
    (open_root / "global" / "smart_rollup_address")

let first_available_level_from_gc_info_encoding =
  (* We only care about first available level *)
  (* NOTE: This encoding is meant to be used with JSON only to remain compatible
     with previous versions of the rollup node. *)
  let open Data_encoding in
  merge_objs (obj1 (req "first_available_level" int32)) unit

let first_available_level :
    ([`GET], unit, unit, unit, unit, int32 * unit) Service.service =
  Service.get_service
    ~description:"Smart rollup address"
    ~query:Query.empty
    ~output:first_available_level_from_gc_info_encoding
    (open_root / "local" / "gc_info")

type state_value_query = {key : string}

module Block_id = struct
  type t = Head | Level of Int32.t | Finalized

  let construct = function
    | Head -> "head"
    | Level level -> Int32.to_string level
    | Finalized -> "finalized"

  let destruct id =
    match id with
    | "head" -> Ok Head
    | n -> (
        match Int32.of_string_opt n with
        | Some n -> Ok (Level n)
        | None -> Error "Cannot parse block id")

  let arg : t Tezos_rpc.Arg.t =
    Tezos_rpc.Arg.make
      ~descr:"An L1 block identifier."
      ~name:"block_id"
      ~construct
      ~destruct
      ()
end

let state_value_query : state_value_query Tezos_rpc.Query.t =
  let open Tezos_rpc.Query in
  query (fun key -> {key})
  |+ field "key" Tezos_rpc.Arg.string "" (fun t -> t.key)
  |> seal

let durable_state_value :
    ( [`GET],
      unit,
      unit * Block_id.t,
      state_value_query,
      unit,
      bytes option )
    Service.service =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve value by key from PVM durable storage. PVM state is taken with \
       respect to the specified block level. Value returned in hex format."
    ~query:state_value_query
    ~output:Data_encoding.(option bytes)
    (open_root / "global" / "block" /: Block_id.arg / "durable" / "wasm_2_0_0"
   / "value")

type injection_query_arg = {drop_duplicate : bool option; order : Z.t option}

let batcher_injection :
    ( [`POST],
      unit,
      unit,
      injection_query_arg,
      string trace,
      string trace )
    Service.service =
  let query =
    let open Tezos_rpc.Query in
    let z_rpc_arg =
      Resto.Arg.make
        ~name:"z"
        ~destruct:(fun s -> Ok (Z.of_string s))
        ~construct:Z.to_string
        ()
    in
    query (fun drop_duplicate order -> {drop_duplicate; order})
    |+ opt_field "drop_duplicate" Tezos_rpc.Arg.bool (fun q -> q.drop_duplicate)
    |+ opt_field "order" z_rpc_arg (fun q -> q.order)
    |> seal
  in
  Tezos_rpc.Service.post_service
    ~description:"Inject messages in the batcher's queue"
    ~query
    ~input:
      Data_encoding.(
        def "messages" ~description:"Messages to inject" (list (string' Hex)))
    ~output:
      Data_encoding.(
        def
          "message_ids"
          ~description:"Ids of injected L2 messages"
          (list string))
    (open_root / "local" / "batcher" / "injection")

let dal_batcher_injection =
  Tezos_rpc.Service.post_service
    ~description:"Inject the given messages in the DAL queue"
    ~query:Tezos_rpc.Query.empty
    ~input:
      Data_encoding.(
        def "messages" ~description:"Messages to inject" (list (string' Plain)))
    ~output:Data_encoding.unit
    (open_root / "local" / "dal" / "batcher" / "injection")

let dal_slot_indices =
  let input_encoding = Data_encoding.(obj1 (req "indices" (list uint8))) in
  Tezos_rpc.Service.post_service
    ~description:
      "Provide the (new) list of slot indices to use to the rollup node's DAL \
       injector"
    ~query:Tezos_rpc.Query.empty
    ~input:
      Data_encoding.(
        def "slot_indices" ~description:"Slot indices to set" input_encoding)
    ~output:Data_encoding.unit
    (open_root / "local" / "dal" / "slot" / "indices")

let dal_injected_operations_statuses :
    ( [`GET],
      unit,
      unit,
      unit,
      unit,
      (Tezos_crypto.Hashed.Injector_operations_hash.t
      * Rollup_node_services.message_status)
      list )
    Service.service =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve the statuses of all known operations injected via DAL."
    ~query:Tezos_rpc.Query.empty
    ~output:
      Data_encoding.(
        list
          (obj2
             (req "id" Tezos_crypto.Hashed.Injector_operations_hash.encoding)
             (req "status" Rollup_node_services.Encodings.message_status)))
    (open_root / "local" / "dal" / "injected" / "operations" / "statuses")

let forget_dal_injection_id =
  Tezos_rpc.Service.post_service
    ~description:"Forget information about the injection whose id is given"
    ~query:Tezos_rpc.Query.empty
    ~input:Data_encoding.unit
    ~output:Data_encoding.unit
    (open_root / "local" / "dal" / "injection"
   /: Tezos_crypto.Hashed.Injector_operations_hash.rpc_arg / "forget")

let simulation :
    ( [`POST],
      unit,
      unit,
      unit,
      Simulation.Encodings.simulate_input,
      Data_encoding.json )
    Service.service =
  Tezos_rpc.Service.post_service
    ~description:
      "Simulate messages evaluation by the PVM, and find result in durable \
       storage"
    ~query:Tezos_rpc.Query.empty
    ~input:Simulation.Encodings.simulate_input
    ~output:Data_encoding.Json.encoding
    (open_root / "global" / "block" / "head" / "simulate")

let global_block_watcher :
    ([`GET], unit, unit, unit, unit, Sc_rollup_block.t) Service.service =
  Tezos_rpc.Service.get_service
    ~description:"Monitor and streaming the L2 blocks"
    ~query:Tezos_rpc.Query.empty
    ~output:Sc_rollup_block.encoding
    (open_root / "global" / "monitor_blocks")

let finalized_block_watcher :
    ([`GET], unit, unit, unit, unit, Sc_rollup_block.t) Service.service =
  Tezos_rpc.Service.get_service
    ~description:"Monitor and streaming the finalized L2 blocks"
    ~query:Tezos_rpc.Query.empty
    ~output:Sc_rollup_block.encoding
    (open_root / "global" / "monitor_finalized_blocks")

let global_current_tezos_level :
    ([`GET], unit, unit, unit, unit, int32 option) Service.service =
  Tezos_rpc.Service.get_service
    ~description:"Current tezos level of the rollup node"
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.(option int32)
    (open_root / "global" / "tezos_level")

(** [retry_connection f] retries the connection using [f]. If an error
    happens in [f] and it has lost the connection, the rpc is
    retried *)
let retry_connection (f : Uri.t -> 'a tzresult Lwt.t) endpoint :
    'a tzresult Lwt.t =
  let open Lwt_result_syntax in
  let rec retry ~delay () =
    let*! result = f endpoint in
    match result with
    | Error err when is_connection_error err ->
        let*! () = Events.retrying_connect ~endpoint ~delay in
        let*! () = Lwt_unix.sleep delay in
        let next_delay = delay *. 2. in
        let delay = Float.min next_delay 30. in
        retry ~delay ()
    | res -> Lwt.return res
  in
  retry ~delay:1. ()

let uri_of_service base rpc b c =
  let req = Tezos_rpc.Service.forge_request ~base rpc b c in
  req.uri

let with_timeout t base rpc b c f =
  let open Lwt_result_syntax in
  Lwt.pick
    [
      f ();
      (let*! () = Lwt_unix.sleep t in
       tzfail (Timeout (t, uri_of_service base rpc b c)));
    ]

let call_service ~base ?(media_types = Media_type.all_media_types) ~timeout rpc
    b c input =
  let open Lwt_result_syntax in
  let*! res =
    with_timeout timeout base rpc b c @@ fun () ->
    Octez_telemetry.HTTP_client.call_service media_types ~base rpc b c input
  in
  match res with
  | Ok res -> return res
  | Error trace when is_connection_error trace ->
      fail
      @@ TzTrace.cons (Connection_error (uri_of_service base rpc b c)) trace
  | Error trace -> fail trace

let call_service ~keep_alive ~base ?media_types ~timeout rpc b c input =
  let f base = call_service ~base ?media_types ~timeout rpc b c input in
  if keep_alive then retry_connection f base else f base

let make_streamed_call ~timeout rollup_node_endpoint service =
  let open Lwt_result_syntax in
  let stream, push = Lwt_stream.create () in
  let on_chunk v = push (Some v) and on_close () = push None in
  let* spill_all =
    with_timeout timeout rollup_node_endpoint global_block_watcher () ()
    @@ fun () ->
    Tezos_rpc_http_client_unix.RPC_client_unix.call_streamed_service
      [Media_type.json]
      ~base:rollup_node_endpoint
      service
      ~on_chunk
      ~on_close
      ()
      ()
      ()
  in
  let close () =
    spill_all () ;
    if Lwt_stream.is_closed stream then () else on_close ()
  in
  return (stream, close)

let monitor_blocks rollup_node_endpoint =
  make_streamed_call rollup_node_endpoint global_block_watcher

let monitor_finalized_blocks rollup_node_endpoint =
  make_streamed_call rollup_node_endpoint finalized_block_watcher

let monitor_finalized_levels ~timeout rollup_node_endpoint =
  let open Lwt_result_syntax in
  let*! res = monitor_finalized_blocks ~timeout rollup_node_endpoint in
  match res with
  | Ok (stream, close) ->
      return
        ( Lwt_stream.map (fun (b : Sc_rollup_block.t) -> b.header.level) stream,
          close )
  | Error e ->
      let*! () = Events.rpc_call_fallback "monitor_finalized_blocks" e in
      let+ stream, close = monitor_blocks ~timeout rollup_node_endpoint in
      ( Lwt_stream.map
          (fun (b : Sc_rollup_block.t) -> Int32.sub b.header.level 2l)
          stream,
        close )

let publish :
    ?drop_duplicate:bool ->
    ?order:Z.t ->
    keep_alive:bool ->
    rollup_node_endpoint:Uri.t ->
    timeout:float ->
    [< `External of string] list ->
    unit tzresult Lwt.t =
 fun ?drop_duplicate ?order ~keep_alive ~rollup_node_endpoint ~timeout inputs ->
  let open Lwt_result_syntax in
  let inputs = List.map (function `External s -> s) inputs in
  let* _answer =
    call_service
      ~keep_alive
      ~base:rollup_node_endpoint
      ~timeout
      batcher_injection
      ()
      {drop_duplicate; order}
      inputs
  in
  return_unit

let publish_on_dal :
    rollup_node_endpoint:Uri.t ->
    timeout:float ->
    messages:string list ->
    unit tzresult Lwt.t =
 fun ~rollup_node_endpoint ~timeout ~messages ->
  call_service
    ~keep_alive:false
    ~base:rollup_node_endpoint
    ~timeout
    dal_batcher_injection
    ()
    ()
    messages

let get_injected_dal_operations_statuses :
    rollup_node_endpoint:Uri.t ->
    timeout:float ->
    (Tezos_crypto.Hashed.Injector_operations_hash.t
    * Rollup_node_services.message_status)
    list
    tzresult
    Lwt.t =
 fun ~rollup_node_endpoint ~timeout ->
  call_service
    ~keep_alive:false
    ~base:rollup_node_endpoint
    ~timeout
    dal_injected_operations_statuses
    ()
    ()
    ()

let set_dal_slot_indices :
    rollup_node_endpoint:Uri.t ->
    timeout:float ->
    slot_indices:Tezos_dal_node_services.Types.slot_index list ->
    unit tzresult Lwt.t =
 fun ~rollup_node_endpoint ~timeout ~slot_indices ->
  call_service
    ~keep_alive:false
    ~base:rollup_node_endpoint
    ~timeout
    dal_slot_indices
    ()
    ()
    slot_indices

let forget_dal_injection_id :
    rollup_node_endpoint:Uri.t ->
    timeout:float ->
    Tezos_crypto.Hashed.Injector_operations_hash.t ->
    unit tzresult Lwt.t =
 fun ~rollup_node_endpoint ~timeout id ->
  call_service
    ~keep_alive:false
    ~base:rollup_node_endpoint
    ~timeout
    forget_dal_injection_id
    ((), id)
    ()
    ()

let durable_state_subkeys :
    ( [`GET],
      unit,
      unit * Block_id.t,
      state_value_query,
      unit,
      string list option )
    Service.service =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve subkeys by key from PVM durable storage. PVM state is taken \
       with respect to the specified block level."
    ~query:state_value_query
    ~output:Data_encoding.(option (list string))
    (open_root / "global" / "block" /: Block_id.arg / "durable" / "wasm_2_0_0"
   / "subkeys")

let durable_state_values :
    ( [`GET],
      unit,
      unit * Block_id.t,
      state_value_query,
      unit,
      (string * bytes) list )
    Service.service =
  Tezos_rpc.Service.get_service
    ~description:
      "Retrieve values directly under a given key from PVM durable storage. \
       PVM state is taken with respect to the specified block level."
    ~query:state_value_query
    ~output:Data_encoding.(list (obj2 (req "key" string) (req "value" bytes)))
    (open_root / "global" / "block" /: Block_id.arg / "durable" / "wasm_2_0_0"
   / "values")

(** [smart_rollup_address base] asks for the smart rollup node's
    address, using the endpoint [base]. *)
let smart_rollup_address ~keep_alive ~timeout base =
  let open Lwt_result_syntax in
  let*! answer =
    call_service
      ~keep_alive
      ~timeout
      ~base
      ~media_types:[Media_type.octet_stream]
      smart_rollup_address
      ()
      ()
      ()
  in
  match answer with
  | Ok address -> return (Bytes.to_string address)
  | Error trace -> fail trace

let oldest_known_l1_level ~keep_alive ~timeout base =
  let open Lwt_result_syntax in
  let+ level, () =
    call_service
      ~keep_alive
      ~timeout
      ~base
      ~media_types:[Media_type.json]
        (* Only JSON, we just look at a single field to be compatible with all
           rollup node versions. *)
      first_available_level
      ()
      ()
      ()
  in
  level

(** [tezos_level base] asks for the smart rollup node's
    latest l1 level, using the endpoint [base]. *)
let tezos_level ~keep_alive ~timeout base =
  let open Lwt_result_syntax in
  let* level_opt =
    call_service
      ~keep_alive
      ~timeout
      ~base
      ~media_types:[Media_type.octet_stream]
      global_current_tezos_level
      ()
      ()
      ()
  in
  let*? level =
    Option.to_result
      ~none:
        [
          error_of_fmt
            "Rollup node is not yet bootstrapped, please wait for the rollup \
             to process an initial block. ";
        ]
      level_opt
  in
  return level
