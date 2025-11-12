(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc

let evm_services_root = Path.(root / "evm")

let get_smart_rollup_address_service =
  Service.get_service
    ~description:"Get the address of the smart rollup hosting the chain"
    ~query:Query.empty
    ~output:Tezos_crypto.Hashed.Smart_rollup_address.encoding
    Path.(evm_services_root / "smart_rollup_address")

let get_time_between_blocks_service =
  Service.get_service
    ~description:"Get the maximum time between two blocks"
    ~query:Query.empty
    ~output:Configuration.time_between_blocks_encoding
    Path.(evm_services_root / "time_between_blocks")

let get_blueprint_service =
  Service.get_service
    ~description:"Fetch the contents of a blueprint"
    ~query:Query.empty
    ~output:Blueprint_types.Legacy.with_events_encoding
    Path.(evm_services_root / "blueprint" /: Arg.uint63)

let get_blueprint_with_events_service =
  Service.get_service
    ~description:
      "Fetch the contents of a blueprint with a complete list of events, \
       including sequencer upgrades"
    ~query:Query.empty
    ~output:Blueprint_types.with_events_encoding
    Path.(evm_services_root / "v2" / "blueprint" /: Arg.uint63)

type blueprints_selector = {from_level : int64; count : int64}

let get_blueprints_service =
  let query =
    Query.(
      query (fun from_level count ->
          let from_level =
            match from_level with
            | Some from_level -> from_level
            | None -> raise (Query.Invalid "`from_level` is missing")
          in
          {from_level; count})
      |+ opt_field "from_level" Arg.uint63 (fun s -> Some s.from_level)
      |+ field "max_count" Arg.uint63 1L (fun s -> s.count)
      |> seal)
  in
  Service.get_service
    ~description:
      "Fetch a sequence of consecutive blueprints, starting from (and \
       containing at least the blueprint for) a given level"
    ~query
    ~output:(Data_encoding.list Blueprint_types.Legacy.with_events_encoding)
    Path.(evm_services_root / "blueprints" / "range")

let get_blueprints_with_events_service =
  let query =
    Query.(
      query (fun from_level count ->
          let from_level =
            match from_level with
            | Some from_level -> from_level
            | None -> raise (Query.Invalid "`from_level` is missing")
          in
          {from_level; count})
      |+ opt_field "from_level" Arg.uint63 (fun s -> Some s.from_level)
      |+ field "max_count" Arg.uint63 1L (fun s -> s.count)
      |> seal)
  in
  Service.get_service
    ~description:
      "Fetch a sequence of consecutive blueprints with a complete list of \
       events, including sequencer upgrades, starting from (and containing at \
       least the blueprint for) a given level"
    ~query
    ~output:(Data_encoding.list Blueprint_types.with_events_encoding)
    Path.(evm_services_root / "v2" / "blueprints" / "range")

let blueprint_watcher_service =
  let level_query =
    Query.(query Fun.id |+ field "from_level" Arg.uint63 0L Fun.id |> seal)
  in

  Service.get_service
    ~description:"Watch for new blueprints"
    ~query:level_query
    ~output:Blueprint_types.Legacy.with_events_encoding
    Path.(evm_services_root / "blueprints")

let message_watcher_service =
  let level_query =
    Query.(query Fun.id |+ field "from_level" Arg.uint63 0L Fun.id |> seal)
  in

  Service.get_service
    ~description:"Watch for new messages"
    ~query:level_query
    ~output:Broadcast.message_encoding
    Path.(evm_services_root / "messages")

let create_blueprint_stream get_next_blueprint_number find_blueprint from_level
    create_stream return_blueprint =
  let open Lwt_syntax in
  let stream, stopper = create_stream () in
  let shutdown () = Lwt_watcher.shutdown stopper in
  (* input source block creating a stream to observe the events *)
  let* (Ethereum_types.Qty next) = get_next_blueprint_number () in
  let* () =
    if Z.(Compare.(next < of_int64 from_level)) then
      Stdlib.failwith "Cannot start watching from a level too far in the future"
    else return_unit
  in
  (* generate the next asynchronous event *)
  let next =
    let next_level_requested = ref Z.(of_int64 from_level) in
    fun () ->
      if Z.Compare.(!next_level_requested < next) then (
        let current_request = !next_level_requested in
        (next_level_requested := Z.(succ current_request)) ;
        let* blueprint = find_blueprint (Ethereum_types.Qty current_request) in
        match blueprint with
        | Ok (Some blueprint) -> return_blueprint blueprint
        | Ok None -> return_none
        | Error _ ->
            Stdlib.failwith
              "Something went wrong when trying to fetch a blueprint")
      else Lwt_stream.get stream
  in
  return (Lwt_stream.from next, shutdown)

let create_blueprint_watcher_service get_next_blueprint_number find_blueprint
    from_level =
  create_blueprint_stream
    get_next_blueprint_number
    find_blueprint
    from_level
    Broadcast.create_blueprint_stream
    Lwt_syntax.return_some

let create_broadcast_service get_next_blueprint_number find_blueprint from_level
    =
  create_blueprint_stream
    get_next_blueprint_number
    find_blueprint
    from_level
    Broadcast.create_broadcast_stream
    (fun b -> Lwt_syntax.return_some @@ Broadcast.Blueprint b)

let register_get_smart_rollup_address_service smart_rollup_address dir =
  Evm_directory.register0 dir get_smart_rollup_address_service (fun () () ->
      let open Lwt_syntax in
      return_ok smart_rollup_address)

let register_get_time_between_block_service time_between_block dir =
  Evm_directory.register0 dir get_time_between_blocks_service (fun () () ->
      let open Lwt_result_syntax in
      return time_between_block)

let register_blueprint_service blueprint_service find_blueprint dir =
  Evm_directory.opt_register1 dir blueprint_service (fun level () () ->
      let open Lwt_result_syntax in
      let number = Ethereum_types.Qty (Z.of_int64 level) in
      let* blueprint = find_blueprint number in
      return blueprint)

let register_blueprints_service blueprints_service find_blueprint dir =
  let open Ethereum_types in
  let open Lwt_result_syntax in
  let rec find_blueprints ?(rev_res = []) (Qty from) (Qty to_excluded) =
    if Z.Compare.(from < to_excluded) then
      let* blueprint = find_blueprint (Qty from) in
      match blueprint with
      | Some blueprint ->
          find_blueprints
            ~rev_res:(blueprint :: rev_res)
            (Qty (Z.succ from))
            (Qty to_excluded)
      | None -> return (List.rev rev_res)
    else return (List.rev rev_res)
  in
  Evm_directory.opt_register0 dir blueprints_service (fun selector () ->
      let from = Ethereum_types.Qty (Z.of_int64 selector.from_level) in
      let count = Int64.min 500L selector.count in
      let to_excluded =
        Ethereum_types.Qty (Z.of_int64 (Int64.add selector.from_level count))
      in
      let* blueprints = find_blueprints from to_excluded in
      match blueprints with
      | [] -> return_none
      | blueprints -> return_some blueprints)

let register_blueprint_watcher_service find_blueprint get_next_blueprint_number
    dir =
  Evm_directory.streamed_register0
    dir
    blueprint_watcher_service
    (fun level () ->
      create_blueprint_watcher_service
        get_next_blueprint_number
        find_blueprint
        level)

let register_broadcast_service find_blueprint get_next_blueprint_number dir =
  Evm_directory.streamed_register0 dir message_watcher_service (fun level () ->
      create_broadcast_service get_next_blueprint_number find_blueprint level)

let register get_next_blueprint_number find_blueprint_legacy find_blueprint
    smart_rollup_address time_between_blocks dir =
  register_get_smart_rollup_address_service smart_rollup_address dir
  |> register_blueprint_service get_blueprint_service find_blueprint_legacy
  |> register_blueprint_service get_blueprint_with_events_service find_blueprint
  |> register_blueprints_service get_blueprints_service find_blueprint_legacy
  |> register_blueprints_service
       get_blueprints_with_events_service
       find_blueprint
  |> register_blueprint_watcher_service
       find_blueprint_legacy
       get_next_blueprint_number
  |> register_get_time_between_block_service time_between_blocks
  |> register_broadcast_service find_blueprint get_next_blueprint_number

let get_smart_rollup_address ~keep_alive ~timeout evm_node_endpoint =
  Rollup_services.call_service
    ~keep_alive
    ~timeout
    ~media_types:[Media_type.octet_stream]
    ~base:evm_node_endpoint
    get_smart_rollup_address_service
    ()
    ()
    ()

let get_time_between_blocks ?fallback ~timeout ~evm_node_endpoint () =
  let open Lwt_result_syntax in
  let*! res =
    Rollup_services.call_service
      ~keep_alive:false
      ~timeout
      ~media_types:[Media_type.octet_stream]
      ~base:evm_node_endpoint
      get_time_between_blocks_service
      ()
      ()
      ()
  in
  match (res, fallback) with
  | Ok res, _ -> return res
  | Error trace, Some res ->
      let*! () = Events.cannot_fetch_time_between_blocks res trace in
      return res
  | Error trace, None -> fail trace

let get_blueprint ~keep_alive ~timeout ~evm_node_endpoint
    Ethereum_types.(Qty level) =
  Rollup_services.call_service
    ~keep_alive
    ~timeout
    ~media_types:[Media_type.octet_stream]
    ~base:evm_node_endpoint
    get_blueprint_service
    ((), Z.to_int64 level)
    ()
    ()

let get_blueprints ~keep_alive ~timeout ~evm_node_endpoint ~count
    Ethereum_types.(Qty level) =
  Rollup_services.call_service
    ~keep_alive
    ~timeout
    ~media_types:[Media_type.octet_stream]
    ~base:evm_node_endpoint
    get_blueprints_service
    ()
    {from_level = Z.to_int64 level; count}
    ()

let get_blueprint_with_events ~keep_alive ~timeout ~evm_node_endpoint
    Ethereum_types.(Qty level) =
  Rollup_services.call_service
    ~keep_alive
    ~timeout
    ~media_types:[Media_type.octet_stream]
    ~base:evm_node_endpoint
    get_blueprint_with_events_service
    ((), Z.to_int64 level)
    ()
    ()

let get_blueprints_with_events ~keep_alive ~timeout ~evm_node_endpoint ~count
    Ethereum_types.(Qty level) =
  Rollup_services.call_service
    ~keep_alive
    ~timeout
    ~media_types:[Media_type.octet_stream]
    ~base:evm_node_endpoint
    get_blueprints_with_events_service
    ()
    {from_level = Z.to_int64 level; count}
    ()

let monitor_blueprints ~evm_node_endpoint ~timeout Ethereum_types.(Qty level) =
  let open Lwt_result_syntax in
  let stream, push = Lwt_stream.create () in
  let on_chunk v = push (Some v) and on_close () = push None in
  let level = Z.to_int64 level in
  let* _spill_all =
    Rollup_services.with_timeout
      timeout
      evm_node_endpoint
      blueprint_watcher_service
      ()
      level
    @@ fun () ->
    Tezos_rpc_http_client_unix.RPC_client_unix.call_streamed_service
      [Media_type.octet_stream]
      ~base:evm_node_endpoint
      blueprint_watcher_service
      ~on_chunk
      ~on_close
      ()
      level
      ()
  in
  return stream

type 'a monitor = {stream : 'a Lwt_stream.t; closefn : unit -> unit}

let close_monitor {closefn; _} = closefn ()

let get_from_monitor {stream; _} = Lwt_stream.get stream

let monitor_messages ~evm_node_endpoint ~timeout Ethereum_types.(Qty level) =
  let open Lwt_result_syntax in
  let stream, push = Lwt_stream.create () in
  let on_chunk v = push (Some v) and on_close () = push None in
  let level = Z.to_int64 level in
  let* closefn =
    Rollup_services.with_timeout
      timeout
      evm_node_endpoint
      message_watcher_service
      ()
      level
    @@ fun () ->
    Tezos_rpc_http_client_unix.RPC_client_unix.call_streamed_service
      [Media_type.octet_stream]
      ~base:evm_node_endpoint
      message_watcher_service
      ~on_chunk
      ~on_close
      ()
      level
      ()
  in
  return {stream; closefn}
