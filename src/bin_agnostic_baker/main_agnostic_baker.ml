(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018-2024 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Cannot_connect_to_node of string
  | Cannot_decode_node_data of string
  | Lost_node_connection

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.cannot_connect_to_node"
    ~title:"Cannot connect to node"
    ~description:"Cannot connect to node."
    ~pp:(fun ppf uri ->
      Format.fprintf
        ppf
        "Cannot connect to node. Connection refused (ECONNREFUSED): %s"
        uri)
    Data_encoding.(obj1 (req "uri" string))
    (function Cannot_connect_to_node uri -> Some uri | _ -> None)
    (fun uri -> Cannot_connect_to_node uri) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.cannot_decode_node_data"
    ~title:"Cannot decode node data"
    ~description:"Cannot decode node data."
    ~pp:(fun ppf err -> Format.fprintf ppf "Cannot decode node data: %s" err)
    Data_encoding.(obj1 (req "err" string))
    (function Cannot_decode_node_data err -> Some err | _ -> None)
    (fun err -> Cannot_decode_node_data err) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.lost_node_connection"
    ~title:"Lost node connection"
    ~description:"Connection with node lost."
    ~pp:(fun ppf () -> Format.fprintf ppf "Connection with node was lost")
    Data_encoding.(unit)
    (function Lost_node_connection -> Some () | _ -> None)
    (fun () -> Lost_node_connection)

module Events = struct
  include Internal_event.Simple

  let section = ["agnostic-baker"]

  (* Notice *)
  let starting_baker =
    declare_2
      ~section
      ~level:Notice
      ~name:"starting_baker"
      ~msg:"starting baker for protocol {proto} with arguments: {args}"
      ("proto", Protocol_hash.encoding)
      ("args", string)
      ~pp1:Protocol_hash.pp_short

  let baker_running =
    declare_1
      ~section
      ~level:Notice
      ~name:"baker_running"
      ~msg:"baker for protocol {proto} is now running"
      ("proto", Protocol_hash.encoding)
      ~pp1:Protocol_hash.pp_short

  let starting_daemon =
    declare_0
      ~section
      ~level:Notice
      ~name:"starting_daemon"
      ~msg:"starting agnostic daemon"
      ()

  let protocol_encountered =
    declare_2
      ~section
      ~level:Notice
      ~name:"protocol_encountered"
      ~msg:"the {status} protocol {proto_hash} was encountered"
      ("status", string)
      ("proto_hash", Protocol_hash.encoding)
      ~pp2:Protocol_hash.pp_short

  let waiting_for_active_protocol =
    declare_0
      ~section
      ~level:Notice
      ~name:"waiting_for_active_protocol"
      ~msg:"waiting for active protocol"
      ()

  let period_status =
    declare_2
      ~section
      ~level:Notice
      ~name:"period_status"
      ~msg:
        "new block on {period} period (remaining period duration {remaining})"
      ("period", string)
      ("remaining", int31)
end

module Parameters = struct
  let default_node_addr = "http://127.0.0.1:8732"

  type status = Active | Frozen

  let pp_status fmt status =
    Format.fprintf
      fmt
      "%s"
      (match status with Active -> "active" | Frozen -> "frozen")

  (* From Manifest/Product_octez/Protocol*)
  let protocol_info = function
    | ( "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im"
      | "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P"
      | "PtCJ7pwoxe8JasnHY8YonnLYjcVHmhiARPJvqcC6VfHT5s8k8sY"
      | "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt"
      | "PsddFKi32cMJ2qPjf43Qv5GDWLDPZb3T3bF6fLKiF5HtvHNU7aP"
      | "Pt24m4xiPbLDhVgVfABUjirbmda3yohdN82Sp9FeuAXJ4eV9otd"
      | "PsBABY5HQTSkA4297zNHfsZNKtxULfL18y95qb3m53QJiXGmrbU"
      | "PsBabyM1eUXZseaJdmXFApDSBqj8YBfwELoxZHHW77EMcAbbwAS"
      | "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb"
      | "PsDELPH1Kxsxt8f9eWbxQeRxkjfbxoqM52jvs5Y5fBxWWh4ifpo"
      | "PtEdoTezd3RHSC31mpxxo1npxFjoWWcFgQtxapi51Z8TLu6v6Uq"
      | "PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA"
      | "PsFLorenaUUuikDWvMDr6fGBRG8kt3e3D3fHoXK1j1BFRxeSH4i"
      | "PtGRANADsDU8R9daYKAgWnQYAJ64omN1o3KMGVCykShA97vQbvV"
      | "PtHangz2aRngywmSRGGvrcTyMbbdpWdpFKuS4uMWxg2RaH9i1qx"
      | "Psithaca2MLRFYargivpo7YvUr7wUDqyxrdhC5CQq78mRvimz6A"
      | "PtJakart2xVj7pYXJBXrqHgd82rdkLey5ZeeGwDgPp9rhQUbSqY"
      | "PtKathmankSpLLDALzWw7CGD2j2MtyveTwboEYokqUCP4a1LxMg"
      | "PtLimaPtLMwfNinJi9rCfDPWea8dFgTZ1MeJ9f1m2SRic6ayiwW"
      | "PtMumbai2TmsJHNGRkD8v8YDbtao7BLUC3wjASn1inAKLFCjaH1"
      | "PtNairobiyssHuh87hEhfVBGCVrK3WnS8Z2FT4ymB5tAa4r1nQf"
      | "ProxfordYmVfjWnRcgjWH36fW6PArwqykTFzotUxRs6gmTcZDuH"
      | "PtParisBxoLz5gzMmn3d9WBQNoPSZakgnkMC2VNuQ3KXfUtUQeZ" ) as full_hash ->
        (String.sub full_hash 0 8, Frozen)
    | ( "PsParisCZo7KAh1Z1smVd9ZMZ1HHn5gkzbM94V3PLCpknFWhUAi"
      | "PsquebeCaYyvBEESCaXL8B8Tn8BcEhps2Zke1xMVtyr7X4qMfxT" ) as full_hash ->
        (String.sub full_hash 0 8, Active)
    | "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK" -> ("alpha", Active)
    | _ -> (*We assume that unmatched protocols are beta ones*) ("beta", Active)

  let protocol_short_hash h = fst (protocol_info h)

  let protocol_status h = snd (protocol_info h)
end

module Baker = struct
  type t = Lwt_process.process_none option

  let baker_path ?(user_path = "./") proto_hash =
    let short_name =
      Parameters.protocol_short_hash (Protocol_hash.to_b58check proto_hash)
    in
    Format.sprintf "%soctez-baker-%s" user_path short_name

  let _shutdown p = p#terminate

  let spawn_baker proto_hash ~binaries_directory ~baker_args =
    let open Lwt_result_syntax in
    let args_as_string =
      Format.asprintf
        "%a"
        (Format.pp_print_list
           ~pp_sep:Format.pp_print_space
           Format.pp_print_string)
        baker_args
    in
    let*! () = Events.(emit starting_baker) (proto_hash, args_as_string) in
    let path = baker_path ?user_path:binaries_directory proto_hash in
    let baker_args = path :: baker_args in
    let baker_args = Array.of_list baker_args in
    let p =
      Lwt_process.open_process_none
        ~stdout:`Keep
        ~stderr:`Keep
        (path, baker_args)
    in
    let*! () = Events.(emit baker_running) proto_hash in
    return p
end

module RPC = struct
  open Cohttp_lwt_unix

  let request_uri ~node_addr ~uri =
    let open Lwt_result_syntax in
    Lwt.catch
      (fun () ->
        let*! r = Client.get (Uri.of_string uri) in
        return r)
      (function
        | Unix.(Unix_error (ECONNREFUSED, _, _)) ->
            tzfail (Cannot_connect_to_node node_addr)
        | e -> raise e)

  let call_and_wrap_rpc ~node_addr ~uri ~f =
    let open Lwt_result_syntax in
    let* resp, body = request_uri ~node_addr ~uri in
    let*! body_str = Cohttp_lwt.Body.to_string body in
    match resp.status with
    | `OK ->
        let* json =
          match Data_encoding.Json.from_string body_str with
          | Ok json -> return json
          | Error e -> tzfail (Cannot_decode_node_data e)
        in
        f json
    | #Cohttp.Code.status_code ->
        let*! () =
          Lwt_fmt.printf
            "Cannot fetch from node %s. Response status code %d\n%!"
            uri
            (Cohttp.Code.code_of_status resp.status)
        in
        raise Not_found

  let get_next_protocol_hash ~node_addr =
    let open Lwt_result_syntax in
    let f json =
      (* Next_protocol hash field in the RPC result *)
      let name = "next_protocol" in
      let* v =
        match json with
        | `O fields -> (
            match List.assoc_opt ~equal:( = ) name fields with
            | None -> tzfail (Cannot_decode_node_data ("missing field " ^ name))
            | Some node -> return node)
        | _ -> tzfail (Cannot_decode_node_data "not an object")
      in
      let hash = Protocol_hash.of_b58check_exn (Ezjsonm.get_string v) in
      return hash
    in
    let uri = Format.sprintf "%s/chains/main/blocks/head/metadata" node_addr in
    call_and_wrap_rpc ~node_addr ~uri ~f
end

module Daemon = struct
  type state = {
    binaries_directory : string option;
    node_endpoint : string;
    baker_args : string list;
    mutable current_baker : Baker.t;
  }

  let get_current_proposal ~node_addr =
    let open Lwt_result_syntax in
    let f json =
      match json with
      | `Null -> return_none
      | `String s -> return_some @@ Protocol_hash.of_b58check_exn s
      | _ -> tzfail (Cannot_decode_node_data "not an object")
    in
    let uri =
      Format.sprintf
        "%s/chains/main/blocks/head/votes/current_proposal"
        node_addr
    in
    RPC.call_and_wrap_rpc ~node_addr ~uri ~f

  let get_current_period ~node_addr =
    let open Lwt_result_syntax in
    let voting_period_field = "voting_period" in
    let kind_field = "kind" in
    let remaining_field = "remaining" in
    let f json =
      let* kind =
        match json with
        | `O fields -> (
            match List.assoc_opt ~equal:( = ) voting_period_field fields with
            | None ->
                tzfail
                  (Cannot_decode_node_data
                     ("missing field " ^ voting_period_field))
            | Some node -> (
                match node with
                | `O fields -> (
                    match List.assoc_opt ~equal:( = ) kind_field fields with
                    | None ->
                        tzfail
                          (Cannot_decode_node_data
                             ("missing field " ^ voting_period_field))
                    | Some node -> return @@ Ezjsonm.get_string node)
                | _ -> tzfail (Cannot_decode_node_data "not an object")))
        | _ -> tzfail (Cannot_decode_node_data "not an object")
      in
      let* remaining =
        match json with
        | `O fields -> (
            match List.assoc_opt ~equal:( = ) remaining_field fields with
            | None ->
                tzfail
                  (Cannot_decode_node_data ("missing field " ^ remaining_field))
            | Some node -> return @@ Ezjsonm.get_int node)
        | _ -> tzfail (Cannot_decode_node_data "not an object")
      in
      return (kind, remaining)
    in
    let uri =
      Format.sprintf "%s/chains/main/blocks/head/votes/current_period" node_addr
    in
    RPC.call_and_wrap_rpc ~node_addr ~uri ~f

  let monitor_heads ~node_addr =
    let open Lwt_result_syntax in
    let uri = Format.sprintf "%s/monitor/heads/main" node_addr in
    let* _, body = RPC.request_uri ~node_addr ~uri in
    let cohttp_stream = Cohttp_lwt.Body.to_stream body in
    let buffer = Buffer.create 2048 in
    let stream, push = Lwt_stream.create () in
    let on_chunk v = push (Some v) and on_close () = push None in
    let rec loop () =
      let*! v = Lwt_stream.get cohttp_stream in
      match v with
      | None ->
          on_close () ;
          Lwt.return_unit
      | Some chunk ->
          Buffer.add_string buffer chunk ;
          let data = Buffer.contents buffer in
          Buffer.reset buffer ;
          on_chunk data ;
          loop ()
    in
    ignore (loop () : unit Lwt.t) ;
    return stream

  let monitor_voting_periods ~node_addr head_stream =
    let open Lwt_result_syntax in
    let rec loop () =
      let*! v = Lwt_stream.get head_stream in
      match v with
      | Some _tick ->
          let* period_kind, remaining = get_current_period ~node_addr in
          let*! () = Events.(emit period_status) (period_kind, remaining) in
          loop ()
      | None -> return_unit
    in
    let* () = loop () in
    return_unit

  (* Aims to start the baker associated to the current protocol. If
     the protocol is considered as frozen (not active anymore), and
     there is thus no actual baker binary anymore, the initial phase
     consist in waiting until an active protocol is observed on
     monitored heads. *)
  let may_start_initial_baker state =
    let open Lwt_result_syntax in
    let rec may_start ?last_known_proto ~head_stream () =
      let* protocol_hash =
        RPC.get_next_protocol_hash ~node_addr:state.node_endpoint
      in
      let proto_status =
        Parameters.protocol_status (Protocol_hash.to_b58check protocol_hash)
      in
      let*! () =
        match last_known_proto with
        | None -> Lwt.return_unit
        | Some h ->
            if not (Protocol_hash.equal h protocol_hash) then
              Events.(emit protocol_encountered)
                ( Format.asprintf "%a" Parameters.pp_status proto_status,
                  protocol_hash )
            else Lwt.return_unit
      in
      match proto_status with
      | Active ->
          let* current_baker =
            Baker.spawn_baker
              protocol_hash
              ~binaries_directory:state.binaries_directory
              ~baker_args:state.baker_args
          in
          state.current_baker <- Some current_baker ;
          return_unit
      | Frozen -> (
          let* head_stream =
            match head_stream with
            | Some v -> return v
            | None ->
                let*! () =
                  Events.(emit protocol_encountered)
                    ( Format.asprintf "%a" Parameters.pp_status proto_status,
                      protocol_hash )
                in
                let*! () = Events.(emit waiting_for_active_protocol) () in
                monitor_heads ~node_addr:state.node_endpoint
          in
          let*! v = Lwt_stream.get head_stream in
          match v with
          | Some _tick ->
              may_start
                ~last_known_proto:protocol_hash
                ~head_stream:(Some head_stream)
                ()
          | None -> tzfail Lost_node_connection)
    in
    may_start ~head_stream:None ()

  let run ~state =
    let open Lwt_result_syntax in
    let node_addr = state.node_endpoint in
    let*! () = Events.(emit starting_daemon) () in
    let* () = may_start_initial_baker state in
    let* _protocol_proposal = get_current_proposal ~node_addr in
    let* head_stream = monitor_heads ~node_addr in
    (* Monitoring voting periods through heads monitoring to avoid
       missing UAUs. *)
    let* () = monitor_voting_periods ~node_addr head_stream in
    return_unit
end

module Args = struct
  let binaries_directory_arg = "--binaries-directory"

  let endpoint_arg = "--endpoint"

  let endpoint_short_arg = "-E"

  let help_arg = "--help"

  let print_help () =
    Format.printf
      "Usage:\n\
      \  octez-agnostic-baker [OCTEZ-AGNOSTIC-BAKER-COMMANDS] -- \
       [OCTEZ-BAKER-COMMANDS]@.@." ;
    Format.printf
      "OCTEZ-AGNOSTIC-BAKER-COMMANDS:\n\
      \  %s: display help\n\
      \  %s: path to the octez-baker binaries@.@."
      help_arg
      binaries_directory_arg ;
    Format.printf
      "OCTEZ-BAKER-COMMANDS:\n Run ./octez-baker-<PROTOCOL_HASH> --help@."

  let help_cmd args =
    if List.mem ~equal:String.equal help_arg args then (
      print_help () ;
      exit 0)
    else ()

  let version_cmd args =
    if List.mem ~equal:String.equal "--version" args then (
      Format.printf "%s@." Tezos_version_value.Bin_version.octez_version_string ;
      exit 0)
    else ()

  let split_args ?(on = "--") =
    let rec loop acc = function
      | [] -> (List.rev acc, [])
      | hd :: tl when hd = on -> (List.rev acc, tl)
      | hd :: tl -> loop (hd :: acc) tl
    in
    loop []

  let get_arg_value ~arg ?(short_arg = "") =
    let rec loop = function
      | [] -> None
      | x :: y :: _ when x = arg || x = short_arg -> Some y
      | _ :: l -> loop l
    in
    loop

  let get_endpoint =
    get_arg_value ~arg:endpoint_arg ~short_arg:endpoint_short_arg

  let get_binaries_directory = get_arg_value ~arg:binaries_directory_arg

  let fail_on_empty_baker_args baker_args =
    if List.is_empty baker_args then (
      Format.eprintf
        "Cannot run agnostic baker without any baker arguments. Please refer \
         to the following help:@." ;
      print_help () ;
      exit 1)

  let parse_args all_args =
    let all_args = Array.to_list all_args in
    (* Specific vesrion case *)
    let () = version_cmd all_args in
    (* Remove the binary path *)
    let all_args = Option.value ~default:[] (List.tl all_args) in
    (* Split agnostic baker and baker arguments, that aims to be delimited by -- *)
    let agnostic_baker_args, baker_args = split_args all_args in
    let () = fail_on_empty_baker_args baker_args in
    let () = help_cmd agnostic_baker_args in
    let endpoint =
      Option.value
        ~default:Parameters.default_node_addr
        (get_endpoint baker_args)
    in
    let binaries_directory = get_binaries_directory agnostic_baker_args in
    (endpoint, binaries_directory, baker_args)
end

let run () =
  let open Lwt_result_syntax in
  let*! () = Tezos_base_unix.Internal_event_unix.init () in
  let node_endpoint, binaries_directory, baker_args =
    Args.parse_args Sys.argv
  in
  let* _daemon =
    Daemon.run
      ~state:
        {binaries_directory; node_endpoint; baker_args; current_baker = None}
  in
  let*! () = Lwt_utils.never_ending () in
  return_unit

let () =
  let open Lwt_result_syntax in
  let main_promise () = run () in
  Stdlib.exit
    (Lwt_main.run
       (let*! retcode =
          let*! r =
            Lwt.catch main_promise (function
                | Failure msg -> failwith "%s" msg
                | exn -> failwith "%s" (Printexc.to_string exn))
          in
          match r with
          | Ok () -> Lwt.return 0
          | Error errs ->
              Format.eprintf "%a" Error_monad.pp_print_trace errs ;
              Lwt.return 1
        in
        Format.pp_print_flush Format.err_formatter () ;
        Format.pp_print_flush Format.std_formatter () ;
        Lwt.return retcode))
