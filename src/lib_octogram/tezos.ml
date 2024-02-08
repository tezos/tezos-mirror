(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Jingoo.Jg_types
open Agent_state
open Services_cache

let int_of_string level =
  try int_of_string level
  with _err -> Test.fail "Bad input: %s is not a valid integer@." level

let positive_int_of_string level =
  let level = int_of_string level in
  if level < 0 then Test.fail "Bad input: integer %d is negative@." level ;
  level

let parse_endpoint str =
  match str =~*** rex {|^(https?)://(.*):(\d+)|} with
  | Some (scheme, host, port_str) ->
      Endpoint.{host; scheme; port = int_of_string port_str}
  | None -> (
      match str =~** rex {|^(.*):(\d+)|} with
      | Some (host, port_str) ->
          {host; scheme = "http"; port = int_of_string port_str}
      | None -> raise (Invalid_argument "parse_endpoint"))

type _ key += Octez_node_k : string -> Node.t key

module Octez_node_key = struct
  type t = string

  type r = Node.t

  let proj : type a. a key -> (t * (a, r) eq) option = function
    | Octez_node_k name -> Some (name, Eq)
    | _ -> None

  let compare = String.compare
end

let () = Agent_state.register_key (module Octez_node_key)

type _ key += Octez_baker_k : string -> Baker.t key

module Octez_baker_key = struct
  type t = string

  type r = Baker.t

  let proj : type a. a key -> (t * (a, r) eq) option = function
    | Octez_baker_k name -> Some (name, Eq)
    | _ -> None

  let compare = String.compare
end

let () = Agent_state.register_key (module Octez_baker_key)

type _ key += Rollup_node_k : string -> Sc_rollup_node.t key

module Rollup_node_key = struct
  type t = string

  type r = Sc_rollup_node.t

  let proj : type a. a key -> (t * (a, r) eq) option = function
    | Rollup_node_k name -> Some (name, Eq)
    | _ -> None

  let compare = String.compare
end

let () = Agent_state.register_key (module Rollup_node_key)

type dac_mode_k = [`Coordinator | `Member | `Observer]

let dac_mode_k_compare k1 k2 =
  let int_of_dac_mode_k = function
    | `Coordinator -> 0
    | `Member -> 1
    | `Observer -> 2
  in
  Int.compare (int_of_dac_mode_k k1) (int_of_dac_mode_k k2)

type _ key += Dac_node_k : dac_mode_k * string -> Dac_node.t key

module Dac_node_key = struct
  type t = dac_mode_k * string

  type r = Dac_node.t

  let proj : type a. a key -> (t * (a, r) eq) option = function
    | Dac_node_k (m, name) -> Some ((m, name), Eq)
    | _ -> None

  let compare k1 k2 =
    let r = dac_mode_k_compare (fst k1) (fst k2) in
    if r = 0 then String.compare (snd k1) (snd k2) else r
end

let () = Agent_state.register_key (module Dac_node_key)

type _ key += Dal_node_k : string -> Dal_node.t key

module Dal_node_key = struct
  type t = string

  type r = Dal_node.t

  let proj : type a. a key -> (t * (a, r) eq) option = function
    | Dal_node_k name -> Some (name, Eq)
    | _ -> None

  let compare = String.compare
end

let () = Agent_state.register_key (module Dal_node_key)

let mk_rpc_config endpoint =
  let open RPC_client_unix in
  let rpc_config = {default_config with endpoint = Endpoint.to_uri endpoint} in
  new http_ctxt rpc_config Tezos_rpc_http.Media_type.all_media_types

(* Returns the current level of the node whose endpoint is given. We use
   {!Monitor_services.heads} because {!Block_services.Empty.header} doesn't work
   and we don't want to instantiate the {!Block_services.Make} with a particular
   protocol. *)
let current_level endpoint =
  let rpc_ctxt = mk_rpc_config endpoint in
  let* heads = Monitor_services.heads rpc_ctxt `Main in
  match heads with
  | Error _e ->
      Test.fail
        "Monitoring L1 blocks with RPC server %s failed when calling the RPC!"
        (Endpoint.as_string endpoint)
  | Ok (stream, stopper) -> (
      let* v = Lwt_stream.get stream in
      match v with
      | None ->
          stopper () ;
          Test.fail
            "Monitoring L1 blocks with RPC server %s failed. Stream closed!"
            (Endpoint.as_string endpoint)
      | Some (_hash, {shell = {level; _}; _}) -> return @@ Int32.to_int level)

(* This function fetches L1 blocks (heads) published to the stream returned by
   calling RPC [/monitor/heads/main/] at address [endpoint] and checks whether
   [target_level] is reached or not. When reached (or exceeded), the level from
   the last received block is returned. *)
let wait_for_l1_level_on_endpoint target_level endpoint =
  let rpc_ctxt = mk_rpc_config endpoint in
  let* heads = Monitor_services.heads rpc_ctxt `Main in
  match heads with
  | Error _e ->
      Test.fail
        "Monitoring L1 level %d with RPC server %s failed when calling the RPC!"
        target_level
        (Endpoint.as_string endpoint)
  | Ok (stream, stopper) ->
      let rec loop () =
        let* v = Lwt_stream.get stream in
        match v with
        | None ->
            stopper () ;
            Test.fail
              "Monitoring L1 level %d with RPC server %s failed. Stream closed!"
              target_level
              (Endpoint.as_string endpoint)
        | Some (_hash, {shell = {level; _}; _}) ->
            let level = Int32.to_int level in
            if level >= target_level then (
              stopper () ;
              return level)
            else loop ()
      in
      loop ()

let wait_for_l1_level cli_endpoint level =
  match cli_endpoint with
  | Client.Node node -> Node.wait_for_level node level
  | Client.Proxy_server proxy_server ->
      Proxy_server.as_rpc_endpoint proxy_server
      |> wait_for_l1_level_on_endpoint level
  | Client.Foreign_endpoint endpoint ->
      wait_for_l1_level_on_endpoint level endpoint

let octez_endpoint state endpoint =
  match endpoint with
  | Uri.Owned {name = node} ->
      Client.Node (Agent_state.find (Octez_node_k node) state)
  | Remote {endpoint} -> Foreign_endpoint (parse_endpoint endpoint)

let dal_foreign_endpoint state endpoint =
  match endpoint with
  | Uri.Owned {name = node} ->
      Dal_node.as_rpc_endpoint (Agent_state.find (Dal_node_k node) state)
  | Remote {endpoint} -> parse_endpoint endpoint

let dac_rpc_info state mode endpoint =
  match endpoint with
  | Uri.Owned {name = node} ->
      let dac_node = Agent_state.find (Dac_node_k (mode, node)) state in
      ("127.0.0.1", Dac_node.rpc_port dac_node)
  | Remote {endpoint} ->
      let foreign = parse_endpoint endpoint in
      (Endpoint.rpc_host foreign, Endpoint.rpc_port foreign)

let dac_endpoint state mode endpoint =
  match endpoint with
  | Uri.Owned {name = node} ->
      let dac_node = Agent_state.find (Dac_node_k (mode, node)) state in
      Dac_client.Node dac_node
  | Remote {endpoint} ->
      let foreign = parse_endpoint endpoint in
      Foreign_endpoint foreign

let resolve_octez_rpc_global_uri ~self ~resolver =
  Uri.agent_uri_of_global_uri ~self ~services:(resolver Octez_node Rpc)

let resolve_dac_rpc_global_uri ~self ~resolver =
  Uri.agent_uri_of_global_uri ~self ~services:(resolver Dac_node Rpc)

let resolve_dal_rpc_global_uri ~self ~resolver =
  Uri.agent_uri_of_global_uri ~self ~services:(resolver Dal_node Rpc)

type start_octez_node_r = {
  name : string;
  rpc_port : int;
  metrics_port : int;
  net_port : int;
}

(* We use strings instead of numbers to allow pattern substitution. *)
type dal_cryptobox_parameters = {
  number_of_shards : string;
  page_size : string;
  slot_size : string;
  redundancy_factor : string;
}

let dal_cryptobox_parameters_encoding =
  let open Data_encoding in
  conv
    (fun {number_of_shards; page_size; slot_size; redundancy_factor} ->
      (number_of_shards, page_size, slot_size, redundancy_factor))
    (fun (number_of_shards, page_size, slot_size, redundancy_factor) ->
      {number_of_shards; page_size; slot_size; redundancy_factor})
    (obj4
       (req "number_of_shards" string)
       (req "page_size" string)
       (req "slot_size" string)
       (req "redundancy_factor" string))

let map_dal_cryptobox_parameters f p =
  {
    slot_size = f p.slot_size;
    page_size = f p.page_size;
    redundancy_factor = f p.redundancy_factor;
    number_of_shards = f p.number_of_shards;
  }

type 'uri start_octez_node = {
  name : string option;
  path_node : 'uri;
  network : string;
  snapshot : 'uri option;
  sync_threshold : int;
  peers : string list;
  net_port : string option;
  metrics_port : string option;
  rpc_port : string option;
  dal_cryptobox_parameters : dal_cryptobox_parameters option;
}

type (_, _) Remote_procedure.t +=
  | Start_octez_node :
      'uri start_octez_node
      -> (start_octez_node_r, 'uri) Remote_procedure.t

module Start_octez_node = struct
  let name = "tezos.start_node"

  type 'uri t = 'uri start_octez_node

  type r = start_octez_node_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Start_octez_node args -> Some args
    | _ -> None

  let to_remote_procedure args = Start_octez_node args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Start_octez_node _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {
               name;
               path_node;
               network;
               snapshot;
               sync_threshold;
               peers;
               net_port;
               metrics_port;
               rpc_port;
               dal_cryptobox_parameters;
             } ->
          ( name,
            path_node,
            network,
            snapshot,
            sync_threshold,
            peers,
            net_port,
            metrics_port,
            rpc_port,
            dal_cryptobox_parameters ))
        (fun ( name,
               path_node,
               network,
               snapshot,
               sync_threshold,
               peers,
               net_port,
               metrics_port,
               rpc_port,
               dal_cryptobox_parameters ) ->
          {
            name;
            path_node;
            network;
            snapshot;
            sync_threshold;
            peers;
            net_port;
            metrics_port;
            rpc_port;
            dal_cryptobox_parameters;
          })
        Data_encoding.(
          obj10
            (opt "name" string)
            (req "path_node" uri_encoding)
            (dft "network" string "{{ network }}")
            (opt "snapshot" uri_encoding)
            (dft "synchronization_threshold" int31 2)
            (dft "peers" (list string) [])
            (opt "net_port" string)
            (opt "metrics_port" string)
            (opt "rpc_port" string)
            (opt "dal_cryptobox_parameters" dal_cryptobox_parameters_encoding)))

  let r_encoding =
    Data_encoding.(
      conv
        (fun ({rpc_port; metrics_port; net_port; name} : start_octez_node_r) ->
          (rpc_port, metrics_port, net_port, name))
        (fun (rpc_port, metrics_port, net_port, name) ->
          {rpc_port; metrics_port; net_port; name})
        (obj4
           (req "rpc_port" int31)
           (req "metrics_port" int31)
           (req "net_port" int31)
           (req "name" string)))

  let tvalue_of_r ({rpc_port; metrics_port; net_port; name} : r) =
    Tobj
      [
        ("rpc_port", Tint rpc_port);
        ("metrics_port", Tint metrics_port);
        ("net_port", Tint net_port);
        ("name", Tstr name);
      ]

  let expand ~self ~run
      {
        name;
        path_node;
        network;
        snapshot;
        sync_threshold;
        peers;
        net_port;
        metrics_port;
        rpc_port;
        dal_cryptobox_parameters;
      } =
    let name = Option.map run name in
    let path_node =
      Remote_procedure.global_uri_of_string ~self ~run path_node
    in
    let snapshot =
      Option.map (Remote_procedure.global_uri_of_string ~self ~run) snapshot
    in
    let net_port = Option.map run net_port in
    let metrics_port = Option.map run metrics_port in
    let rpc_port = Option.map run rpc_port in
    let network = run network in
    let peers = List.map run peers in
    let dal_cryptobox_parameters =
      Option.map (map_dal_cryptobox_parameters run) dal_cryptobox_parameters
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6205
       Allow to use templates to define [sync_threshold] *)
    {
      name;
      path_node;
      network;
      snapshot;
      sync_threshold;
      peers;
      net_port;
      metrics_port;
      rpc_port;
      dal_cryptobox_parameters;
    }

  let resolve ~self resolver
      {
        name;
        path_node;
        network;
        snapshot;
        sync_threshold;
        peers;
        net_port;
        metrics_port;
        rpc_port;
        dal_cryptobox_parameters;
      } =
    let path_node = Remote_procedure.file_agent_uri ~self ~resolver path_node in
    let snapshot =
      Option.map (Remote_procedure.file_agent_uri ~self ~resolver) snapshot
    in
    {
      name;
      path_node;
      network;
      snapshot;
      sync_threshold;
      peers;
      net_port;
      metrics_port;
      rpc_port;
      dal_cryptobox_parameters;
    }

  let config_dal_srs node dal_cryptobox_parameters =
    let p = dal_cryptobox_parameters in
    let dal_cryptobox : Tezos_crypto_dal.Cryptobox.parameters =
      {
        slot_size = int_of_string p.slot_size;
        page_size = int_of_string p.page_size;
        redundancy_factor = int_of_string p.redundancy_factor;
        number_of_shards = int_of_string p.number_of_shards;
      }
    in
    let config : Tezos_crypto_dal_octez_dal_config.Dal_config.t =
      {
        activated = true;
        use_mock_srs_for_testing = Some dal_cryptobox;
        bootstrap_peers = [];
      }
    in
    Node.Config_file.update
      node
      (Node.Config_file.set_sandbox_network_with_dal_config config)

  let setup_octez_node ~network ~sync_threshold ~path_node ~metrics_port
      ~rpc_port ~net_port ~peers ?name ?snapshot ?dal_cryptobox_parameters () =
    let l1_node_args =
      Node.
        [
          (* By default, Tezt set the difficulty to generate the identity file
             of the Octez node to 0 (`--expected-pow 0`). The default value
             used in network like mainnet, Weeklynet etc. is 26 (see
             `lib_node_config/config_file.ml`). *)
          Expected_pow 0;
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/6283

             Use default PoW and add an option to override it. *)
          Synchronisation_threshold sync_threshold;
          Network network;
          Metrics_addr (sf "0.0.0.0:%d" metrics_port);
        ]
      @ List.map (fun x -> Node.Peer x) peers
    in
    let node =
      Node.create
        ?name
        ~net_addr:"0.0.0.0"
        ~rpc_host:"0.0.0.0"
        ~rpc_port
        ~net_port
        ~path:path_node
        l1_node_args
    in
    let* () = Node.config_init node [] in
    Option.iter (config_dal_srs node) dal_cryptobox_parameters ;
    let* () =
      match snapshot with
      | Some snapshot ->
          Log.info "Import snapshot" ;
          let* () = Node.snapshot_import ~no_check:true node snapshot in
          Log.info "Snapshot imported" ;
          unit
      | None -> unit
    in
    let* () = Node.run node [] in
    let* () = Node.wait_for_ready node in
    return node

  let run state args =
    let* path_node =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.path_node
    in
    let* snapshot =
      match args.snapshot with
      | Some snapshot ->
          let* local_path =
            Http_client.local_path_from_agent_uri
              (Agent_state.http_client state)
              snapshot
          in
          return (Some local_path)
      | None -> return None
    in
    let metrics_port =
      match args.metrics_port with
      | Some port -> int_of_string port
      | None -> Port.fresh ()
    in
    let rpc_port =
      match args.rpc_port with
      | Some port -> int_of_string port
      | None -> Port.fresh ()
    in
    let net_port =
      match args.net_port with
      | Some port -> int_of_string port
      | None -> Port.fresh ()
    in
    let* octez_node =
      setup_octez_node
        ?name:args.name
        ~rpc_port
        ~path_node
        ~network:args.network
        ~sync_threshold:args.sync_threshold
        ~net_port
        ~metrics_port
        ~peers:args.peers
        ?snapshot
        ?dal_cryptobox_parameters:args.dal_cryptobox_parameters
        ()
    in
    Agent_state.add (Octez_node_k (Node.name octez_node)) octez_node state ;
    return
      {
        rpc_port = Node.rpc_port octez_node;
        metrics_port;
        net_port = Node.net_port octez_node;
        name = Node.name octez_node;
      }

  let on_completion ~on_new_service ~on_new_metrics_source (res : r) =
    let open Services_cache in
    on_new_service res.name Octez_node Rpc res.rpc_port ;
    on_new_service res.name Octez_node Metrics res.metrics_port ;
    on_new_service res.name Octez_node P2p res.net_port ;
    on_new_metrics_source res.name Octez_node res.metrics_port
end

type dal_parameters = {
  feature_enable : string;
  cryptobox : dal_cryptobox_parameters;
  attestation_lag : string;
  attestation_threshold : string;
  number_of_slots : string;
  blocks_per_epoch : string;
}

(* This encoding should be compatible with the protocol parameters. *)
let dal_parameters_encoding =
  let open Data_encoding in
  conv
    (fun {
           feature_enable;
           cryptobox;
           attestation_lag;
           attestation_threshold;
           number_of_slots;
           blocks_per_epoch;
         } ->
      ( cryptobox,
        ( attestation_lag,
          attestation_threshold,
          number_of_slots,
          blocks_per_epoch,
          feature_enable ) ))
    (fun ( cryptobox,
           ( attestation_lag,
             attestation_threshold,
             number_of_slots,
             blocks_per_epoch,
             feature_enable ) ) ->
      {
        feature_enable;
        cryptobox;
        attestation_lag;
        attestation_threshold;
        number_of_slots;
        blocks_per_epoch;
      })
    (merge_objs
       dal_cryptobox_parameters_encoding
       (obj5
          (req "attestation_lag" string)
          (req "attestation_threshold" string)
          (req "number_of_slots" string)
          (req "blocks_per_epoch" string)
          (req "feature_enable" string)))

type 'uri generate_protocol_parameters_file = {
  base_file : 'uri;
  (* the existing protocol parameters file whose parameters
     will be adjusted *)
  output_file_name : string option;
  wallet : 'uri option; (* directory containing bootstrap keys *)
  pk_revealed_accounts_prefix : string option;
  pk_unrevealed_accounts_prefix : string option;
  default_balance : string option;
  balance_updates : (string * string) list;
  dal : dal_parameters;
  minimal_block_delay : string option;
  path_client : 'uri option;
}

type generate_protocol_parameters_r = {filename : string}

type (_, _) Remote_procedure.t +=
  | Generate_protocol_parameters_file :
      'uri generate_protocol_parameters_file
      -> (generate_protocol_parameters_r, 'uri) Remote_procedure.t

module Generate_protocol_parameters_file = struct
  let name = "tezos.generate_protocol_parameters_file"

  type 'uri t = 'uri generate_protocol_parameters_file

  type r = generate_protocol_parameters_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Generate_protocol_parameters_file args -> Some args
    | _ -> None

  let to_remote_procedure args = Generate_protocol_parameters_file args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Generate_protocol_parameters_file _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    let open Data_encoding in
    conv
      (fun {
             base_file;
             output_file_name;
             wallet;
             pk_revealed_accounts_prefix;
             pk_unrevealed_accounts_prefix;
             default_balance;
             balance_updates;
             dal;
             minimal_block_delay;
             path_client;
           } ->
        ( base_file,
          output_file_name,
          wallet,
          pk_revealed_accounts_prefix,
          pk_unrevealed_accounts_prefix,
          default_balance,
          balance_updates,
          dal,
          minimal_block_delay,
          path_client ))
      (fun ( base_file,
             output_file_name,
             wallet,
             pk_revealed_accounts_prefix,
             pk_unrevealed_accounts_prefix,
             default_balance,
             balance_updates,
             dal,
             minimal_block_delay,
             path_client ) ->
        {
          base_file;
          output_file_name;
          wallet;
          pk_revealed_accounts_prefix;
          pk_unrevealed_accounts_prefix;
          default_balance;
          balance_updates;
          dal;
          minimal_block_delay;
          path_client;
        })
      (obj10
         (req "base_file" uri_encoding)
         (opt "output_file_name" string)
         (opt "wallet" uri_encoding)
         (opt "pk_revealed_accounts_prefix" string)
         (opt "pk_unrevealed_accounts_prefix" string)
         (opt "default_balance" string)
         (dft "balance_updates" (list (tup2 string string)) [])
         (req "dal" dal_parameters_encoding)
         (opt "minimal_block_delay" string)
         (opt "path_client" uri_encoding))

  let r_encoding =
    let open Data_encoding in
    conv
      (fun {filename} -> filename)
      (fun filename -> {filename})
      (obj1 (req "filename" string))

  let tvalue_of_r {filename} = Tobj [("filename", Tstr filename)]

  let expand ~self ~run base =
    let base_file =
      Remote_procedure.global_uri_of_string ~self ~run base.base_file
    in
    let output_file_name = Option.map run base.output_file_name in
    let wallet =
      base.wallet
      |> Option.map (Remote_procedure.global_uri_of_string ~self ~run)
    in
    let pk_revealed_accounts_prefix =
      Option.map run base.pk_revealed_accounts_prefix
    in
    let pk_unrevealed_accounts_prefix =
      Option.map run base.pk_unrevealed_accounts_prefix
    in
    let default_balance = Option.map run base.default_balance in
    let dal =
      {
        feature_enable = run base.dal.feature_enable;
        cryptobox = map_dal_cryptobox_parameters run base.dal.cryptobox;
        attestation_lag = run base.dal.attestation_lag;
        attestation_threshold = run base.dal.attestation_threshold;
        number_of_slots = run base.dal.number_of_slots;
        blocks_per_epoch = run base.dal.blocks_per_epoch;
      }
    in
    let minimal_block_delay = Option.map run base.minimal_block_delay in
    let balance_updates =
      List.map
        (fun (alias, balance) -> (alias, run balance))
        base.balance_updates
    in
    let path_client =
      Option.map
        (Remote_procedure.global_uri_of_string ~self ~run)
        base.path_client
    in
    {
      base_file;
      output_file_name;
      wallet;
      pk_revealed_accounts_prefix;
      pk_unrevealed_accounts_prefix;
      default_balance;
      balance_updates;
      dal;
      minimal_block_delay;
      path_client;
    }

  let resolve ~self resolver base =
    let base_file =
      Remote_procedure.file_agent_uri ~self ~resolver base.base_file
    in
    let path_client =
      Option.map
        (Remote_procedure.file_agent_uri ~self ~resolver)
        base.path_client
    in
    let wallet =
      base.wallet |> Option.map (resolve_octez_rpc_global_uri ~self ~resolver)
    in
    {base with base_file; wallet; path_client}

  let run state
      {
        base_file;
        output_file_name;
        wallet;
        pk_revealed_accounts_prefix;
        pk_unrevealed_accounts_prefix;
        default_balance;
        balance_updates;
        dal;
        minimal_block_delay;
        path_client;
      } =
    let* base_file =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        base_file
    in
    let* path_client =
      match path_client with
      | None -> return None
      | Some path_client ->
          let* path_client =
            Http_client.local_path_from_agent_uri
              (Agent_state.http_client state)
              path_client
          in
          return @@ Some path_client
    in
    let* base_dir =
      match wallet with
      | None -> return None
      | Some dir ->
          let* dir =
            Http_client.local_path_from_agent_uri
              (Agent_state.http_client state)
              dir
          in
          return (Some dir)
    in
    let client = Client.create ?path:path_client ?base_dir () in
    let* all_addresses = Client.list_known_addresses client in
    let filter_by_prefix prefix =
      List.filter_map
        (fun (alias, _pkh) ->
          if String.starts_with ~prefix alias then Some alias else None)
        all_addresses
    in
    let pk_aliases =
      match pk_revealed_accounts_prefix with
      | None -> List.map fst all_addresses
      | Some prefix -> filter_by_prefix prefix
    in
    let pkh_aliases =
      match pk_unrevealed_accounts_prefix with
      | None -> []
      | Some prefix -> filter_by_prefix prefix
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6377

       Reading the public_keys, public_key_hashes and secret_keys files would be
       more time-efficient in case we have many keys.*)
    let to_accounts =
      Lwt_list.map_s (fun alias -> Client.show_address ~alias client)
    in
    let* pk_revealed_accounts = to_accounts pk_aliases in
    let* pk_unrevealed_accounts = to_accounts pkh_aliases in
    let default_balance =
      match default_balance with
      | None -> string_of_int Protocol.default_bootstrap_balance
      | Some str -> str
    in
    let bootstrap_accounts_overrides =
      let json_accounts : JSON.u list =
        (pk_revealed_accounts
        |> List.map (fun (account : Account.key) ->
               let balance =
                 Option.value
                   ~default:default_balance
                   (List.assoc_opt account.alias balance_updates)
               in
               `A
                 [
                   (* We pass the public key of the account, therefore it will
                      not have to be revealed later. *)
                   `String account.public_key;
                   `String balance;
                 ]))
        @ (pk_unrevealed_accounts
          |> List.map (fun (account : Account.key) ->
                 let balance =
                   Option.value
                     ~default:default_balance
                     (List.assoc_opt account.alias balance_updates)
                 in
                 `A
                   [
                     (* We only pass the public key hash of the account,
                        therefore the public key will be considered as not yet
                        revealed for these accounts. *)
                     `String account.public_key_hash;
                     `String balance;
                   ]))
      in
      let key = ["bootstrap_accounts"] in
      let path = key in
      (path, `A json_accounts)
    in
    let dal_overrides : string list * JSON.u =
      let value =
        let aux_json =
          Data_encoding.Json.construct dal_parameters_encoding dal
          |> JSON.annotate ~origin:"yml params"
        in
        JSON.filter_map_object aux_json (fun key v ->
            (* Read value as a string, then parse to replace string values by
               integer/boolean values. *)
            v |> JSON.as_string |> JSON.parse ~origin:key |> Option.some)
        |> JSON.unannotate
      in
      let key = ["dal_parametric"] in
      let path = key in
      (path, value)
    in
    let minimal_block_delay_overrides =
      Option.fold
        ~none:[]
        ~some:(fun v -> [(["minimal_block_delay"], `String v)])
        minimal_block_delay
    in
    let overrides =
      ([bootstrap_accounts_overrides; dal_overrides]
       @ minimal_block_delay_overrides
        :> Protocol.parameter_overrides)
    in
    let* params_file =
      Protocol.write_parameter_file ~base:(Either.Left base_file) overrides
    in
    let agent_dir = Agent_state.home_dir state in
    let filename =
      match output_file_name with None -> "parameters.json" | Some str -> str
    in
    let path = Filename.concat agent_dir filename in
    let* () = Helpers.exec "mv" [params_file; path] in
    Lwt.return {filename}

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ _ = ()
end

type 'uri activate_protocol = {
  endpoint : 'uri;
  path_client : 'uri;
  protocol : Protocol.t;
  parameter_file : 'uri;
}

type (_, _) Remote_procedure.t +=
  | Active_protocol : 'uri activate_protocol -> (unit, 'uri) Remote_procedure.t

module Activate_protocol = struct
  let name = "tezos.activate_protocol"

  type 'uri t = 'uri activate_protocol

  type r = unit

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Active_protocol args -> Some args
    | _ -> None

  let to_remote_procedure args = Active_protocol args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Active_protocol _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    let open Data_encoding in
    conv
      (fun {endpoint; path_client; protocol; parameter_file} ->
        (endpoint, path_client, protocol, parameter_file))
      (fun (endpoint, path_client, protocol, parameter_file) ->
        {endpoint; path_client; protocol; parameter_file})
      (obj4
         (req "endpoint" uri_encoding)
         (req "path_client" uri_encoding)
         (req "protocol" Protocol.encoding)
         (req "parameter_file" uri_encoding))

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand ~self ~run base =
    let path_client =
      Remote_procedure.global_uri_of_string ~self ~run base.path_client
    in
    let endpoint =
      Remote_procedure.global_uri_of_string ~self ~run base.endpoint
    in
    let parameter_file =
      Remote_procedure.global_uri_of_string ~self ~run base.parameter_file
    in
    {base with path_client; endpoint; parameter_file}

  let resolve ~self resolver base =
    let path_client =
      Remote_procedure.file_agent_uri ~self ~resolver base.path_client
    in
    let endpoint = resolve_octez_rpc_global_uri ~self ~resolver base.endpoint in
    let parameter_file =
      resolve_octez_rpc_global_uri ~self ~resolver base.parameter_file
    in
    {base with path_client; endpoint; parameter_file}

  let run state {endpoint; path_client; protocol; parameter_file} =
    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        path_client
    in
    let* parameter_file =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        parameter_file
    in
    let endpoint = octez_endpoint state endpoint in
    let client = Client.create ~path:path_client ~endpoint () in
    Account.write Constant.all_secret_keys ~base_dir:(Client.base_dir client) ;
    (* The protocol is activated few seconds in the past. *)
    let timestamp = Tezos_base.Time.System.Span.of_seconds_exn 5. in
    Client.activate_protocol
      ~protocol
      ~parameter_file
      ~timestamp:(Ago timestamp)
      client

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ () = ()
end

type 'uri client_base_args = {path_client : 'uri; endpoint : 'uri}

let client_base_args_encoding uri_encoding =
  Data_encoding.(
    conv
      (fun {path_client; endpoint} -> (path_client, endpoint))
      (fun (path_client, endpoint) -> {path_client; endpoint})
      (obj2 (req "path_client" uri_encoding) (req "endpoint" uri_encoding)))

let expand_client_base_args ~self ~run base =
  let path_client =
    Remote_procedure.global_uri_of_string ~self ~run base.path_client
  in
  let endpoint =
    Remote_procedure.global_uri_of_string ~self ~run base.endpoint
  in
  {path_client; endpoint}

let resolve_client_args_base ~self resolver base =
  let path_client =
    Remote_procedure.file_agent_uri ~self ~resolver base.path_client
  in
  let endpoint = resolve_octez_rpc_global_uri ~self ~resolver base.endpoint in
  {path_client; endpoint}

type 'uri wait_for_bootstrapped = 'uri client_base_args

type (_, _) Remote_procedure.t +=
  | Wait_for_bootstrapped :
      'uri wait_for_bootstrapped
      -> (unit, 'uri) Remote_procedure.t

module Wait_for_bootstrapped = struct
  let name = "tezos.wait_for_bootstrapped"

  type 'uri t = 'uri wait_for_bootstrapped

  type r = unit

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Wait_for_bootstrapped args -> Some args
    | _ -> None

  let to_remote_procedure args = Wait_for_bootstrapped args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Wait_for_bootstrapped _ -> Eq
    | _ -> Neq

  let encoding uri_encoding = client_base_args_encoding uri_encoding

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand = expand_client_base_args

  let resolve = resolve_client_args_base

  let run state args =
    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.path_client
    in
    let endpoint = octez_endpoint state args.endpoint in
    let client = Client.create ~path:path_client ~endpoint () in
    let* () = Client.bootstrapped client in
    unit

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ () = ()
end

type 'uri originate_smart_rollup = {
  client_base : 'uri client_base_args;
  wallet : string;
  alias : string;
  src : string;
  kernel_path : 'uri;
  parameters_type : string;
  wait : string;
}

type originate_smart_rollup_r = {address : string; hex_address : string}

type (_, _) Remote_procedure.t +=
  | Originate_smart_rollup :
      'uri originate_smart_rollup
      -> (originate_smart_rollup_r, 'uri) Remote_procedure.t

module Originate_smart_rollup = struct
  let name = "tezos.operations.originate_smart_rollup"

  type 'uri t = 'uri originate_smart_rollup

  type r = originate_smart_rollup_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Originate_smart_rollup args -> Some args
    | _ -> None

  let to_remote_procedure args = Originate_smart_rollup args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Originate_smart_rollup _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {
               client_base;
               wallet;
               alias;
               src;
               kernel_path;
               parameters_type;
               wait;
             } ->
          (client_base, (wallet, alias, src, kernel_path, parameters_type, wait)))
        (fun ( client_base,
               (wallet, alias, src, kernel_path, parameters_type, wait) ) ->
          {client_base; wallet; alias; src; kernel_path; parameters_type; wait})
        (merge_objs
           (client_base_args_encoding uri_encoding)
           (obj6
              (req "wallet" string)
              (dft "alias" string "rollup")
              (req "source" string)
              (req "kernel_path" uri_encoding)
              (req "parameters_type" string)
              (dft "wait" string "0"))))

  let r_encoding =
    Data_encoding.(
      conv
        (fun {address; hex_address} -> (address, hex_address))
        (fun (address, hex_address) -> {address; hex_address})
        (obj2 (req "address" string) (req "hex_address" string)))

  let tvalue_of_r {address; hex_address} =
    Tobj [("address", Tstr address); ("hex_address", Tstr hex_address)]

  let expand ~self ~run args =
    let client_base = expand_client_base_args ~self ~run args.client_base in
    let wallet = run args.wallet in
    let alias = run args.alias in
    let src = run args.src in
    let kernel_path =
      Remote_procedure.global_uri_of_string ~self ~run args.kernel_path
    in
    let wait = run args.wait in
    let parameters_type = run args.parameters_type in
    {client_base; wallet; alias; src; kernel_path; parameters_type; wait}

  let resolve ~self resolver args =
    let client_base =
      resolve_client_args_base ~self resolver args.client_base
    in
    let kernel_path =
      Remote_procedure.file_agent_uri ~self ~resolver args.kernel_path
    in
    {args with client_base; kernel_path}

  let run state args =
    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.client_base.path_client
    in
    let endpoint = octez_endpoint state args.client_base.endpoint in

    let client =
      Client.create ~path:path_client ~endpoint ~base_dir:args.wallet ()
    in

    let* kernel_path =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.kernel_path
    in

    let boot_sector = read_file kernel_path in

    let* address =
      Client.Sc_rollup.originate
        client
        ~wait:args.wait
        ~alias:args.alias
        ~src:args.src
        ~kind:"wasm_2_0_0"
        ~parameters_ty:args.parameters_type
        ~boot_sector
        ~burn_cap:(Tez.of_int 2)
    in
    Log.info "Rollup %s originated" address ;
    let (`Hex hex_address) =
      Tezos_crypto.Hashed.Smart_rollup_address.(
        of_b58check_exn address |> to_string |> Hex.of_string)
    in
    return {address; hex_address}

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_
      {address = _; hex_address = _} =
    ()
end

type 'uri originate_smart_contract = {
  client_base : 'uri client_base_args;
  wallet : string;
  alias : string;
  src : string;
  script_path : 'uri;
  amount : Tez.t;
  init : string;
  wait : string;
}

type originate_smart_contract_r = {address : string; hex_address : string}

type (_, _) Remote_procedure.t +=
  | Originate_smart_contract :
      'uri originate_smart_contract
      -> (originate_smart_contract_r, 'uri) Remote_procedure.t

let tez_encoding =
  Data_encoding.conv Tez.to_mutez Tez.of_mutez_int Data_encoding.int31

module Originate_smart_contract = struct
  let contract_hash = "\002\090\121" (* KT1(36) *)

  module H =
    Tezos_crypto.Blake2B.Make
      (Tezos_crypto.Base58)
      (struct
        let name = "Contract_hash"

        let title = "A contract ID"

        let b58check_prefix = contract_hash

        let size = Some 20
      end)

  let name = "tezos.operations.originate_smart_contract"

  type 'uri t = 'uri originate_smart_contract

  type r = originate_smart_contract_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Originate_smart_contract args -> Some args
    | _ -> None

  let to_remote_procedure args = Originate_smart_contract args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Originate_smart_contract _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {client_base; wallet; alias; src; script_path; amount; init; wait} ->
          (client_base, (wallet, alias, src, script_path, amount, init, wait)))
        (fun (client_base, (wallet, alias, src, script_path, amount, init, wait))
             ->
          {client_base; wallet; alias; src; script_path; amount; init; wait})
        (merge_objs
           (client_base_args_encoding uri_encoding)
           (obj7
              (req "wallet" string)
              (dft "alias" string "rollup")
              (req "source" string)
              (req "script_path" uri_encoding)
              (dft "amount" tez_encoding Tez.zero)
              (req "init" string)
              (dft "wait" string "1"))))

  let r_encoding =
    Data_encoding.(
      conv
        (fun {address; hex_address} -> (address, hex_address))
        (fun (address, hex_address) -> {address; hex_address})
        (obj2 (req "address" string) (req "hex_address" string)))

  let tvalue_of_r {address; hex_address} =
    Tobj [("address", Tstr address); ("hex_address", Tstr hex_address)]

  let expand ~self ~run args =
    let client_base = expand_client_base_args ~self ~run args.client_base in
    let wallet = run args.wallet in
    let alias = run args.alias in
    let src = run args.src in
    let script_path =
      Remote_procedure.global_uri_of_string ~self ~run args.script_path
    in
    let init = run args.init in
    let wait = run args.wait in
    {
      client_base;
      wallet;
      alias;
      src;
      script_path;
      amount = args.amount;
      init;
      wait;
    }

  let resolve ~self resolver args =
    let client_base =
      resolve_client_args_base ~self resolver args.client_base
    in
    let script_path =
      Remote_procedure.file_agent_uri ~self ~resolver args.script_path
    in
    {args with client_base; script_path}

  let run state args =
    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.client_base.path_client
    in
    let endpoint = octez_endpoint state args.client_base.endpoint in

    let client =
      Client.create ~path:path_client ~endpoint ~base_dir:args.wallet ()
    in

    let* script_path =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.script_path
    in

    let script = read_file script_path in

    let* address =
      Client.originate_contract
        client
        ~wait:args.wait
        ~alias:args.alias
        ~src:args.src
        ~prg:script
        ~burn_cap:(Tez.of_int 2)
        ~amount:args.amount
        ~init:args.init
    in
    Log.info "Contract %s originated" address ;
    let (`Hex hex_address) =
      H.(of_b58check_exn address |> to_string |> Hex.of_string)
    in
    return {address; hex_address}

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_
      {address = _; hex_address = _} =
    ()
end

type 'uri transfer = {
  client_base : 'uri client_base_args;
  wallet : string;
  src : string;
  dst : string;
  amount : Tez.t;
  arg : string option;
  entrypoint : string option;
  wait : string;
}

type (_, _) Remote_procedure.t +=
  | Transfer : 'uri transfer -> (unit, 'uri) Remote_procedure.t

module Transfer = struct
  let name = "tezos.operations.transfer"

  type 'uri t = 'uri transfer

  type r = unit

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Transfer args -> Some args
    | _ -> None

  let to_remote_procedure args = Transfer args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Transfer _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {client_base; wallet; src; dst; amount; entrypoint; arg; wait} ->
          (client_base, (wallet, src, dst, amount, entrypoint, arg, wait)))
        (fun (client_base, (wallet, src, dst, amount, entrypoint, arg, wait)) ->
          {client_base; wallet; src; dst; amount; entrypoint; arg; wait})
        (merge_objs
           (client_base_args_encoding uri_encoding)
           (obj7
              (req "wallet" string)
              (req "source" string)
              (req "destination" string)
              (dft "amount" tez_encoding Tez.zero)
              (opt "entrypoint" string)
              (opt "arg" string)
              (dft "wait" string "0"))))

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand ~self ~run args =
    let client_base = expand_client_base_args ~self ~run args.client_base in
    let wallet = run args.wallet in
    let src = run args.src in
    let dst = run args.dst in
    let arg = Option.map run args.arg in
    let entrypoint = Option.map run args.entrypoint in
    let wait = run args.wait in
    {client_base; wallet; src; dst; amount = args.amount; arg; entrypoint; wait}

  let resolve ~self resolver args =
    let client_base =
      resolve_client_args_base ~self resolver args.client_base
    in
    {args with client_base}

  let run state args =
    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.client_base.path_client
    in
    let endpoint = octez_endpoint state args.client_base.endpoint in

    let client =
      Client.create ~path:path_client ~endpoint ~base_dir:args.wallet ()
    in

    let* () =
      Client.transfer
        client
        ~wait:args.wait
        ~giver:args.src
        ~receiver:args.dst
        ~burn_cap:(Tez.of_int 2)
        ~amount:args.amount
        ?arg:args.arg
        ?entrypoint:args.entrypoint
    in

    unit

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ () = ()
end

let () = Remote_procedure.register (module Transfer)

type 'uri start_rollup_node = {
  name : string option;
  path_rollup_node : 'uri;
  path_client : 'uri;
  wallet : string;
  endpoint : 'uri;
  operator : string;
  mode : string;
  address : string;
  data_dir_path : string option;
  rpc_port : string option;
  metrics_port : string option;
  kernel_log_path : string option;
}

type start_rollup_node_r = {name : string; rpc_port : int; metrics_port : int}

type (_, _) Remote_procedure.t +=
  | Start_rollup_node :
      'uri start_rollup_node
      -> (start_rollup_node_r, 'uri) Remote_procedure.t

module Start_rollup_node = struct
  let name = "smart_rollup.start_node"

  type 'uri t = 'uri start_rollup_node

  type r = start_rollup_node_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Start_rollup_node args -> Some args
    | _ -> None

  let to_remote_procedure args = Start_rollup_node args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Start_rollup_node _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {
               name;
               path_rollup_node;
               path_client;
               wallet;
               endpoint;
               operator;
               mode;
               address;
               data_dir_path;
               rpc_port;
               metrics_port;
               kernel_log_path;
             } ->
          ( ( name,
              path_rollup_node,
              path_client,
              wallet,
              endpoint,
              operator,
              mode,
              address,
              data_dir_path,
              rpc_port ),
            (metrics_port, kernel_log_path) ))
        (fun ( ( name,
                 path_rollup_node,
                 path_client,
                 wallet,
                 endpoint,
                 operator,
                 mode,
                 address,
                 data_dir_path,
                 rpc_port ),
               (metrics_port, kernel_log_path) ) ->
          {
            name;
            path_rollup_node;
            path_client;
            wallet;
            endpoint;
            operator;
            mode;
            address;
            data_dir_path;
            rpc_port;
            metrics_port;
            kernel_log_path;
          })
        (merge_objs
           (obj10
              (opt "name" string)
              (req "path_rollup_node" uri_encoding)
              (req "path_client" uri_encoding)
              (req "wallet" string)
              (req "endpoint" uri_encoding)
              (req "operator" string)
              (req "mode" string)
              (req "address" string)
              (opt "data_dir_path" string)
              (opt "rpc_port" string))
           (obj2 (opt "metrics_port" string) (opt "kernel_log_path" string))))

  let r_encoding =
    Data_encoding.(
      conv
        (fun ({rpc_port; metrics_port; name} : r) ->
          (rpc_port, metrics_port, name))
        (fun (rpc_port, metrics_port, name) -> {rpc_port; metrics_port; name})
        (obj3
           (req "rpc_port" int31)
           (req "metrics_port" int31)
           (req "name" string)))

  let tvalue_of_r ({rpc_port; metrics_port; name} : r) =
    Tobj
      [
        ("rpc_port", Tint rpc_port);
        ("metrics_port", Tint metrics_port);
        ("name", Tstr name);
      ]

  let expand ~self ~run (args : _ t) =
    let name = Option.map run args.name in
    let path_rollup_node =
      Remote_procedure.global_uri_of_string ~self ~run args.path_rollup_node
    in
    let path_client =
      Remote_procedure.global_uri_of_string ~self ~run args.path_client
    in
    let wallet = run args.wallet in
    let endpoint =
      Remote_procedure.global_uri_of_string ~self ~run args.endpoint
    in
    let operator = run args.operator in
    let mode = run args.mode in
    let address = run args.address in
    let data_dir_path = Option.map run args.data_dir_path in
    let rpc_port = Option.map run args.rpc_port in
    let metrics_port = Option.map run args.metrics_port in
    let kernel_log_path = Option.map run args.kernel_log_path in
    {
      name;
      path_rollup_node;
      path_client;
      wallet;
      endpoint;
      operator;
      mode;
      address;
      data_dir_path;
      rpc_port;
      metrics_port;
      kernel_log_path;
    }

  let resolve ~self resolver args =
    let path_rollup_node =
      Remote_procedure.file_agent_uri ~self ~resolver args.path_rollup_node
    in
    let path_client =
      Remote_procedure.file_agent_uri ~self ~resolver args.path_client
    in
    let endpoint = resolve_octez_rpc_global_uri ~self ~resolver args.endpoint in
    {args with path_rollup_node; path_client; endpoint}

  let run state args =
    let* path =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.path_rollup_node
    in
    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.path_client
    in
    let l1_endpoint = octez_endpoint state args.endpoint in
    let rollup_node =
      Sc_rollup_node.(
        create_with_endpoint
          ?name:args.name
          ~rpc_host:"0.0.0.0"
          ?data_dir:args.data_dir_path
          ?rpc_port:(Option.map int_of_string args.rpc_port)
          ?metrics_port:(Option.map int_of_string args.metrics_port)
          ~path
          (mode_of_string args.mode)
          ~default_operator:args.operator
          l1_endpoint
          ~base_dir:args.wallet)
    in
    let kernel_log_args =
      match args.kernel_log_path with
      | Some path ->
          Sc_rollup_node.[Log_kernel_debug; Log_kernel_debug_file path]
      | None -> []
    in
    let* () = Sc_rollup_node.run rollup_node args.address kernel_log_args in

    let* _ = Sc_rollup_node.unsafe_wait_sync ~path_client rollup_node in
    Agent_state.add
      (Rollup_node_k (Sc_rollup_node.name rollup_node))
      rollup_node
      state ;
    let _metric_addr, metrics_port = Sc_rollup_node.metrics rollup_node in
    return
      {
        name = Sc_rollup_node.name rollup_node;
        rpc_port = Sc_rollup_node.rpc_port rollup_node;
        metrics_port;
      }

  let on_completion ~on_new_service ~on_new_metrics_source (res : r) =
    let open Services_cache in
    on_new_service res.name Rollup_node Rpc res.rpc_port ;
    on_new_service res.name Rollup_node Metrics res.metrics_port ;
    on_new_metrics_source res.name Octez_node res.metrics_port
end

type 'uri prepare_kernel_installer = {
  installer_generator_path : 'uri;
  kernel_path : 'uri;
  preimage_directory_path : string;
  installer_kernel_path : string;
  setup : string option;
}

type (_, _) Remote_procedure.t +=
  | Prepare_kernel_installer :
      'uri prepare_kernel_installer
      -> (unit, 'uri) Remote_procedure.t

module Prepare_kernel_installer = struct
  let name = "smart_rollup.prepare_kernel_installer"

  type 'uri t = 'uri prepare_kernel_installer

  type r = unit

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Prepare_kernel_installer args -> Some args
    | _ -> None

  let to_remote_procedure args = Prepare_kernel_installer args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Prepare_kernel_installer _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {
               installer_generator_path;
               kernel_path;
               preimage_directory_path;
               installer_kernel_path;
               setup;
             } ->
          ( installer_generator_path,
            kernel_path,
            preimage_directory_path,
            installer_kernel_path,
            setup ))
        (fun ( installer_generator_path,
               kernel_path,
               preimage_directory_path,
               installer_kernel_path,
               setup ) ->
          {
            installer_generator_path;
            kernel_path;
            preimage_directory_path;
            installer_kernel_path;
            setup;
          })
        (obj5
           (req "installer_generator_path" uri_encoding)
           (req "kernel_path" uri_encoding)
           (req "preimages_directory_path" string)
           (req "installer_kernel_path" string)
           (opt "setup" string)))

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand ~self ~run args =
    let installer_generator_path =
      Remote_procedure.global_uri_of_string
        ~self
        ~run
        args.installer_generator_path
    in
    let kernel_path =
      Remote_procedure.global_uri_of_string ~self ~run args.kernel_path
    in
    let preimage_directory_path = run args.preimage_directory_path in
    let installer_kernel_path = run args.installer_kernel_path in
    let setup = Option.map run args.setup in
    {
      installer_generator_path;
      kernel_path;
      preimage_directory_path;
      installer_kernel_path;
      setup;
    }

  let resolve ~self resolver args =
    let installer_generator_path =
      Remote_procedure.file_agent_uri
        ~self
        ~resolver
        args.installer_generator_path
    in
    let kernel_path =
      Remote_procedure.file_agent_uri ~self ~resolver args.kernel_path
    in
    {args with kernel_path; installer_generator_path}

  let run state args =
    assert (Filename.is_relative args.installer_kernel_path) ;
    assert (Filename.is_relative args.preimage_directory_path) ;
    let* installer_generator_path =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.installer_generator_path
    in
    let* kernel_path =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.kernel_path
    in
    let preimage_dir = args.preimage_directory_path // "wasm_2_0_0" in
    let* () = Helpers.mkdir ~p:true preimage_dir in
    let* () = Helpers.mkdir ~p:true (Filename.dirname kernel_path) in
    let setup_file =
      match args.setup with
      | Some contents ->
          let path = Temp.file "setup_file" in
          write_file path ~contents ;
          ["-S"; path]
      | _ -> []
    in

    let* () =
      Helpers.exec installer_generator_path
      @@ [
           "get-reveal-installer";
           "-u";
           kernel_path;
           "-P";
           preimage_dir;
           "-o";
           args.installer_kernel_path;
         ]
      @ setup_file
    in
    unit

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ () = ()
end

type 'uri message =
  | Text : string -> 'uri message
  | Hex : string -> 'uri message
  | File : 'uri -> 'uri message

let message_encoding (type uri) (uri_encoding : uri Data_encoding.t) :
    uri message Data_encoding.t =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    union
      [
        c.mk_case
          "text"
          (obj1 (req "text" string))
          (function Text str -> Some str | _ -> None)
          (fun str -> Text str);
        c.mk_case
          "hex"
          (obj1 (req "hex" string))
          (function (Hex str : uri message) -> Some str | _ -> None)
          (fun str -> Hex str);
        c.mk_case
          "file"
          (obj1 (req "file" uri_encoding))
          (function File uri -> Some uri | _ -> None)
          (fun uri -> File uri);
      ])

let message_maximum_size = 4_095

let expand_message ~self ~run = function
  | Text str ->
      let str = run str in
      assert (String.length str <= 4_095) ;
      Text str
  | Hex str ->
      let str = run str in
      assert (String.length str <= 4_095 * 2) ;
      assert (str =~ rex {|^[a-f0-9]+$|}) ;
      Hex str
  | File uri ->
      let uri = Remote_procedure.global_uri_of_string ~self ~run uri in
      File uri

let resolve_message ~self resolver = function
  | Text str -> Text str
  | Hex str -> Hex str
  | File uri -> File (Remote_procedure.file_agent_uri ~self ~resolver uri)

let octez_client_arg_of_message state = function
  | Text str ->
      let (`Hex str) = Hex.of_string str in
      assert (String.length str <= message_maximum_size * 2) ;
      return str
  | Hex str ->
      assert (String.length str <= message_maximum_size * 2) ;
      return str
  | File uri ->
      let* path =
        Http_client.local_path_from_agent_uri
          (Agent_state.http_client state)
          uri
      in
      let contents = read_file path in
      let (`Hex str) = Hex.of_string contents in
      assert (String.length str <= message_maximum_size * 2) ;
      return str

type 'uri smart_rollups_add_messages = {
  client_base : 'uri client_base_args;
  wallet : string;
  source : string;
  messages : 'uri message list;
  wait : string;
}

type (_, _) Remote_procedure.t +=
  | Smart_rollups_add_messages :
      'uri smart_rollups_add_messages
      -> (unit, 'uri) Remote_procedure.t

module Smart_rollups_add_messages = struct
  let name = "tezos.operations.add_messages"

  type 'uri t = 'uri smart_rollups_add_messages

  type r = unit

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Smart_rollups_add_messages args -> Some args
    | _ -> None

  let to_remote_procedure args = Smart_rollups_add_messages args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Smart_rollups_add_messages _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {client_base; wallet; source; messages; wait} ->
          (client_base, (wallet, source, messages, wait)))
        (fun (client_base, (wallet, source, messages, wait)) ->
          {client_base; wallet; source; messages; wait})
        (merge_objs
           (client_base_args_encoding uri_encoding)
           (obj4
              (req "wallet" string)
              (req "source" string)
              (req "messages" (list (message_encoding uri_encoding)))
              (dft "wait" string "0"))))

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand ~self ~run args =
    let client_base = expand_client_base_args ~self ~run args.client_base in
    let wallet = run args.wallet in
    let source = run args.source in
    let messages = List.map (expand_message ~self ~run) args.messages in
    let wait = run args.wait in
    {client_base; wallet; source; messages; wait}

  let resolve ~self resolver args =
    let client_base =
      resolve_client_args_base ~self resolver args.client_base
    in
    let messages = List.map (resolve_message ~self resolver) args.messages in
    {args with client_base; messages}

  let run state args =
    let* messages =
      Lwt_list.map_p
        (fun m ->
          let* str = octez_client_arg_of_message state m in
          return (`String str))
        args.messages
    in
    let payload = "hex:" ^ Ezjsonm.to_string (`A messages) in

    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.client_base.path_client
    in
    let endpoint = octez_endpoint state args.client_base.endpoint in

    let client =
      Client.create ~path:path_client ~base_dir:args.wallet ~endpoint ()
    in
    Client.Sc_rollup.send_message
      ~wait:args.wait
      ~msg:payload
      ~src:args.source
      client

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ _args = ()
end

type 'uri dac_mode =
  | Coordinator of {committee_members_aliases : string list}
  | Member of {coordinator : 'uri; alias : string}
  | Observer of {
      coordinator : 'uri;
      committee_members : 'uri list;
      reveal_data_dir_path : string;
    }

let dac_mode_encoding uri_encoding =
  let c = Helpers.make_mk_case () in
  Data_encoding.(
    union
      [
        c.mk_case
          "coordinator"
          (obj2
             (req "mode" (constant "coordinator"))
             (req "committee_members_aliases" (list string)))
          (function
            | Coordinator {committee_members_aliases} ->
                Some ((), committee_members_aliases)
            | _ -> None)
          (fun ((), committee_members_aliases) ->
            Coordinator {committee_members_aliases});
        c.mk_case
          "member"
          (obj3
             (req "mode" (constant "member"))
             (req "alias" string)
             (req "coordinator" uri_encoding))
          (function
            | Member {alias; coordinator} -> Some ((), alias, coordinator)
            | _ -> None)
          (fun ((), alias, coordinator) -> Member {alias; coordinator});
        c.mk_case
          "observer"
          (obj4
             (req "mode" (constant "observer"))
             (req "coordinator" uri_encoding)
             (req "committee_members" (list uri_encoding))
             (req "reveal_data_dir_path" string))
          (function
            | Observer {coordinator; committee_members; reveal_data_dir_path} ->
                Some ((), coordinator, committee_members, reveal_data_dir_path)
            | _ -> None)
          (fun ((), coordinator, committee_members, reveal_data_dir_path) ->
            Observer {coordinator; committee_members; reveal_data_dir_path});
      ])

let expand_dac_mode ~self ~run = function
  | Coordinator {committee_members_aliases} ->
      let committee_members_aliases = List.map run committee_members_aliases in
      Coordinator {committee_members_aliases}
  | Member {alias; coordinator} ->
      let alias = run alias in
      let coordinator =
        Remote_procedure.global_uri_of_string ~self ~run coordinator
      in
      Member {alias; coordinator}
  | Observer {coordinator; committee_members; reveal_data_dir_path} ->
      let coordinator =
        Remote_procedure.global_uri_of_string ~self ~run coordinator
      in
      let committee_members =
        List.map
          (Remote_procedure.global_uri_of_string ~self ~run)
          committee_members
      in
      let reveal_data_dir_path = run reveal_data_dir_path in
      Observer {coordinator; committee_members; reveal_data_dir_path}

let resolve_dac_mode ~self resolver = function
  | Coordinator args -> Coordinator args
  | Member args ->
      let coordinator =
        resolve_dac_rpc_global_uri ~self ~resolver args.coordinator
      in
      Member {args with coordinator}
  | Observer {coordinator; committee_members; reveal_data_dir_path} ->
      let coordinator =
        resolve_dac_rpc_global_uri ~self ~resolver coordinator
      in
      let committee_members =
        List.map (resolve_dac_rpc_global_uri ~self ~resolver) committee_members
      in
      Observer {coordinator; committee_members; reveal_data_dir_path}

type 'uri start_dac_node = {
  path_dac_node : 'uri;
  path_client : 'uri;
  endpoint : 'uri;
  name : string option;
  wallet : string;
  rpc_port : string option;
  mode : 'uri dac_mode;
}

type start_dac_node_r = {name : string; rpc_port : int}

type (_, _) Remote_procedure.t +=
  | Start_dac_node :
      'uri start_dac_node
      -> (start_dac_node_r, 'uri) Remote_procedure.t

module Start_dac_node = struct
  let name = "dac.start_node"

  type 'uri t = 'uri start_dac_node

  type r = start_dac_node_r

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {
               path_dac_node;
               path_client;
               endpoint;
               name;
               wallet;
               rpc_port;
               mode;
             } ->
          (path_dac_node, path_client, endpoint, name, wallet, rpc_port, mode))
        (fun (path_dac_node, path_client, endpoint, name, wallet, rpc_port, mode)
             ->
          {path_dac_node; path_client; endpoint; name; wallet; rpc_port; mode})
        (obj7
           (req "path_dac_node" uri_encoding)
           (req "path_client" uri_encoding)
           (req "endpoint" uri_encoding)
           (opt "name" string)
           (req "wallet" string)
           (opt "rpc_port" string)
           (req "settings" (dac_mode_encoding uri_encoding))))

  let r_encoding =
    Data_encoding.(
      conv
        (fun {rpc_port; name} -> (rpc_port, name))
        (fun (rpc_port, name) -> {rpc_port; name})
        (obj2 (req "rpc_port" int31) (req "name" string)))

  let tvalue_of_r res =
    Tobj [("rpc_port", Tint res.rpc_port); ("name", Tstr res.name)]

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Start_dac_node args -> Some args
    | _ -> None

  let to_remote_procedure args = Start_dac_node args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Start_dac_node _ -> Eq
    | _ -> Neq

  let expand ~self ~run (args : _ t) =
    let name = Option.map run args.name in
    let path_dac_node =
      Remote_procedure.global_uri_of_string ~self ~run args.path_dac_node
    in
    let path_client =
      Remote_procedure.global_uri_of_string ~self ~run args.path_client
    in
    let endpoint =
      Remote_procedure.global_uri_of_string ~self ~run args.endpoint
    in
    let wallet = run args.wallet in
    let rpc_port = Option.map run args.rpc_port in
    let mode = expand_dac_mode ~self ~run args.mode in
    {name; path_dac_node; path_client; endpoint; wallet; rpc_port; mode}

  let resolve ~self resolver args =
    let path_dac_node =
      Remote_procedure.file_agent_uri ~self ~resolver args.path_dac_node
    in
    let path_client =
      Remote_procedure.file_agent_uri ~self ~resolver args.path_client
    in
    let endpoint = resolve_octez_rpc_global_uri ~self ~resolver args.endpoint in
    let mode = resolve_dac_mode ~self resolver args.mode in
    {args with path_dac_node; path_client; endpoint; mode}

  let run state args =
    let* path_dac_node =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.path_dac_node
    in
    let* path_client =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.path_client
    in
    let rpc_port =
      match args.rpc_port with
      | Some port_str -> int_of_string port_str
      | None -> Port.fresh ()
    in
    let endpoint = octez_endpoint state args.endpoint in
    let client =
      Client.create ~path:path_client ~base_dir:args.wallet ~endpoint ()
    in
    let* dac_node =
      match args.mode with
      | Coordinator {committee_members_aliases} ->
          let* committee_members =
            Lwt_list.map_p
              (fun name ->
                let* account = Client.bls_show_address client ~alias:name in
                return account.aggregate_public_key)
              committee_members_aliases
          in
          let dac_node =
            Dac_node.create_coordinator_with_endpoint
              ~path:path_dac_node
              ~rpc_host:"0.0.0.0"
              ~rpc_port
              ?name:args.name
              ~client
              ~endpoint
              ~committee_members
              ()
          in
          Agent_state.add
            (Dac_node_k (`Coordinator, Dac_node.name dac_node))
            dac_node
            state ;
          return dac_node
      | Member {alias; coordinator} ->
          let* member_account = Client.bls_show_address client ~alias in
          let coordinator_rpc_host, coordinator_rpc_port =
            dac_rpc_info state `Coordinator coordinator
          in
          let dac_node =
            Dac_node.create_committee_member_with_endpoint
              ~path:path_dac_node
              ~rpc_host:"0.0.0.0"
              ~rpc_port
              ?name:args.name
              ~client
              ~endpoint
              ~address:member_account.aggregate_public_key_hash
              ~coordinator_rpc_host
              ~coordinator_rpc_port
              ()
          in
          Agent_state.add
            (Dac_node_k (`Member, Dac_node.name dac_node))
            dac_node
            state ;
          return dac_node
      | Observer {coordinator; committee_members; reveal_data_dir_path} ->
          let coordinator_rpc_host, coordinator_rpc_port =
            dac_rpc_info state `Coordinator coordinator
          in
          let committee_member_rpcs =
            List.map (dac_rpc_info state `Member) committee_members
          in
          let dac_node =
            Dac_node.create_observer_with_endpoint
              ~path:path_dac_node
              ~rpc_host:"0.0.0.0"
              ?name:args.name
              ~client
              ~endpoint
              ~coordinator_rpc_host
              ~coordinator_rpc_port
              ~committee_member_rpcs
              ~reveal_data_dir:reveal_data_dir_path
              ()
          in
          Agent_state.add
            (Dac_node_k (`Observer, Dac_node.name dac_node))
            dac_node
            state ;
          return dac_node
    in

    let* _dir = Dac_node.init_config dac_node in
    let* () = Dac_node.run dac_node in
    return
      {name = Dac_node.name dac_node; rpc_port = Dac_node.rpc_port dac_node}

  let on_completion ~on_new_service ~on_new_metrics_source:_ res =
    on_new_service res.name Dac_node Rpc res.rpc_port
end

type 'uri dac_post_file = {
  dac_client_path : 'uri;
  coordinator : 'uri;
  file_path : 'uri;
  wallet : string;
  threshold : int;
}

type dac_post_file_r = {certificate : Hex.t}

type (_, _) Remote_procedure.t +=
  | Dac_post_file :
      'uri dac_post_file
      -> (dac_post_file_r, 'uri) Remote_procedure.t

module Dac_post_file = struct
  let name = "dac.post_file"

  type 'uri t = 'uri dac_post_file

  type r = dac_post_file_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Dac_post_file args -> Some args
    | _ -> None

  let to_remote_procedure args = Dac_post_file args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Dac_post_file _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {dac_client_path; coordinator; file_path; wallet; threshold} ->
          (dac_client_path, coordinator, file_path, wallet, threshold))
        (fun (dac_client_path, coordinator, file_path, wallet, threshold) ->
          {dac_client_path; coordinator; file_path; wallet; threshold})
        (obj5
           (req "dac_client_path" uri_encoding)
           (req "coordinator" uri_encoding)
           (req "file_path" uri_encoding)
           (req "wallet" string)
           (req "threshold" int31)))

  let r_encoding =
    Data_encoding.(
      conv
        (fun {certificate = `Hex s} -> s)
        (fun s -> {certificate = `Hex s})
        (obj1 (req "certificate" string)))

  let tvalue_of_r {certificate = `Hex s} = Tobj [("certificate", Tstr s)]

  let expand ~self ~run args =
    let dac_client_path =
      Remote_procedure.global_uri_of_string ~self ~run args.dac_client_path
    in
    let file_path =
      Remote_procedure.global_uri_of_string ~self ~run args.file_path
    in
    let coordinator =
      Remote_procedure.global_uri_of_string ~self ~run args.coordinator
    in
    let wallet = run args.wallet in
    {args with dac_client_path; wallet; coordinator; file_path}

  let resolve ~self resolver args =
    let dac_client_path =
      Remote_procedure.file_agent_uri ~self ~resolver args.dac_client_path
    in
    let file_path =
      Remote_procedure.file_agent_uri ~self ~resolver args.file_path
    in
    let coordinator =
      resolve_dac_rpc_global_uri ~self ~resolver args.coordinator
    in
    {args with dac_client_path; file_path; coordinator}

  let run state args =
    let* client_path =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.dac_client_path
    in
    let endpoint = dac_endpoint state `Coordinator args.coordinator in
    let client =
      Dac_client.create_with_endpoint
        ~path:client_path
        ~base_dir:args.wallet
        endpoint
    in
    let* file =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        args.file_path
    in
    let* output =
      Dac_client.send_payload_from_file client file ~threshold:args.threshold
    in
    match output with
    | Certificate hex -> return {certificate = hex}
    | _ -> Test.fail "Should be a certificate"

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ _res = ()
end

type 'uri kind =
  | Default (* Generate default Tezt bootstrap keys (from 0 to 5). *)
  | Fresh of {
      count : int; (* Generate [n] fresh keys *)
      path_client : 'uri; (* Path to octez-client binary. *)
      alias_prefix : string; (* A prefix for generated keys aliases. *)
    }

type 'uri generate_keys = {
  base_dir : 'uri; (* The directory where the generated keys will be stored. *)
  kind : 'uri kind; (* Kind of wallets to generate. *)
}

type generate_keys_r = unit

type (_, _) Remote_procedure.t +=
  | Generate_keys :
      'uri generate_keys
      -> (generate_keys_r, 'uri) Remote_procedure.t

module Generate_keys = struct
  let name = "tezos.generate_keys"

  type 'uri t = 'uri generate_keys

  type r = generate_keys_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Generate_keys args -> Some args
    | _ -> None

  let to_remote_procedure args = Generate_keys args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Generate_keys _ -> Eq
    | _ -> Neq

  let kind_encoding uri_encoding =
    let c = Helpers.make_mk_case () in
    Data_encoding.(
      union
        [
          c.mk_case
            "default"
            (obj1 (req "default" unit))
            (function Default -> Some () | _ -> None)
            (fun () -> Default);
          c.mk_case
            "fresh"
            (obj1
               (req
                  "fresh"
                  (obj3
                     (req "count" uint16)
                     (req "path_client" uri_encoding)
                     (dft "alias_prefix" string "boot"))))
            (function
              | Fresh {count; path_client; alias_prefix} ->
                  Some (count, path_client, alias_prefix)
              | _ -> None)
            (fun (count, path_client, alias_prefix) ->
              Fresh {count; path_client; alias_prefix});
        ])

  let encoding uri_encoding =
    let open Data_encoding in
    conv
      (fun {base_dir; kind} -> (base_dir, kind))
      (fun (base_dir, kind) -> {base_dir; kind})
      (obj2
         (req "base_dir" uri_encoding)
         (dft "kind" (kind_encoding uri_encoding) Default))

  let r_encoding = Data_encoding.null

  let tvalue_of_r (() : r) = Tnull

  let expand ~self ~run {base_dir; kind} =
    let kind =
      match kind with
      | Default -> Default
      | Fresh {count; path_client; alias_prefix} ->
          Fresh
            {
              count;
              path_client =
                Remote_procedure.global_uri_of_string ~self ~run path_client;
              alias_prefix = run alias_prefix;
            }
    in
    {base_dir = Remote_procedure.global_uri_of_string ~self ~run base_dir; kind}

  let resolve ~self resolver {base_dir; kind} =
    let kind =
      match kind with
      | Default -> Default
      | Fresh {count; path_client; alias_prefix} ->
          Fresh
            {
              count;
              path_client =
                Remote_procedure.file_agent_uri ~self ~resolver path_client;
              alias_prefix;
            }
    in
    {base_dir = Remote_procedure.file_agent_uri ~self ~resolver base_dir; kind}

  let run state {base_dir; kind} =
    let client = Agent_state.http_client state in
    let* base_dir = Http_client.local_path_from_agent_uri client base_dir in
    let* base_dir_exists = Lwt_unix.file_exists base_dir in
    let* () =
      (* this distinction is only needed in the Fresh case *)
      if not base_dir_exists then Lwt_unix.mkdir base_dir 0x755 else unit
    in
    match kind with
    | Default ->
        Account.write Constant.all_secret_keys ~base_dir ;
        unit
    | Fresh {count; path_client; alias_prefix} ->
        let* path = Http_client.local_path_from_agent_uri client path_client in
        let* _accounts =
          Client.create ~path ~base_dir ()
          |> Client.stresstest_gen_keys ~alias_prefix count
        in
        unit

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ (_ : r) = ()
end

type start_dal_node_r = {
  name : string;
  rpc_port : int;
  metrics_port : int;
  net_port : int;
}

type dal_attester_profile = string (* a public key hash or an alias *)

type dal_producer_profile =
  string (* slot index, as a string to be able to expand it. *)

type 'uri start_dal_node = {
  name : string option;
  path_node : 'uri;
  rpc_port : string option;
  metrics_port : string option;
  net_port : string option;
  l1_node_uri : 'uri;
  peers : string list;
  bootstrap_profile : bool;
  attester_profiles : dal_attester_profile list;
  producer_profiles : dal_producer_profile list;
  path_client : 'uri option; (* Needed if some attester profiles are aliases. *)
  base_dir : 'uri option; (* Needed if some attester profiles are aliases. *)
}

type (_, _) Remote_procedure.t +=
  | Start_octez_dal_node :
      'uri start_dal_node
      -> (start_dal_node_r, 'uri) Remote_procedure.t

module Start_octez_dal_node = struct
  let name = "tezos.start_dal_node"

  type 'uri t = 'uri start_dal_node

  type r = start_dal_node_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Start_octez_dal_node args -> Some args
    | _ -> None

  let to_remote_procedure args = Start_octez_dal_node args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Start_octez_dal_node _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    let open Data_encoding in
    conv
      (fun {
             name;
             path_node;
             rpc_port;
             metrics_port;
             net_port;
             l1_node_uri;
             peers;
             bootstrap_profile;
             attester_profiles;
             producer_profiles;
             path_client;
             base_dir;
           } ->
        ( ( name,
            path_node,
            rpc_port,
            metrics_port,
            net_port,
            l1_node_uri,
            peers,
            bootstrap_profile,
            attester_profiles,
            producer_profiles ),
          (path_client, base_dir) ))
      (fun ( ( name,
               path_node,
               rpc_port,
               metrics_port,
               net_port,
               l1_node_uri,
               peers,
               bootstrap_profile,
               attester_profiles,
               producer_profiles ),
             (path_client, base_dir) ) ->
        {
          name;
          path_node;
          rpc_port;
          metrics_port;
          net_port;
          l1_node_uri;
          peers;
          bootstrap_profile;
          attester_profiles;
          producer_profiles;
          path_client;
          base_dir;
        })
      (merge_objs
         (obj10
            (opt "name" string)
            (req "path_node" uri_encoding)
            (opt "rpc_port" string)
            (opt "metrics_port" string)
            (opt "net_port" string)
            (req "l1_node_uri" uri_encoding)
            (dft "peers" (list string) [])
            (dft "bootstrap_profile" bool false)
            (dft "attester_profiles" (list string) [])
            (dft "producer_profiles" (list string) []))
         (obj2 (opt "path_client" uri_encoding) (opt "base_dir" uri_encoding)))

  let r_encoding =
    Data_encoding.(
      conv
        (fun ({name; rpc_port; metrics_port; net_port} : start_dal_node_r) ->
          (name, rpc_port, metrics_port, net_port))
        (fun (name, rpc_port, metrics_port, net_port) ->
          {name; rpc_port; metrics_port; net_port})
        (obj4
           (req "name" string)
           (req "rpc_port" int31)
           (req "metrics_port" int31)
           (req "net_port" int31)))

  let tvalue_of_r ({name; rpc_port; metrics_port; net_port} : r) =
    Tobj
      [
        ("name", Tstr name);
        ("rpc_port", Tint rpc_port);
        ("metrics_port", Tint metrics_port);
        ("net_port", Tint net_port);
      ]

  let expand ~self ~run
      {
        name;
        path_node;
        rpc_port;
        metrics_port;
        net_port;
        l1_node_uri;
        peers;
        bootstrap_profile;
        attester_profiles;
        producer_profiles;
        path_client;
        base_dir;
      } =
    let uri_run = Remote_procedure.global_uri_of_string ~self ~run in
    {
      name = Option.map run name;
      path_node = uri_run path_node;
      rpc_port = Option.map run rpc_port;
      metrics_port = Option.map run metrics_port;
      net_port = Option.map run net_port;
      l1_node_uri = uri_run l1_node_uri;
      peers = List.map run peers;
      bootstrap_profile;
      attester_profiles = List.map run attester_profiles;
      producer_profiles = List.map run producer_profiles;
      path_client = Option.map uri_run path_client;
      base_dir = Option.map uri_run base_dir;
    }

  let resolve ~self resolver
      {
        name;
        path_node;
        rpc_port;
        metrics_port;
        net_port;
        l1_node_uri;
        peers;
        bootstrap_profile;
        attester_profiles;
        producer_profiles;
        path_client;
        base_dir;
      } =
    let file_agent_uri = Remote_procedure.file_agent_uri ~self ~resolver in
    {
      name;
      path_node = file_agent_uri path_node;
      rpc_port;
      metrics_port;
      net_port;
      l1_node_uri = resolve_octez_rpc_global_uri ~self ~resolver l1_node_uri;
      peers;
      bootstrap_profile;
      attester_profiles;
      producer_profiles;
      path_client = Option.map file_agent_uri path_client;
      base_dir = Option.map file_agent_uri base_dir;
    }

  let run state
      {
        name;
        path_node;
        rpc_port;
        metrics_port;
        net_port;
        l1_node_uri;
        peers;
        bootstrap_profile;
        attester_profiles;
        producer_profiles;
        path_client;
        base_dir;
      } =
    let* path_dal_node =
      Http_client.local_path_from_agent_uri
        (Agent_state.http_client state)
        path_node
    in
    let get_port_or_fresh = function
      | Some port -> int_of_string port
      | None -> Port.fresh ()
    in
    let mk_addr port = Format.sprintf "0.0.0.0:%d" port in
    let producer_profiles = List.map positive_int_of_string producer_profiles in
    let* client_opt =
      match (path_client, base_dir) with
      | Some path_client, Some base_dir ->
          let* base_dir =
            (Http_client.local_path_from_agent_uri
               (Agent_state.http_client state))
              base_dir
          in
          let* path_client =
            (Http_client.local_path_from_agent_uri
               (Agent_state.http_client state))
              path_client
          in
          Client.create ~path:path_client ~base_dir () |> Option.some |> return
      | _ -> return None
    in
    let* attester_profiles =
      Lwt_list.map_s
        (fun attester ->
          match
            ( Tezos_crypto.Signature.Public_key_hash.of_b58check_opt attester,
              client_opt )
          with
          | Some _, _ -> return attester (* a valid tz address is given *)
          | None, Some client ->
              (* not a valid tz address. Probably an alias. *)
              let* account = Client.show_address ~alias:attester client in
              return account.Account.public_key_hash
          | None, None ->
              Test.fail
                "A client_path and a base_dir are needed to retrieve the pkh \
                 of the attester profile with alias %s"
                attester)
        attester_profiles
    in
    let rpc_port = get_port_or_fresh rpc_port in
    let metrics_port = get_port_or_fresh metrics_port in
    let net_port = get_port_or_fresh net_port in
    let dal_node =
      Dal_node.create_from_endpoint
        ~path:path_dal_node
        ?name
        ~rpc_port
        ~listen_addr:(mk_addr net_port)
        ~metrics_addr:(mk_addr metrics_port)
        ~l1_node_endpoint:(octez_endpoint state l1_node_uri)
        ()
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/6283

       Use default PoW and add an option to override it. *)
    let* () =
      Dal_node.init_config
        ~expected_pow:0.
        ~peers
        ~attester_profiles
        ~producer_profiles
        ~bootstrap_profile
        dal_node
    in
    (* Start the DAL node and wait until it's ready. *)
    let* () = Dal_node.run dal_node in
    Agent_state.add (Dal_node_k (Dal_node.name dal_node)) dal_node state ;
    return {name = Dal_node.name dal_node; rpc_port; metrics_port; net_port}

  let on_completion ~on_new_service ~on_new_metrics_source (res : r) =
    let open Services_cache in
    on_new_service res.name Dal_node Rpc res.rpc_port ;
    on_new_service res.name Dal_node Metrics res.metrics_port ;
    on_new_service res.name Dal_node P2p res.net_port ;
    on_new_metrics_source res.name Dal_node res.metrics_port
end

type 'uri start_octez_baker = {
  name : string option;
      (** A name for this agent. It will be generated if no name is given. *)
  protocol : Protocol.t;
      (** The baker's protocol. One of those in {!Protocol.t}. *)
  base_dir : 'uri;  (** The --base-dir of the baker. *)
  node_uri : 'uri;
      (** An URI to a Layer 1 node (either "Owned/Managed" or "Remote". *)
  node_data_dir : 'uri option;
      (** A path to the --data-dir of the Layer 1 node. Only set if the node_uri
          is "Remote".  Otherwise, this path is already provided by the
          [node_uri]. *)
  dal_node_uri : 'uri option;  (** An URI to a DAL node. *)
  delegates : string list;
      (** A list of delegates handled by this baker agent. The baker will handle
          all the delegates in the wallet found in [base_dir] if no
          delegate is given. *)
  baker_path : 'uri option;
}

type start_octez_baker_r = {name : string}

type (_, _) Remote_procedure.t +=
  | Start_octez_baker :
      'uri start_octez_baker
      -> (start_octez_baker_r, 'uri) Remote_procedure.t

module Start_octez_baker = struct
  let name = "tezos.start_baker"

  type 'uri t = 'uri start_octez_baker

  type r = start_octez_baker_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Start_octez_baker args -> Some args
    | _ -> None

  let to_remote_procedure args = Start_octez_baker args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Start_octez_baker _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    let open Data_encoding in
    conv
      (fun {
             name;
             protocol;
             base_dir;
             node_uri;
             node_data_dir;
             dal_node_uri;
             delegates;
             baker_path;
           } ->
        ( name,
          protocol,
          base_dir,
          node_uri,
          node_data_dir,
          dal_node_uri,
          delegates,
          baker_path ))
      (fun ( name,
             protocol,
             base_dir,
             node_uri,
             node_data_dir,
             dal_node_uri,
             delegates,
             baker_path ) ->
        {
          name;
          protocol;
          base_dir;
          node_uri;
          node_data_dir;
          dal_node_uri;
          delegates;
          baker_path;
        })
      (obj8
         (opt "name" string)
         (dft "protocol" Protocol.encoding Protocol.Alpha)
         (req "base_dir" uri_encoding)
         (req "node_uri" uri_encoding)
         (opt "node_data_dir" uri_encoding)
         (opt "dal_node_uri" uri_encoding)
         (dft "delegates" (list string) [])
         (opt "baker_path" uri_encoding))

  let r_encoding =
    let open Data_encoding in
    conv (fun {name} -> name) (fun name -> {name}) (obj1 (req "name" string))

  let tvalue_of_r ({name} : r) = Tobj [("name", Tstr name)]

  let expand ~self ~run
      {
        name;
        protocol;
        base_dir;
        node_uri;
        node_data_dir;
        dal_node_uri;
        delegates;
        baker_path;
      } =
    let uri_run = Remote_procedure.global_uri_of_string ~self ~run in
    {
      name = Option.map run name;
      protocol;
      base_dir = uri_run base_dir;
      node_uri = uri_run node_uri;
      node_data_dir = Option.map uri_run node_data_dir;
      dal_node_uri = Option.map uri_run dal_node_uri;
      delegates = List.map run delegates;
      baker_path = Option.map uri_run baker_path;
    }

  let resolve ~self resolver
      {
        name;
        protocol;
        base_dir;
        node_uri;
        node_data_dir;
        dal_node_uri;
        delegates;
        baker_path;
      } =
    let file_agent_uri = Remote_procedure.file_agent_uri in
    {
      name;
      protocol;
      base_dir = file_agent_uri ~self ~resolver base_dir;
      node_data_dir = Option.map (file_agent_uri ~self ~resolver) node_data_dir;
      node_uri = resolve_octez_rpc_global_uri ~self ~resolver node_uri;
      dal_node_uri =
        Option.map (resolve_dal_rpc_global_uri ~self ~resolver) dal_node_uri;
      delegates;
      baker_path =
        Option.map (resolve_dal_rpc_global_uri ~self ~resolver) baker_path;
    }

  let run state
      {
        name;
        protocol;
        base_dir;
        node_uri;
        node_data_dir;
        delegates;
        dal_node_uri;
        baker_path;
      } =
    let client = Agent_state.http_client state in
    (* Get the L1 node's data-dir and RPC endpoint. *)
    let* node_data_dir, node_rpc_endpoint =
      match (node_data_dir, octez_endpoint state node_uri) with
      | Some node_data_dir, Client.Foreign_endpoint fe ->
          let* node_data_dir =
            Http_client.local_path_from_agent_uri client node_data_dir
          in
          Lwt.return (node_data_dir, fe)
      | None, Client.Node node ->
          Lwt.return (Node.data_dir node, Node.as_rpc_endpoint node)
      | _, Client.Proxy_server _ ->
          Test.fail "Proxy_server not supported as a Node endpoint for baking"
      | Some _, Client.Node _ ->
          Test.fail
            "Should not provide both a node data dir and an 'owned' node"
      | None, Client.Foreign_endpoint _ ->
          Test.fail
            "Should provide a node data dir for a node given as foreign \
             endpoint"
    in
    let* baker_path =
      match baker_path with
      | None -> return None
      | Some baker_path ->
          let* baker_path =
            Http_client.local_path_from_agent_uri client baker_path
          in
          return @@ Some baker_path
    in
    (* Get the wallet's base-dir. *)
    let* base_dir = Http_client.local_path_from_agent_uri client base_dir in
    (* Get the DAL node's RPC endpoint. *)
    let dal_node_rpc_endpoint =
      Option.map (dal_foreign_endpoint state) dal_node_uri
    in
    (* Create a baker state. *)
    let octez_baker =
      Baker.create_from_uris
        ?name
        ?path:baker_path
        ~protocol
        ~base_dir
        ~node_data_dir
        ~node_rpc_endpoint
        ?dal_node_rpc_endpoint
        ~delegates
        ()
    in
    (* Register the baker state. *)
    let name = Baker.name octez_baker in
    Agent_state.add (Octez_baker_k name) octez_baker state ;
    (* Start the baker. *)
    let* () = Baker.run octez_baker in
    return {name}

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ (_ : r) = ()
end

type publish_dal_slot_r = {commitment : string}

type publish_slot_info = {
  slot_index : string;
  slot_size : string;
  payload : string;
}

let publish_slot_info_encoding =
  let open Data_encoding in
  conv
    (fun {slot_index; slot_size; payload} -> (slot_index, slot_size, payload))
    (fun (slot_index, slot_size, payload) -> {slot_index; slot_size; payload})
    (obj3
       (req "slot_index" string)
       (req "slot_size" string)
       (req "payload" string))

type 'uri publish_dal_slot = {
  slot_info : publish_slot_info;
  target_published_level : string option;
      (** We target the inclusion of the publish commitment operation at this
          level, so the operation should be inject at least one level before. *)
  l1_node_uri : 'uri;
      (** An URI to a Layer 1 node. If [None], we target the next level. *)
  dal_node_uri : 'uri;  (** An URI to a DAL node used for injection. *)
  base_dir : 'uri;
  source : string;  (** The alias of the account that signs the operation. *)
  path_client : 'uri;
}

type (_, _) Remote_procedure.t +=
  | Publish_dal_slot :
      'uri publish_dal_slot
      -> (publish_dal_slot_r, 'uri) Remote_procedure.t

module Publish_dal_slot : Remote_procedure.S = struct
  let name = "tezos.publish_dal_slot"

  type 'uri t = 'uri publish_dal_slot

  type r = publish_dal_slot_r

  let of_remote_procedure :
      type a. (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Publish_dal_slot args -> Some args
    | _ -> None

  let to_remote_procedure args = Publish_dal_slot args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Publish_dal_slot _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    let open Data_encoding in
    conv
      (fun {
             slot_info;
             target_published_level;
             l1_node_uri;
             dal_node_uri;
             base_dir;
             source;
             path_client;
           } ->
        ( slot_info,
          target_published_level,
          l1_node_uri,
          dal_node_uri,
          base_dir,
          source,
          path_client ))
      (fun ( slot_info,
             target_published_level,
             l1_node_uri,
             dal_node_uri,
             base_dir,
             source,
             path_client ) ->
        {
          slot_info;
          target_published_level;
          l1_node_uri;
          dal_node_uri;
          base_dir;
          source;
          path_client;
        })
      (obj7
         (req "slot_info" publish_slot_info_encoding)
         (opt "target_published_level" string)
         (req "l1_node_uri" uri_encoding)
         (req "dal_node_uri" uri_encoding)
         (req "base_dir" uri_encoding)
         (req "source" string)
         (req "path_client" uri_encoding))

  let r_encoding =
    let open Data_encoding in
    conv
      (fun {commitment} -> commitment)
      (fun commitment -> {commitment})
      (obj1 (req "commitment" string))

  let tvalue_of_r ({commitment} : r) = Tobj [("commitment", Tstr commitment)]

  let expand ~self ~run
      {
        slot_info = {slot_index; slot_size; payload};
        target_published_level;
        l1_node_uri;
        dal_node_uri;
        base_dir;
        source;
        path_client;
      } =
    let slot_info =
      {
        slot_index = run slot_index;
        slot_size = run slot_size;
        payload = run payload;
      }
    in
    let uri_run = Remote_procedure.global_uri_of_string ~self ~run in
    {
      slot_info;
      target_published_level = Option.map run target_published_level;
      l1_node_uri = uri_run l1_node_uri;
      dal_node_uri = uri_run dal_node_uri;
      base_dir = uri_run base_dir;
      source = run source;
      path_client = uri_run path_client;
    }

  let resolve ~self (resolver : Uri_resolver.t)
      {
        slot_info;
        target_published_level;
        l1_node_uri;
        dal_node_uri;
        base_dir;
        source;
        path_client;
      } =
    {
      slot_info;
      target_published_level;
      l1_node_uri = resolve_octez_rpc_global_uri ~self ~resolver l1_node_uri;
      dal_node_uri = resolve_dal_rpc_global_uri ~self ~resolver dal_node_uri;
      base_dir = Remote_procedure.file_agent_uri ~self ~resolver base_dir;
      source;
      path_client = Remote_procedure.file_agent_uri ~self ~resolver path_client;
    }

  let run state
      {
        slot_info = {slot_index; slot_size; payload};
        target_published_level;
        l1_node_uri;
        dal_node_uri;
        base_dir;
        source;
        path_client;
      } =
    let mk_path =
      Http_client.local_path_from_agent_uri (Agent_state.http_client state)
    in
    let* path_client = mk_path path_client in
    let* base_dir = mk_path base_dir in
    let endpoint = octez_endpoint state l1_node_uri in
    let dal_endpoint = dal_foreign_endpoint state dal_node_uri in
    let client = Client.create ~path:path_client ~endpoint ~base_dir () in

    (* step 1: we publish the slot to the DAL node even before [injection_level]
       is reached, as the crypto part takes time. *)
    let* commitment, proof =
      Dal_common.Helpers.(
        make_slot ~slot_size:(positive_int_of_string slot_size) payload
        |> store_slot ~with_proof:true (Either.Right dal_endpoint))
    in

    let* publish_to_l1 =
      match target_published_level with
      | None ->
          (* step 2.a: if no target_published_level is given, we publish to L1
             after waiting for a new block with an increasing level. *)
          let* current_level =
            Client.as_foreign_endpoint endpoint |> current_level
          in
          let* _ = wait_for_l1_level endpoint (current_level + 1) in
          return true
      | Some target_published_level ->
          let injection_level =
            positive_int_of_string target_published_level - 1
          in
          (* step 2.b: if a target_published_level is given, we wait until level
             [target_published_level - 1] to publish to L1. If the current level
             is expected one, the operations will be injected and will (hopefully)
             be included in the next block. Otherwise, the current level is greater
             than the expected level, and we don't publish. *)
          let* current_level = wait_for_l1_level endpoint injection_level in
          return (current_level = injection_level)
    in
    let* () =
      if publish_to_l1 then
        (* step 3: Now, we publish the corresponding DAL commitment to L1. Note
           that the operation can be branch-delayed if there is another pending
           commitment for the previous level from the same source. *)
        let*! () =
          Client.publish_dal_commitment
            ~src:source
            ~slot_index:(positive_int_of_string slot_index)
            ~commitment
            ~proof
            client
        in
        unit
      else unit
    in
    return {commitment}

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ (_res : r) = ()
end

let register_procedures () =
  Remote_procedure.register (module Generate_keys) ;
  Remote_procedure.register (module Start_octez_node) ;
  Remote_procedure.register (module Generate_protocol_parameters_file) ;
  Remote_procedure.register (module Activate_protocol) ;
  Remote_procedure.register (module Wait_for_bootstrapped) ;
  Remote_procedure.register (module Originate_smart_rollup) ;
  Remote_procedure.register (module Originate_smart_contract) ;
  Remote_procedure.register (module Start_rollup_node) ;
  Remote_procedure.register (module Prepare_kernel_installer) ;
  Remote_procedure.register (module Smart_rollups_add_messages) ;
  Remote_procedure.register (module Start_dac_node) ;
  Remote_procedure.register (module Dac_post_file) ;
  Remote_procedure.register (module Start_octez_dal_node) ;
  Remote_procedure.register (module Start_octez_baker) ;
  Remote_procedure.register (module Publish_dal_slot)
