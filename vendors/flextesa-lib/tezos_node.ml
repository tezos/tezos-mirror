open Internal_pervasives

type custom_network = [`Json of Ezjsonm.value]

type t =
  { id: string
  ; expected_connections: int
  ; rpc_port: int
  ; p2p_port: int
  ; (* Ports: *)
    peers: int list
  ; exec: Tezos_executable.t
  ; protocol: Tezos_protocol.t
  ; history_mode: [`Archive | `Full of int | `Rolling of int] option
  ; single_process: bool
  ; cors_origin: string option
  ; custom_network: custom_network option }

let compare a b = Base.String.compare a.id b.id
let equal a b = Base.String.equal a.id b.id

let ef t =
  EF.(
    desc_list (af "Node:%S" t.id)
      [ desc (af "rpc") (af ":%d" t.rpc_port)
      ; desc (af "p2p") (af ":%d" t.p2p_port)
      ; desc_list (af "peers") (List.map t.peers ~f:(af ":%d")) ])

let pp fmt t = Easy_format.Pretty.to_formatter fmt (ef t)
let id t = t.id

let make ?cors_origin ~exec ?(protocol = Tezos_protocol.default ())
    ?custom_network ?(single_process = true) ?history_mode id
    ~expected_connections ~rpc_port ~p2p_port peers =
  { id
  ; expected_connections
  ; rpc_port
  ; p2p_port
  ; peers
  ; exec
  ; protocol
  ; history_mode
  ; single_process
  ; cors_origin
  ; custom_network }

let make_path p ~config t = Paths.root config // sprintf "node-%s" t.id // p

(* Data-dir should not exist OR be fully functional. *)
let data_dir config t = make_path "data-dir" ~config t
let config_file config t = data_dir config t // "config.json"
let identity_file config t = data_dir config t // "identity.json"
let log_output config t = make_path "node-output.log" ~config t
let exec_path config t = make_path ~config "exec" t

module Config_file = struct
  (*
     This module purposely avoids using the node's modules because we
     want the sandbox to be able to configure ≥ 1 versions of the
     node.

     Choosing the default hash:

      $ ./flextesa vani SBox --att 10_000_000
     Flextesa.vanity-chain-id:  Looking for "SBox"
     Flextesa.vanity-chain-id:
       Results:
         * Seed: "flextesa5316422" → block: "BLdZYwNF8Rn6zrTWkuRRNyrj6bQWPkfBog2YKhWhn5z3ApmpzBf" → chain-id: "NetXKMbjQL2SBox"


   *)

  open Tezos_rpc_http_server

  let network
      ?(genesis_hash = "BLdZYwNF8Rn6zrTWkuRRNyrj6bQWPkfBog2YKhWhn5z3ApmpzBf") ()
      =
    let open Ezjsonm in
    [ ( "genesis"
      , dict
          [ ("timestamp", string "2018-06-30T16:07:32Z")
          ; ("block", string genesis_hash)
          ; ( "protocol"
            , string "Ps9mPmXaRzmzk35gbAYNCAw6UXdE2qoABTHbN2oEEc1qM7CwT9P" ) ]
      ); ("chain_name", string "TEZOS_MAINNET")
    ; ("old_chain_name", string "TEZOS_BETANET_2018-06-30T16:07:32Z")
    ; ("incompatible_chain_name", string "INCOMPATIBLE")
    ; ("sandboxed_chain_name", string "SANDBOXED_TEZOS_MAINNET") ]

  let default_network = network ()

  (* These tests try to do more than the default RPC ACL allows over the
     network. Even though all processes are run on localhost, nodes are set up
     to listen to RPC requests on address 0.0.0.0 (see below), which matches all
     possible addresses. Hence the "localhost rule" of RPC ACLs does not apply
     and the secure ACL is chosen by default. Thus a specific, more permissive
     policy is needed. For more details see Node Configuration manual page or
     https://gitlab.com/tezos/tezos/-/merge_requests/3164#note_616452409 *)
  let acl : string list =
    match RPC_server.Acl.secure with
    | Allow_all _ ->
        raise
          (Failure
             "Expected RPC secure ACL in the form of whitelist. Got a \
              blacklist instead." )
    | Deny_all {except} ->
        List.map ~f:RPC_server.Acl.matcher_to_string except
        @ [ "GET /chains/*/blocks/*/helpers/baking_rights"
          ; "GET /chains/*/blocks/*/helpers/endorsing_rights"
          ; "GET /chains/*/blocks/*/helpers/proposers"
          ; "GET /chains/*/blocks/*/helpers/validators"
          ; "GET /chains/*/blocks/*/helpers/levels_in_current_cycle"
          ; "POST /chains/*/blocks/*/helpers/forge/operations"
          ; "POST /chains/*/blocks/*/helpers/preapply/*"
          ; "POST /chains/*/blocks/*/helpers/scripts/run_operation"
          ; "POST /chains/*/blocks/*/helpers/scripts/simulate_operation"
          ; "POST /chains/*/mempool/request_operations"; "POST /injection/block"
          ; "POST /injection/protocol" ]

  let of_node state t =
    let open Ezjsonm in
    let shell =
      match t.history_mode with
      | None -> []
      | Some h ->
          [ ( "shell"
            , dict
                [ ( "history_mode"
                  , match h with
                    | `Archive -> string "archive"
                    | `Full off ->
                        dict [("full", dict [("additional_cycles", int off)])]
                    | `Rolling off ->
                        dict [("rolling", dict [("additional_cycles", int off)])]
                  ) ] ) ] in
    let network =
      Option.value_map t.custom_network
        ~default:[("network", `O default_network)]
        ~f:(function `Json j -> [("network", j)]) in
    let rpc_listen_addr = sprintf "0.0.0.0:%d" t.rpc_port in
    [ ("data-dir", data_dir state t |> string)
    ; ( "rpc"
      , dict
          [ ("listen-addrs", strings [rpc_listen_addr])
          ; ( "acl"
            , list dict
                [ [ ("address", string rpc_listen_addr)
                  ; ("whitelist", strings acl) ] ] ) ] )
    ; ( "p2p"
      , dict
          [ ( "expected-proof-of-work"
            , int (Tezos_protocol.expected_pow t.protocol) )
          ; ("listen-addr", ksprintf string "0.0.0.0:%d" t.p2p_port)
          ; ( "limits"
            , dict
                [ ("maintenance-idle-time", int 3); ("swap-linger", int 2)
                ; ("connection-timeout", int 2) ] ) ] )
    ; ("log", dict [("output", string (log_output state t))]) ]
    @ shell @ network
    |> dict |> to_string ~minify:false
end

open Tezos_executable.Make_cli

let node_command state t cmd options =
  Tezos_executable.call state t.exec ~path:(exec_path state t)
    ( cmd
    @ opt "config-file" (config_file state t)
    @ opt "data-dir" (data_dir state t)
    @ options )

let run_command state t =
  let peers = List.concat_map t.peers ~f:(optf "peer" "127.0.0.1:%d") in
  let cors_origin =
    match t.cors_origin with
    | Some _ as s -> s
    | None -> Environment_configuration.default_cors_origin state in
  node_command state t ["run"]
    ( flag "private-mode" @ flag "no-bootstrap-peers" @ peers
    @ optf "synchronisation-threshold" "0"
    @ optf "connections" "%d" t.expected_connections
    @ (if t.single_process then flag "singleprocess" else [])
    @ Option.value_map cors_origin
        ~f:(fun s ->
          flag "cors-header=content-type" @ Fmt.kstr flag "cors-origin=%s" s )
        ~default:[]
    @ opt "sandbox" (Tezos_protocol.sandbox_path state t.protocol) )

let start_script state t =
  let open Genspio.EDSL in
  let gen_id =
    node_command state t
      [ "identity"; "generate"
      ; sprintf "%d" (Tezos_protocol.expected_pow t.protocol) ]
      [] in
  let tmp_config = tmp_file (config_file state t) in
  check_sequence ~verbosity:`Output_all
    [ ("reset-config", node_command state t ["config"; "reset"] [])
    ; ( "write-config"
      , seq
          [ tmp_config#set (Config_file.of_node state t |> str)
          ; call [str "mv"; tmp_config#path; str (config_file state t)] ] )
    ; ( "ensure-identity"
      , ensure "node-id"
          ~condition:(file_exists (str (identity_file state t)))
          ~how:[("gen-id", gen_id)] ); ("start", run_command state t) ]

let process state t =
  Running_processes.Process.genspio t.id (start_script state t)

let protocol t = t.protocol

let connections node_list =
  let module Connection = struct
    type node = t

    type t =
      [`Duplex of node * node | `From_to of node * node | `Missing of node * int]

    let compare a b =
      match (a, b) with
      | `Duplex (a, b), `Duplex (c, d) when equal a d && equal b c -> 0
      | `Duplex _, _ -> -1
      | _, `Duplex _ -> 1
      | _, _ -> Caml.compare a b
  end in
  let module Connection_set = Caml.Set.Make (Connection) in
  let res = ref Connection_set.empty in
  List.iter node_list ~f:(fun node ->
      let peer_nodes =
        List.map node.peers ~f:(fun p2p ->
            match
              List.find node_list ~f:(fun {p2p_port; _} -> p2p_port = p2p)
            with
            | None -> `Unknown p2p
            | Some n -> `Peer n ) in
      List.iter peer_nodes ~f:(fun peer_opt ->
          let conn =
            match peer_opt with
            | `Unknown p2p -> `Missing (node, p2p)
            | `Peer peer ->
                if List.mem peer.peers node.p2p_port ~equal:Int.equal then
                  `Duplex (node, peer)
                else `From_to (node, peer) in
          res := Connection_set.add conn !res ) ) ;
  Connection_set.elements !res

module History_modes = struct
  type 'error edit = t list -> (t list, 'error) Asynchronous_result.t

  let cmdliner_term state : _ edit Cmdliner.Term.t =
    let open Cmdliner in
    let open Term in
    let history_mode_converter =
      Arg.enum
        [("archive", `Archive); ("full", `Full 5); ("rolling", `Rolling 5)]
    in
    let docs =
      Manpage_builder.section state ~rank:2 ~name:"NODE HISTORY MODES"
        ~man:
          [`P "One can specify history modes for a given subset of the nodes."]
    in
    pure (fun edits node_list ->
        try
          return
            (List.map node_list ~f:(fun node ->
                 match
                   List.filter edits ~f:(fun (prefix, _) ->
                       String.is_prefix node.id ~prefix )
                 with
                 | [] -> node
                 | [one] -> (
                   match
                     List.filter node_list ~f:(fun n ->
                         String.is_prefix n.id ~prefix:(fst one) )
                   with
                   | [_] -> {node with history_mode= Some (snd one)}
                   | a_bunch_maybe_zero ->
                       Fmt.kstr failwith
                         "Prefix %S does not match exactly one node: [%s]"
                         (fst one)
                         (String.concat ~sep:", "
                            (List.map a_bunch_maybe_zero ~f:id) ) )
                 | more ->
                     Fmt.kstr failwith "Prefixes %s match the same node: %s"
                       (String.concat ~sep:", " (List.map more ~f:fst))
                       node.id ) )
        with Failure s ->
          System_error.fail_fatalf "Failed to compose history-modes: %s" s )
    $ Arg.(
        value
          (opt_all
             (pair ~sep:':' string history_mode_converter)
             []
             (info ["set-history-mode"] ~docs ~docv:"NODEPREFIX:MODE"
                ~doc:
                  "Set the history mode for a given (named) node, e.g. \
                   `N000:archive`." ) ))
end
