open Internal_pervasives

type t =
  { id: string
  ; expected_connections: int
  ; rpc_port: int
  ; p2p_port: int
  ; (* Ports: *)
    peers: int list
  ; exec: Tezos_executable.t
  ; protocol: Tezos_protocol.t
  ; history_mode: [`Full | `Archive | `Rolling] option }

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

let make ~exec ?(protocol = Tezos_protocol.default ()) ?history_mode id
    ~expected_connections ~rpc_port ~p2p_port peers =
  { id
  ; expected_connections
  ; rpc_port
  ; p2p_port
  ; peers
  ; exec
  ; protocol
  ; history_mode }

let make_path p ~config t = Paths.root config // sprintf "node-%s" t.id // p

(* Data-dir should not exist OR be fully functional. *)
let data_dir ~config t = make_path "data-dir" ~config t
let config_file ~config t = data_dir ~config t // "config.json"
let identity_file ~config t = data_dir ~config t // "identity.json"
let log_output ~config t = make_path "node-output.log" ~config t
let exec_path ~config t = make_path ~config "exec" t

module Config_file = struct
  (* 
     This module pruposedly avoids using the node's modules because we
     want the sandbox to be able to configure ≥ 1 versions of the
     node.
  *)
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
                    | `Full -> string "full"
                    | `Rolling -> string "rolling" ) ] ) ] in
    [ ("data-dir", data_dir ~config:state t |> string)
    ; ( "rpc"
      , dict [("listen-addrs", strings [sprintf "0.0.0.0:%d" t.rpc_port])] )
    ; ( "p2p"
      , dict
          [ ( "expected-proof-of-work"
            , int (Tezos_protocol.expected_pow t.protocol) )
          ; ("listen-addr", ksprintf string "0.0.0.0:%d" t.p2p_port)
          ; ( "limits"
            , dict
                [ ("maintenance-idle-time", int 3)
                ; ("swap-linger", int 2)
                ; ("connection-timeout", int 2) ] ) ] )
    ; ("log", dict [("output", string (log_output ~config:state t))]) ]
    @ shell
    |> dict |> to_string ~minify:false
end

open Tezos_executable.Make_cli

let node_command t ~config cmd options =
  Tezos_executable.call t.exec ~path:(exec_path t ~config)
    ( cmd
    @ opt "config-file" (config_file ~config t)
    @ opt "data-dir" (data_dir ~config t)
    @ options )

let run_command t ~config =
  let peers = List.concat_map t.peers ~f:(optf "peer" "127.0.0.1:%d") in
  node_command
    t
    ~config
    ["run"]
    ( flag "private-mode" @ flag "no-bootstrap-peers"
    @ peers
    @ optf "bootstrap-threshold" "0"
    @ optf "connections" "%d" t.expected_connections
    @ opt "sandbox" (Tezos_protocol.sandbox_path ~config t.protocol) )

let start_script t ~config =
  let open Genspio.EDSL in
  let gen_id =
    node_command t ~config
      [ "identity"; "generate"
      ; sprintf "%d" (Tezos_protocol.expected_pow t.protocol) ]
      [] in
  let tmp_config = tmp_file (config_file t ~config) in
  check_sequence ~verbosity:`Output_all
    [ ("reset-config", node_command t ~config ["config"; "reset"] [])
    ; ( "write-config"
      , seq
          [ tmp_config#set (Config_file.of_node config t |> str)
          ; call [str "mv"; tmp_config#path; str (config_file t ~config)] ] )
    ; ( "ensure-identity"
      , ensure "node-id"
          ~condition:(file_exists (str (identity_file t ~config)))
          ~how:[("gen-id", gen_id)] )
    ; ("start", run_command t ~config) ]

let process config t =
  Running_processes.Process.genspio t.id (start_script t ~config)

let protocol t = t.protocol

let connections node_list =
  let module Connection = struct
    type node = t

    type t =
      [ `Duplex of node * node
      | `From_to of node * node
      | `Missing of node * int ]

    let compare a b =
      match (a, b) with
      | `Duplex (a, b), `Duplex (c, d) when equal a d && equal b c -> 0
      | `Duplex _, _ -> -1
      | _, `Duplex _ -> 1
      | _, _ -> Caml.Pervasives.compare a b
  end in
  let module Connection_set = Set.Make (Connection) in
  let res = ref Connection_set.empty in
  List.iter node_list ~f:(fun node ->
      let peer_nodes =
        List.map node.peers ~f:(fun p2p ->
            match
              List.find node_list ~f:(fun {p2p_port; _} -> p2p_port = p2p)
            with
            | None -> `Unknown p2p
            | Some n -> `Peer n) in
      List.iter peer_nodes ~f:(fun peer_opt ->
          let conn =
            match peer_opt with
            | `Unknown p2p -> `Missing (node, p2p)
            | `Peer peer ->
                if List.mem peer.peers node.p2p_port ~equal:Int.equal then
                  `Duplex (node, peer)
                else `From_to (node, peer) in
          res := Connection_set.add conn !res)) ;
  Connection_set.elements !res

module History_modes = struct
  type 'error edit = t list -> (t list, 'error) Asynchronous_result.t

  let cmdliner_term () : _ edit Cmdliner.Term.t =
    let open Cmdliner in
    let open Term in
    let history_mode_converter =
      Arg.enum [("archive", `Archive); ("full", `Full); ("rolling", `Rolling)]
    in
    pure (fun edits node_list ->
        try
          return
            (List.map node_list ~f:(fun node ->
                 match
                   List.filter edits ~f:(fun (prefix, _) ->
                       String.is_prefix node.id ~prefix)
                 with
                 | [] -> node
                 | [one] -> (
                   match
                     List.filter node_list ~f:(fun n ->
                         String.is_prefix n.id ~prefix:(fst one))
                   with
                   | [_] -> {node with history_mode= Some (snd one)}
                   | a_bunch_maybe_zero ->
                       Fmt.kstrf failwith
                         "Prefix %S does not match exactly one node: [%s]"
                         (fst one)
                         (String.concat ~sep:", "
                            (List.map a_bunch_maybe_zero ~f:id)) )
                 | more ->
                     Fmt.kstrf failwith "Prefixes %s match the same node: %s"
                       (String.concat ~sep:", " (List.map more ~f:fst))
                       node.id))
        with Failure s ->
          System_error.fail_fatalf "Failed to compose history-modes: %s" s)
    $ Arg.(
        value
          (opt_all
             (pair ~sep:':' string history_mode_converter)
             []
             (info ["set-history-mode"]
                ~doc:"Set the history mode for a given (named) node.")))
end
