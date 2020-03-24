open Internal_pervasives

module Inconsistency_error = struct
  type t =
    [ `Empty_protocol_list
    | `Too_many_protocols of Tezos_protocol.t list
    | `Too_many_timestamp_delays of Tezos_protocol.t list ]

  let should_be_one_protocol = function
    | [one] -> return one
    | [] -> fail `Empty_protocol_list
    | more -> fail (`Too_many_protocols more)

  let should_be_one_timestamp_delay = function
    | [_] -> return ()
    | [] -> fail `Empty_protocol_list
    | more -> fail (`Too_many_timestamp_delays more)

  let pp fmt err =
    match err with
    | `Empty_protocol_list ->
        Caml.Format.fprintf fmt "Wrong number of protocols in network: 0"
    | `Too_many_protocols p ->
        Caml.Format.fprintf fmt "Wrong number of protocols in network: %d"
          (List.length p)
    | `Too_many_timestamp_delays p ->
        Caml.Format.fprintf fmt
          "Wrong number of protocol timestamp delays in network: %d"
          (List.length p)
end

module Topology = struct
  type node = Tezos_node.t

  type _ t =
    | Mesh : {size: int} -> node list t
    | Bottleneck :
        {name: string; left: 'a network; right: 'b network}
        -> ('a * node * 'b) t
    | Net_in_the_middle :
        {middle: 'm network; left: 'a network; right: 'b network}
        -> ('a * 'm * 'b) t

  and 'a network = {topology: 'a t; name: string}

  let make name topology = {name; topology}
  let mesh name size = Mesh {size} |> make name
  let sub = make
  let bottleneck name left right = Bottleneck {name; left; right} |> make name

  let net_in_the_middle name middle left right =
    Net_in_the_middle {middle; left; right} |> make name

  let rec node_count : type a. a t -> int = function
    | Mesh {size} -> size
    | Bottleneck {left; right; _} ->
        1 + node_count left.topology + node_count right.topology
    | Net_in_the_middle {left; right; middle} ->
        node_count middle.topology + node_count left.topology
        + node_count right.topology

  let rec node_ids : type a. a t -> a -> string list =
   fun topo res ->
    match (topo, res) with
    | Mesh _, l -> List.map l ~f:(fun nod -> nod.Tezos_node.id)
    | Bottleneck {left; right; _}, (l, i, r) ->
        (i.Tezos_node.id :: node_ids left.topology l)
        @ node_ids right.topology r
    | Net_in_the_middle {left; right; middle}, (l, i, r) ->
        node_ids middle.topology i @ node_ids left.topology l
        @ node_ids right.topology r

  let rec node_names : type a. ?prefix:string -> a network -> string list =
   fun ?(prefix = "") {name; topology} ->
    let make_ith i = sprintf "%s%03d" prefix i in
    let continue a = node_names ~prefix:(prefix ^ name) a in
    match topology with
    | Mesh {size} -> List.init size ~f:make_ith
    | Bottleneck {name; left; right} ->
        (sprintf "%s%s" prefix name :: continue left) @ continue right
    | Net_in_the_middle {left; right; middle} ->
        continue middle @ continue left @ continue right

  let build ?(external_peer_ports = []) ?(base_port = 15_001) ~make_node
      network =
    let all_ports = ref [] in
    let next_port = ref (base_port + Int.rem base_port 2) in
    let rpc name =
      match List.find !all_ports ~f:(fun (n, _) -> String.equal n name) with
      | Some (_, p) -> p
      | None ->
          let p = !next_port in
          all_ports := (name, p) :: !all_ports ;
          next_port := !next_port + 2 ;
          p in
    let p2p n = rpc n + 1 in
    let node peers id =
      let rpc_port = rpc id in
      let p2p_port = p2p id in
      let expected_connections =
        List.length peers + List.length external_peer_ports in
      let peers =
        List.filter_map peers ~f:(fun p ->
            if not (String.equal p id) then Some (p2p p) else None) in
      make_node id ~expected_connections ~rpc_port ~p2p_port
        (external_peer_ports @ peers) in
    let dbgp prefx names =
      Dbg.f (fun pf ->
          pf "%s:\n  %s\n%!" prefx
            (String.concat ~sep:"\n  "
               (List.map names ~f:(fun n -> sprintf "%s:%d" n (p2p n))))) in
    let rec make :
        type a. ?extra_peers:string list -> prefix:string -> a network -> a =
     fun ?(extra_peers = []) ~prefix network ->
      let prefix = prefix ^ network.name in
      let make ?extra_peers n = make ?extra_peers ~prefix n in
      match network.topology with
      | Bottleneck {name; left; right} ->
          let intermediate = name in
          let extra_peers = [intermediate] in
          let left_nodes = make ~extra_peers left in
          let right_nodes = make ~extra_peers right in
          let intermediate_node =
            let peers =
              node_ids left.topology left_nodes
              @ node_ids right.topology right_nodes in
            node peers intermediate in
          (left_nodes, intermediate_node, right_nodes)
      | Net_in_the_middle {middle; left; right} ->
          let middle_names = node_names ~prefix:(prefix ^ middle.name) middle in
          dbgp "Mid-name" middle_names ;
          let left_nodes =
            make ~extra_peers:(extra_peers @ middle_names) left in
          let right_nodes =
            make ~extra_peers:(extra_peers @ middle_names) right in
          let intermediate_nodes =
            let peers =
              node_ids left.topology left_nodes
              @ node_ids right.topology right_nodes in
            dbgp "peers" peers ;
            dbgp "extr-peers" extra_peers ;
            dbgp "left-names" (node_names ~prefix:(prefix ^ left.name) left) ;
            dbgp "right-names" (node_names ~prefix:(prefix ^ right.name) right) ;
            make ~extra_peers:(peers @ extra_peers) middle in
          (left_nodes, intermediate_nodes, right_nodes)
      | Mesh _ ->
          let all = node_names ~prefix network in
          dbgp "mesh-names" all ;
          let nodes = List.map all ~f:(fun n -> node (all @ extra_peers) n) in
          nodes in
    make ~prefix:"" network
end

module Queries = struct
  let all_levels ?(chain = "main") state ~nodes =
    List.fold nodes ~init:(return [])
      ~f:(fun prevm {Tezos_node.id; rpc_port; _} ->
        prevm
        >>= fun prev ->
        Running_processes.run_cmdf state
          (* the header RPC is the most consistent across protocols: *)
          "curl http://localhost:%d/chains/%s/blocks/head/header" rpc_port
          chain
        >>= fun metadata ->
        Console.display_errors_of_command state metadata ~should_output:true
        >>= (function
              | true -> (
                try
                  `Level
                    ( Jqo.of_lines metadata#out |> Jqo.field ~k:"level"
                    |> Jqo.get_int )
                  |> return
                with _ -> return `Failed )
              | false -> return `Failed)
        >>= fun res -> return ((id, res) :: prev))
    >>= fun results ->
    let sorted =
      List.sort results ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    return sorted

  let wait_for_all_levels_to_be ?attempts_factor ?chain state ~attempts
      ~seconds nodes level =
    let check_level =
      match level with
      | `Equal_to l -> ( = ) l
      | `At_least l -> fun x -> x >= l in
    let level_string =
      match level with
      | `Equal_to l -> sprintf "= %d" l
      | `At_least l -> sprintf "≥ %d" l in
    let msg ids =
      let show_node (id, res) =
        sprintf "%s (%s)" id
          ( match res with
          | `Failed -> "failed"
          | `Level l -> sprintf "%d" l
          | `Null -> "null"
          | `Unknown s -> sprintf "¿¿ %S ??" s ) in
      sprintf "Waiting for %s to reach level %s"
        (String.concat (List.map ~f:show_node ids) ~sep:", ")
        level_string in
    Console.say state
      EF.(
        wf "Checking for all levels to be %s (nodes: %s%s)" level_string
          (String.concat ~sep:", "
             (List.map nodes ~f:(fun n -> n.Tezos_node.id)))
          (Option.value_map chain ~default:"" ~f:(sprintf ", chain: %s")))
    >>= fun () ->
    Helpers.wait_for state ?attempts_factor ~attempts ~seconds (fun _nth ->
        all_levels state ~nodes ?chain
        >>= fun results ->
        let not_readys =
          List.filter_map results ~f:(function
            | _, `Level n when check_level n -> None
            | id, res -> Some (id, res)) in
        match not_readys with
        | [] -> return (`Done ())
        | ids -> return (`Not_done (msg ids)))
end

module Network = struct
  type t = {nodes: Tezos_node.t list}

  let make nodes = {nodes}

  let start_up ?(do_activation = true) ?(check_ports = true) state ~client_exec
      {nodes} =
    ( if check_ports then
      Helpers.Netstat.used_listening_ports state
      >>= fun all_used ->
      let taken port = List.find all_used ~f:(fun (p, _) -> Int.equal p port) in
      List_sequential.iter nodes
        ~f:(fun {Tezos_node.id; rpc_port; p2p_port; _} ->
          let fail s (p, `Tcp (_, row)) =
            System_error.fail_fatalf
              "Node: %S's %s port %d already in use {%s}" id s p
              (String.concat ~sep:"|" row) in
          let time_wait (_, `Tcp (_, row)) =
            Poly.equal (List.last row) (Some "TIME_WAIT") in
          match (taken rpc_port, taken p2p_port) with
          | None, None -> return ()
          | Some p, _ -> if time_wait p then return () else fail "RPC" p
          | _, Some p -> if time_wait p then return () else fail "P2P" p)
    else return () )
    >>= fun () ->
    let protocols =
      List.map ~f:Tezos_node.protocol nodes
      |> List.dedup_and_sort ~compare:Tezos_protocol.compare in
    Inconsistency_error.should_be_one_protocol protocols
    >>= fun protocol ->
    Inconsistency_error.should_be_one_timestamp_delay protocols
    >>= fun () ->
    Tezos_protocol.ensure state protocol
    >>= fun () ->
    List.fold nodes ~init:(return ()) ~f:(fun prev_m node ->
        prev_m
        >>= fun () ->
        Running_processes.start state (Tezos_node.process state node)
        >>= fun _ -> return ())
    >>= fun () ->
    let node_0 = List.hd_exn nodes in
    let client = Tezos_client.of_node node_0 ~exec:client_exec in
    Dbg.e EF.(af "Trying to bootstrap client") ;
    Tezos_client.wait_for_node_bootstrap state client
    >>= fun () ->
    Tezos_client.rpc state ~client `Get ~path:"/chains/main/blocks/head/header"
    >>= fun json ->
    let do_activation =
      (* If the level is 0 we still do the activation. *)
      do_activation
      ||
      try Int.equal Jqo.(field ~k:"level" json |> get_int) 0 with _ -> false
    in
    ( if do_activation then Tezos_client.activate_protocol state client protocol
    else return () )
    >>= fun () ->
    Dbg.e EF.(af "Waiting for all nodes to be bootstrapped") ;
    List_sequential.iter nodes ~f:(fun node ->
        let client = Tezos_client.of_node node ~exec:client_exec in
        Tezos_client.wait_for_node_bootstrap state client)
    >>= fun () ->
    (* We make sure all nodes got activation before trying to continue: *)
    Queries.wait_for_all_levels_to_be state ~attempts:20 ~seconds:0.5
      ~attempts_factor:0.8 nodes (`At_least 1)
end

let network_with_protocol ?do_activation ?node_custom_network
    ?external_peer_ports ?base_port ?(size = 5) ?protocol
    ?(nodes_history_mode_edits = return) state ~node_exec ~client_exec =
  let pre_edit_nodes =
    Topology.build ?base_port ?external_peer_ports
      ~make_node:(fun id ~expected_connections ~rpc_port ~p2p_port peers ->
        Tezos_node.make ?protocol ~exec:node_exec id ~expected_connections
          ?custom_network:node_custom_network ~rpc_port ~p2p_port peers)
      (Topology.mesh "N" size) in
  nodes_history_mode_edits pre_edit_nodes
  >>= fun nodes ->
  let protocols =
    List.map ~f:Tezos_node.protocol nodes
    |> List.dedup_and_sort ~compare:Tezos_protocol.compare in
  Inconsistency_error.should_be_one_protocol protocols
  >>= fun protocol ->
  Inconsistency_error.should_be_one_timestamp_delay protocols
  >>= fun () ->
  Network.start_up ?do_activation state ~client_exec (Network.make nodes)
  >>= fun () -> return (nodes, protocol)
