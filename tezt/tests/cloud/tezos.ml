(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let create_dir ?runner dir =
  let* () = Process.spawn ?runner "rm" ["-rf"; dir] |> Process.check in
  let* () = Process.spawn ?runner "mkdir" ["-p"; dir] |> Process.check in
  Lwt.return_unit

module Node = struct
  include Tezt_tezos.Node

  module Agent = struct
    let create ?(group = "L1") ?rpc_external ?(metadata_size_limit = true)
        ?(arguments = []) ?data_dir ?(path = Uses.path Constant.octez_node)
        ?name ?net_addr cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:"octez-validator"
          ()
      in
      let* () =
        if Option.is_some rpc_external then
          Cloud.register_binary
            cloud
            ~agents:[agent]
            ~group
            ~name:"octez-rpc-process"
            ()
        else Lwt.return_unit
      in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let arguments =
        if metadata_size_limit then
          Metadata_size_limit (Some 10_000) :: arguments
        else arguments
      in
      let node =
        create
          ?data_dir
          ?name
          ~path
          ?runner
          ?rpc_external
          ?net_addr
          ~rpc_port
          ~net_port
          ~metrics_port
          arguments
      in
      let name = Node.name node in
      let executable = Node.path node in
      Cloud.service_register ~name ~executable agent ;
      Lwt.return node

    let init ?(group = "L1") ?rpc_external ?(metadata_size_limit = true)
        ?(arguments = []) ?data_dir ?(path = Uses.path Constant.octez_node)
        ?net_addr ?name cloud agent =
      let runner = Agent.runner agent in
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:"octez-validator"
          ()
      in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:"octez-rpc-process"
          ()
      in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let arguments =
        if metadata_size_limit then
          Metadata_size_limit (Some 10_000) :: arguments
        else arguments
      in
      init
        ?name
        ?data_dir
        ~path
        ?runner
        ?rpc_external
        ~rpc_port
        ?net_addr
        ~net_port
        ~metrics_port
        ~event_level:`Notice
        arguments

    let run ?env ?patch_config ?on_terminate ?event_level ?event_sections_levels
        node args =
      let name = name node in
      let* () =
        run
          ?env
          ?patch_config
          ?on_terminate
          ?event_level
          ?event_sections_levels
          node
          args
      in
      (* Notify service manager. Need to run before to get the pid *)
      let () =
        match Node.pid node with
        | None ->
            Log.error
              "Cannot update service %s: no pid. Is the program running ?"
              name
        | Some pid -> Cloud.notify_service_start ~name ~pid
      in
      Lwt.return_unit

    let terminate ?timeout node =
      let name = name node in
      (* Notify the Service manager. *)
      let () =
        match Node.pid node with
        | None ->
            Log.error
              "Cannot update service %s: no pid. Is the program running ?"
              name
        | Some pid -> Cloud.notify_service_start ~name ~pid
      in
      terminate ?timeout node
  end
end

module Yes_wallet = struct
  include Tezt_tezos.Yes_wallet

  module Agent = struct
    let create ?(path = Uses.path Constant.yes_wallet) ?name agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      Lwt.return (create ?runner ~path ?name ())
  end
end

module Dal_node = struct
  include Tezt_tezos.Dal_node

  module Agent = struct
    let create_from_endpoint ?(group = "DAL") ?net_port
        ?(path = Uses.path Constant.octez_dal_node) ?name ?rpc_port
        ~l1_node_endpoint cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      let rpc_port =
        match rpc_port with
        | None -> Agent.next_available_port agent
        | Some rpc_port -> rpc_port
      in
      let net_port =
        match net_port with
        | None -> Agent.next_available_port agent
        | Some port -> port
      in
      let metrics_port = Agent.next_available_port agent in
      let metrics_addr = Format.asprintf "0.0.0.0:%d" metrics_port in
      let listen_addr = Format.asprintf "0.0.0.0:%d" net_port in
      let node =
        create_from_endpoint
          ?name
          ~path
          ?runner
          ~rpc_port
          ~metrics_addr
          ~listen_addr
          ~l1_node_endpoint
          ()
      in
      let name = Dal_node.name node in
      let executable = Dal_node.path node in
      Cloud.service_register ~name ~executable agent ;
      Lwt.return node

    let create ?net_port ?path ?name ~node agent =
      create_from_endpoint
        ?net_port
        ?path
        ?name
        ~l1_node_endpoint:(Node.as_rpc_endpoint node)
        agent

    let run ?otel ?(memtrace = false) ?event_level dal_node =
      let name = name dal_node in
      let filename =
        Format.asprintf "%s/%s-trace.ctf" (Filename.get_temp_dir_name ()) name
      in
      let env =
        let memtrace_env =
          if memtrace then String_map.singleton "MEMTRACE" filename
          else String_map.empty
        in
        let otel_env =
          match otel with
          | None -> String_map.empty
          | Some endpoint ->
              [
                ("OTEL", "true");
                ("OTEL_SERVICE_NAME", name);
                ("OTEL_EXPORTER_OTLP_ENDPOINT", endpoint);
              ]
              |> List.to_seq |> String_map.of_seq
        in
        String_map.union (fun _ _ _ -> None) otel_env memtrace_env
      in
      let* () = run ~env ?event_level dal_node in
      (* Update the state in the service manager *)
      let () =
        match Dal_node.pid dal_node with
        | None ->
            (* Here, we simply log the error (in RED)
               Users of tezt-cloud are expected to read their logs.
               As a rule of thumb, if error arise in components of tezt-cloud,
               tezt-cloud SHALL NOT hinder the scenario and raise errors
               in deployment, unless errors are fatal *)
            Log.error
              "Cannot update service state %s: no pid. Is the program running ?"
              name
        | Some pid -> Cloud.notify_service_start ~name ~pid
      in
      Lwt.return_unit

    let terminate ?timeout dal_node =
      let name = Dal_node.name dal_node in
      Cloud.notify_service_stop ~name ;
      terminate ?timeout dal_node
  end
end

module Floodgate = struct
  include Tezt_etherlink.Floodgate

  module Agent = struct
    let run ?(group = "Etherlink") ?(path = "floodgate") ?scenario ~rpc_endpoint
        ~controller ?relay_endpoint ?max_active_eoa
        ?max_transaction_batch_length ?spawn_interval ?tick_interval
        ?base_fee_factor ?initial_balance cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      run
        ?runner
        ~path
        ?scenario
        ~rpc_endpoint
        ~controller
        ?relay_endpoint
        ?max_active_eoa
        ?max_transaction_batch_length
        ?spawn_interval
        ?tick_interval
        ?base_fee_factor
        ?initial_balance
        ()
  end
end

module Sc_rollup_node = struct
  include Tezt_tezos.Sc_rollup_node

  module Agent = struct
    let create ?(group = "Etherlink")
        ?(path = Uses.path Constant.octez_smart_rollup_node) ?name
        ?default_operator ?operators ?dal_node ~base_dir cloud agent mode
        l1_node =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let metrics_addr = "0.0.0.0" in
      create
        ?name
        ?default_operator
        ?operators
        ?dal_node
        ~path
        ?runner
        ~rpc_port
        ~metrics_addr
        ~metrics_port
        ~base_dir
        mode
        l1_node
      |> Lwt.return
  end
end

module Sc_rollup_helpers = struct
  include Sc_rollup_helpers

  module Agent = struct
    let prepare_installer_kernel_with_arbitrary_file ~preimages_dir ?config
        installee agent =
      let* installee = Agent.copy agent ~source:installee in
      let* smart_rollup_installer_path =
        Agent.copy agent ~source:(Uses.path Constant.smart_rollup_installer)
      in
      let runner = Agent.runner agent in
      prepare_installer_kernel_with_arbitrary_file
        ~smart_rollup_installer_path
        ?runner
        ~boot_sector:`Filename
        ~preimages_dir
        ?config
        installee

    let prepare_installer_kernel ~preimages_dir ?config installee agent =
      prepare_installer_kernel_with_arbitrary_file
        ~preimages_dir
        ?config
        (Uses.path installee)
        agent

    let originate_sc_rollup ?hooks ?(burn_cap = Tez.(of_int 9999999)) ?whitelist
        ?(alias = "rollup") ?(src = Constant.bootstrap1.alias) ~kind
        ?(parameters_ty = "string") ~boot_sector client =
      let boot_sector =
        (* FIXME: This is not ideal. A better way would be to extend the
           originate command so that it takes a filename as input and read its
           content (actually maybe the feature already exists). *)
        "file:" ^ boot_sector
      in
      let* sc_rollup =
        Client.Sc_rollup.(
          originate
            ?hooks
            ~burn_cap
            ?whitelist
            ~alias
            ~src
            ~kind
            ~parameters_ty
            ~boot_sector
            client)
      in
      let* () = Lwt_unix.sleep 4. in
      return sc_rollup
  end
end

module Evm_node = struct
  include Tezt_etherlink.Evm_node

  module Agent = struct
    (* Use for compatibility with `tezt-cloud`. *)
    let create ?(group = "Etherlink")
        ?(path = Uses.path Constant.octez_evm_node) ?name ?data_dir ?mode
        endpoint cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      create ?name ~path ?runner ?data_dir ~rpc_port ?mode endpoint
      |> Lwt.return

    let init ?patch_config ?name ?mode ?data_dir rollup_node cloud agent =
      let* evm_node = create ?name ?mode ?data_dir rollup_node cloud agent in
      let* () = Process.check @@ spawn_init_config evm_node in
      let* () =
        match patch_config with
        | Some patch_config -> Config_file.update evm_node patch_config
        | None -> unit
      in
      let* () = run evm_node in
      return evm_node
  end
end

module Client = struct
  include Client

  module Agent = struct
    let create ?(path = Uses.path Constant.octez_client) ?name ?endpoint agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      create ?runner ?name ~path ?endpoint () |> Lwt.return
  end
end

module Agnostic_baker = struct
  include Agnostic_baker

  module Agent = struct
    let init ?(group = "L1") ?env ?name ~delegates
        ?(path = Uses.path Constant.octez_agnostic_baker) ~client
        ?dal_node_rpc_endpoint ?dal_node_timeout_percentage node cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      init
        ?env
        ?name
        ~event_level:`Notice
        ?runner
        ~path
        ~delegates
        ?dal_node_rpc_endpoint
        ?dal_node_timeout_percentage
        node
        client
  end
end

module Accuser = struct
  include Accuser

  module Agent = struct
    let init ?(group = "L1") ?name ~protocol
        ?(path = Uses.path (Protocol.accuser protocol)) node cloud agent =
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      let runner = Agent.runner agent in
      init ?name ~event_level:`Notice ?runner ~path ~protocol node
  end
end

module Teztale = struct
  include Teztale

  module Server = struct
    include Teztale.Server

    let run ?(group = "teztale") cloud agent
        ?(path = Uses.path Constant.teztale_server) ?address ?name ?port ?users
        ?admin ?public_directory () =
      let runner = Agent.runner agent in
      let port =
        match port with
        | Some port -> port
        | None -> Agent.next_available_port agent
      in
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      Teztale.Server.run
        ?runner
        ~path
        ?address
        ~port
        ?name
        ?users
        ?admin
        ?public_directory
        ()
  end

  module Archiver = struct
    include Teztale.Archiver

    let run ?(group = "teztale") cloud agent
        ?(path = Uses.path Constant.teztale_archiver) ?name ~node_port user feed
        =
      let runner = Agent.runner agent in
      let* path = Agent.copy agent ~source:path in
      let* () =
        Cloud.register_binary
          cloud
          ~agents:[agent]
          ~group
          ~name:(Filename.basename path)
          ()
      in
      Teztale.Archiver.run ?runner ~path ?name ~node_port user feed
  end

  type t = {
    agent : Agent.t;
    server : Server.t;
    address : string;
    mutable archivers : Archiver.t list;
  }

  let user ~agent_name ~node_name : user =
    let login = "teztale-archiver-" ^ agent_name ^ "-" ^ node_name in
    {login; password = login}

  let run_server
      ?(path =
        Uses.(path (make ~tag:"codec" ~path:"./octez-teztale-server" ()))) cloud
      agent =
    let public_directory = "/tmp/teztale/public" in
    let* () = create_dir public_directory in
    let aliases_filename =
      Filename.concat public_directory "delegates-aliases.json"
    in
    write_file aliases_filename ~contents:"[]" ;
    let* _ =
      Agent.copy ~source:aliases_filename ~destination:aliases_filename agent
    in
    let* _ =
      Agent.copy
        ~source:"tezt/lib_cloud/teztale-visualisation/index.html"
        ~destination:(Format.asprintf "%s/index.html" public_directory)
        agent
    in
    let* _ =
      Agent.copy
        ~is_directory:true
        ~source:"tezt/lib_cloud/teztale-visualisation/assets"
        ~destination:public_directory
        agent
    in
    let* server = Server.run ~path ~public_directory cloud agent () in
    let address =
      match Agent.point agent with
      | None -> "127.0.0.1"
      | Some point -> fst point
    in
    Lwt.return {agent; server; address; archivers = []}

  let wait_server t = Server.wait_for_readiness t.server

  let add_archiver
      ?(path =
        Uses.(path (make ~tag:"codec" ~path:"./octez-teztale-archiver" ()))) t
      cloud agent ~node_name ~node_port =
    let user = user ~agent_name:(Agent.name agent) ~node_name in
    let feed : interface list =
      [{address = t.address; port = t.server.conf.interface.port}]
    in
    let* () =
      Lwt_result.get_exn
        (Server.add_user
           ?runner:(Agent.runner agent)
           ~public_address:t.address
           t.server
           user)
    in
    let* archiver = Archiver.run cloud agent ~path user feed ~node_port in
    t.archivers <- archiver :: t.archivers ;
    Lwt.return_unit

  let update_alias t ~address ~alias =
    let dir = Option.get t.server.conf.public_directory in
    let filename = Filename.concat dir "delegates-aliases.json" in
    let aliases =
      match JSON.parse_file filename |> JSON.unannotate with
      | exception _ -> []
      | `A aliases -> aliases
      | _ -> assert false
    in
    let update = `O [("alias", `String alias); ("address", `String address)] in
    let aliases = update :: aliases in
    JSON.encode_to_file_u filename (`A aliases) ;
    let* _ =
      Agent.copy ~refresh:true ~source:filename ~destination:filename t.agent
    in
    Lwt.return_unit
end
