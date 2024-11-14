(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Node = struct
  include Tezt_tezos.Node

  module Agent = struct
    let create ?(arguments = []) ?data_dir
        ?(path = Uses.path Constant.octez_node) ?name agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      create
        ?data_dir
        ?name
        ~path
        ?runner
        ~rpc_port
        ~net_port
        ~metrics_port
        arguments
      |> Lwt.return

    let init ?(arguments = []) ?data_dir ?(path = Uses.path Constant.octez_node)
        ?name agent =
      let runner = Agent.runner agent in
      let* path = Agent.copy agent ~source:path in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      init
        ?name
        ?data_dir
        ~path
        ?runner
        ~rpc_port
        ~net_port
        ~metrics_port
        ~event_level:`Notice
        arguments
  end
end

module Dal_node = struct
  include Tezt_tezos.Dal_node

  module Agent = struct
    let create_from_endpoint ?net_port
        ?(path = Uses.path Constant.octez_dal_node |> Filename.basename) ?name
        ?rpc_port ~l1_node_endpoint agent =
      let* path = Agent.copy agent ~source:path in
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
      create_from_endpoint
        ?name
        ~path
        ?runner
        ~rpc_port
        ~metrics_addr
        ~listen_addr
        ~l1_node_endpoint
        ()
      |> Lwt.return

    let create ?net_port ?path ?name ~node agent =
      create_from_endpoint
        ?net_port
        ?path
        ?name
        ~l1_node_endpoint:(Node.as_rpc_endpoint node)
        agent

    let run ?(memtrace = false) ?event_level dal_node =
      let name = name dal_node in
      let filename =
        Format.asprintf "%s/%s-trace.ctf" (Filename.get_temp_dir_name ()) name
      in
      let env =
        if memtrace then Some (String_map.singleton "MEMTRACE" filename)
        else None
      in
      run ?env ?event_level dal_node
  end
end

module Sc_rollup_node = struct
  include Tezt_tezos.Sc_rollup_node

  module Agent = struct
    let create ?(path = Uses.path Constant.octez_smart_rollup_node) ?name
        ?default_operator ?dal_node ~base_dir agent mode l1_node =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let metrics_addr = "0.0.0.0" in
      create
        ?name
        ?default_operator
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
    let create ?(path = Uses.path Constant.octez_evm_node) ?name ?data_dir ?mode
        endpoint agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      create ?name ~path ?runner ?data_dir ~rpc_port ?mode endpoint
      |> Lwt.return

    let init ?patch_config ?name ?mode ?data_dir rollup_node agent =
      let* evm_node = create ?name ?mode ?data_dir rollup_node agent in
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
    let create ?(path = Uses.path Constant.octez_client) ?node agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      let endpoint = Option.map (fun x -> Node x) node in
      create ?runner ~path ?endpoint () |> Lwt.return
  end
end

module Baker = struct
  include Baker

  module Agent = struct
    let init ?name ~delegate ~protocol
        ?(path = Uses.path (Protocol.baker protocol)) ~client dal_node
        ?dal_node_timeout_percentage node agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      init
        ?name
        ~event_level:`Notice
        ?runner
        ~path
        ~delegates:[delegate]
        ~protocol
        ~dal_node
        ?dal_node_timeout_percentage
        node
        client
  end
end

module Teztale = struct
  include Teztale

  module Server = struct
    include Teztale.Server

    let run agent ?(path = Uses.path Constant.teztale_server) ?address ?name
        ?port ?users ?admin ?public_directory () =
      let runner = Agent.runner agent in
      let port =
        match port with
        | Some port -> port
        | None -> Agent.next_available_port agent
      in
      let* path = Agent.copy agent ~source:path in
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

    let run agent ?(path = Uses.path Constant.teztale_archiver) ?name ~node_port
        user feed =
      let runner = Agent.runner agent in
      let* path = Agent.copy agent ~source:path in
      Teztale.Archiver.run ?runner ~path ?name ~node_port user feed
  end
end

let create_dir ?runner dir =
  let* () = Process.spawn ?runner "rm" ["-rf"; dir] |> Process.check in
  let* () = Process.spawn ?runner "mkdir" ["-p"; dir] |> Process.check in
  Lwt.return_unit
