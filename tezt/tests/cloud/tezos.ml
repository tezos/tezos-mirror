(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Node = struct
  include Tezt_tezos.Node

  module Agent = struct
    let create ?(path = Uses.path Constant.octez_node) ?name agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      create ?name ~path ~runner ~rpc_port ~net_port ~metrics_port []
      |> Lwt.return

    let init ?(arguments = []) ?(path = Uses.path Constant.octez_node) ?name
        agent =
      let runner = Agent.runner agent in
      let* path = Agent.copy agent ~source:path in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      init
        ?name
        ~path
        ~runner
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
    let create ?(path = Uses.path Constant.octez_dal_node |> Filename.basename)
        ?name ~node agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let net_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let metrics_addr = Format.asprintf "0.0.0.0:%d" metrics_port in
      let listen_addr = Format.asprintf "0.0.0.0:%d" net_port in
      create ?name ~path ~runner ~rpc_port ~metrics_addr ~listen_addr ~node ()
      |> Lwt.return
  end
end

module Sc_rollup_node = struct
  include Tezt_tezos.Sc_rollup_node

  module Agent = struct
    let create ?(path = Uses.path Constant.octez_smart_rollup_node) ?name
        ?default_operator ~base_dir agent mode l1_node =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      let rpc_port = Agent.next_available_port agent in
      let metrics_port = Agent.next_available_port agent in
      let metrics_addr = "0.0.0.0" in
      create
        ?name
        ?default_operator
        ~path
        ~runner
        ~rpc_port
        ~metrics_addr
        ~metrics_port
        ~base_dir
        mode
        l1_node
      |> Lwt.return
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
      create ?name ~path ~runner ?data_dir ~rpc_port ?mode endpoint
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
      create ~runner ~path ?endpoint () |> Lwt.return
  end
end

module Baker = struct
  include Baker

  module Agent = struct
    let init ?name ~delegate ~protocol
        ?(path = Uses.path (Protocol.baker protocol)) ~client dal_node node
        agent =
      let* path = Agent.copy agent ~source:path in
      let runner = Agent.runner agent in
      init
        ?name
        ~event_level:`Notice
        ~runner
        ~path
        ~delegates:[delegate]
        ~protocol
        ~dal_node
        node
        client
  end
end
