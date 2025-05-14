(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let group =
  {name = "octez-p2p-node"; title = "Commands related to the octez P2P node."}

let default_p2p_port = 19732

let default_rpc_port = 18732

let net_addr_arg =
  Tezos_clic.default_arg
    ~long:"net-addr"
    ~placeholder:"ADDR:PORT|:PORT"
    ~default:("[::]:" ^ string_of_int default_p2p_port)
    ~doc:
      (Format.sprintf
         "P2P address that the node listens on. Defaults to %d"
         default_p2p_port)
    (Tezos_clic.parameter (fun () s -> Lwt_result.return s))

let peers_arg =
  Tezos_clic.arg
    ~long:"peers"
    ~doc:"Comma-separated list of peer addresses to accept connections from."
    ~placeholder:"ADDR:PORT|:PORT, ADDR2:PORT2|:PORT2 ..."
    (Tezos_clic.parameter (fun () s ->
         let peers = if s = "" then [] else String.split_on_char ',' s in
         Lwt_result_syntax.return peers))

let discovery_addr_arg =
  Tezos_clic.arg
    ~long:"discovery-addr"
    ~doc:"The UDP address and port used for local peer discovery."
    ~placeholder:"ADDR:PORT|:PORT"
    (Tezos_clic.parameter (fun () s -> Lwt_result.return s))

let rpc_addr_arg =
  Tezos_clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"ADDR:PORT|:PORT"
    ~default:("[::]:" ^ string_of_int default_rpc_port)
    ~doc:
      (Format.sprintf
         "The RPC address and port. Defaults to %d"
         default_rpc_port)
    (Tezos_clic.parameter (fun () s -> Lwt_result.return s))

let p2p_node_args =
  Tezos_clic.args4 net_addr_arg peers_arg discovery_addr_arg rpc_addr_arg

let run_command =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Run the octez-p2p-node"
    p2p_node_args
    (prefix "run" @@ stop)
    (fun (listen_addr, peers, discovery_addr, rpc_addr) () ->
      P2p_node_run_command.run ~listen_addr ?peers ?discovery_addr ~rpc_addr ())

let p2p_node_commands = [run_command]
