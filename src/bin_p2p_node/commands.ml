(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let group =
  {name = "octez-p2p-node"; title = "Commands related to the octez P2P node."}

let default_p2p_port = 19732

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

let p2p_node_args = Tezos_clic.args2 net_addr_arg peers_arg

let run_command =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Run the octez-p2p-node"
    p2p_node_args
    (prefix "run" @@ stop)
    (fun (p2p_addr, _peers) () ->
      Format.eprintf
        "TODO: Implement run command for P2P node on address %s@."
        p2p_addr ;
      Lwt_result_syntax.return_unit)

let p2p_node_commands = [run_command]
