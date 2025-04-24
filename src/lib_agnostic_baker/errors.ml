(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type error +=
  | Lost_node_connection
  | Cannot_connect_to_node of string
  | Cannot_decode_node_data of string
  | Missing_current_agent of string
  | Agent_process_error of string

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.lost_node_connection"
    ~title:"Lost node connection"
    ~description:"Connection with node lost."
    ~pp:(fun ppf () -> Format.fprintf ppf "Connection with node was lost")
    Data_encoding.(unit)
    (function Lost_node_connection -> Some () | _ -> None)
    (fun () -> Lost_node_connection) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.cannot_connect_to_node"
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
    ~id:"agnostic_agent.cannot_decode_node_data"
    ~title:"Cannot decode node data"
    ~description:"Cannot decode node data."
    ~pp:(fun ppf err -> Format.fprintf ppf "Cannot decode node data: %s" err)
    Data_encoding.(obj1 (req "err" string))
    (function Cannot_decode_node_data err -> Some err | _ -> None)
    (fun err -> Cannot_decode_node_data err) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.missing_current_agent"
    ~title:"Missing current agent"
    ~description:"The current agent process is missing."
    ~pp:(fun ppf agent -> Format.fprintf ppf "Missing current %s" agent)
    Data_encoding.(obj1 (req "agent" string))
    (function Missing_current_agent agent -> Some agent | _ -> None)
    (fun agent -> Missing_current_agent agent) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_agent.agent_process_error"
    ~title:"Underlying agent process error"
    ~description:"There is an error in the underlying agent process."
    ~pp:(fun ppf agent ->
      Format.fprintf ppf "Error in the underlying %s process" agent)
    Data_encoding.(obj1 (req "agent" string))
    (function Agent_process_error agent -> Some agent | _ -> None)
    (fun agent -> Agent_process_error agent)
