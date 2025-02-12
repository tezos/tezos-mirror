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
  | Missing_current_baker
  | Missing_agnostic_baker_plugin of string

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.lost_node_connection"
    ~title:"Lost node connection"
    ~description:"Connection with node lost."
    ~pp:(fun ppf () -> Format.fprintf ppf "Connection with node was lost")
    Data_encoding.(unit)
    (function Lost_node_connection -> Some () | _ -> None)
    (fun () -> Lost_node_connection) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.cannot_connect_to_node"
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
    ~id:"agnostic_baker.cannot_decode_node_data"
    ~title:"Cannot decode node data"
    ~description:"Cannot decode node data."
    ~pp:(fun ppf err -> Format.fprintf ppf "Cannot decode node data: %s" err)
    Data_encoding.(obj1 (req "err" string))
    (function Cannot_decode_node_data err -> Some err | _ -> None)
    (fun err -> Cannot_decode_node_data err) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.missing_current_baker"
    ~title:"Missing current baker"
    ~description:"The current baker binary is missing."
    ~pp:(fun ppf () -> Format.fprintf ppf "Missing current baker")
    Data_encoding.(unit)
    (function Missing_current_baker -> Some () | _ -> None)
    (fun () -> Missing_current_baker) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"agnostic_baker.missing_agnostic_baker_plugin"
    ~title:"Missing agnostic baker plugin"
    ~description:"Missing agnostic baker plugin."
    ~pp:(fun ppf proto_hash ->
      Format.fprintf
        ppf
        "Cannot find agnostic baker plugin for protocol %s"
        proto_hash)
    Data_encoding.(obj1 (req "proto_hash" string))
    (function
      | Missing_agnostic_baker_plugin proto_hash -> Some proto_hash | _ -> None)
    (fun proto_hash -> Missing_agnostic_baker_plugin proto_hash)
