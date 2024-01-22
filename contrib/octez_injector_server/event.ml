(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)
include Internal_event.Simple

let section = ["injector"; "daemon"]

let data_dir_not_found =
  declare_1
    ~section
    ~name:"injector_server_no_data_dir"
    ~msg:
      "The injector server data directory {path} doesn't exist. Create using: \
       init-config --data-dir={path} "
    ~level:Error
    ("path", Data_encoding.(string))

let listening =
  declare_1
    ~section
    ~level:Notice
    ~name:"injector_server_listening"
    ~msg:"listening on address: {address}"
    ("address", P2p_addr.encoding)

let accepting_requests =
  declare_2
    ~section
    ~level:Notice
    ~name:"injector_server_accepting_requests"
    ~msg:"accepting {transport_protocol} requests on port {port}"
    ("transport_protocol", Data_encoding.string)
    ("port", Data_encoding.int31)
