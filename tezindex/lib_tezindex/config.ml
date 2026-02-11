(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

include Client_config

type t =
  string option
  * string option
  * Uri.t option
  * Media_type.Command_line.t option

let default_media_type =
  Media_type.Command_line.Json (* Media_type.Command_line.Binary *)

let base_dir_env_name = "OCTEZ_INDEXER_DIR"

let default_base_dir =
  try Sys.getenv base_dir_env_name
  with Not_found -> Filename.concat home ".octez-indexer"

let base_dir_arg () =
  Tezos_clic.arg
    ~long:"base-dir"
    ~short:'d'
    ~placeholder:"path"
    ~doc:
      (Format.asprintf
         "@[<v>@[<2>indexer data directory (absent: %s env)@,\
          The directory where the Octez indexer will store all its data.@,\
          If absent, its value is the value of the %s@,\
          environment variable. If %s is itself not specified,@,\
          defaults to %s@]@]@."
         base_dir_env_name
         base_dir_env_name
         base_dir_env_name
         default_base_dir)
    (string_parameter ())

let rpc_listen_addr_arg () =
  Tezos_clic.arg
    ~long:"rpc-addr"
    ~placeholder:"ADDR:PORT"
    ~doc:
      "The URL at which this RPC server instance can be reached. Note that: as \
       a local RPC server is handled by the node itself, calling computational \
       intensive RPCs can affect the performances of the node."
    (string_parameter ())

let external_rpc_listen_addr_arg () =
  Tezos_clic.arg
    ~long:"external-rpc-addr"
    ~placeholder:"ADDR:PORT"
    ~doc:"The URL at which this external RPC server instance can be reached."
    (string_parameter ())

let db_name_arg () =
  Tezos_clic.default_arg
    ~long:"db-name"
    ~placeholder:"name"
    ~doc:"name of the db prefixed by the db scheme such as <sqlit3:db.sqlite>"
    ~default:"db.sqlite"
    (string_parameter ())

let watched_address_arg () =
  Tezos_clic.multiple_arg
    ~long:"watched-address"
    ~placeholder:"PKH"
    ~doc:
      "Public key hash of the account to index, also index delegator and \
       staker accounts if any. May be used multiple times"
    (string_parameter ())

let global_options () =
  Tezos_clic.args4
    (base_dir_arg ())
    (config_file_arg ())
    (endpoint_arg ())
    (media_type_arg ())

let clic_commands ~base_dir ~config_commands ~builtin_commands ~other_commands
    ~require_auth =
  clic_commands
    ~base_dir
    ~config_commands
    ~builtin_commands
    ~other_commands
    ~require_auth

type config = {
  basedir : string;
  rpc_addr : Uri.t option;
  db_name : string;
  watched_addresses : Signature.Public_key_hash.Set.t;
}

let pp_config logger config =
  let pp_watched_addresses fmt addresses =
    Signature.Public_key_hash.Set.iter
      (fun address ->
        Format.fprintf fmt "%a," Signature.Public_key_hash.pp_short address)
      addresses
  in
  Log.info logger (fun () ->
      Format.asprintf
        "@.Config:@.  base-dir: %s@.  db-name: %s@.  watched addresses: %a"
        config.basedir
        config.db_name
        pp_watched_addresses
        config.watched_addresses)

let get_config basedir rpc_listen_addr external__rpc_listen_addr db_name
    watched_addresses : config =
  let _external_rpc_listen_addr = external__rpc_listen_addr in
  let rpc_addr =
    Option.map (fun addr -> Uri.of_string ("//" ^ addr)) rpc_listen_addr
  in
  let watched_addresses =
    match watched_addresses with
    | None -> Signature.Public_key_hash.Set.empty
    | Some watched_addresses ->
        List.fold_left
          (fun acc address ->
            match Signature.Public_key_hash.of_b58check_opt address with
            | Some pkh -> Signature.Public_key_hash.Set.add pkh acc
            | None -> acc)
          Signature.Public_key_hash.Set.empty
          watched_addresses
  in
  {basedir; rpc_addr; db_name; watched_addresses}
