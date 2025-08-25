(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type tls_conf = {crt : string; key : string}

type connection = {source : string option; port : int; tls : tls_conf option}

type opt_with_transactions = NONE | SAFE | FULL

type t = {
  db_uri : string;
  network_interfaces : connection list;
  public_directory : string option;
  admins : (string * string) list;
  users : (string * string) list;
  max_batch_size : int32;
  with_transaction : opt_with_transactions;
  with_metrics : bool;
  verbosity : Log.level;
}

let tls_conf_encoding =
  let open Data_encoding in
  conv
    (fun {crt; key} -> (crt, key))
    (fun (crt, key) -> {crt; key})
    (obj2 (req "certfile" string) (req "keyfile" string))

let connection_encoding =
  let open Data_encoding in
  conv
    (fun {source; port; tls} -> (source, port, tls))
    (fun (source, port, tls) -> {source; port; tls})
    (obj3
       (opt
          ~description:"network interface address to bind to"
          "address"
          string)
       (req ~description:"tcp port on which listen" "port" int31)
       (opt
          ~description:"serve in https using the given certificate"
          "tls"
          tls_conf_encoding))

let login_encoding =
  let open Data_encoding in
  obj2 (req "login" string) (req "password" string)

let opt_with_transactions_encoding :
    opt_with_transactions option Data_encoding.t =
  Data_encoding.option
    (Data_encoding.string_enum [("NONE", NONE); ("SAFE", SAFE); ("FULL", FULL)])

let encoding =
  let open Data_encoding in
  conv
    (fun {
           db_uri;
           network_interfaces;
           public_directory;
           admins;
           users;
           max_batch_size;
           with_transaction;
           with_metrics;
           verbosity;
         }
       ->
      ( db_uri,
        network_interfaces,
        public_directory,
        admins,
        users,
        max_batch_size,
        Some with_transaction,
        with_metrics,
        verbosity ))
    (fun ( db_uri,
           network_interfaces,
           public_directory,
           admins,
           users,
           max_batch_size,
           with_transaction,
           with_metrics,
           verbosity )
       ->
      let with_transaction =
        (* FULL mode used by default with SQLite backend.
           caqti-driver-sqlite3 handles URIs starting with "sqlite3://" *)
        match with_transaction with
        | None ->
            if String.starts_with ~prefix:"sqlite" db_uri then FULL else NONE
        | Some x -> x
      in
      {
        db_uri;
        network_interfaces;
        public_directory;
        admins;
        users;
        max_batch_size;
        with_transaction;
        with_metrics;
        verbosity;
      })
    (obj9
       (req
          ~description:
            "Uri to reach the database: sqlite3:path or postgresql://host:port"
          "db"
          string)
       (dft "interfaces" (list connection_encoding) [])
       (opt
          "public_directory"
          ~description:
            "Path of the directory used to server static files (e.g. a dataviz \
             frontend application). If none provided this feature will not be \
             used (i.e. no default value)."
          string)
       (req "admins" (list login_encoding))
       (req "users" (list login_encoding))
       (dft "max_batch_size" int32 0l)
       (dft "with_transaction" opt_with_transactions_encoding None)
       (dft "with_metrics" bool false)
       (dft "verbosity" Log.level_encoding ERROR))
