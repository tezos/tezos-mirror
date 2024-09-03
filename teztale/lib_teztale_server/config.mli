(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type tls_conf = {crt : string; key : string}

type connection = {source : string option; port : int; tls : tls_conf option}

(** Define the way teztale server use SQL transactions or not.

  - NONE: No transaction used
  - SAFE: Transaction are used for batch insertions that are specific
          to one archiver.
          i.e. for data that should not conflict with other archivers activity
          e.g. (pre)attestation: data unique withing one archiver
  - FULL: Transactions are used for all batch insertions, including data
          that would impact other archivers requests
          e.g. blocks data are indentical and sent by all archivers

  It should be safe to use FULL mode for any use case, but other modes are
  proposed because it has not been **rigorously** tested.

  FULL mode is used by default with SQLite backend,
  because is can quickly suffer from poor performances otherwise.

  Postgresql is less impacted by this problem and use NONE as default.
  *)
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

(** Encoding driver for a structure with two required fields: "login" and "password" *)
val login_encoding : (string * string) Data_encoding.t

val encoding : t Data_encoding.t
