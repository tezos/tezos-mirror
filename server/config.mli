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
  verbosity : Teztale_lib.Log.level;
}

(** Encoding driver for a structure with two required fields: "login" and "password" *)
val login_encoding : (string * string) Data_encoding.t

val encoding : t Data_encoding.t
