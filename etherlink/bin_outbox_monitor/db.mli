(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Data is stored in an SQLite database. *)
type t = Sqlite.t

module Contract = Tezos_raw_protocol_alpha.Alpha_context.Contract

type withdrawal_kind = Xtz | FA of Ethereum_types.Address.t

type withdrawal = {
  kind : withdrawal_kind;
  amount : Ethereum_types.quantity;
  sender : Ethereum_types.Address.t;
  receiver : Contract.t;
  withdrawal_id : Ethereum_types.quantity;
}

type withdrawal_log = {
  transactionHash : Ethereum_types.hash;
  transactionIndex : Ethereum_types.quantity;
  logIndex : Ethereum_types.quantity;
  blockHash : Ethereum_types.block_hash;
  blockNumber : Ethereum_types.quantity;
  removed : bool;
  withdrawal : withdrawal;
}

type l2_levels_range = {
  start_l2 : Ethereum_types.quantity;
  end_l2 : Ethereum_types.quantity;
}

(** Human readable encoding for quantities (levels, amounts, etc.) *)
val quantity_hum_encoding : Ethereum_types.quantity Data_encoding.t

val pp_withdrawal_kind : Format.formatter -> withdrawal_kind -> unit

val withdrawal_kind_encoding : withdrawal_kind Data_encoding.t

val withdrawal_encoding : withdrawal Data_encoding.t

val withdrawal_log_encoding : withdrawal_log Data_encoding.t

(** Initialize the database by creating it if it doesn't exist and applying
    migrations when needed. *)
val init : data_dir:string -> [`Read_only | `Read_write] -> t tzresult Lwt.t

module Withdrawals : sig
  val store : ?conn:Sqlite.conn -> t -> withdrawal_log -> unit tzresult Lwt.t
end

module Levels : sig
  val store :
    ?conn:Sqlite.conn ->
    t ->
    l1_level:int32 ->
    start_l2_level:Ethereum_types.quantity ->
    end_l2_level:Ethereum_types.quantity ->
    unit tzresult Lwt.t

  val get_l2_range :
    ?conn:Sqlite.conn ->
    t ->
    l1_level:int32 ->
    l2_levels_range option tzresult Lwt.t

  val get_l1 :
    ?conn:Sqlite.conn ->
    t ->
    l2_level:Ethereum_types.quantity ->
    int32 option tzresult Lwt.t

  val last :
    ?conn:Sqlite.conn -> t -> (int32 * l2_levels_range) option tzresult Lwt.t
end

module Pointers : sig
  module type S = sig
    val set :
      ?conn:Sqlite.conn -> t -> Ethereum_types.quantity -> unit tzresult Lwt.t

    val get :
      ?conn:Sqlite.conn -> t -> Ethereum_types.quantity option tzresult Lwt.t
  end

  module Finalized_L1_head : S

  module L2_head : S
end
