(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Path of EVM state store. *)
val store_path : data_dir:string -> string

type store_info = {
  rollup_address : Address.t;
  current_number : Ethereum_types.quantity;
  history_mode : Configuration.history_mode;
  first_number : Ethereum_types.quantity;
}

(** [export_store ~data_dir ~output_db_file] exports the store database with
    data from the [data_dir] into the [output_db_file] and returns the rollup
    address and the current level. *)
val export_store :
  data_dir:string -> output_db_file:string -> store_info tzresult Lwt.t

(** [store_info ~data_dir_file] returns information about the store
    database in [data_dir] such as the address and the current level. *)
val store_info : data_dir:string -> store_info tzresult Lwt.t

(** [lock ~data_dir] takes an exclusive lock on [data_dir] for the
    duration of the process. It fails if there is already another evm node
    with a lock. *)
val lock : data_dir:string -> unit tzresult Lwt.t

(** [use ~data_dir k] creates [data_dir] if necessary before executing [k ()],
    and deletes [data_dir] if it was created and [k ()] fails. *)
val use : data_dir:string -> (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

(** Returns [true] if the data dir is already populated with a store and/or evm
    context. *)
val populated : data_dir:string -> bool Lwt.t
