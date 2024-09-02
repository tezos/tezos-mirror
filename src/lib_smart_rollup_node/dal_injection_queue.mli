(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori     <contact@functori.com>          *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [init  node_ctxt] initializes and starts the DAL injection
    worker. *)
val init : _ Node_context.t -> unit tzresult Lwt.t

(** [register_dal_slot ~slot_content ~slot_index] registers a new DAL slot
    [slot_content] in the queue for future injection on L1 and DAL with index
    [slot_index]. *)
val register_dal_slot :
  slot_content:string -> slot_index:int -> Injector_sigs.Id.t tzresult Lwt.t

(** [get_injection_ids ()] returns the current injection IDs known by the DAL
    injection queue.  *)
val get_injection_ids : unit -> Injector.Inj_operation.id list tzresult
