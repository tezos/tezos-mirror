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

(** [register_dal_messages ~messages] registers new [messages] in the
    queue for future injection on L1 and DAL. *)
val register_dal_messages : messages:string list -> unit tzresult Lwt.t

(** [get_injection_ids ()] returns the current injection IDs known by the DAL
    injection queue.  *)
val get_injection_ids : unit -> Injector.Inj_operation.id list tzresult

(** [forget_injection_id id] removes the given injection [id] and its associated
    information from the DAL injection queue, if it exists. *)
val forget_injection_id : Injector_sigs.Id.t -> unit tzresult

(** [produce_dal_slots ~level] produces and injects DAL slots on top of block at
    level [level] from the pending data injected to the DAL injection queue. *)
val produce_dal_slots : level:int32 -> unit tzresult Lwt.t

(** [set_dal_slot_indices idx] sets the list of slot indices on which the
    DAL injection worker will publish DAL slots. *)
val set_dal_slot_indices :
  Tezos_dal_node_services.Types.slot_index list -> unit tzresult Lwt.t
