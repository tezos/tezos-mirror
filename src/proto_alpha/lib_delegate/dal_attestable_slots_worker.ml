(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Baking_state_types
open Tezos_dal_node_services

(** A handle to a single delegate’s DAL monitoring subscription. *)
type stream_handle = {
  stream : Types.Attestable_event.t Lwt_stream.t;
  stopper : Tezos_rpc.Context.stopper;
}

type slots_by_delegate = Types.attestable_slots Delegate_id.Table.t

type t = {
  attestation_lag : int;
  number_of_slots : int;
  streams : stream_handle Delegate_id.Table.t;
      (** Active per-delegate subscriptions. *)
  cache : (int32, slots_by_delegate) Stdlib.Hashtbl.t;
      (** Cache of attestable slots, keyed by attestation levels. *)
}

let create_delegate_table () = Delegate_id.Table.create 10

let create ~attestation_lag ~number_of_slots =
  {
    attestation_lag;
    number_of_slots;
    streams = create_delegate_table ();
    cache = Stdlib.Hashtbl.create 16;
  }

let shutdown_worker state =
  let open Lwt_syntax in
  let* stoppers =
    let stoppers =
      Delegate_id.Table.to_seq state.streams
      |> Seq.map (fun (_delegate_id, {stopper; _}) -> stopper)
      |> List.of_seq
    in
    Delegate_id.Table.clear state.streams ;
    Stdlib.Hashtbl.reset state.cache ;
    return stoppers
  in
  List.iter (fun stopper -> stopper ()) stoppers ;
  return_unit
