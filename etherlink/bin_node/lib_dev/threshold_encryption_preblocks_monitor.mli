(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** Module for monitoring preblocks produced by the Dsn node sequencer sidecar.
    Exactly one instance of [Threshold_encryption_preblocks_monitor.t] is
    created and used by the EVM node, when running in
    `threshold encryption sequencer` mode.
    The instance of [Threshold_encryption_preblocks_monitor.t] connects to the
    monitor endpoint where the Dsn node sequencer sidecar publishes preblocks.
    It also monitors notifications from the Evm node that a request to
    submit a proposal, handled by the [Threshold_encryption_proposals_handler],
    did not lead to a proposal being constructed and sent to the Dsn node,
    and therefore no preblock will be published by the latter. This
    ensures that there is a one-to-one mapping between proposal submission
    requests and items streamed by the
    [Threshold_encryption_preblocks_monitor], as long as the Dsn node sequencer
    sidecar is running. Because components that send requests to submit a
    proposal to the [Threshold_encryption_proposals_handler] (namely the
    [Threshold_encryption_sequencer] main loop and the `produceBlock` private
    RPC handler) always wait for an item to
    be delivered by the [Threshold_encryption_preblocks_monitor.t] instance,
    the one-to-one mapping property between proposal submission requests and
    items delivered to the [Threshold_encryption_preblocks_monitor.t]
    guarantees that these components will not wait indefinitely for a preblock.
    *)

type error += Timeout of (float * Uri.t)

(** The type of a preblock monitor. *)
type t

(* The type of preblock notifications. *)
type preblock_notification =
  | No_preblock
  | Preblock of Threshold_encryption_types.preblock

(** [init endpoint] returns a preblock monitor that receives preblocks from
    the DSN node sequencer sidecar running at [endpoint], together with
    a function that can be used to notify that a proposal has been processed
    but was not forwarded to the DSN sequencer node. *)
val init : Uri.t -> (t * (unit -> unit)) tzresult Lwt.t

(** [next ~timeout t] delivers the next preblock from the endpoint that [t]
    monitors, or [No_preblock] when a notification that a proposal will not
    lead to a preblock is sent to [t].
    This function waits up to [timeout] milliseconds for a preblock to
    be delivered, and it waits indefinitely if no [timeout] parameter is
    specified. If [timeout] is specified and no preblock is delivered before
    the timeout expires, this function fails with error
    [Timeout (timeout, endpoint)].
    Concurrent calls to [next t] are mutually exclusive, to guarantee that
    the same preblock is not delivered to different components.  *)
val next : ?timeout:float -> t -> preblock_notification tzresult Lwt.t

(** [submit_and_fetch ~timeout ~force ~timestamp t] adds a new proposal request
  with parameters [(timestamp, force)] to the the
  [Threshold_encryption_proposals_handler], and waits for a preblock (or
  [No_preblock] if the proposal results in a notification to [t] that no
  preblock will be produced) to be delivered by [t]. This function is atomic in
  the sense that, while it is executing, no call to [next t] will deliver a
  preblock. This function can be used when there is the need to guarantee that
  a proposal will lead to a preblock (or notification that no preblock has
  been produced by the proposal) to be delivered. *)
val submit_and_fetch :
  ?timeout:float ->
  force:bool ->
  timestamp:Time.Protocol.t ->
  t ->
  preblock_notification tzresult Lwt.t
