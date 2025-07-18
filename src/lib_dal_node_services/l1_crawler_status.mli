(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Status of the DAL node's L1 crawler, indicating its sync state with the L1
    chain. *)
type t =
  | Catching_up of {levels_to_process : int32}
      (** The crawler is processing a large number of levels at startup to catch
          up with the last L1 finalized level. *)
  | Synced
      (** The crawler is fully synchronized with the L1 chain (it processed the
          last finalized L1 block. *)
  | Lagging of {levels_to_process : int32}
      (** The crawler is behind the L1 finalized block and not progressing.
          This may indicate issues or temporary delays. *)
  | L1_bootstrapping
      (** The L1 node is currently bootstrapping and has not yet reached a
          stable state. This status is possible when starting the DAL node. *)
  | L1_unreachable  (** The DAL node is unable to reach the L1 node. *)
  | Unknown  (** The crawler's status is unknown. *)

(** Encoding of the L1 crawler status. *)
val encoding : t Data_encoding.t

(** [catching_up_or_synced_status ~head_level ~last_processed_level] returns the
    appropriate status of the crawler: [Catching_up] if the
    [last_processed_level] is more than 2 levels behind [head_level],
    considering the Tenderbake finality lag; [Synced] otherwise.
*)
val catching_up_or_synced_status :
  head_level:int32 -> last_processed_level:int32 -> t

(** Similar to {!catching_up_or_synced_status} below, but returns [Lagging] if
    the crawler is behind instead of [Catching_up]. *)
val lagging_or_synced_status :
  head_level:int32 -> last_processed_level:int32 -> t
