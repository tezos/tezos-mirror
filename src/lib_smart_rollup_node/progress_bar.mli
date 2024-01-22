(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Small wrapper around the {!Progress} library to easily create progress bars
    for use in the rollup node. *)

include module type of Progress

(** {2 Simple wrappers for progress bars} *)

(** The type of progress line information which can also be displayed on non tty
    outputs.  *)
type 'a line

(** [progress_bar ~message ~counter ~color total] creates a progress bar with a
    [message] of the specified [color] to count until [total]. If counter is
    [`Bytes], the progress bar represents bytes amounts and if [`Int], it counts
    integer units. *)
val progress_bar :
  message:string ->
  counter:[`Bytes | `Int] ->
  ?color:Terminal.Color.t ->
  int ->
  int line

(** [spinner ~message] creates a spinner that can be used to indicate progress
    for an unknown quantity. *)
val spinner : message:string -> 'a line

(** Same as {!Progress.with_reporter} but for non tty outputs, only displays the
    message without animation. *)
val with_reporter : 'a line -> (('a -> unit) -> 'b) -> 'b

(** {2 Lwt compatible progress bars} *)

module Lwt : sig
  (** Same as {!with_reporter} for Lwt functions. *)
  val with_reporter : 'a line -> (('a -> unit Lwt.t) -> 'b Lwt.t) -> 'b Lwt.t
end
