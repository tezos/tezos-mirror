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

(** [progress_bar ~message ~counter ?update_interval ?color ?no_tty_quiet total]
    creates a progress bar with a [message] of the specified [color] to count
    until [total]. If counter is [`Bytes], the progress bar represents bytes
    amounts and if [`Int], it counts integer units. When provided,
    [update_interval = f] makes the progress bar update every [f] seconds. If
    [no_tty_quiet] is set to false, no message is displayed when the output is
    not a TTY (by default the [message] is displayed instead of the progress
    bar). *)
val progress_bar :
  message:string ->
  counter:[`Bytes | `Bytes_speed | `Int] ->
  ?update_interval:float ->
  ?color:Terminal.Color.t ->
  ?no_tty_quiet:bool ->
  int ->
  int line

(** [spinner ?no_tty_quiet message] creates a spinner that can be used to
    indicate progress for an unknown quantity. See {!progress_bar} for the
    description of [no_tty_quiet]. *)
val spinner : ?no_tty_quiet:bool -> string -> 'a line

(** Same as {!Progress.with_reporter} but for non tty outputs, only displays the
    message without animation. *)
val with_reporter : 'a line -> (('a -> unit) -> 'b) -> 'b

(** {2 Lwt compatible progress bars} *)

module Lwt : sig
  (** Same as {!with_reporter} for Lwt functions. *)
  val with_reporter : 'a line -> (('a -> unit Lwt.t) -> 'b Lwt.t) -> 'b Lwt.t

  (** [with_background_spinner ~message promise] displays a spinner with
      [messages] while the [promise] is pending. This function is to be used for
      code that cannot be instrumented for progress. *)
  val with_background_spinner :
    ?no_tty_quiet:bool -> message:string -> 'a Lwt.t -> 'a Lwt.t
end
