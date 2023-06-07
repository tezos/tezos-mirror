(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** {1} Function evaluation in a detached process

    This library uses a process detached in a separated unix process to
   execute a given function, with bidirectional communication channels
   and transmission of the function result at the end of the
   execution.

    The communication channels does not require a data encoding, but
   the data encoding can be given. In absence of data encoding the
   Marshal mecanism will be used. Be aware that extensible types, like
   error or exception cannot be safely exchanged between processes.

    Flags for Marshal serialisation can be passed at the creation of
   the detached process.

  *)
open Error_monad

exception Exited of int

(** Endpoint of a bidirectionnal channel.  *)
module Channel : sig
  (** A bidirectionnal channel endpoint.   *)
  type ('sent, 'received) t

  (** Asynchronously sending a value *)
  val push : ('sent, 'received) t -> 'sent -> unit tzresult Lwt.t

  (** Waiting for a value. *)
  val pop : ('sent, 'received) t -> 'received tzresult Lwt.t
end

(** Detached process. *)
type ('sent, 'received, 'result) t

(** Executing a function in a detached process.
    [prefix] will be used in detached process logs.
    On canceling of [canceler], the detached process will be killed by
    SIGKILL.

    [input_encoding] and [output_encoding], if provided,  will be used
    to exchange values between the main process and the detached process
    [input_encoding] is for values received by the detached process.
    [output_encoding] is for values sent by the detached process.

    In absence of data encoding the Marshal mecanism will be  used.
    Be aware that extensible types, like error or exception cannot be
    safely exchanged between processes.

    [value_encoding] is for the encoding of the result of the detached
    function.
    The Error_monad encapsulation part of the value computed by the
    detached function will be safely encoded, even without encoding
    for the ['result] type. Ie if the detached function end with an
    error, the error will safely be serialized-deserialised.

   If no encoding is given, the values will be serialized using the
   Marshal module, with the given [flags] (if any is provided).

  *)
val detach :
  ?prefix:string ->
  ?canceler:Lwt_canceler.t ->
  ?input_encoding:'received Data_encoding.encoding ->
  ?output_encoding:'sent Data_encoding.encoding ->
  ?value_encoding:'result Data_encoding.encoding ->
  ?flags:Marshal.extern_flags list ->
  (('sent, 'received) Channel.t -> 'result tzresult Lwt.t) ->
  ('sent, 'received, 'result) t tzresult Lwt.t

(** Sending a data to the detached process   *)
val send : ('a, 'received, 'c) t -> 'received -> unit tzresult Lwt.t

(** Receiving a data from the detached process.
    This call is blocking.
   *)
val receive : ('sent, 'b, 'c) t -> 'sent tzresult Lwt.t

(** Receiving the result of the detached function.
    This call is blocking.
   *)
val wait_result : ('a, 'b, 'result) t -> 'result tzresult Lwt.t

(** {2 Working with list of detached process} *)

(** Waiting for all the detached function to finish, unless one
    of the function return an error.

    If all detached functions succesfully compute a value, return the list of
    values.

    If at least one function end with en error, cancel all the
    unfinished process, and return the trace of errors of unsuccesful process.

  *)
val wait_all_results : ('a, 'b, 'c) t list -> 'c list tzresult Lwt.t

(** Waiting for all the detached function to finish, unless one
    of the function return an error.

    If all detached functions succesfully compute a value, return unit.

    If at least one function end with en error, cancel all the
    unfinished process, and fail with a message containing all the errors.

  *)
val wait_all : ('a, 'b, 'c) t list -> unit tzresult Lwt.t
