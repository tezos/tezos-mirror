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

(** A global thread that resumes the first time {!exit} is called
    anywhere in the program. Called by the main to wait for any other
    thread in the system to call {!exit}. *)
val termination_thread : int Lwt.t

(** Awakens the {!termination_thread} with the given return value, and
    raises an exception that cannot be caught, except by a
    catch-all. Should only be called once. *)
val exit : int -> 'a

(** [exit_on signal] sets a signal handler for [signal] that exits cleanly using
    the [exit] function above. *)
val exit_on : ?log:(string -> unit) -> int -> unit

val retcode_of_unit_result_lwt : (unit, 'a) Result.result Lwt.t -> int Lwt.t

(** [wrap_promise p] is a promise [w] that resolves when either [p] resolves, or
    when [termination_thread] resolves. In the latter case, [p] is canceled,
    giving it a chance to clean up resources. *)
val wrap_promise : int Lwt.t -> int Lwt.t
