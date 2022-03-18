(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Logging levels.  See [docs/developer/guidelines.rst] for their meaning *)
type level = Debug | Info | Notice | Warning | Error | Fatal

(** Logs a message. It is the shell's responsibility to manage the actual
    logging.

    Even though logging may involve system calls, formatting, or other work, the
    shell guarantees that calling this function doesn't transfer control over
    another promise. Consequently, the performance of this function can be
    considered predictable from the point of view of gas-consumption.

    Note that the function call has predictable performance, but that it is the
    caller's responsibility to ensure that argument evaluation has predictable
    performance too. E.g., [log Notice "%s" (Format.asprint …)] may spend time
    formatting the argument string. *)
val log : level -> ('a, Format.formatter, unit, unit) format4 -> 'a

(** Same as [log] but more efficient with a simpler interface. *)
val log_string : level -> string -> unit
