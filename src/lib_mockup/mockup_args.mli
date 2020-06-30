(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Chain_id : sig
  (** [of_string s] converts any string to its valid [chain_id] representation.
   **
   ** This function is useful to get valid, but dummy, chain identifiers out of
   ** command lines in the context of a mockup client.
   *)
  val of_string : string -> Chain_id.t

  (** [dummy] is a valid dummy chain identifier.
   **
   ** Do not use it outside of the context of a mockup client.
   *)
  val dummy : Chain_id.t

  (** [choose ~from_config_file]
   ** produces a valid chain id from the optionally given one.
   **
   ** The value from the command line, if any, has highest precedence, over the
   ** one from the config file.
   ** When both values are [None], it uses the {!dummy}.
   *)
  val choose : from_config_file:Chain_id.t option -> Chain_id.t
end
