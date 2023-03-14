(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {
  factor : float;
  initial_delay : Time.System.Span.t;
  disconnection_delay : Time.System.Span.t;
  increase_cap : Time.System.Span.t;
}

let default =
  {
    factor = 1.2;
    initial_delay = Ptime.Span.of_int_s 1;
    disconnection_delay = Ptime.Span.of_int_s 60;
    increase_cap = Ptime.Span.of_int_s 172800 (* 2 days *);
  }

let encoding =
  let open Data_encoding in
  conv
    (fun {factor; initial_delay; disconnection_delay; increase_cap} ->
      (factor, initial_delay, disconnection_delay, increase_cap))
    (fun (factor, initial_delay, disconnection_delay, increase_cap) ->
      {factor; initial_delay; disconnection_delay; increase_cap})
    (obj4
       (dft
          "factor"
          ~description:
            "The factor by which the reconnection delay is increased when a \
             peer that was previously disconnected is disconnected again. This \
             value should be set to 1 for a linear back-off and to >1 for an \
             exponential back-off."
          float
          default.factor)
       (dft
          "initial-delay"
          ~description:
            "The span of time a peer is disconnected for when it is first \
             disconnected."
          Time.System.Span.encoding
          default.initial_delay)
       (dft
          "disconnection-delay"
          ~description:
            "The span of time a peer is disconnected for when it is \
             disconnected as the result of an error."
          Time.System.Span.encoding
          default.disconnection_delay)
       (dft
          "increase-cap"
          ~description:
            "The maximum amount by which the reconnection is extended. This \
             limits the rate of the exponential back-off, which eventually \
             becomes linear when it reaches this limit. This limit is set to \
             avoid reaching the End-of-Time when repeatedly reconnection a \
             peer."
          Time.System.Span.encoding
          default.increase_cap))
