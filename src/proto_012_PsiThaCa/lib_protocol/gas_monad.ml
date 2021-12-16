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

open Alpha_context

(* The outer [tzresult] is for gas exhaustion only. The inner [result] is for all
   other (non-gas) errors. *)
type ('a, 'trace) t = context -> (('a, 'trace) result * context) tzresult

type ('a, 'trace) gas_monad = ('a, 'trace) t

let of_result x ctxt = ok (x, ctxt)

let return x = of_result (ok x)

let ( >>$ ) m f ctxt =
  m ctxt >>? fun (x, ctxt) ->
  match x with Ok y -> f y ctxt | Error _ as err -> of_result err ctxt

let ( >|$ ) m f ctxt = m ctxt >>? fun (x, ctxt) -> of_result (x >|? f) ctxt

let ( >?$ ) m f = m >>$ fun x -> of_result (f x)

let ( >??$ ) m f ctxt = m ctxt >>? fun (x, ctxt) -> f x ctxt

let consume_gas cost ctxt = Gas.consume ctxt cost >>? return ()

let run ctxt x = x ctxt

let record_trace_eval f m ctxt =
  m ctxt >>? fun (x, ctxt) -> of_result (record_trace_eval f x) ctxt
