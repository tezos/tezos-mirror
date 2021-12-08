(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

(* The outer option is for gas exhaustion. The inner [result] is for all other
   errors. *)
type ('a, 'trace) t =
  Local_gas_counter.local_gas_counter ->
  (('a, 'trace) result * Local_gas_counter.local_gas_counter) option

type ('a, 'trace) gas_monad = ('a, 'trace) t

let of_result x gas = Some (x, gas)

let return x = of_result (ok x)

(* Inlined [Option.bind] for performance. *)
let ( >>?? ) m f =
  match m with None -> None | Some x -> f x
  [@@ocaml.inline always]

let ( >>$ ) m f gas =
  m gas >>?? fun (res, gas) ->
  match res with Ok y -> f y gas | Error _ as err -> of_result err gas

let ( >|$ ) m f gas = m gas >>?? fun (x, gas) -> of_result (x >|? f) gas

let ( >?$ ) m f = m >>$ fun x -> of_result (f x)

let ( >??$ ) m f gas = m gas >>?? fun (x, gas) -> f x gas

let consume_gas cost gas =
  match Local_gas_counter.update_and_check gas cost with
  | None -> None
  | Some gas -> Some (ok (), gas)

let run ctxt m =
  match Gas.level ctxt with
  | Gas.Unaccounted -> (
      match m (Saturation_repr.saturated :> int) with
      | Some (res, _new_gas_counter) -> ok (res, ctxt)
      | None -> error Gas.Operation_quota_exceeded)
  | Limited {remaining} -> (
      match m (remaining :> int) with
      | Some (res, new_gas_counter) ->
          let ctxt =
            Local_gas_counter.update_context
              new_gas_counter
              (Local_gas_counter.outdated ctxt)
          in
          ok (res, ctxt)
      | None -> error Gas.Operation_quota_exceeded)

let record_trace_eval f m gas =
  m gas >>?? fun (x, gas) -> of_result (record_trace_eval f x) gas
