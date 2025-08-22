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

let of_result x gas = Some (x, gas) [@@ocaml.inline always]

let return x = of_result (Ok x) [@@ocaml.inline always]

let return_unit = return ()

(* Inlined [Option.bind] for performance. *)
let ( >>?? ) m f = match m with None -> None | Some x -> f x
[@@ocaml.inline always]

let bind m f gas =
  m gas >>?? fun (res, gas) ->
  match res with Ok y -> f y gas | Error _ as err -> of_result err gas
[@@ocaml.inline always]

let map f m gas =
  let open Result_syntax in
  m gas >>?? fun (x, gas) ->
  of_result
    (let+ x in
     f x)
    gas
[@@ocaml.inline always]

let bind_result m f = bind (of_result m) f [@@ocaml.inline always]

let bind_recover m f gas = m gas >>?? fun (x, gas) -> f x gas
[@@ocaml.inline always]

let consume_gas cost gas =
  match Local_gas_counter.consume_opt gas cost with
  | None -> None
  | Some gas -> Some (Ok (), gas)

let run ctxt m =
  let open Local_gas_counter in
  let open Result_syntax in
  match Gas.level ctxt with
  | Gas.Unaccounted -> (
      match m (Local_gas_counter (Saturation_repr.saturated :> int)) with
      | Some (res, _new_gas_counter) -> return (res, ctxt)
      | None -> tzfail Gas.Operation_quota_exceeded)
  | Limited {remaining = _} -> (
      let gas_counter, outdated_ctxt =
        local_gas_counter_and_outdated_context ctxt
      in
      match m gas_counter with
      | Some (res, new_gas_counter) ->
          let ctxt = update_context new_gas_counter outdated_ctxt in
          return (res, ctxt)
      | None -> tzfail Gas.Operation_quota_exceeded)

let record_trace_eval : type error_trace error_context.
    error_details:(error_context, error_trace) Script_tc_errors.error_details ->
    (error_context -> error) ->
    ('a, error_trace) t ->
    ('a, error_trace) t =
 fun ~error_details ->
  match error_details with
  | Fast -> fun _f m -> m
  | Informative err_ctxt ->
      fun f m gas ->
        m gas >>?? fun (x, gas) ->
        of_result (record_trace_eval (fun () -> f err_ctxt) x) gas

let fail e = of_result (Error e) [@@ocaml.inline always]

module Syntax = struct
  let return = return

  let return_unit = return_unit

  let return_none = return None

  let return_some x = return (Some x)

  let return_nil = return []

  let return_true = return true

  let return_false = return false

  let fail = fail

  let ( let* ) = bind

  let ( let+ ) m f = map f m

  let ( let*? ) = bind_result
end
