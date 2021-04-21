(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make (Trace : Traced_sigs.Trace.S) :
  Traced_sigs.Monad.S with type 'error trace = 'error Trace.trace = struct
  include Bare_structs.Monad

  type 'error trace = 'error Trace.trace

  let error_trace e = error (Trace.make e)

  let fail_trace e = fail (Trace.make e)

  let rec join_e_errors trace_acc = function
    | Ok _ :: ts ->
        join_e_errors trace_acc ts
    | Error trace :: ts ->
        join_e_errors (Trace.conp trace_acc trace) ts
    | [] ->
        Error trace_acc

  let rec join_e = function
    | [] ->
        unit_e
    | Ok () :: ts ->
        join_e ts
    | Error trace :: ts ->
        join_e_errors trace ts

  let all_e ts =
    let rec aux acc = function
      | [] ->
          Ok (Stdlib.List.rev acc)
      | Ok v :: ts ->
          aux (v :: acc) ts
      | Error trace :: ts ->
          join_e_errors trace ts
    in
    aux [] ts

  let both_e a b =
    match (a, b) with
    | (Ok a, Ok b) ->
        Ok (a, b)
    | (Error err, Ok _) | (Ok _, Error err) ->
        Error err
    | (Error erra, Error errb) ->
        Error (Trace.conp erra errb)

  let join_ep ts = all_p ts >|= join_e

  let all_ep ts = all_p ts >|= all_e

  let both_ep a b = both_p a b >|= fun (a, b) -> both_e a b
end
