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

module Make (Trace : Sig.TRACE) :
  Sig.MONAD with type 'err trace := 'err Trace.trace = struct
  let ( >>= ) = Lwt.( >>= )

  let[@inline] ok v = Ok v

  let ok_unit = Ok ()

  let ok_none = Ok None

  let[@inline] ok_some x = Ok (Some x)

  let ok_nil = Ok []

  let ok_true = Ok true

  let ok_false = Ok false

  let[@inline] error s = Error (Trace.make s)

  let[@inline] return v = Lwt.return_ok v

  let return_unit = Lwt.return ok_unit

  let return_none = Lwt.return ok_none

  let[@inline] return_some x = Lwt.return (Ok (Some x))

  let return_nil = Lwt.return ok_nil

  let return_true = Lwt.return ok_true

  let return_false = Lwt.return ok_false

  let[@inline] fail s = Lwt.return_error @@ Trace.make s

  let ( >>? ) v f = match v with Error _ as err -> err | Ok v -> f v

  let ( >>=? ) v f =
    v >>= function Error _ as err -> Lwt.return err | Ok v -> f v

  let ( >>?= ) v f = match v with Error _ as e -> Lwt.return e | Ok v -> f v

  let ( >|?= ) v f =
    match v with Error _ as e -> Lwt.return e | Ok v -> f v >>= Lwt.return_ok

  let ( >|=? ) v f = v >>=? fun v -> Lwt.return_ok (f v)

  let ( >|= ) = Lwt.( >|= )

  let ( >|? ) v f = v >>? fun v -> Ok (f v)

  let join_p = Lwt.join

  let all_p = Lwt.all

  let both_p = Lwt.both

  let rec join_e_errors trace_acc = function
    | Ok _ :: ts ->
        join_e_errors trace_acc ts
    | Error trace :: ts ->
        join_e_errors (Trace.conp trace_acc trace) ts
    | [] ->
        Error trace_acc

  let rec join_e = function
    | [] ->
        ok_unit
    | Ok () :: ts ->
        join_e ts
    | Error trace :: ts ->
        join_e_errors trace ts

  let all_e ts =
    let rec aux acc = function
      | [] ->
          Ok (List.rev acc)
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

  let record_trace err result =
    match result with
    | Ok _ as res ->
        res
    | Error trace ->
        Error (Trace.cons err trace)

  let trace err f =
    f
    >>= function
    | Error trace ->
        Lwt.return_error (Trace.cons err trace)
    | ok ->
        Lwt.return ok

  let record_trace_eval mk_err = function
    | Error trace ->
        mk_err () >>? fun err -> Error (Trace.cons err trace)
    | ok ->
        ok

  let trace_eval mk_err f =
    f
    >>= function
    | Error trace ->
        mk_err () >>=? fun err -> Lwt.return_error (Trace.cons err trace)
    | ok ->
        Lwt.return ok

  let error_unless cond exn = if cond then ok_unit else error exn

  let error_when cond exn = if cond then error exn else ok_unit

  let fail_unless cond exn = if cond then return_unit else fail exn

  let fail_when cond exn = if cond then fail exn else return_unit

  let unless cond f = if cond then return_unit else f ()

  let when_ cond f = if cond then f () else return_unit

  let dont_wait exc_handler err_handler f =
    Lwt_utils.dont_wait exc_handler (fun () ->
        f ()
        >>= function
        | Ok () ->
            Lwt.return_unit
        | Error trace ->
            err_handler trace ; Lwt.return_unit)
end
