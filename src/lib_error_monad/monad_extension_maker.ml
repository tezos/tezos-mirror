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

module Make (Error : sig
  type error = ..

  include Sig.CORE with type error := error
end)
(Trace : Sig.TRACE)
(Monad : Tezos_lwt_result_stdlib.Lwtreslib.TRACED_MONAD
           with type 'error trace := 'error Trace.trace) :
  Sig.MONAD_EXTENSION
    with type error := Error.error
     and type 'error trace := 'error Trace.trace = struct
  (* we default to exposing the combined monad syntax everywhere.
     We do the bulk of this by including [Lwt_traced_result_syntax] directly. *)
  include Monad.Lwt_traced_result_syntax

  (* Some globals that Lwtreslib does not expose but that the Tezos code uses a
     lot. *)
  let ( >>= ) = Monad.Lwt_syntax.( let* )

  let ( >|= ) = Monad.Lwt_syntax.( let+ )

  let ( >>? ) = Monad.Result_syntax.( let* )

  let ( >|? ) = Monad.Result_syntax.( let+ )

  let ok = Monad.Result_syntax.return

  let error = Monad.Traced_result_syntax.fail

  let ( >>=? ) = Monad.Lwt_result_syntax.( let* )

  let ( >|=? ) = Monad.Lwt_result_syntax.( let+ )

  let ( >>?= ) = Monad.Lwt_result_syntax.( let*? )

  let ( >|?= ) r f =
    match r with Error _ as e -> Lwt.return e | Ok o -> Lwt_result.ok (f o)

  (* default (traced-everywhere) helper types *)
  type tztrace = Error.error Trace.trace

  type 'a tzresult = ('a, tztrace) result

  let trace_encoding = Trace.encoding Error.error_encoding

  let result_encoding a_encoding =
    let open Data_encoding in
    let trace_encoding = obj1 (req "error" trace_encoding) in
    let a_encoding = obj1 (req "result" a_encoding) in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          a_encoding
          ~title:"Ok"
          (function Ok x -> Some x | _ -> None)
          (function res -> Ok res);
        case
          (Tag 1)
          trace_encoding
          ~title:"Error"
          (function Error x -> Some x | _ -> None)
          (function x -> Error x);
      ]

  let pp_print_trace = Trace.pp_print Error.pp

  let pp_print_top_error_of_trace = Trace.pp_print_top Error.pp

  let classify_trace trace =
    Trace.fold
      (fun c e -> Error_classification.combine c (Error.classify_error e))
      Error_classification.default
      trace

  let record_trace err result =
    match result with
    | Ok _ as res -> res
    | Error trace -> Error (Trace.cons err trace)

  let trace err f =
    let open Monad.Lwt_syntax in
    let* r = f in
    match r with
    | Error trace -> Lwt.return_error (Trace.cons err trace)
    | ok -> Lwt.return ok

  let record_trace_eval mk_err = function
    | Error trace ->
        let err = mk_err () in
        Error (Trace.cons err trace)
    | ok -> ok

  let trace_eval mk_err f =
    let open Monad.Lwt_syntax in
    let* r = f in
    match r with
    | Error trace ->
        let err = mk_err () in
        Lwt.return_error (Trace.cons err trace)
    | ok -> Lwt.return ok

  let error_unless cond exn =
    if cond then Monad.Traced_result_syntax.return_unit else error exn

  let error_when cond exn =
    if cond then error exn else Monad.Traced_result_syntax.return_unit

  let fail_unless cond exn = if cond then return_unit else fail exn

  let fail_when cond exn = if cond then fail exn else return_unit

  let unless cond f = if cond then return_unit else f ()

  let when_ cond f = if cond then f () else return_unit

  let dont_wait f err_handler exc_handler =
    let open Monad.Lwt_syntax in
    Lwt.dont_wait
      (fun () ->
        let* r = f () in
        match r with
        | Ok () -> Lwt.return_unit
        | Error trace ->
            err_handler trace ;
            Lwt.return_unit)
      exc_handler
end
