(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Tezos Protocol Implementation - Error Monad *)

(*-- Error classification ----------------------------------------------------*)

type error_category = [`Branch | `Temporary | `Permanent]

include TzCore
include TzMonad
module TzTrace = TzTrace

type 'error trace = 'error TzTrace.trace

type error += Exn of exn

let () =
  register_error_kind
    `Temporary
    ~id:"failure"
    ~title:"Exception"
    ~description:"Exception safely wrapped in an error"
    ~pp:(fun ppf s -> Format.fprintf ppf "@[<h 0>%a@]" Format.pp_print_text s)
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Exn (Failure msg) -> Some msg
      | Exn exn -> Some (Printexc.to_string exn)
      | _ -> None)
    (fun msg -> Exn (Failure msg))

let generic_error fmt = Format.kasprintf (fun s -> error (Exn (Failure s))) fmt

let failwith fmt = Format.kasprintf (fun s -> fail (Exn (Failure s))) fmt

let error_of_exn e = TzTrace.make @@ Exn e

let error_exn s = Error (TzTrace.make @@ Exn s)

let tzresult_of_exn_result r = Result.map_error error_of_exn r

let trace_exn exn f = trace (Exn exn) f

let generic_trace fmt =
  Format.kasprintf (fun str -> trace_exn (Failure str)) fmt

let record_trace_exn exn f = record_trace (Exn exn) f

let failure fmt = Format.kasprintf (fun str -> Exn (Failure str)) fmt

let pp_exn ppf exn = pp ppf (Exn exn)

type error += Canceled

let () =
  register_error_kind
    `Temporary
    ~id:"canceled"
    ~title:"Canceled"
    ~description:"A promise was unexpectedly canceled"
    ~pp:(fun f () ->
      Format.pp_print_string f "The promise was unexpectedly canceled")
    Data_encoding.unit
    (function Canceled -> Some () | _ -> None)
    (fun () -> Canceled)

let protect_no_canceler ?on_error t =
  let res = Lwt.catch t (fun exn -> fail (Exn exn)) in
  res >>= function
  | Ok _ -> res
  | Error trace -> (
      match on_error with
      | None -> res
      | Some on_error ->
          Lwt.catch (fun () -> on_error trace) (fun exn -> fail (Exn exn)))

let protect_canceler ?on_error canceler t =
  let cancellation =
    Lwt_canceler.when_canceling canceler >>= fun () -> fail Canceled
  in
  let res = Lwt.pick [cancellation; Lwt.catch t (fun exn -> fail (Exn exn))] in
  res >>= function
  | Ok _ -> res
  | Error trace -> (
      let trace =
        if Lwt_canceler.canceled canceler then TzTrace.make Canceled else trace
      in
      match on_error with
      | None -> Lwt.return_error trace
      | Some on_error ->
          Lwt.catch (fun () -> on_error trace) (fun exn -> fail (Exn exn)))

let protect ?on_error ?canceler t =
  match canceler with
  | None -> protect_no_canceler ?on_error t
  | Some canceler -> protect_canceler ?on_error canceler t

type error += Timeout

let () =
  register_error_kind
    `Temporary
    ~id:"utils.Timeout"
    ~title:"Timeout"
    ~description:"Timeout"
    ~pp:(fun f () -> Format.pp_print_string f "The request has timed out")
    Data_encoding.unit
    (function Timeout -> Some () | _ -> None)
    (fun () -> Timeout)

let with_timeout ?(canceler = Lwt_canceler.create ()) timeout f =
  let target = f canceler in
  Lwt.choose [timeout; (target >|= fun _ -> ())] >>= fun () ->
  if Lwt.state target <> Lwt.Sleep then (
    Lwt.cancel timeout ;
    target)
  else
    Lwt_canceler.cancel canceler >>= function
    | Ok () | Error [] -> fail Timeout
    | Error (h :: _) -> raise h

let errs_tag = Tag.def ~doc:"Errors" "errs" pp_print_error

let cancel_with_exceptions canceler =
  Lwt_canceler.cancel canceler >>= function
  | Ok () | Error [] -> Lwt.return_unit
  | Error (h :: _) -> raise h

let catch ?catch_only f = TzLwtreslib.Result.catch_f ?catch_only f error_of_exn

let catch_e ?catch_only f =
  TzLwtreslib.Result.catch_f ?catch_only f error_of_exn |> Result.join

let catch_f ?catch_only f exc_mapper =
  TzLwtreslib.Result.catch_f ?catch_only f (fun exc ->
      TzTrace.make (exc_mapper exc))

let catch_s ?catch_only f =
  TzLwtreslib.Result.catch_s ?catch_only f >|= Result.map_error error_of_exn

let catch_es ?catch_only f =
  TzLwtreslib.Result.catch_s ?catch_only f
  >|= Result.map_error error_of_exn
  >|= Result.join
