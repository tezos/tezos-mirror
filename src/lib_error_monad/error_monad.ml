(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

include Core
include Monad

module Make (Prefix : Sig.PREFIX) : sig
  include Sig.CORE

  include Sig.EXT with type error := error

  include Sig.WITH_WRAPPED with type error := error
end = struct
  include Core_maker.Make (Prefix)
end

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
      | Exn (Failure msg) ->
          Some msg
      | Exn exn ->
          Some (Printexc.to_string exn)
      | _ ->
          None)
    (fun msg -> Exn (Failure msg))

let generic_error fmt = Format.kasprintf (fun s -> error (Exn (Failure s))) fmt

let failwith fmt = Format.kasprintf (fun s -> fail (Exn (Failure s))) fmt

let error_exn s = Error [Exn s]

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
    ~id:"utils.Canceled"
    ~title:"Canceled"
    ~description:"Canceled"
    Data_encoding.unit
    (function Canceled -> Some () | _ -> None)
    (fun () -> Canceled)

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

let protect ?on_error ?canceler t =
  let cancellation =
    match canceler with
    | None ->
        Lwt_utils.never_ending ()
    | Some canceler ->
        Lwt_canceler.cancellation canceler >>= fun () -> fail Canceled
  in
  let res = Lwt.pick [cancellation; Lwt.catch t (fun exn -> fail (Exn exn))] in
  res
  >>= function
  | Ok _ ->
      res
  | Error err -> (
      let canceled =
        Option.unopt_map canceler ~default:false ~f:Lwt_canceler.canceled
      in
      let err = if canceled then [Canceled] else err in
      match on_error with
      | None ->
          Lwt.return_error err
      | Some on_error ->
          Lwt.catch (fun () -> on_error err) (fun exn -> fail (Exn exn)) )

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
  Lwt.choose [timeout; (target >|= fun _ -> ())]
  >>= fun () ->
  if Lwt.state target <> Lwt.Sleep then (Lwt.cancel timeout ; target)
  else Lwt_canceler.cancel canceler >>= fun () -> fail Timeout

let errs_tag = Tag.def ~doc:"Errors" "errs" pp_print_error
