(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2021 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

open Internal_event.Simple

let section = ["node"; "protocol"]

let make n level =
  declare_1
    ~section
    ~name:(n ^ "_from_protocol")
    ~msg:"{message}"
    ~pp1:Format.pp_print_string
    ~level
    ("message", Data_encoding.string)

let debug = make "debug" Debug

let info = make "info" Info

let notice = make "notice" Notice

let warning = make "warning" Warning

let error = make "error" Error

let fatal = make "fatal" Fatal

let logging_failure =
  declare_1
    ~section
    ~name:"logging_failure"
    ~msg:"Failure to log a protocol message: {exc}"
    ~pp1:Format.pp_print_string
    ~level:Error
    ("exc", Data_encoding.string)

let make_asynchronous_log_message_consumer () =
  let (stream, push) = Lwt_stream.create () in
  let alive = ref true in
  Lwt.dont_wait
    (fun () ->
      Lwt_stream.iter_s
        (fun (level, s) ->
          let open Lwt_syntax in
          (* Pause to avoid interleaving of execution *)
          let* () = Lwt.pause () in
          Lwt.catch
            (fun () ->
              match level with
              | Internal_event.Debug -> emit debug s
              | Info -> emit info s
              | Notice -> emit notice s
              | Warning -> emit warning s
              | Error -> emit error s
              | Fatal -> emit fatal s)
            (fun exc -> emit logging_failure (Printexc.to_string exc)))
        stream)
    (fun exc ->
      (* We ignore the exception because it can only be the exception raised
         within the other exception handler which already attempted to log an
         error. *)
      ignore (exc : exn) ;
      (* If the [iter_s] raises, then there are no more listeners on the stream
         and we shouldn't push values on the stream. *)
      alive := false) ;
  fun level s ->
    if !alive then
      try push (Some (level, s)) with Lwt_stream.Closed -> alive := false
    else ()

let make_log_message_consumer () level s =
  Lwt.dont_wait
    (fun () ->
      match level with
      | Internal_event.Debug -> emit debug s
      | Info -> emit info s
      | Notice -> emit notice s
      | Warning -> emit warning s
      | Error -> emit error s
      | Fatal -> emit fatal s)
    (fun exc ->
      Lwt.dont_wait
        (fun () -> emit logging_failure (Printexc.to_string exc))
        (fun _exn ->
          (* Ignoring: everything went wrong*)
          ()))
