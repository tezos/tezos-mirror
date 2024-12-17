(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let resolvers : string option Lwt.u list ref = ref []

let stdin_closed = ref false

let next () =
  if !stdin_closed then Lwt.return_none
  else
    let t, u = Lwt.task () in
    resolvers := u :: !resolvers ;
    t

let rec loop () =
  let* input = Lwt_io.read_line Lwt_io.stdin in
  !resolvers
  |> List.iter (fun resolver -> Lwt.wakeup_later resolver (Some input)) ;
  resolvers := [] ;
  loop ()

let _ =
  (* This is a bit weird since this is executed outside of
     `Lwt_main.run`. It works because this function only uses pure
     `Lwt` code without relying on Unix dependencies. However, any
     call to `next` must be done within `Lwt_main.run`. *)
  Lwt.catch
    (fun () -> loop ())
    (fun _exn ->
      !resolvers |> List.iter (fun resolver -> Lwt.wakeup_later resolver None) ;
      stdin_closed := true ;
      Lwt.return_unit)

let eof =
  let promise, resolver = Lwt.task () in
  Lwt.dont_wait
    (fun () ->
      let rec loop () =
        let* input = next () in
        match input with
        | None ->
            Lwt.wakeup resolver () ;
            Lwt.return_unit
        | Some _ -> loop ()
      in
      loop ())
    (fun _ -> Lwt.wakeup resolver ()) ;
  promise
