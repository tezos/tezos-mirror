(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

external cast_chan : 'a Lwt_io.channel -> unit Lwt_io.channel = "%identity"
(* Transform a channel into a channel that only support closing. *)

let pool = lazy (Lwt_domain.setup_pool 1)

class virtual common pid channels =
  let open Lwt_syntax in
  let waitproc = Lwt_unix.wait4 [] pid in
  object (self)
    val mutable closed = false

    method close =
      if closed then self#status
      else (
        closed <- true ;
        let* () = Lwt_list.iter_p Lwt_io.close channels in
        self#status)

    method kill signum =
      if Lwt.state waitproc = Lwt.Sleep then Unix.kill pid signum

    method pid = pid

    method rusage =
      let+ _, _, rusage = Lwt.protected waitproc in
      rusage

    method state =
      match Lwt.poll waitproc with
      | None -> Lwt_process.Running
      | Some (_pid, status, _rusage) -> Exited status

    method status =
      let+ _, status, _ = Lwt.protected waitproc in
      status

    method terminate = self#kill Sys.sigkill

    initializer
      (* Ensure the channels are closed *)
      List.iter (Gc.finalise (fun c -> ignore (Lwt_io.close c))) channels
  end

let open_process_full (p_name, args) : Lwt_process.process_full Lwt.t =
  let open Lwt_syntax in
  let+ ((pin, pout, perr) as proc) =
    Lwt_domain.detach
      (Lazy.force pool)
      (fun () -> Unix.open_process_args_full p_name args (Unix.environment ()))
      ()
  in
  let pid = Unix.process_full_pid proc in
  let pin = Unix.descr_of_in_channel pin |> Lwt_io.of_unix_fd ~mode:Input in
  let pout = Unix.descr_of_out_channel pout |> Lwt_io.of_unix_fd ~mode:Output in
  let perr = Unix.descr_of_in_channel perr |> Lwt_io.of_unix_fd ~mode:Input in
  object
    inherit common pid [cast_chan pin; cast_chan pout; cast_chan perr]

    method stdout = pin

    method stdin = pout

    method stderr = perr
  end

let open_process (p_name, args) : Lwt_process.process Lwt.t =
  let open Lwt_syntax in
  let+ ((pin, pout) as proc) =
    Lwt_domain.detach
      (Lazy.force pool)
      (fun () -> Unix.open_process_args p_name args)
      ()
  in
  let pid = Unix.process_pid proc in
  let pin = Unix.descr_of_in_channel pin |> Lwt_io.of_unix_fd ~mode:Input in
  let pout = Unix.descr_of_out_channel pout |> Lwt_io.of_unix_fd ~mode:Output in
  object
    inherit common pid [cast_chan pin; cast_chan pout]

    method stdout = pin

    method stdin = pout
  end

let open_process_in (p_name, args) : Lwt_process.process_in Lwt.t =
  let open Lwt_syntax in
  let+ proc =
    Lwt_domain.detach
      (Lazy.force pool)
      (fun () -> Unix.open_process_args_in p_name args)
      ()
  in
  let pid = Unix.process_in_pid proc in
  let pin = Unix.descr_of_in_channel proc |> Lwt_io.of_unix_fd ~mode:Input in
  object
    inherit common pid [cast_chan pin]

    method stdout = pin
  end

let open_process_out (p_name, args) : Lwt_process.process_out Lwt.t =
  let open Lwt_syntax in
  let+ proc =
    Lwt_domain.detach
      (Lazy.force pool)
      (fun () -> Unix.open_process_args_out p_name args)
      ()
  in
  let pid = Unix.process_out_pid proc in
  let pin = Unix.descr_of_out_channel proc |> Lwt_io.of_unix_fd ~mode:Output in
  object
    inherit common pid [cast_chan pin]

    method stdin = pin
  end

let open_process_none (p_name, args) : Lwt_process.process_none Lwt.t =
  let open Lwt_syntax in
  let+ pid =
    Lwt_domain.detach
      (Lazy.force pool)
      (fun () ->
        Unix.create_process_env
          p_name
          args
          (Unix.environment ())
          Unix.stdin
          Unix.stdout
          Unix.stderr)
      ()
  in
  object
    inherit common pid []
  end

let with_process_gen backend cmd f =
  let open Lwt_syntax in
  let* proc = backend cmd in
  Lwt.finalize
    (fun () -> f proc)
    (fun () ->
      let* _ = proc#close in
      return_unit)

let with_process cmd f = with_process_gen open_process cmd f

let with_process_in cmd f = with_process_gen open_process_in cmd f

let with_process_out cmd f = with_process_gen open_process_out cmd f

let with_process_full cmd f = with_process_gen open_process_full cmd f

let with_process_none cmd f = with_process_gen open_process_none cmd f
