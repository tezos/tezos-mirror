(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Base

module Scheduler : Test.SCHEDULER = struct
  type request = Run_test of {test_title : string}

  type response = Test_result of Log.test_result

  type status = Idle | Working of (response -> unit) | Dead

  type worker = {
    pid : int;
    mutable status : status;
    pipe_to_worker : out_channel;
    pipe_from_worker : in_channel;
  }

  let send_request channel request =
    Marshal.to_channel channel (request : request) [] ;
    flush channel

  let read_request channel =
    try Some (Marshal.from_channel channel : request) with End_of_file -> None

  let send_response channel response =
    Marshal.to_channel channel (response : response) [] ;
    flush channel

  let read_response channel =
    try Some (Marshal.from_channel channel : response)
    with End_of_file -> None

  let internal_worker_error x =
    Printf.ksprintf
      (fun s ->
        Log.error "internal error in worker: %s" s ;
        exit 1)
      x

  let internal_scheduler_error x =
    Printf.ksprintf
      (fun s ->
        Log.error "internal error in scheduler: %s" s ;
        exit 1)
      x

  let perform_request (Run_test {test_title}) =
    match Test.get_test_by_title test_title with
    | None ->
        internal_worker_error
          "scheduler requested to run test %S, but worker doesn't know about \
           this test"
          test_title
    | Some test ->
        let clean_up () =
          Lwt.catch Process.clean_up @@ fun exn ->
          Log.warn "Failed to clean up processes: %s" (Printexc.to_string exn) ;
          unit
        in
        let test_result =
          Lwt_main.run @@ Test.run_one ~sleep:Lwt_unix.sleep ~clean_up test
        in
        Test_result test_result

  let rec worker_listen_loop pipe_from_scheduler pipe_to_scheduler =
    let request = read_request pipe_from_scheduler in
    match request with
    | None ->
        (* End of file: no more request will come. *)
        exit 0
    | Some request ->
        let response = perform_request request in
        send_response pipe_to_scheduler response ;
        worker_listen_loop pipe_from_scheduler pipe_to_scheduler

  let worker_listen pipe_from_scheduler pipe_to_scheduler =
    try worker_listen_loop pipe_from_scheduler pipe_to_scheduler
    with exn ->
      (* Note: if a test fails, its exception is caught and handled by [really_run].
         So here we have an error of Tezt itself. *)
      internal_worker_error "%s" (Printexc.to_string exn)

  let next_worker_id = ref 0

  let current_worker_id = ref None

  let spawn_worker () =
    let worker_id = !next_worker_id in
    incr next_worker_id ;
    let pipe_to_worker_exit, pipe_to_worker_entrance = Unix.pipe () in
    let pipe_from_worker_exit, pipe_from_worker_entrance = Unix.pipe () in
    let pid = Lwt_unix.fork () in
    if pid = 0 then (
      (* This is now a worker process. *)
      current_worker_id := Some worker_id ;
      Temp.set_pid (Unix.getpid ()) ;
      Unix.close pipe_to_worker_entrance ;
      Unix.close pipe_from_worker_exit ;
      worker_listen
        (Unix.in_channel_of_descr pipe_to_worker_exit)
        (Unix.out_channel_of_descr pipe_from_worker_entrance))
    else (
      (* This is the scheduler process. *)
      Unix.close pipe_to_worker_exit ;
      Unix.close pipe_from_worker_entrance ;
      {
        pid;
        status = Idle;
        pipe_to_worker = Unix.out_channel_of_descr pipe_to_worker_entrance;
        pipe_from_worker = Unix.in_channel_of_descr pipe_from_worker_exit;
      })

  let kill_worker worker =
    match worker.status with
    | Dead -> ()
    | Idle | Working _ ->
        worker.status <- Dead ;
        close_out worker.pipe_to_worker ;
        close_in worker.pipe_from_worker ;
        Unix.kill worker.pid Sys.sigterm ;
        let (_ : int * Unix.process_status) = Unix.waitpid [] worker.pid in
        ()

  let rec run_single_process ~on_worker_available =
    Temp.set_pid (Unix.getpid ()) ;
    match on_worker_available () with
    | None -> ()
    | Some (request, on_response) ->
        let response = perform_request request in
        on_response response ;
        run_single_process ~on_worker_available

  let run_multi_process ~on_worker_available ~worker_count =
    (* Start workers. *)
    let workers = List.init worker_count (fun _ -> spawn_worker ()) in
    (* Handle Ctrl+C in the scheduler process.
       Note: Ctrl+C is also received by workers automatically. *)
    let received_sigint = ref false in
    Sys.(set_signal sigint)
      (Signal_handle
         (fun _ ->
           received_sigint := true ;
           (* If the user presses Ctrl+C again, let the program die immediately. *)
           Sys.(set_signal sigint) Signal_default)) ;
    (* Give work to workers until there is no work to give. *)
    let trigger_worker_available worker =
      if !received_sigint then kill_worker worker
      else
        match on_worker_available () with
        | None -> kill_worker worker
        | Some (request, on_response) ->
            worker.status <- Working on_response ;
            send_request worker.pipe_to_worker request
    in
    let rec loop () =
      (* Calling [trigger_worker_available] not only gives work to idle workers,
         it also kills them if we don't need them any more.
         It also ensures that [file_descriptors_to_read] will only be empty if there
         are no working workers. *)
      List.iter
        (fun worker ->
          match worker.status with
          | Dead | Working _ -> ()
          | Idle -> trigger_worker_available worker)
        workers ;
      let file_descriptors_to_read =
        List.filter_map
          (fun worker ->
            match worker.status with
            | Idle | Dead -> None
            | Working _ ->
                Some (Unix.descr_of_in_channel worker.pipe_from_worker))
          workers
      in
      match file_descriptors_to_read with
      | [] ->
          (* We maintain the invariant that if there is work to do, at least one
             worker is [Working] at this particular point.
             This is enforced by the [List.iter] of [trigger_worker_available] above.
             So if there is no working worker, we can stop the loop. *)
          ()
      | _ :: _ ->
          let ready, _, _ =
            (* In case of SIGINT, this returns EINTR. *)
            try Unix.select file_descriptors_to_read [] [] (-1.)
            with Unix.Unix_error (EINTR, _, _) -> ([], [], [])
          in
          let read_response file_descriptor =
            match
              List.find_opt
                (fun worker ->
                  match worker.status with
                  | Idle | Dead -> false
                  | Working _ ->
                      Unix.descr_of_in_channel worker.pipe_from_worker
                      = file_descriptor)
                workers
            with
            | None ->
                internal_scheduler_error
                  "received a response from an unknown worker"
            | Some worker -> (
                match worker.status with
                | Idle | Dead ->
                    (* Please do not consider this error message to be political. *)
                    internal_scheduler_error
                      "worker is idle or dead while it should be working"
                | Working on_response -> (
                    (* Note: [read_response] is blocking.
                       We assume that if a worker starts writing something,
                       it will finish writing almost immediately. *)
                    let response = read_response worker.pipe_from_worker in
                    match response with
                    | None -> internal_scheduler_error "no response from worker"
                    | Some response ->
                        on_response response ;
                        worker.status <- Idle))
          in
          List.iter read_response ready ;
          loop ()
    in
    loop ()

  let run ~on_worker_available ~worker_count continue =
    (if worker_count = 1 then run_single_process ~on_worker_available
    else
      try run_multi_process ~on_worker_available ~worker_count
      with exn -> internal_scheduler_error "%s" (Printexc.to_string exn)) ;
    continue ()

  let get_current_worker_id () = !current_worker_id
end

let run () = Test.run_with_scheduler (module Scheduler)
