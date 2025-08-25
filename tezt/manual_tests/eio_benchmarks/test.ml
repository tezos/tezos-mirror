(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018-2025 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

(* You can run the benchmark using:
   TEZOS_PPX_PROFILER=t dune build tezt/manual_tests/eio_benchmarks && PROFILING="main->debug" PROFILING_BACKENDS=txt _build/default/tezt/manual_tests/eio_benchmarks/test.exe 32
*)

open Tezos_base
open TzPervasives
open Helpers

let () =
  if Array.length Sys.argv = 1 then (
    Format.eprintf
      "Missing number of domains to benchmark. Run the test with <NB_DOMAINS> \
       as argument@." ;
    exit 1)

let () =
  match Tezos_profiler_unix.Profiler_instance.selected_backends () with
  | Some backends ->
      List.iter
        (fun Tezos_profiler_unix.Profiler_instance.{instance_maker; _} ->
          let profiler_maker = instance_maker ~directory:"/tmp" ~name:"main" in
          match profiler_maker with
          | Some instance -> Tezos_profiler.Profiler.(plug main) instance
          | None -> ())
        backends
  | None -> ()

module Profiler =
  (val Tezos_profiler.Profiler.wrap Tezos_profiler.Profiler.main)

let msg_size = 138 + 4

let number = 2000

let data = Stdlib.Hashtbl.create 5

let prepare_data label algo =
  match Stdlib.Hashtbl.find_opt data label with
  | Some v -> v
  | None ->
      let mk_msg i =
        let buf = Bytes.create msg_size in
        Bytes.set_int64_be buf 0 (Int64.of_int i) ;
        buf
      in
      let keys =
        List.map
          (fun i ->
            let _pkh, pk, sk = Signature.generate_key ~algo () in
            let msg = mk_msg i in
            (i, (pk, sk, msg)))
          (1 -- number)
      in
      let keys_and_signatures =
        List.map
          (fun (_i, (pk, sk, msg)) -> (pk, sk, msg, Signature.sign sk msg))
          keys
      in
      Stdlib.Hashtbl.add data label keys_and_signatures ;
      keys_and_signatures

let label_and_algos =
  [
    ("Secp256k1", Signature.Secp256k1);
    ("Ed255519", Signature.Ed25519);
    ("P256", Signature.P256);
    ("BLS", Signature.Bls);
  ]

let () =
  let () = (() [@profiler.record {verbosity = Notice} "prepare_data"]) in
  List.iter
    (fun (label, algo) ->
      Eio.traceln "Preparing data for %s@." label ;
      let _ = prepare_data label algo in
      ())
    label_and_algos ;
  () [@profiler.stop]

let compute worker label algo f =
  let label = Printf.sprintf "%s" label in
  ignore label ;
  let keys_and_signatures = prepare_data label algo in
  f worker keys_and_signatures

let eio_compute worker label algo =
  (fun worker keys_and_signatures ->
    (Eio.Fiber.all
       (Stdlib.List.map
          (fun (pk, _sk, msg, signature) () ->
            let res =
              Eio.Promise.await
                (Worker.Queue.push_request_and_wait_eio
                   (* ~label:"check_signature" *)
                   worker
                   (Request.Check_signature (pk, signature, msg)))
            in
            match res with Ok b -> assert b | Error _ -> assert false)
          keys_and_signatures)
     [@profiler.aggregate_f {verbosity = Notice} (Printf.sprintf "%s" label)]))
  |> compute worker label algo

let eio_lwt_compute worker label algo =
  (fun worker keys_and_signatures ->
    let open Lwt_syntax in
    assert (
      Lwt_eio.run_lwt @@ fun () ->
      (Lwt_list.for_all_p
         (fun (pk, _sk, msg, signature) ->
           let* r =
             Worker.Queue.push_request_and_wait
               worker
               (Request.Check_signature (pk, signature, msg))
           in
           match r with Ok b -> Lwt.return b | Error _ -> Lwt.return_false)
         keys_and_signatures
       [@profiler.aggregate_s {verbosity = Notice} (Printf.sprintf "%s" label)])))
  |> compute worker label algo

let compute_lwt worker label algo =
  (fun worker keys_and_signatures ->
    let open Lwt_syntax in
    assert (
      Lwt_eio.run_lwt @@ fun () ->
      (Lwt_list.for_all_p
         (fun (pk, _sk, msg, signature) ->
           let* r =
             Lwt_worker.Queue.push_request_and_wait
               worker
               (Request.Check_signature (pk, signature, msg))
           in
           match r with Ok b -> Lwt.return b | Error _ -> Lwt.return_false)
         keys_and_signatures
       [@profiler.aggregate_s {verbosity = Notice} (Printf.sprintf "%s" label)])))
  |> compute worker label algo

let run compute worker =
  List.iter (fun (label, algo) -> compute worker label algo) label_and_algos

let () =
  Tezos_base_unix.Event_loop.main_run
    ~process_name:"lwt_worker_lwt_handlers"
    ~eio:true
  @@ fun _env ->
  ((* Lwt worker running with Lwt handlers *)
   let () =
     (() [@profiler.record {verbosity = Notice} "lwt_worker_lwt_handlers"])
   in
   let worker =
     Lwt_eio.run_lwt @@ fun () ->
     Lwt_worker.launch table "lwt_worker" () (module Handlers.MAKE (Lwt_worker))
   in

   Eio.traceln "Lwt Worker running with lwt handlers@." ;
   let worker = match worker with Ok w -> w | Error _ -> assert false in

   run compute_lwt worker ;

   let _ = Lwt_worker.shutdown worker in
   let () = (() [@profiler.stop]) in

   (* Eio worker running with Lwt handlers *)
   let () =
     (() [@profiler.record {verbosity = Notice} "eio_worker_lwt_handlers"])
   in
   let table = Worker.create_table Worker.Queue in

   let worker =
     Lwt_eio.run_lwt @@ fun () ->
     Worker.launch table "eio_lwt_worker" () (module Handlers.MAKE (Worker))
   in
   Eio.traceln "Eio Worker running with lwt handlers@." ;
   let worker = match worker with Ok w -> w | Error _ -> assert false in

   run eio_lwt_compute worker ;
   let () = Lwt_eio.run_lwt @@ fun () -> Worker.shutdown worker in

   let () = (() [@profiler.stop]) in
   ()) ;

  (* Eio Worker running with Eio handler *)
  let rec bench_all domains =
    if domains > int_of_string Sys.argv.(1) then Lwt.return_unit
    else (
      (Eio.traceln
         "Eio Worker running with eio handlers and %d domains@."
         domains ;
       let () =
         (()
         [@profiler.record
           {verbosity = Notice}
             (Format.sprintf "eio_worker_eio_handler_%d_domains" domains)])
       in
       let table = Worker.create_table Worker.Queue in
       let worker =
         Worker.launch_eio
           table
           ~domains
           ~name:"eio_eio_worker"
           ()
           (module Eio_handlers.MAKE (Worker))
       in

       let worker = match worker with Ok w -> w | Error _ -> assert false in
       run eio_compute worker ;
       let _ = Worker.shutdown_eio worker in

       let () = (() [@profiler.stop]) in
       Eio.traceln "Task_worker running %d domains@." domains ;
       ()
       [@profiler.record
         {verbosity = Notice} (Format.sprintf "task_worker_%d_domains" domains)]) ;
      let eio_task_worker _worker label algo =
        ((let res :
              ( (bool, 'a) result,
                'b Tezos_bees.Task_worker.message_error )
              result
              list =
            (fun _worker keys_and_signatures ->
              Tezos_bees.Task_worker.launch_tasks_and_wait
                "task_worker_checks"
                (fun (pk, _sk, msg, signature) ->
                  Ok (Signature.check pk signature msg))
                keys_and_signatures)
            |> compute () label algo
          in

          let res =
            List.map
              (function
                | Ok (Ok b) -> b
                | Ok (Error _) -> assert false
                | Error _ -> assert false)
              res
          in
          assert (List.for_all Fun.id res))
        [@profiler.aggregate_f {verbosity = Notice} label])
      in

      run eio_task_worker () ;
      let () = (() [@profiler.stop]) in

      Format.printf "stop@." ;

      bench_all (domains * 2))
  in

  bench_all 1
