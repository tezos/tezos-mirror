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

(* Testing
   -------
   Component:    Shell
   Invocation:   dune exec src/lib_shell/test/main.exe
   Subject:      Test the consensus heuristic
*)

module Assert = Assert
open Consensus_heuristic

let pp ppf = function
  | Consensus hash -> Format.fprintf ppf "Consensus with %a" Block_hash.pp hash
  | No_consensus _ -> Format.fprintf ppf "No consensus"
  | Need_more_candidates -> Format.fprintf ppf "Synchronised (not stuck)"

let forge_peer_id () =
  let identity = P2p_identity.generate_with_pow_target_0 () in
  identity.peer_id

let forge_hash () =
  Block_hash.hash_bytes [Bytes.init 32 (fun _ -> Char.chr (Random.int 256))]

let create_state_init_1 () =
  let h = create ~expected:1 ~threshold:1 () in
  Assert.equal (get_state h) Need_more_candidates

let create_state_bad_threshold () =
  try
    let _ = create ~expected:1 ~threshold:(-1) () in
    Assert.fail_msg ~expected:"Invalid_argument" ""
  with Invalid_argument _ -> ()

let create_state_bad_expected () =
  try
    let _ = create ~expected:0 ~threshold:1 () in
    Assert.fail_msg ~expected:"Invalid_argument" ""
  with Invalid_argument _ -> ()

let create_consensus () =
  let h = create ~expected:1 ~threshold:1 () in
  let peer_id = forge_peer_id () in
  let zero = Block_hash.zero in
  update h (peer_id, zero) ;
  Assert.equal ~pp (get_state h) (Consensus zero)

let create_no_consensus () =
  let h = create ~expected:3 ~threshold:3 () in
  let peer_1 = forge_peer_id () in
  let peer_2 = forge_peer_id () in
  let peer_3 = forge_peer_id () in
  let hash_1 = Block_hash.zero in
  let hash_2 = forge_hash () in
  update h (peer_1, hash_1) ;
  update h (peer_2, hash_2) ;
  update h (peer_3, hash_2) ;
  Assert.equal ~pp (get_state h) (No_consensus [(hash_1, 1); (hash_2, 2)])

let tests_raw : (string * (unit -> unit)) list =
  [
    ("create_state_init_1", create_state_init_1);
    ("create_state_bad_threshold", create_state_bad_threshold);
    ("create_state_bad_expected", create_state_bad_expected);
    ("create_consensus", create_consensus);
    ("create_no_consensus", create_no_consensus);
  ]

let pp ppf = function
  | Lwt.Fail exn -> Format.fprintf ppf "fail: %s" (Printexc.to_string exn)
  | Lwt.Sleep -> Format.fprintf ppf "sleep"
  | Lwt.Return _ -> Format.fprintf ppf "return"

let job_failed () =
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> Lwt.fail_with "failed")
  in
  let p = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p) (Lwt.state (Lwt.fail_with "failed"))

let job_canceled () =
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> Lwt.fail Lwt.Canceled)
  in
  let p = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let job_return_hash () =
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> Lwt.return (Consensus Block_hash.zero))
  in
  let p = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p) (Lwt.state (Lwt.return Block_hash.zero))

let job_sleep () =
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> fst @@ Lwt.task ())
  in
  let p = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p) Lwt.Sleep ;
  let p = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p) Lwt.Sleep

let job_protected () =
  let t, u = Lwt.task () in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> t)
  in
  let p = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p) Lwt.Sleep ;
  Lwt.cancel p ;
  Lwt.wakeup u (Consensus Block_hash.zero) ;
  let p' = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p') (Lwt.state (Lwt.return Block_hash.zero))

let worker_canceled () =
  let t, _ = Lwt.task () in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> t)
  in
  let p = Worker.wait worker in
  Assert.equal ~pp (Lwt.state p) Lwt.Sleep ;
  Worker.cancel worker ;
  Assert.equal ~pp (Lwt.state p) (Lwt.state (Lwt.fail Lwt.Canceled))

let tests_worker_raw : (string * (unit -> unit)) list =
  [
    ("job failed", job_failed);
    ("job canceled", job_canceled);
    ("job return hash", job_return_hash);
    ("job sleep", job_sleep);
    ("job protected", job_protected);
    ("worker canceled", job_canceled);
  ]

let job_return_no_consensus () =
  let open Lwt_syntax in
  let counter = ref 0 in
  let hash_1 = Block_hash.zero in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () ->
        if !counter = 0 then (
          incr counter ;
          Lwt.return (No_consensus []))
        else (
          Assert.equal !counter 1 ;
          Lwt.return (Consensus hash_1)))
  in
  let* res = Worker.wait worker in
  Assert.equal res hash_1 ;
  Lwt.return_unit

let job_return_need_more_candidates () =
  let open Lwt_syntax in
  let counter = ref 0 in
  let hash_1 = Block_hash.zero in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () ->
        if !counter = 0 then (
          incr counter ;
          Lwt.return Need_more_candidates)
        else (
          Assert.equal !counter 1 ;
          Lwt.return (Consensus hash_1)))
  in
  let* res = Worker.wait worker in
  Assert.equal res hash_1 ;
  Lwt.return_unit

let job_age_limit_twice () =
  let open Lwt_syntax in
  let cpt = ref 0 in
  let condition = Lwt_condition.create () in
  let p = Lwt_condition.wait condition in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () ->
        incr cpt ;
        Lwt_condition.broadcast condition () ;
        Lwt.return (Consensus ()))
  in
  let* () = Worker.wait worker in
  Assert.equal !cpt 1 ;
  let* () = p in
  let* () = Lwt_unix.sleep 1. in
  let* () = Worker.wait worker in
  Assert.equal !cpt 2 ;
  Lwt.return_unit

let job_on_next_consensus_1 () =
  let open Lwt_syntax in
  let cpt = ref 0 in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_next_consensus worker (fun () -> incr cpt) ;
  let* () = Worker.wait worker in
  Assert.equal !cpt 1 ;
  Lwt.return_unit

let job_on_next_consensus_2 () =
  let open Lwt_syntax in
  let cpt = ref 0 in
  let t, u = Lwt.task () in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () ->
        let* () = t in
        Lwt.return (Consensus ()))
  in
  Worker.on_next_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  let* () = p in
  Assert.equal !cpt 1 ;
  Lwt.return_unit

let job_on_all_consensus_1 () =
  let open Lwt_syntax in
  let cpt = ref 0 in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () -> Lwt.return (Consensus ()))
  in
  Worker.on_all_consensus worker (fun () -> incr cpt) ;
  let* () = Worker.wait worker in
  Assert.equal !cpt 1 ;
  Lwt.return_unit

let job_on_all_consensus_2 () =
  let open Lwt_syntax in
  let cpt = ref 0 in
  let t, u = Lwt.task () in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () ->
        let* () = t in
        Lwt.return (Consensus ()))
  in
  Worker.on_all_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  let* () = p in
  Assert.equal !cpt 1 ;
  Lwt.return_unit

let job_on_all_consensus_3 () =
  let open Lwt_syntax in
  let cpt = ref 0 in
  let t, u = Lwt.task () in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () ->
        let* () = t in
        Lwt.return (Consensus ()))
  in
  Worker.on_all_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  let* () = p in
  Assert.equal !cpt 1 ;
  let* () = Lwt_unix.sleep 1. in
  let* () = Worker.wait worker in
  Assert.equal !cpt 2 ;
  Lwt.return_unit

let job_on_next_consensus_3 () =
  let open Lwt_syntax in
  let cpt = ref 0 in
  let t, u = Lwt.task () in
  let worker =
    Worker.create
      ~expire_time:Ptime.Span.zero
      ~restart_delay:Ptime.Span.zero
      ~job:(fun () ->
        let* () = t in
        Lwt.return (Consensus ()))
  in
  Worker.on_next_consensus worker (fun () -> incr cpt) ;
  let p = Worker.wait worker in
  Assert.equal !cpt 0 ;
  Lwt.wakeup_later u () ;
  let* () = p in
  Assert.equal !cpt 1 ;
  let* () = Worker.wait worker in
  Assert.equal !cpt 1 ;
  Lwt.return_unit

let wrap f _switch () = f ()

let tests_worker_lwt_raw : (string * (Lwt_switch.t -> unit -> unit Lwt.t)) list
    =
  [
    ("job return no consensus", wrap job_return_no_consensus);
    ("job return need more canddidates", wrap job_return_need_more_candidates);
    ("job age limit twice", wrap job_age_limit_twice);
    ("job on next consensus_1", wrap job_on_next_consensus_1);
    ("job on next consensus_2", wrap job_on_next_consensus_2);
    ("job on next consensus_3", wrap job_on_next_consensus_3);
    ("job on all consensus_1", wrap job_on_all_consensus_1);
    ("job on all consensus_2", wrap job_on_all_consensus_2);
    ("job on all consensus_3", wrap job_on_all_consensus_3);
  ]

let tests =
  List.map
    (fun (s, f) -> Alcotest_lwt.test_case_sync s `Quick f)
    (tests_raw @ tests_worker_raw)

let tests_lwt =
  List.map
    (fun (s, f) -> Alcotest_lwt.test_case s `Quick f)
    tests_worker_lwt_raw
