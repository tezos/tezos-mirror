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
   Invocation:   dune exec src/lib_shell/runtest
   Subject:      Test the synchronisation heuristic with a reference implementation
*)

open Lib_test.Qcheck_helpers

(* Interface implemented by the synchronisation heuristic. *)

module type S = sig
  type status = Chain_validator_worker_state.Event.synchronisation_status =
    | Synchronised of {is_chain_stuck : bool}
    | Not_synchronised

  type t

  val update : t -> Time.Protocol.t * P2p_peer.Id.t -> unit

  val get_status : t -> status

  val create : threshold:int -> latency:int -> t
end

(* This is a reference implementation for the synchronisation
   heuristic. It should behave exactly as the one provided in the
   [Synchronisation_heuristic] module but it is less efficient. With
   QCheck, we check that both implementations have the same
   behavior. *)
module Reference : S = struct
  type status = Chain_validator_worker_state.Event.synchronisation_status =
    | Synchronised of {is_chain_stuck : bool}
    | Not_synchronised

  type t = {
    threshold : int;
    latency : int;
    mutable candidates : (Time.Protocol.t * P2p_peer.Id.t) list;
  }

  (* [replace_or_add (timestamp, peer) t]
     - inserts [(timestamp, peer)] if there are currently no entries associated
     with [peer] in [t],
     - updates the [timestamp] associated with [peer] in [t] if the one provided
     as argument is more recent than the one in [t],
     - does nothing if the [timestamp] provided as argument is older than the
     one associated to [peer] in [t]. *)
  let rec replace_or_add (ts, peer) = function
    | [] -> [(ts, peer)]
    | (ts', peer') :: l ->
        if P2p_peer.Id.(peer = peer') then
          if Time.Protocol.(ts > ts') then (ts, peer) :: l
          else (ts', peer') :: l
        else (ts', peer') :: replace_or_add (ts, peer) l

  let compare (ts, _) (ts', _) = Time.Protocol.compare ts ts'

  let update state (ts, peer) =
    if state.threshold <= 0 then ()
    else
      let candidates =
        replace_or_add (ts, peer) state.candidates
        |> List.take_n ~compare state.threshold
      in
      state.candidates <- candidates

  let create ~threshold ~latency = {threshold; latency; candidates = []}

  let least_of = function [] -> assert false | a :: _ -> a

  let rec best_of = function
    | [] -> assert false
    | [a] -> a
    | _ :: y :: l -> best_of (y :: l)

  let get_status {threshold; latency; candidates} =
    if threshold < 0 then Not_synchronised
    else if threshold = 0 then Synchronised {is_chain_stuck = false}
    else
      let now = Time.System.to_protocol @@ Systime_os.now () in
      if Compare.List_length_with.(candidates < threshold) then Not_synchronised
      else
        match (best_of candidates, least_of candidates) with
        | ((best, _), (least, _)) ->
            let least_timestamp_drifted =
              Time.Protocol.add least (Int64.of_int latency)
            in
            if Time.Protocol.(least_timestamp_drifted >= now) then
              Synchronised {is_chain_stuck = false}
            else if Time.Protocol.(best = least) && threshold <> 1 then
              Synchronised {is_chain_stuck = true}
            else Not_synchronised
end

(* The test below is completely parametric over the latency. *)
let latency = 100

let forge_peer_id () =
  let identity = P2p_identity.generate_with_pow_target_0 () in
  identity.peer_id

let peer_id =
  let open QCheck in
  let p1 = forge_peer_id () in
  let p2 = forge_peer_id () in
  let p3 = forge_peer_id () in
  let p4 = forge_peer_id () in
  let p5 = forge_peer_id () in
  let p6 = forge_peer_id () in
  let p7 = forge_peer_id () in
  let p8 = forge_peer_id () in
  let p9 = forge_peer_id () in
  let pp_peer_id pid =
    let id =
      if pid == p1 then "P1"
      else if pid == p2 then "P2"
      else if pid == p3 then "P3"
      else if pid == p4 then "P4"
      else if pid == p5 then "P5"
      else if pid == p6 then "P6"
      else if pid == p7 then "P7"
      else if pid == p8 then "P8"
      else if pid == p9 then "P9"
      else "fresh"
    in
    Format.asprintf "peer: %s" id
  in
  (map (fun () -> forge_peer_id ()) unit |> set_print pp_peer_id)
  ::
  List.map
    (fun p -> make ~print:pp_peer_id (Gen.return p))
    [p1; p2; p3; p4; p5; p6; p7; p8; p9]
  |> choose

let now = Time.System.to_protocol @@ Systime_os.now ()

let forge_timestamp ~delay = Time.Protocol.add now (Int64.of_int delay)

let timestamp =
  let open QCheck in
  let timestamp_pp n =
    let delay = Time.Protocol.diff n now in
    Format.asprintf "delay: %Ld" delay
  in
  map
    (fun pre_delay ->
      let delay = (pre_delay * 20) - 300 in
      (* ~ [ -300; 100] with a step of 20 *)
      forge_timestamp ~delay)
    (make (Gen.oneof [Gen.return 5; Gen.int_range 0 20]))
  |> set_print timestamp_pp

let value =
  let open QCheck in
  pair timestamp peer_id

let values =
  let open QCheck in
  list value

let pp fmt =
  let open Reference in
  function
  | Synchronised {is_chain_stuck = true} ->
      Format.fprintf fmt "Synchronised (stuck)"
  | Not_synchronised -> Format.fprintf fmt "Not synchronised"
  | Synchronised {is_chain_stuck = false} ->
      Format.fprintf fmt "Synchronised (not stuck)"

let make_tests check_update lcreate rcreate threshold latency =
  let threshold_1 =
    QCheck.Test.make
      ~name:
        (Format.asprintf
           "Shell.synchronisation_heuristic.equivalence-with-reference-implementation \
            (threshold %d) (latency %d)"
           1
           latency)
      QCheck.(pair value value)
      (fun (v1, v2) ->
        let state_left = lcreate ~threshold:1 ~latency in
        let state_right = rcreate ~threshold:1 ~latency in
        check_update state_left state_right v1
        && check_update state_left state_right v2)
  in
  let threshold_n =
    List.map
      (fun threshold ->
        QCheck.Test.make
          ~name:
            (Format.asprintf
               "Shell.synchronisation_heuristic.equivalence-with-reference-implementation \
                (threshold %d) (latency %d)"
               threshold
               latency)
          values
          (fun values ->
            let state_left = lcreate ~threshold ~latency in
            let state_right = rcreate ~threshold ~latency in
            List.for_all
              (fun value -> check_update state_left state_right value)
              values))
      (2 -- threshold)
  in
  threshold_1 :: threshold_n

let tests =
  (* The module Synchronisation_heuristic should have the same
     semantics as the reference implementation given in the Reference
     module. We use QCheck to generate a bunch of updates and check
     that both implementations send the same result. *)
  let module L = Synchronisation_heuristic.Core in
  let module R = Reference in
  let check_update state_left state_right value =
    L.update state_left value ;
    R.update state_right value ;
    qcheck_eq'
      ~pp
      ~expected:(R.get_status state_right)
      ~actual:(L.get_status state_left)
      ()
  in
  make_tests check_update L.create R.create 8 latency

let () =
  Alcotest.run
    "synchronisation heuristic fuzzy"
    [("synchronisation heuristic fuzzy", qcheck_wrap tests)]
