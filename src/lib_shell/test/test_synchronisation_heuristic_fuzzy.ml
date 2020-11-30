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
   Invocation:   dune exec src/lib_shell/test/test_synchronisation_heuristic_fuzzy.exe
   Subject:      Test the synchronisation heuristic with a reference implementation
*)

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
   Crowbar, we check that both implementations have the same
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
    | [] ->
        [(ts, peer)]
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
    | [] ->
        assert false
    | [a] ->
        a
    | _ :: y :: l ->
        best_of (y :: l)

  let get_status {threshold; latency; candidates} =
    if threshold < 0 then Not_synchronised
    else if threshold = 0 then Synchronised {is_chain_stuck = false}
    else
      let now = Time.System.to_protocol @@ Systime_os.now () in
      if List.length candidates < threshold then Not_synchronised
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
  let open Crowbar in
  let p1 = forge_peer_id () in
  let p2 = forge_peer_id () in
  let p3 = forge_peer_id () in
  let p4 = forge_peer_id () in
  let p5 = forge_peer_id () in
  let p6 = forge_peer_id () in
  let p7 = forge_peer_id () in
  let p8 = forge_peer_id () in
  let p9 = forge_peer_id () in
  with_printer (fun fmt pid ->
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
      Format.fprintf fmt "peer: %s" id)
  @@ choose
       [ const p1;
         const p2;
         const p3;
         const p4;
         const p5;
         const p6;
         const p7;
         const p8;
         const p9;
         map [const ()] (fun () -> forge_peer_id ()) ]

let now = Time.System.to_protocol @@ Systime_os.now ()

let forge_timestamp ~delay = Time.Protocol.add now (Int64.of_int delay)

let timestamp =
  let open Crowbar in
  with_printer (fun fmt n ->
      let delay = Time.Protocol.diff n now in
      Format.fprintf fmt "delay: %Ld" delay)
  @@ map
       [choose [const 5; range 20]]
       (fun pre_delay ->
         let delay = (pre_delay * 20) - 300 in
         (* ~ [ -300; 100] with a step of 20 *)
         forge_timestamp ~delay)

let value =
  let open Crowbar in
  map [timestamp; peer_id] (fun timestamp peer_id -> (timestamp, peer_id))

let values =
  let open Crowbar in
  (* This is similar to [list value] but it generates longer lists (in
     quick-check mode) *)
  fix (fun values ->
      choose
        [map [value; values] (fun v vs -> v :: vs); map [value] (fun v -> [v])])

let pp fmt =
  let open Reference in
  function
  | Synchronised {is_chain_stuck = true} ->
      Format.fprintf fmt "Synchronised (stuck)"
  | Not_synchronised ->
      Format.fprintf fmt "Not synchronised"
  | Synchronised {is_chain_stuck = false} ->
      Format.fprintf fmt "Synchronised (not stuck)"

let () =
  (* The module Synchronisation_heuristic should have the same
     semantics as the reference implementation given in the Reference
     module. We use crowbar to generate a bunch of updates and check
     that both implementations send the same result. *)
  let module L = Synchronisation_heuristic in
  let module R = Reference in
  let check_update state_left state_right value =
    L.update state_left value ;
    R.update state_right value ;
    Crowbar.check_eq ~pp (L.get_status state_left) (R.get_status state_right)
  in
  Crowbar.add_test
    ~name:
      (Format.asprintf
         "Shell.synchronisation_heuristic.equivalence-with-reference-implementation \
          (threshold %d) (latency %d)"
         1
         latency)
    [value; value]
    (fun v1 v2 ->
      let state_left = L.create ~threshold:1 ~latency in
      let state_right = R.create ~threshold:1 ~latency in
      check_update state_left state_right v1 ;
      check_update state_left state_right v2) ;
  for threshold = 2 to 7 do
    Crowbar.add_test
      ~name:
        (Format.asprintf
           "Shell.synchronisation_heuristic.equivalence-with-reference-implementation \
            (threshold %d) (latency %d)"
           threshold
           latency)
      [values]
      (fun values ->
        let state_left = L.create ~threshold ~latency in
        let state_right = R.create ~threshold ~latency in
        List.iter (check_update state_left state_right) values)
  done
