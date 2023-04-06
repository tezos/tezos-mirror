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
   Subject:      Test the synchronisation heuristic with a reference implementation
*)

open Qcheck2_helpers

(* Interface implemented by the synchronisation heuristic. *)

module type S = sig
  type status = Chain_validator_worker_state.synchronisation_status =
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
   QCheck2, we check that both implementations have the same
   behavior. *)
module Reference : S = struct
  type status = Chain_validator_worker_state.synchronisation_status =
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
        |> List.sort compare |> List.rev
        |> List.rev_take_n state.threshold
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
      let now = Time.System.to_protocol @@ Time.System.now () in
      if Compare.List_length_with.(candidates < threshold) then Not_synchronised
      else
        match (best_of candidates, least_of candidates) with
        | (best, _), (least, _) ->
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
  let open QCheck2.Gen in
  (* These are generated upfront *)
  let static =
    ["P1"; "P2"; "P3"; "P4"; "P5"; "P6"; "P7"; "P8"; "P9"]
    |> List.map (fun name -> pure (forge_peer_id (), name))
  in
  (* The returned generator either produces one of [P1] to [P9] or a fresh one *)
  delay (fun () -> oneof (pure (forge_peer_id (), "fresh") :: static))

let now = Time.System.to_protocol @@ Time.System.now ()

let forge_timestamp ~delay = Time.Protocol.add now (Int64.of_int delay)

let timestamp_pp n =
  let delay = Time.Protocol.diff n now in
  Format.asprintf "delay: %Ld" delay

let timestamp =
  let open QCheck2 in
  Gen.map
    (fun pre_delay ->
      let delay = (pre_delay * 20) - 300 in
      (* ~ [ -300; 100] with a step of 20 *)
      forge_timestamp ~delay)
    Gen.(oneof [pure 5; 0 -- 20])

let value =
  let open QCheck2.Gen in
  pair timestamp peer_id

let print_value (time_stamp, (_, peer_id_str)) =
  Printf.sprintf "(%s, %s)" (timestamp_pp time_stamp) peer_id_str

let values =
  let open QCheck2.Gen in
  list value

let print_values = QCheck2.Print.list print_value

let pp fmt =
  let open Reference in
  function
  | Synchronised {is_chain_stuck = true} ->
      Format.fprintf fmt "Synchronised (stuck)"
  | Not_synchronised -> Format.fprintf fmt "Not synchronised"
  | Synchronised {is_chain_stuck = false} ->
      Format.fprintf fmt "Synchronised (not stuck)"

let make_tests check_update lcreate rcreate threshold latency =
  let open QCheck2 in
  let threshold_1 =
    Test.make
      ~name:
        (Format.asprintf
           "Shell.synchronisation_heuristic.equivalence-with-reference-implementation \
            (threshold %d) (latency %d)"
           1
           latency)
      ~print:Print.(pair print_value print_value)
      Gen.(pair value value)
      (fun (v1, v2) ->
        let state_left = lcreate ~threshold:1 ~latency in
        let state_right = rcreate ~threshold:1 ~latency in
        check_update state_left state_right v1
        && check_update state_left state_right v2)
  in
  let threshold_n =
    List.map
      (fun threshold ->
        Test.make
          ~name:
            (Format.asprintf
               "Shell.synchronisation_heuristic.equivalence-with-reference-implementation \
                (threshold %d) (latency %d)"
               threshold
               latency)
          ~print:print_values
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
     module. We use QCheck2 to generate a bunch of updates and check
     that both implementations send the same result. *)
  let module L = Synchronisation_heuristic.Core in
  let module R = Reference in
  let check_update state_left state_right (time_stamp, (peer_id, _)) =
    let value = (time_stamp, peer_id) in
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
    ~__FILE__
    "synchronisation heuristic fuzzy"
    [("synchronisation heuristic fuzzy", qcheck_wrap tests)]
