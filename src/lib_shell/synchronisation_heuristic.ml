(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type status = Chain_validator_worker_state.synchronisation_status =
  | Synchronised of {is_chain_stuck : bool}
  | Not_synchronised

type candidate = Time.Protocol.t * P2p_peer.Id.t

(* An order is used on candidates. This order is given by the
   predicate (and implemented in the [earlier] function) :

   forall v, None < Some v \/ forall t t' p p', Time.Protocol.(t < t')
   -> Some (t,p) < Some (t',p') = true. The reflexive closure
   (according to the timestamp) of this order is implemented in
   [earlier_or_coincident].

   Variants provide specifalised option/non-option versions *)

let earlier_o l r =
  match (l, r) with
  | None, None -> false
  | None, Some _ -> true
  | Some (i, _), Some (j, _) -> Time.Protocol.(i < j)
  | Some _, None -> false

let earlier_ro (i, _) r =
  match r with Some (j, _) -> Time.Protocol.(i < j) | None -> false

let earlier l (j, _) =
  match l with None -> true | Some (i, _) -> Time.Protocol.(i < j)

let coincident_o l r =
  match (l, r) with
  | None, None -> true
  | Some (i, _), Some (j, _) -> Time.Protocol.(i = j)
  | _ -> false

let earlier_or_coincident_o l r = earlier_o l r || coincident_o l r

module Core = struct
  type t = {
    (* Number of peers which are taken into account to be considered as
       synchronized *)
    threshold : int;
    (* The least recent block validated from the [threshold] peers
       should be dated no more than [latency] seconds. *)
    latency : int;
    (* Store the [threshold] best validated block timestamps with their peer. *)
    candidates : candidate option Array.t;
    (* Index of one of the candidates in [candidates] with the oldest timestamp. *)
    mutable index_of_oldest_candidate : int;
    (* Index of one of the candidates in [candidates] with the most recent
       timestamp. *)
    mutable index_of_youngest_candidate : int;
    (* Current status of the heuristic. *)
    mutable current_status : status;
  }

  (* Invariants:

     - forall state, state.threshold >= 0 -> Array.length
     state.candidates = state.threshold

     - forall state, state.threshold > 0 -> state.least_index is a valid
     index of state.candidates

     - forall state, state.threshold > 0 -> state.best_index is a valid
     index of state.candidates

     - forall state i, state.threshold > 0 -> 0 <= i < state.threshold
     -> state.candidates.(state.index_of_oldest_candidate) <= state.candidates.(i)

     - forall state i, state.threshold > 0 -> 0 <= i < state.threshold
     -> state.candidates.(state.index_of_youngest_candidate) >= state.candidates.(i)

     - forall state i j, 0 <= i,j < state.threshold ->
     snd(state.candidates(i)) = snd (state.candidates(j)) -> i = j

     This is denoted as "valid(state)". *)

  (* Update only if the timestamp is greater at the given index (see
     [earlier]). *)
  let may_update_binding state index candidate =
    if earlier state.candidates.(index) candidate then
      state.candidates.(index) <- Some candidate

  (* Return [true] if the candidate's peer is [peer_id] *)
  let same_peer (_, peer_id) = function
    | None -> false
    | Some (_, peer) -> P2p_peer.Id.equal peer peer_id

  (* Invariant:

     - forall threshold, latency, valid(create ~threshold ~latency). *)
  let create ~threshold ~latency : t =
    let current_status =
      if threshold = 0 then Synchronised {is_chain_stuck = false}
      else Not_synchronised
    in
    {
      latency;
      threshold;
      candidates = (if threshold <= 0 then [||] else Array.make threshold None);
      index_of_youngest_candidate = 0;
      index_of_oldest_candidate = 0;
      current_status;
    }

  let compute_status state =
    if state.threshold < 0 then Not_synchronised
    else if state.threshold = 0 then Synchronised {is_chain_stuck = false}
    else
      let now = Time.System.to_protocol @@ Time.System.now () in
      match
        ( state.candidates.(state.index_of_youngest_candidate),
          state.candidates.(state.index_of_oldest_candidate) )
      with
      | None, _ | _, None ->
          (* The threshold is not reached *)
          Not_synchronised
      | Some (best, _), Some (least, _) ->
          let least_timestamp_drifted =
            Time.Protocol.add least (Int64.of_int state.latency)
          in
          if Time.Protocol.(least_timestamp_drifted >= now) then
            Synchronised {is_chain_stuck = false}
          else if Time.Protocol.(best = least) && state.threshold <> 1 then
            (* The reason why the heuristic does not allow to be stuck
               when threshold is one is related to the behavior of the
               node. A node should not be bootstrapped while
               bootstrapping. When the threshold is one, if the node
               validates a block in the past, then it will be declared
               as [Synchronised {is_stuck=true}]. Once the threshold is
               2, this cannot happen for new validated blocks since a
               new validated block comes only from one peer. *)
            Synchronised {is_chain_stuck = true}
          else Not_synchronised

  (* Invariant:

     - forall state candidate, valid(state) ->
       valid(update state candidate; state). *)
  let update state candidate =
    if state.threshold <= 0 then ()
    else if state.threshold = 1 then may_update_binding state 0 candidate
    else if
      earlier_ro candidate state.candidates.(state.index_of_oldest_candidate)
    then ()
    else
      (* If we find a candidate for the same peer as candidate's, we'll
         set this, but otherwise we should update the oldest candidate *)
      let index_to_update = ref state.index_of_oldest_candidate in
      (* We search for the second-worst entry by starting with the best
         and just recording whatever is worse than currently known
         except for the known-worst *)
      let index_of_second_oldest_candidate =
        ref state.index_of_youngest_candidate
      in
      Array.iteri
        (fun i known_candidate ->
          (* check that we have found the same peer as the candidate *)
          if same_peer candidate known_candidate then index_to_update := i ;
          (* check if we have found the (an) index for the second oldest candidate *)
          if
            (* we are looking for the second-oldest, not the oldest
               (remember threshold >= 2 so they are distinct) *)
            i <> state.index_of_oldest_candidate
            &&
            (* has to be at least as old as the previously known second oldest *)
            earlier_or_coincident_o
              known_candidate
              state.candidates.(!index_of_second_oldest_candidate)
          then index_of_second_oldest_candidate := i)
        state.candidates ;
      (* Properties at this time:

         - forall v, index_of_second_oldest_candidate <> state.index_of_oldest_candidate &&
           (either v = least or v >= state.candidates.(index_of_second_oldest_candidate))
      *)
      (* patch the candidate that needs patching *)
      may_update_binding state !index_to_update candidate ;
      (* patch the pointer to the oldest candidate in case it was rewritten *)
      if
        !index_to_update = state.index_of_oldest_candidate
        && earlier
             state.candidates.(!index_of_second_oldest_candidate)
             candidate
      then state.index_of_oldest_candidate <- !index_of_second_oldest_candidate ;
      (* patch the pointer to the youngest candidate in case we wrote something
         younger *)
      if earlier state.candidates.(state.index_of_youngest_candidate) candidate
      then state.index_of_youngest_candidate <- !index_to_update

  (* We shadow update to ensure the current_status is updated. *)
  let update state candidate =
    update state candidate ;
    state.current_status <- compute_status state

  let get_status state = state.current_status
end

module Bootstrapping = struct
  type t = {
    heuristic : Core.t;
    mutable bootstrapped : bool;
    when_status_changes : status -> unit Lwt.t;
    when_bootstrapped_changes : bool -> unit Lwt.t;
    on_bootstrapped : unit Lwt_condition.t;
  }

  (* [initalisation] is a particular case when the heuristic is
     created to ensure that we call the [when_bootstrapped_changes]
     callback. *)
  let set_bootstrapped ?(initialisation = false) state bootstrapped =
    let old_value = state.bootstrapped in
    state.bootstrapped <- bootstrapped ;
    if old_value = false && bootstrapped then
      Lwt_condition.signal state.on_bootstrapped () ;
    if old_value <> bootstrapped || initialisation then
      state.when_bootstrapped_changes bootstrapped
    else Lwt.return_unit

  let create ?(when_bootstrapped_changes = fun _ -> Lwt.return_unit)
      ?(when_status_changes = fun _ -> Lwt.return_unit) ~threshold ~latency () :
      t =
    let heuristic = Core.create ~threshold ~latency in
    {
      heuristic;
      when_status_changes;
      when_bootstrapped_changes;
      on_bootstrapped = Lwt_condition.create ();
      bootstrapped = false;
    }

  let activate state =
    let open Lwt_syntax in
    let is_synchronised =
      match state.heuristic.current_status with
      | Synchronised _ -> true
      | _ -> false
    in
    let* () = set_bootstrapped ~initialisation:true state is_synchronised in
    state.when_status_changes (Core.get_status state.heuristic)

  let update state candidate =
    let open Lwt_syntax in
    let old_status = Core.get_status state.heuristic in
    Core.update state.heuristic candidate ;
    let new_status = Core.get_status state.heuristic in
    let* () =
      if old_status <> new_status then state.when_status_changes new_status
      else Lwt.return_unit
    in
    match new_status with
    | Synchronised _ when state.bootstrapped = false ->
        set_bootstrapped state true
    | _ -> Lwt.return_unit

  let get_status state = Core.get_status state.heuristic

  let is_bootstrapped state = state.bootstrapped

  let force_bootstrapped state b = set_bootstrapped state b

  let bootstrapped state =
    if state.bootstrapped then Lwt.return_unit
    else Lwt_condition.wait state.on_bootstrapped
end
