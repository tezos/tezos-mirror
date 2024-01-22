(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type 'a t = {
  threshold : int;
  expected : int;
  candidates : 'a P2p_peer.Id.Table.t;
  compare : 'a -> 'a -> int;
  values_map_module : (module Map.S with type key = 'a);
}

let update t (peer_id, data) =
  (* To guarantee that the memory used by the table is bounded by
     [t.expected]. *)
  if
    P2p_peer.Id.Table.length t.candidates < t.expected
    || P2p_peer.Id.Table.mem t.candidates peer_id
  then P2p_peer.Id.Table.replace t.candidates peer_id data

type 'a state =
  | Consensus of 'a
  | No_consensus of ('a * int) list
  | Need_more_candidates

let create (type a) ?(compare = compare) ~expected ~threshold () =
  if threshold < 0 then invalid_arg "Hash_heuristic.create: threshold negative" ;
  if expected < threshold then
    invalid_arg "Hash_heuristic.create: expected lower than threshold" ;
  if expected >= threshold * 2 then
    invalid_arg
      "Hash_heuristic.create: expected greater than twice the threshold" ;
  (* We store the result of the functor application so that we don't
     do it at every call of `get_state`. *)
  let (values_map_module : (module Map.S with type key = a)) =
    (module Map.Make (struct
      type t = a

      let compare = compare
    end))
  in
  {
    expected;
    threshold;
    candidates = P2p_peer.Id.Table.create 11;
    compare;
    values_map_module;
  }

let get_state (type a) (t : a t) =
  if P2p_peer_id.Table.length t.candidates < t.threshold then
    Need_more_candidates
  else
    (* We retrieve the result of the functor application. *)
    let (module Map : Map.S with type key = a) = t.values_map_module in
    (* We use Error as an early return mechanism. *)
    P2p_peer_id.Table.fold_e
      (fun _ value map ->
        let count = Map.find value map |> Option.fold ~some:succ ~none:1 in
        let map' = Map.add value count map in
        if count = t.threshold then Error value else Ok map')
      t.candidates
      Map.empty
    |> function
    | Ok map ->
        if P2p_peer_id.Table.length t.candidates < t.expected then
          Need_more_candidates
        else No_consensus (Map.bindings map)
    | Error value -> Consensus value

module Worker = struct
  type 'a t = {
    expire_time : Ptime.Span.t;
    (* Expiration delay for a consensus value *)
    state : 'a state;
    (* Internal state of the consensus heuristic *)
    mutable expired : unit Lwt.t;
    (* Expiration promise which is fulfilled when the consensus value
       has expired. The expiration mechanism could be simply
       implemented as a timestamp check too. *)
    job : unit -> 'a state Lwt.t;
    (* Job associated to the consensus heuristic *)
    mutable result : 'a Lwt.t;
    (* Promise which is fulfilled when a consensus is found *)
    restart_delay : Ptime.Span.t;
    (* Restart delay when the consensus heuristic did not reach a consensus *)
    mutable all_consensus_hooks : ('a -> unit) list;
    (* Hooks to be executed on all the consensus values found by the
         heuristic. *)
    mutable next_consensus_hooks : ('a -> unit) list;
        (* Hooks to be executed for the next consensus value found by the
           heuristic. *)
  }

  let create ~expire_time ~job ~restart_delay =
    {
      expire_time;
      state = Need_more_candidates (* only for initialisation *);
      expired = Lwt.return_unit;
      job;
      result = Lwt.fail Lwt.Canceled (* only for initialisation *);
      restart_delay;
      all_consensus_hooks = [];
      next_consensus_hooks = [];
    }

  let rec loop t () =
    let open Lwt_syntax in
    (* If we cancel the worker, we cancel [t.result]. This triggers
       the cancellation of [loop] if t.result was not fulfilled. If
       [t.job] is not a cancelable promise, the cancellation will stop
       here and consequently, if [t.job] returns
       [Need_more_candidates] for example, we will rerun the
       [loop]. To provent this, we wrap [t.job] as a cancelable
       promise. *)
    let* v = Lwt.wrap_in_cancelable (t.job ()) in
    match v with
    | Need_more_candidates | No_consensus _ ->
        let* () = Systime_os.sleep t.restart_delay in
        loop t ()
    | Consensus data ->
        t.expired <- Systime_os.sleep t.expire_time ;
        (* We call [List.rev] to ensure hooks are called in the same
           order they were registered. *)
        let one_shot_hooks = List.rev t.next_consensus_hooks in
        t.next_consensus_hooks <- [] ;
        let forever_hooks = List.rev t.all_consensus_hooks in
        List.iter (fun hook -> hook data) one_shot_hooks ;
        List.iter (fun hook -> hook data) forever_hooks ;
        Lwt.return data

  let wait t =
    (* [t]'s job is ongoing if its [result] promise is pending
       ([Lwt.Sleep]).  [t]'s result is expired if its [expired]
       promise is resolved ([Lwt.Return]). We start/restart the job if
       the current [result] has expired meaning the [result] promise
       is not pending. *)
    if Lwt.state t.result <> Lwt.Sleep && Lwt.state t.expired = Lwt.Return ()
    then t.result <- loop t () ;
    Lwt.protected t.result

  let on_next_consensus t hook =
    match Lwt.state t.result with
    | Lwt.Return data -> hook data
    | _ -> t.next_consensus_hooks <- hook :: t.next_consensus_hooks

  let on_all_consensus t hook =
    t.all_consensus_hooks <- hook :: t.all_consensus_hooks ;
    match Lwt.state t.result with Lwt.Return data -> hook data | _ -> ()

  let cancel t =
    (* We cancel the promises handled by the worker. We also ensure
       that these two promises are resolved with a [Canceled]
       state. Finally, we remove all the hooks to avoid any memory
       leaks. *)
    Lwt.cancel t.expired ;
    t.expired <- Lwt.fail Lwt.Canceled ;
    Lwt.cancel t.result ;
    t.result <- Lwt.fail Lwt.Canceled ;
    t.all_consensus_hooks <- [] ;
    t.next_consensus_hooks <- []
end
