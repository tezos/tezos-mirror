(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Lwt.Syntax

exception Closed

module Bounded = struct
  type 'a t = {
    queue : (int * 'a) Queue.t;
    mutable current_size : int;
    max_size : int;
    compute_size : 'a -> int;
    mutable closed : bool;
    mutable push_waiter : (unit Lwt.t * unit Lwt.u) option;
    mutable pop_waiter : (unit Lwt.t * unit Lwt.u) option;
  }

  let is_closed {closed; _} = closed

  let push_overhead = 4 * (Sys.word_size / 8)

  let create ~max_size ~compute_size () =
    if max_size < 0 then
      raise (Invalid_argument "Lwt_pipe.create: negative size") ;
    {
      queue = Queue.create ();
      current_size = 0;
      max_size;
      compute_size;
      closed = false;
      push_waiter = None;
      pop_waiter = None;
    }

  let notify_push q =
    match q.push_waiter with
    | None -> ()
    | Some (_, w) ->
        q.push_waiter <- None ;
        Lwt.wakeup_later w ()

  let notify_pop q =
    match q.pop_waiter with
    | None -> ()
    | Some (_, w) ->
        q.pop_waiter <- None ;
        Lwt.wakeup_later w ()

  let wait_push q =
    match q.push_waiter with
    | Some (t, _) -> Lwt.protected t
    | None ->
        let waiter, wakener = Lwt.wait () in
        q.push_waiter <- Some (waiter, wakener) ;
        Lwt.protected waiter

  let wait_pop q =
    match q.pop_waiter with
    | Some (t, _) -> Lwt.protected t
    | None ->
        let waiter, wakener = Lwt.wait () in
        q.pop_waiter <- Some (waiter, wakener) ;
        Lwt.protected waiter

  let length {queue; _} = Queue.length queue

  let is_empty {queue; _} = Queue.is_empty queue

  let rec push ({closed; queue; current_size; max_size; compute_size; _} as q)
      elt =
    if closed then Lwt.fail Closed
    else
      let elt_size = compute_size elt in
      if current_size + elt_size < max_size || Queue.is_empty queue then (
        Queue.push (elt_size, elt) queue ;
        q.current_size <- current_size + elt_size ;
        notify_push q ;
        Lwt.return_unit)
      else
        let* () = wait_pop q in
        push q elt

  let push_now ({closed; queue; compute_size; current_size; max_size; _} as q)
      elt =
    if closed then raise Closed ;
    let elt_size = compute_size elt in
    (current_size + elt_size < max_size || Queue.is_empty queue)
    &&
    (Queue.push (elt_size, elt) queue ;
     q.current_size <- current_size + elt_size ;
     notify_push q ;
     true)

  let rec pop ({closed; queue; current_size; _} as q) =
    if not (Queue.is_empty queue) then (
      let elt_size, elt = Queue.pop queue in
      notify_pop q ;
      q.current_size <- current_size - elt_size ;
      Lwt.return elt)
    else if closed then Lwt.fail Closed
    else
      let* () = wait_push q in
      pop q

  let rec pop_with_timeout timeout q =
    if not (Queue.is_empty q.queue) then (
      Lwt.cancel timeout ;
      let* v = pop q in
      Lwt.return_some v)
    else if Lwt.is_sleeping timeout then
      if q.closed then (
        Lwt.cancel timeout ;
        Lwt.fail Closed)
      else
        let waiter = wait_push q in
        let* () = Lwt.pick [timeout; waiter] in
        pop_with_timeout timeout q
    else Lwt.return_none

  let rec peek ({closed; queue; _} as q) =
    if not (Queue.is_empty queue) then
      let _elt_size, elt = Queue.peek queue in
      Lwt.return elt
    else if closed then Lwt.fail Closed
    else
      let* () = wait_push q in
      peek q

  let peek_all_now {queue; closed; _} =
    if not (Queue.is_empty queue) then
      List.rev (Queue.fold (fun acc (_, e) -> e :: acc) [] queue)
    else if closed then raise Closed
    else []

  let pop_now ({closed; queue; current_size; _} as q) =
    (* We only check for closed-ness when the queue is empty to allow reading from
       a closed pipe. This is because closing is just closing the write-end of the
       pipe. *)
    if Queue.is_empty queue && closed then raise Closed ;
    Queue.take_opt queue
    |> Stdlib.Option.map (fun (elt_size, elt) ->
           q.current_size <- current_size - elt_size ;
           notify_pop q ;
           elt)

  let pop_all_queue q =
    let rec aux rev_acc =
      match Queue.pop q with
      | exception Queue.Empty -> List.rev rev_acc
      | elt -> (aux [@ocaml.tailcall]) (elt :: rev_acc)
    in
    aux []

  let pop_all q =
    if not (Queue.is_empty q.queue) then (
      let elements = pop_all_queue q.queue in
      q.current_size <- 0 ;
      notify_pop q ;
      Lwt.return (List.map snd elements))
    else if q.closed then Lwt.fail Closed
    else
      let* () = wait_push q in
      let _, element = Queue.pop q.queue in
      q.current_size <- 0 ;
      notify_pop q ;
      Lwt.return [element]

  let pop_all_now q =
    if not (Queue.is_empty q.queue) then (
      let elements = pop_all_queue q.queue in
      q.current_size <- 0 ;
      notify_pop q ;
      List.map snd elements)
    else if q.closed then raise Closed
    else []

  let close q =
    if not q.closed then (
      q.closed <- true ;
      notify_push q ;
      notify_pop q)
end

module Unbounded = struct
  type 'a t = {
    queue : 'a Queue.t;
    mutable closed : bool;
    mutable push_waiter : (unit Lwt.t * unit Lwt.u) option;
  }

  let is_closed {closed; _} = closed

  let create () = {queue = Queue.create (); closed = false; push_waiter = None}

  let notify_push q =
    match q.push_waiter with
    | None -> ()
    | Some (_, w) ->
        q.push_waiter <- None ;
        Lwt.wakeup_later w ()

  let wait_push q =
    match q.push_waiter with
    | Some (t, _) -> Lwt.protected t
    | None ->
        let waiter, wakener = Lwt.wait () in
        q.push_waiter <- Some (waiter, wakener) ;
        Lwt.protected waiter

  let length {queue; _} = Queue.length queue

  let is_empty {queue; _} = Queue.is_empty queue

  let push ({closed; queue; _} as q) elt =
    if closed then raise Closed
    else (
      Queue.push elt queue ;
      notify_push q)

  let rec pop ({closed; queue; _} as q) =
    if not (Queue.is_empty queue) then Lwt.return @@ Queue.pop queue
    else if closed then Lwt.fail Closed
    else
      let* () = wait_push q in
      pop q

  let rec pop_with_timeout timeout q =
    if not (Queue.is_empty q.queue) then (
      Lwt.cancel timeout ;
      let* v = pop q in
      Lwt.return_some v)
    else if Lwt.is_sleeping timeout then
      if q.closed then (
        Lwt.cancel timeout ;
        Lwt.fail Closed)
      else
        let waiter = wait_push q in
        let* () = Lwt.pick [timeout; waiter] in
        pop_with_timeout timeout q
    else Lwt.return_none

  let rec peek ({closed; queue; _} as q) =
    if not (Queue.is_empty queue) then Lwt.return @@ Queue.peek queue
    else if closed then Lwt.fail Closed
    else
      let* () = wait_push q in
      peek q

  let peek_all_now {queue; closed; _} =
    if not (Queue.is_empty queue) then
      List.rev (Queue.fold (fun acc e -> e :: acc) [] queue)
    else if closed then raise Closed
    else []

  let pop_now {closed; queue; _} =
    (* We only check for closed-ness when the queue is empty to allow reading from
       a closed pipe. This is because closing is just closing the write-end of the
       pipe. *)
    if Queue.is_empty queue && closed then raise Closed ;
    Queue.take_opt queue

  let pop_all_queue q =
    let rec aux rev_acc =
      match Queue.pop q with
      | exception Queue.Empty -> List.rev rev_acc
      | elt -> (aux [@ocaml.tailcall]) (elt :: rev_acc)
    in
    aux []

  let pop_all q =
    if not (Queue.is_empty q.queue) then Lwt.return @@ pop_all_queue q.queue
    else if q.closed then Lwt.fail Closed
    else
      let* () = wait_push q in
      let element = Queue.pop q.queue in
      Lwt.return [element]

  let pop_all_now q =
    if not (Queue.is_empty q.queue) then pop_all_queue q.queue
    else if q.closed then raise Closed
    else []

  let close q =
    if not q.closed then (
      q.closed <- true ;
      notify_push q)
end

module Maybe_bounded = struct
  include Bounded

  let null_compute_size _ = 0

  let create ?bound () =
    match bound with
    | Some (max_size, compute_size) -> create ~max_size ~compute_size ()
    | None -> create ~max_size:max_int ~compute_size:null_compute_size ()

  let bounded t = t.compute_size == null_compute_size
end
