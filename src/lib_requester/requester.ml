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

module Profiler = (val Profiler.wrap Shell_profiling.requester_profiler)

let profiler_init = ref false

module type REQUESTER = sig
  type t

  type key

  type value

  type param

  val known : t -> key -> bool Lwt.t

  type error += Missing_data of key

  type error += Canceled of key

  type error += Timeout of key

  val read : t -> key -> value tzresult Lwt.t

  val read_opt : t -> key -> value option Lwt.t

  val inject : t -> key -> value -> bool Lwt.t

  val fetch :
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key ->
    param ->
    value tzresult Lwt.t

  val clear_or_cancel : t -> key -> unit
end

module type FULL_REQUESTER = sig
  include REQUESTER

  type store

  type request_param

  type notified_value

  val pending : t -> key -> bool

  val watch : t -> (key * value) Lwt_stream.t * Lwt_watcher.stopper

  val notify : t -> P2p_peer.Id.t -> key -> notified_value -> unit Lwt.t

  val memory_table_length : t -> int

  val pending_requests : t -> int

  val create :
    ?random_table:bool ->
    ?global_input:(key * value) Lwt_watcher.input ->
    request_param ->
    store ->
    t

  val shutdown : t -> unit Lwt.t
end

module type DISK_TABLE = sig
  type store

  type key

  type value

  val known : store -> key -> bool Lwt.t

  val read : store -> key -> value tzresult Lwt.t

  val read_opt : store -> key -> value option Lwt.t
end

module type MEMORY_TABLE = sig
  type 'a t

  type key

  val create : entry_type:string -> ?random:bool -> int -> 'a t

  val find : 'a t -> key -> 'a option

  val add : 'a t -> key -> 'a -> unit

  val replace : 'a t -> key -> 'a -> unit

  val remove : 'a t -> key -> unit

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val length : 'a t -> int
end

module type SCHEDULER = sig
  type t

  type key

  type param

  val request : t -> P2p_peer.Id.t option -> key -> unit

  val notify : t -> P2p_peer.Id.t -> key -> unit Lwt.t

  val notify_cancellation : t -> key -> unit

  val notify_unrequested : t -> P2p_peer.Id.t -> key -> unit Lwt.t

  val notify_duplicate : t -> P2p_peer.Id.t -> key -> unit Lwt.t

  val notify_invalid : t -> P2p_peer.Id.t -> key -> unit Lwt.t

  val pending_requests : t -> int

  val create : param -> t

  val shutdown : t -> unit Lwt.t
end

module type PROBE = sig
  type key

  type param

  type notified_value

  type value

  val probe : key -> param -> notified_value -> value option
end

module type REQUEST = sig
  type key

  type param

  val initial_delay : Time.System.Span.t

  val active : param -> P2p_peer.Set.t

  val send : param -> P2p_peer.Id.t -> key list -> unit
end

module type HASH = sig
  type t

  val name : string

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

(** The requester uses a generic scheduler to schedule its requests.
    The [Memory_table] must be shared between the scheduler and the requester
    as it is used to store both pending requests and found values. *)

module Make_request_scheduler
    (Hash : HASH)
    (Table : MEMORY_TABLE with type key := Hash.t)
    (Request : REQUEST with type key := Hash.t) : sig
  include SCHEDULER with type key := Hash.t and type param := Request.param
end = struct
  module Request = struct
    include Request

    let send p peer keys =
      ()
      [@profiler.overwrite
        List.iter
          (fun _ -> Profiler.mark Profiler.Info (["p2p requests sent"], []))
          keys] ;
      send p peer keys
  end

  module Events = Requester_event.Make (Hash)

  type key = Hash.t

  type t = {
    param : Request.param;
    pending : status Table.t;
    mutable min_next_request : Time.System.t option;
    (* The time of the next pending request to timeout. *)
    queue : event Lwt_pipe.Unbounded.t;
    mutable events : event list Lwt.t;
    canceler : Lwt_canceler.t;
    mutable worker : unit Lwt.t;
  }

  and status = {
    unrequested_peers : P2p_peer.Set.t;
    requested_peers : P2p_peer.Set.t;
    next_request : Time.System.t;
    delay : Time.System.Span.t;
  }

  and event =
    | Request of P2p_peer.Id.t option * key
    | Notify of P2p_peer.Id.t * key
    | Notify_cancellation of key
    | Notify_invalid of P2p_peer.Id.t * key
    | Notify_duplicate of P2p_peer.Id.t * key
    | Notify_unrequested of P2p_peer.Id.t * key

  let request t p k = Lwt_pipe.Unbounded.push t.queue (Request (p, k))

  let notify t p k =
    let open Lwt_syntax in
    let* () = Events.(emit notify_push) (k, p) in
    Lwt_pipe.Unbounded.push t.queue (Notify (p, k)) ;
    Lwt.return_unit

  (* [notify_cancellation] is used within non-Lwt context and needs to
     perform logging without yielding. We use
     [emit__dont_wait__use_with_care] to that end. Other events are used
     within Lwt context so we use the recommended [emit] for them. *)
  let notify_cancellation t k =
    Events.(emit__dont_wait__use_with_care notify_push_cancellation) k ;
    Lwt_pipe.Unbounded.push t.queue (Notify_cancellation k)

  let notify_invalid t p k =
    let open Lwt_syntax in
    let* () = Events.(emit notify_push_invalid) (k, p) in
    Lwt_pipe.Unbounded.push t.queue (Notify_invalid (p, k)) ;
    Lwt.return_unit

  let notify_duplicate t p k =
    let open Lwt_syntax in
    let* () = Events.(emit notify_push_duplicate) (k, p) in
    Lwt_pipe.Unbounded.push t.queue (Notify_duplicate (p, k)) ;
    Lwt.return_unit

  let notify_unrequested t p k =
    let open Lwt_syntax in
    let* () = Events.(emit notify_push_unrequested) (k, p) in
    Lwt_pipe.Unbounded.push t.queue (Notify_unrequested (p, k)) ;
    Lwt.return_unit

  let compute_timeout state =
    match state.min_next_request with
    | None -> fst @@ Lwt.task ()
    | Some next ->
        let now = Time.System.now () in
        let delay = Ptime.diff next now in
        if Ptime.Span.compare delay Ptime.Span.zero <= 0 then Lwt.return_unit
        else Systime_os.sleep delay

  let process_event state now =
    let open Lwt_syntax in
    function
    | Request (peer, key) -> (
        let* () = Events.(emit registering_request) (key, peer) in
        match Table.find state.pending key with
        | Some data ->
            (match peer with
            | None -> ()
            | Some peer ->
                let unrequested_peers =
                  P2p_peer.Set.add peer data.unrequested_peers
                in
                let requested_peers =
                  P2p_peer.Set.remove peer data.requested_peers
                in
                Table.replace
                  state.pending
                  key
                  {data with unrequested_peers; requested_peers}) ;
            Events.(emit registering_request_replaced) (key, peer)
        | None ->
            let unrequested_peers =
              match peer with
              | None -> P2p_peer.Set.empty
              | Some peer -> P2p_peer.Set.singleton peer
            in
            Table.replace
              state.pending
              key
              {
                unrequested_peers;
                requested_peers = P2p_peer.Set.empty;
                next_request = now;
                delay = Request.initial_delay;
              } ;
            Events.(emit registering_request_added) (key, peer))
    | Notify (peer, key) ->
        Table.remove state.pending key ;
        Events.(emit notify_received) (key, peer)
    | Notify_cancellation key ->
        Table.remove state.pending key ;
        Events.(emit notify_cancelled) key
    | Notify_invalid (peer, key) ->
        (* TODO: Punish peer *)
        Events.(emit notify_invalid) (key, peer)
    | Notify_unrequested (peer, key) ->
        (* TODO: Punish peer *)
        Events.(emit notify_unrequested) (key, peer)
    | Notify_duplicate (peer, key) ->
        (* TODO: Punish peer *)
        Events.(emit notify_duplicate) (key, peer)

  type update_table_action =
    | Replace of {key : key; status : status}
    | Remove of {key : key}

  let worker_loop state =
    let open Lwt_syntax in
    let shutdown = Lwt_canceler.when_canceling state.canceler in
    let rec loop state =
      (* It is possible that numerous pending requests may be canceled
         sequentially. If this occurs, we will recalculate the
         subsequent timeout for each cancellation. Calculating the
         next timeout could be resource-intensive. By allowing for a
         brief sleep, multiple cancellations can take place
         simultaneously.

         Note: using `Lwt.pause` or `Lwt.yield` might not be
         sufficient, e.g., when the scheduler does not timeout
         cancelers are not given a chance to be executed.

         Note: This constant was selected using the sophisticated
         "damp digit" method. *)
      let* () = Lwt_unix.sleep 0.001 in
      let timeout = compute_timeout state in
      let* () =
        Lwt.choose
          [
            (let* _ = state.events in
             Lwt.return_unit);
            timeout;
            shutdown;
          ]
      in
      if Lwt.state shutdown <> Lwt.Sleep then Events.(emit terminated) ()
      else if Lwt.state state.events <> Lwt.Sleep then (
        let now = Time.System.now () in
        let* events = state.events in
        state.events <- Lwt_pipe.Unbounded.pop_all state.queue ;
        let* () = List.iter_s (process_event state now) events in
        (* Requests are either added or deleted: either way, we need
           to go through the table to update the timeout. Setting it
           to now do just that. As a consequence, of that, the next
           call to `compute_timeout` will always return
           instantaneously. *)
        state.min_next_request <- Some now ;
        loop state)
      else
        let* () = Events.(emit timeout) () in
        let now = Time.System.now () in
        let active_peers = Request.active state.param in
        let compute_new_min_next_request min_next_request next_request =
          match min_next_request with
          | None -> Some next_request
          | Some min_next_request' ->
              if Ptime.is_earlier min_next_request' ~than:next_request then
                min_next_request
              else Some next_request
        in
        if
          P2p_peer.Set.is_empty active_peers
          && not (Table.length state.pending = 0)
        then
          (* When there is no active peer but there is already pending
             requests, we wait a few seconds. *)
          let* () = Events.(emit no_active_peers) () in
          Lwt_unix.sleep 5.
        else
          let actions, min_next_request, requests =
            Table.fold
              (fun key
                   {unrequested_peers; requested_peers; next_request; delay}
                   (actions, min_next_request, acc) ->
                if Ptime.is_later next_request ~than:now then
                  ( actions,
                    compute_new_min_next_request min_next_request next_request,
                    acc )
                else
                  (* Removing deactivated peers from sets of peers. *)
                  let remaining_unrequested_peers =
                    P2p_peer.Set.inter unrequested_peers active_peers
                  in
                  let remaining_requested_peers =
                    P2p_peer.Set.inter requested_peers active_peers
                  in
                  if
                    (P2p_peer.Set.is_empty remaining_unrequested_peers
                    && P2p_peer.Set.is_empty remaining_requested_peers)
                    && not
                         (P2p_peer.Set.is_empty unrequested_peers
                         && P2p_peer.Set.is_empty requested_peers)
                  then (Remove {key} :: actions, min_next_request, acc)
                  else
                    let ( requested_peer,
                          remaining_unrequested_peers,
                          remaining_requested_peers ) =
                      match
                        ( P2p_peer.Set.is_empty remaining_unrequested_peers,
                          P2p_peer.Set.is_empty remaining_requested_peers )
                      with
                      | true, true ->
                          (* If there is no specific peer to request, one of the
                             active peers is randomly selected. *)
                          ( P2p_peer.Id.Set.random_elt active_peers,
                            remaining_unrequested_peers,
                            remaining_requested_peers )
                      | true, false ->
                          (* If all requestable peers have already been
                             requested, one of the already requested peers is
                             randomly selected. *)
                          ( P2p_peer.Id.Set.random_elt remaining_requested_peers,
                            remaining_unrequested_peers,
                            remaining_requested_peers )
                      | false, _ ->
                          (* If there are unrequested peers, one is randomly
                             selected and moved to the set of requested peers. *)
                          let peer =
                            P2p_peer.Id.Set.random_elt
                              remaining_unrequested_peers
                          in
                          ( peer,
                            P2p_peer.Set.remove peer remaining_unrequested_peers,
                            P2p_peer.Set.add peer remaining_requested_peers )
                    in
                    let next_request =
                      Option.value ~default:Ptime.max (Ptime.add_span now delay)
                    in
                    let next =
                      {
                        unrequested_peers = remaining_unrequested_peers;
                        requested_peers = remaining_requested_peers;
                        next_request;
                        delay = Time.System.Span.multiply_exn 1.5 delay;
                      }
                    in
                    let new_acc =
                      P2p_peer.Map.update
                        requested_peer
                        (function
                          | None -> Some [key] | Some l -> Some (key :: l))
                        acc
                    in
                    ( Replace {key; status = next} :: actions,
                      compute_new_min_next_request min_next_request next_request,
                      new_acc ))
              state.pending
              ([], None, P2p_peer.Map.empty)
          in
          (* Update pending table *)
          List.iter
            (function
              | Remove {key} -> Table.remove state.pending key
              | Replace {key; status} -> Table.replace state.pending key status)
            actions ;
          state.min_next_request <- min_next_request ;
          P2p_peer.Map.iter (Request.send state.param) requests ;
          let* () =
            P2p_peer.Map.iter_s
              (fun peer request ->
                List.iter_s
                  (fun (key : key) -> Events.(emit requested) (key, peer))
                  request)
              requests
          in
          loop state
    in
    loop state

  let create param =
    let state =
      {
        param;
        queue = Lwt_pipe.Unbounded.create ();
        min_next_request = None;
        pending = Table.create ~entry_type:"pending_requests" ~random:true 17;
        events = Lwt.return_nil;
        canceler = Lwt_canceler.create ();
        worker = Lwt.return_unit;
      }
    in
    state.worker <-
      Lwt_utils.worker
        "db_request_scheduler"
        ~on_event:Internal_event.Lwt_worker_logger.on_event
        ~run:(fun () -> worker_loop state)
        ~cancel:(fun () -> Error_monad.cancel_with_exceptions state.canceler) ;
    state

  let shutdown s = Error_monad.cancel_with_exceptions s.canceler

  let pending_requests s = Table.length s.pending
end

module Make
    (Hash : HASH)
    (Disk_table : DISK_TABLE with type key := Hash.t)
    (Memory_table : MEMORY_TABLE with type key := Hash.t)
    (Request : REQUEST with type key := Hash.t)
    (Probe : PROBE with type key := Hash.t and type value := Disk_table.value) :
  FULL_REQUESTER
    with type key = Hash.t
     and type value = Disk_table.value
     and type param = Probe.param
     and type request_param = Request.param
     and type notified_value = Probe.notified_value
     and type store = Disk_table.store = struct
  type key = Hash.t

  type value = Disk_table.value

  type param = Probe.param

  type request_param = Request.param

  type notified_value = Probe.notified_value

  type store = Disk_table.store

  module Scheduler = Make_request_scheduler (Hash) (Memory_table) (Request)

  type t = {
    scheduler : Scheduler.t;
    disk : Disk_table.store;
    memory : status Memory_table.t;
    global_input : (key * value) Lwt_watcher.input option;
    input : (key * value) Lwt_watcher.input;
  }

  and status =
    | Pending of {
        waiter : value tzresult Lwt.t;
        wakener : value tzresult Lwt.u;
        mutable waiters : int;
        param : param;
      }
    | Found of value

  let known s k =
    match Memory_table.find s.memory k with
    | None -> Disk_table.known s.disk k
    | Some (Pending _) -> Lwt.return_false
    | Some (Found _) -> Lwt.return_true

  let read_opt s k =
    match Memory_table.find s.memory k with
    | None -> Disk_table.read_opt s.disk k
    | Some (Found v) -> Lwt.return_some v
    | Some (Pending _) -> Lwt.return_none

  type error += Missing_data of key

  type error += Canceled of key

  type error += Timeout of key

  let () =
    (* Missing data key *)
    register_error_kind
      `Permanent
      ~id:("requester." ^ Hash.name ^ ".missing")
      ~title:("Missing " ^ Hash.name)
      ~description:("Some " ^ Hash.name ^ " is missing from the requester")
      ~pp:(fun ppf key ->
        Format.fprintf ppf "Missing %s %a" Hash.name Hash.pp key)
      (Data_encoding.obj1 (Data_encoding.req "key" Hash.encoding))
      (function Missing_data key -> Some key | _ -> None)
      (fun key -> Missing_data key) ;
    (* Canceled key *)
    register_error_kind
      `Permanent
      ~title:("Canceled fetch of a " ^ Hash.name)
      ~description:("The fetch of a " ^ Hash.name ^ " has been canceled")
      ~id:("requester." ^ Hash.name ^ ".fetch_canceled")
      ~pp:(fun ppf key ->
        Format.fprintf ppf "Fetch of %s %a canceled" Hash.name Hash.pp key)
      Data_encoding.(obj1 (req "key" Hash.encoding))
      (function Canceled key -> Some key | _ -> None)
      (fun key -> Canceled key) ;
    (* Timeout key *)
    register_error_kind
      `Permanent
      ~title:("Timed out fetch of a " ^ Hash.name)
      ~description:("The fetch of a " ^ Hash.name ^ " has timed out")
      ~id:("requester." ^ Hash.name ^ ".fetch_timeout")
      ~pp:(fun ppf key ->
        Format.fprintf ppf "Fetch of %s %a timed out" Hash.name Hash.pp key)
      Data_encoding.(obj1 (req "key" Hash.encoding))
      (function Timeout key -> Some key | _ -> None)
      (fun key -> Timeout key)

  let read s k =
    let open Lwt_result_syntax in
    match Memory_table.find s.memory k with
    | None -> trace (Missing_data k) @@ Disk_table.read s.disk k
    | Some (Found v) -> return v
    | Some (Pending _) -> tzfail (Missing_data k)

  let wrap s k ?timeout t =
    let open Lwt_syntax in
    let t = Lwt.protected t in
    Lwt.on_cancel t (fun () ->
        match Memory_table.find s.memory k with
        | None -> ()
        | Some (Found _) -> ()
        | Some (Pending ({wakener = w; _} as data)) ->
            data.waiters <- data.waiters - 1 ;
            if data.waiters = 0 then (
              Memory_table.remove s.memory k ;
              Scheduler.notify_cancellation s.scheduler k ;
              Lwt.wakeup_later w (Result_syntax.tzfail (Canceled k)))) ;
    match timeout with
    | None -> t
    | Some delay ->
        let timeout =
          let* () = Systime_os.sleep delay in
          Lwt_result_syntax.tzfail (Timeout k)
        in
        Lwt.pick [t; timeout]

  let fetch s ?peer ?timeout k param =
    let open Lwt_syntax in
    let[@warning "-26"] debug_p2p_message =
      (match peer with
      | None -> []
      | Some peer -> [Format.asprintf "Peer : %a" P2p_peer.Id.pp peer])
      @ [Format.asprintf "Hash : %a" Hash.pp k]
    in
    match[@profiler.span_s {verbosity = Notice} ["fetch"]]
      Memory_table.find s.memory k
    with
    | None -> (
        (let* o = Disk_table.read_opt s.disk k in
         match o with
         | Some v ->
             return_ok
               v
             [@profiler.span_s
               {verbosity = Info} ["fetch"; "cache miss"; "disk hit"]]
         | None -> (
             let[@warning "-26"] profiling_prefix info =
               ["fetch"; "cache miss"; "disk miss"] @ info
             in
             match(* It is necessary to check the memory-table again in case another
                     promise has altered it whilst this one was waiting for the
                     disk-table query. *)
                  [@profiler.span_s
                    {verbosity = Info} ["fetch"; "cache miss"; "disk miss"]]
               Memory_table.find s.memory k
             with
             | None ->
                 (let waiter, wakener = Lwt.wait () in
                  Memory_table.add
                    s.memory
                    k
                    (Pending {waiter; wakener; waiters = 1; param}) ;
                  Scheduler.request s.scheduler peer k ;
                  wrap s k ?timeout waiter)
                 [@profiler.span_s
                   {verbosity = Debug}
                     (profiling_prefix
                        (["cache miss : request creation"] @ debug_p2p_message))]
             | Some status -> (
                 match[@profiler.span_s
                        {verbosity = Info} (profiling_prefix ["cache hit"])]
                   status
                 with
                 | Pending data ->
                     (Scheduler.request s.scheduler peer k ;
                      data.waiters <- data.waiters + 1 ;
                      wrap s k ?timeout data.waiter)
                     [@profiler.span_s
                       {verbosity = Debug}
                         (profiling_prefix
                            (["cache hit"; "pending : add peer"]
                            @ debug_p2p_message))]
                 | Found v ->
                     return_ok
                       v
                     [@profiler.span_s
                       {verbosity = Info}
                         (profiling_prefix ["cache hit"; "found"])])))
        [@profiler.span_s {verbosity = Notice} ["fetch"; "cache miss"]])
    | Some status -> (
        match[@profiler.span_s {verbosity = Notice} ["fetch"; "cache hit"]]
          status
        with
        | Pending data ->
            (Scheduler.request s.scheduler peer k ;
             data.waiters <- data.waiters + 1 ;
             wrap s k ?timeout data.waiter)
            [@profiler.span_s
              {verbosity = Debug}
                (["fetch"; "cache hit"; "pending : add peer"]
                @ debug_p2p_message)]
        | Found v ->
            return_ok
              v
            [@profiler.span_s
              {verbosity = Debug}
                (["fetch"; "cache hit"; "found"] @ debug_p2p_message)])

  let notify_when_pending s p k w param v =
    let open Lwt_syntax in
    match Probe.probe k param v with
    | None -> Scheduler.notify_invalid s.scheduler p k
    | Some v ->
        let* () = Scheduler.notify s.scheduler p k in
        Memory_table.replace s.memory k (Found v) ;
        Lwt.wakeup_later w (Ok v) ;
        Option.iter
          (fun input -> Lwt_watcher.notify input (k, v))
          s.global_input ;
        Lwt_watcher.notify s.input (k, v) ;
        Lwt.return_unit

  let notify s p k v =
    let open Lwt_syntax in
    match Memory_table.find s.memory k with
    | None -> (
        let* b = Disk_table.known s.disk k in
        match b with
        | true -> Scheduler.notify_duplicate s.scheduler p k
        | false -> (
            (* It is necessary to check the memory-table again in case another
               promise has altered it whilst this one was waiting for the
               disk-table query. *)
            match Memory_table.find s.memory k with
            | None -> Scheduler.notify_unrequested s.scheduler p k
            | Some (Pending {wakener = w; param; _}) ->
                notify_when_pending s p k w param v
            | Some (Found _) -> Scheduler.notify_duplicate s.scheduler p k))
    | Some (Pending {wakener = w; param; _}) ->
        notify_when_pending s p k w param v
    | Some (Found _) -> Scheduler.notify_duplicate s.scheduler p k

  let inject s k v =
    let open Lwt_syntax in
    match Memory_table.find s.memory k with
    | None -> (
        let* b = Disk_table.known s.disk k in
        match b with
        | true -> Lwt.return_false
        | false -> (
            (* It is necessary to check the memory-table again in case another
               promise has altered it whilst this one was waiting for the
               disk-table query. *)
            match Memory_table.find s.memory k with
            | None ->
                Memory_table.add s.memory k (Found v) ;
                Lwt.return_true
            | Some (Pending _) | Some (Found _) -> Lwt.return_false))
    | Some (Pending _) | Some (Found _) -> Lwt.return_false

  let clear_or_cancel s k =
    match[@profiler.span_f {verbosity = Notice} ["clear_or_cancel"]]
      Memory_table.find s.memory k
    with
    | None ->
        ()
        [@profiler.span_f {verbosity = Notice} ["clear_or_cancel"; "no data"]]
    | Some (Pending status) ->
        (if status.waiters <= 1 then (
           (Scheduler.notify_cancellation s.scheduler k ;
            Memory_table.remove s.memory k ;
            Lwt.wakeup_later status.wakener (Result_syntax.tzfail (Canceled k)))
           [@profiler.span_f
             {verbosity = Info}
               ["clear_or_cancel"; "pending"; "no more waiters"]])
         else
           status.waiters <-
             (status.waiters - 1)
             [@profiler.span_f
               {verbosity = Info}
                 ["clear_or_cancel"; "pending"; "existing waiters"]])
        [@profiler.span_f {verbosity = Notice} ["clear_or_cancel"; "pending"]]
    | Some (Found _) ->
        Memory_table.remove
          s.memory
          k
        [@profiler.span_f {verbosity = Notice} ["clear_or_cancel"; "completed"]]

  let watch s = Lwt_watcher.create_stream s.input

  let[@warning "-32"] init_profiler () =
    if not !profiler_init then (
      () [@profiler.record {verbosity = Notice} "start"] ;
      profiler_init := true)

  let create ?random_table:random ?global_input request_param disk =
    () [@profiler.overwrite init_profiler ()] ;
    let scheduler = Scheduler.create request_param in
    let memory = Memory_table.create ~entry_type:"entries" ?random 17 in
    let input = Lwt_watcher.create_input () in
    {scheduler; disk; memory; input; global_input}

  let pending s k =
    match Memory_table.find s.memory k with
    | None -> false
    | Some (Found _) -> false
    | Some (Pending _) -> true

  let memory_table_length s = Memory_table.length s.memory

  let pending_requests s = Scheduler.pending_requests s.scheduler

  let shutdown s = Scheduler.shutdown s.scheduler
end
