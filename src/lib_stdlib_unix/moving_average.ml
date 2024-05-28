(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Inttbl = Hashtbl.Make (struct
  type t = int

  let equal (x : int) (y : int) = x = y

  let hash = Hashtbl.hash
end)

type t = {
  id : int;
  alpha : int;
  mutable total : int64;
  mutable current : int;
  mutable average : int;
}

type state = {
  refresh_interval : float;
  counters : t Inttbl.t;
  updated : unit Lwt_condition.t;
  mutable update_hook : (unit -> unit) list;
}

let worker_loop state () =
  let to_ms x =
    (* code duplicated from Time in lib_base *)
    Int64.to_int
      Mtime.Span.(Int64.unsigned_div (to_uint64_ns x) (to_uint64_ns ms))
  in
  let rec inner sleep time_at_entry =
    let* () = sleep in
    let sleep = Lwt_unix.sleep state.refresh_interval in
    let now = Mtime_clock.elapsed () in
    let elapsed = to_ms Mtime.Span.(abs_diff now time_at_entry) in
    Inttbl.iter
      (fun _ c ->
        c.average <-
          (c.alpha * c.current / elapsed) + ((1000 - c.alpha) * c.average / 1000) ;
        c.current <- 0)
      state.counters ;
    List.iter (fun f -> f ()) state.update_hook ;
    Lwt_condition.broadcast state.updated () ;
    inner sleep now
  in
  inner (Lwt_unix.sleep state.refresh_interval) (Mtime_clock.elapsed ())

let fresh_state ~id ~refresh_interval =
  if refresh_interval <= 0.0 then
    raise (Invalid_argument "Moving_average.fresh_state") ;
  let state =
    {
      refresh_interval;
      counters = Inttbl.create 41;
      updated = Lwt_condition.create ();
      update_hook = [];
    }
  in
  Lwt.ignore_result
    (Lwt_utils.worker
       (Format.asprintf "counter(%s)" id)
       ~on_event:Internal_event.Lwt_worker_logger.on_event
       ~run:(worker_loop state)
       ~cancel:(fun _ -> Lwt.return_unit)) ;
  state

let on_update state f = state.update_hook <- f :: state.update_hook

let create =
  let cpt = ref 0 in
  fun state ~init ~alpha ->
    if not (0. < alpha && alpha <= 1.) then
      raise (Invalid_argument "Moving_average.create") ;
    let id = !cpt in
    incr cpt ;
    let alpha = int_of_float (1000. *. alpha) in
    let c = {id; alpha; total = 0L; current = 0; average = init} in
    Inttbl.add state.counters id c ;
    c

let add c x =
  c.total <- Int64.(add c.total (of_int x)) ;
  c.current <- c.current + x

let destroy state c = Inttbl.remove state.counters c.id

let updated {updated; _} = updated

type stat = {total : int64; average : int}

let stat ({total; average; _} : t) : stat = {total; average}
