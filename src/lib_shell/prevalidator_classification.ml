(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

type classification =
  [ `Applied
  | `Branch_delayed of tztrace
  | `Branch_refused of tztrace
  | `Refused of tztrace ]

type bounded_map = {
  ring : Operation_hash.t Ringo.Ring.t;
  mutable map : (Operation.t * error list) Operation_hash.Map.t;
}

let map bounded_map = bounded_map.map

(** [mk_empty_bounded_map ring_size] returns a {!bounded_map} whose ring
    *  holds at mosts [ring_size] values. {!Invalid_argument} is raised
    *  if [ring_size] is [0] or less. *)
let mk_empty_bounded_map ring_size =
  {ring = Ringo.Ring.create ring_size; map = Operation_hash.Map.empty}

type parameters = {
  map_size_limit : int;
  on_discarded_operation : Operation_hash.t -> unit;
}

type t = {
  parameters : parameters;
  refused : bounded_map;
  branch_refused : bounded_map;
  branch_delayed : bounded_map;
  mutable applied : (Operation_hash.t * Operation.t) list;
  mutable in_mempool : Operation_hash.Set.t;
}

let create parameters =
  {
    parameters;
    refused = mk_empty_bounded_map parameters.map_size_limit;
    branch_refused = mk_empty_bounded_map parameters.map_size_limit;
    branch_delayed = mk_empty_bounded_map parameters.map_size_limit;
    in_mempool = Operation_hash.Set.empty;
    applied = [];
  }

let clear (classes : t) =
  Ringo.Ring.clear classes.branch_refused.ring ;
  classes.branch_refused.map <- Operation_hash.Map.empty ;
  Ringo.Ring.clear classes.branch_delayed.ring ;
  classes.branch_delayed.map <- Operation_hash.Map.empty ;
  classes.applied <- [] ;
  classes.in_mempool <- Operation_hash.Set.empty

let is_in_mempool oph classes = Operation_hash.Set.mem oph classes.in_mempool

let is_applied oph classes =
  List.exists (fun (h, _) -> Operation_hash.equal h oph) classes.applied

(* Removing an operation is currently used for operations which are
   banned (this can only be achieved by the adminstrator of the
   node). However, removing an operation which is applied invalidates
   the classification of all the operations. Hence, the
   classifications of all the operations should be reset. Currently,
   this is not enforced by the function and has to be done by the
   caller.

   Later on, it would be probably better if this function returns a
   set of pending operations instead. *)
let remove oph classes =
  classes.refused.map <- Operation_hash.Map.remove oph classes.refused.map ;
  classes.branch_refused.map <-
    Operation_hash.Map.remove oph classes.branch_refused.map ;
  classes.branch_delayed.map <-
    Operation_hash.Map.remove oph classes.branch_delayed.map ;
  classes.in_mempool <- Operation_hash.Set.remove oph classes.in_mempool ;
  classes.applied <-
    List.filter (fun (op, _) -> Operation_hash.(op <> oph)) classes.applied

let handle_applied oph op classes =
  classes.applied <- (oph, op) :: classes.applied ;
  classes.in_mempool <- Operation_hash.Set.add oph classes.in_mempool

(* 1. Add the operation to the ring underlying the corresponding
   error map class.

    2a. If the ring is full, remove the discarded operation from the
   map and the [in_mempool] set, and calls the callback with the
   discarded operation.

    2b. If the operation is [Refused], call the callback with it, as
   the operation is discarded. In this case it means the operation
   should not be propagated. It is still stored in a bounded map for
   the [pending_operations] RPC.

    3. Add the operation to the underlying map.

    4. Add the operation to the [in_mempool] set. *)
let handle_error oph op classification classes =
  let (bounded_map, tztrace) =
    match classification with
    | `Branch_refused tztrace -> (classes.branch_refused, tztrace)
    | `Branch_delayed tztrace -> (classes.branch_delayed, tztrace)
    | `Refused tztrace -> (classes.refused, tztrace)
  in
  Ringo.Ring.add_and_return_erased bounded_map.ring oph
  |> Option.iter (fun e ->
         bounded_map.map <- Operation_hash.Map.remove e bounded_map.map ;
         classes.parameters.on_discarded_operation e ;
         classes.in_mempool <- Operation_hash.Set.remove e classes.in_mempool) ;
  (match classification with
  | `Refused _ -> classes.parameters.on_discarded_operation oph
  | _ -> ()) ;
  bounded_map.map <- Operation_hash.Map.add oph (op, tztrace) bounded_map.map ;
  classes.in_mempool <- Operation_hash.Set.add oph classes.in_mempool

let add ~notify classification oph op classes =
  notify () ;
  match classification with
  | `Applied -> handle_applied oph op classes
  | (`Branch_refused _ | `Branch_delayed _ | `Refused _) as classification ->
      handle_error oph op classification classes
