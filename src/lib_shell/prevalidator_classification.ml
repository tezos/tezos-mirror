(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Shell_operation

module Event = struct
  let section = ["prevalidator_classification"]

  include Internal_event.Simple

  let predecessor_less_block =
    declare_1
      ~section
      ~name:"predecessor_less_block"
      ~msg:"Observing that a parent of block {blk_h} has no predecessor"
      ~level:Warning
      ("blk_h", Block_hash.encoding)
end

type error_classification =
  [ `Branch_delayed of tztrace
  | `Branch_refused of tztrace
  | `Refused of tztrace
  | `Outdated of tztrace ]

type classification = [`Applied | `Validated | error_classification]

module Map = Operation_hash.Map
module Sized_map = Tezos_base.Sized.MakeSizedMap (Map)

(** This type wraps together:

    - a bounded ring of keys (size book-keeping)
    - a regular (unbounded) map of key/values (efficient read)

    All operations must maintain integrity between the 2!
*)
type 'protocol_data bounded_map = {
  ring : Operation_hash.t Ringo.Ring.t;
  mutable map : ('protocol_data operation * error list) Map.t;
}

let map bounded_map = bounded_map.map

let cardinal bounded_map = Ringo.Ring.length bounded_map.ring

(** [mk_empty_bounded_map ring_size] returns a {!bounded_map} whose ring
    holds at most [ring_size] values. {!Invalid_argument} is raised
    if [ring_size <= 0]. *)
let mk_empty_bounded_map ring_size =
  {ring = Ringo.Ring.create ring_size; map = Map.empty}

type parameters = {
  map_size_limit : int;
  on_discarded_operation : Operation_hash.t -> unit;
}

(** Note that [applied] and [in_mempool] are intentionally unbounded.
    See the mli for detailed documentation.
    All operations must maintain the invariant about [in_mempool]
    described in the mli. *)
type 'protocol_data t = {
  parameters : parameters;
  refused : 'protocol_data bounded_map;
  outdated : 'protocol_data bounded_map;
  branch_refused : 'protocol_data bounded_map;
  branch_delayed : 'protocol_data bounded_map;
  mutable applied_rev : 'protocol_data operation list;
  mutable validated : 'protocol_data operation Sized_map.t;
  mutable unparsable : Operation_hash.Set.t;
  mutable in_mempool : ('protocol_data operation * classification) Map.t;
}

let create parameters =
  {
    parameters;
    refused = mk_empty_bounded_map parameters.map_size_limit;
    outdated = mk_empty_bounded_map parameters.map_size_limit;
    branch_refused = mk_empty_bounded_map parameters.map_size_limit;
    branch_delayed = mk_empty_bounded_map parameters.map_size_limit;
    validated = Sized_map.empty;
    unparsable = Operation_hash.Set.empty;
    in_mempool = Map.empty;
    applied_rev = [];
  }

let is_empty
    {
      (* All fields are intentionaly mentioned, so that we get a warning
         when we add a field. This will force to think whether this
         function needs to be updated or not. *)
      parameters = _;
      refused = _;
      outdated = _;
      branch_refused = _;
      branch_delayed = _;
      validated = _;
      applied_rev = _;
      unparsable;
      in_mempool;
    } =
  (* By checking only [in_mempool] here, we rely on the invariant that
     [in_mempool] is the union of all other fields (see the MLI for
     detailed documentation of this invariant) except unparsable
     operations which are not classified yet. *)
  Map.is_empty in_mempool && Operation_hash.Set.is_empty unparsable

let set_of_bounded_map bounded_map =
  Map.fold
    (fun oph _ acc -> Operation_hash.Set.add oph acc)
    bounded_map.map
    Operation_hash.Set.empty

let flush (classes : 'protocol_data t) ~handle_branch_refused =
  let remove_map_from_in_mempool map =
    classes.in_mempool <-
      Map.fold
        (fun oph _ mempool -> Map.remove oph mempool)
        map
        classes.in_mempool
  in
  let remove_list_from_in_mempool list =
    classes.in_mempool <-
      List.fold_left
        (fun mempool op -> Map.remove op.hash mempool)
        classes.in_mempool
        list
  in
  if handle_branch_refused then (
    remove_map_from_in_mempool classes.branch_refused.map ;
    Ringo.Ring.clear classes.branch_refused.ring ;
    classes.branch_refused.map <- Map.empty) ;
  remove_map_from_in_mempool classes.branch_delayed.map ;
  Ringo.Ring.clear classes.branch_delayed.ring ;
  classes.branch_delayed.map <- Map.empty ;
  remove_list_from_in_mempool classes.applied_rev ;
  classes.applied_rev <- [] ;
  remove_map_from_in_mempool (Sized_map.to_map classes.validated) ;
  classes.unparsable <- Operation_hash.Set.empty ;
  classes.validated <- Sized_map.empty

let is_in_mempool oph classes = Map.find oph classes.in_mempool

let is_known_unparsable oph classes =
  Operation_hash.Set.mem oph classes.unparsable

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
  match Map.find oph classes.in_mempool with
  | None -> None
  | Some (op, classification) ->
      (classes.in_mempool <- Map.remove oph classes.in_mempool ;
       match classification with
       | `Refused _ -> classes.refused.map <- Map.remove oph classes.refused.map
       | `Outdated _ ->
           classes.outdated.map <- Map.remove oph classes.outdated.map
       | `Branch_refused _ ->
           classes.branch_refused.map <-
             Map.remove oph classes.branch_refused.map
       | `Branch_delayed _ ->
           classes.branch_delayed.map <-
             Map.remove oph classes.branch_delayed.map
       | `Validated ->
           classes.validated <- Sized_map.remove oph classes.validated
       | `Applied ->
           classes.applied_rev <-
             List.filter
               (fun op -> Operation_hash.(op.hash <> oph))
               classes.applied_rev) ;
      Some (op, classification)

let handle_applied oph op classes =
  classes.applied_rev <- op :: classes.applied_rev ;
  classes.in_mempool <- Map.add oph (op, `Applied) classes.in_mempool

let handle_validated oph op classes =
  classes.validated <- Sized_map.add oph op classes.validated ;
  classes.in_mempool <- Map.add oph (op, `Validated) classes.in_mempool

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
  let bounded_map, tztrace =
    match classification with
    | `Branch_refused tztrace -> (classes.branch_refused, tztrace)
    | `Branch_delayed tztrace -> (classes.branch_delayed, tztrace)
    | `Refused tztrace -> (classes.refused, tztrace)
    | `Outdated tztrace -> (classes.outdated, tztrace)
  in
  Ringo.Ring.add_and_return_erased bounded_map.ring oph
  |> Option.iter (fun e ->
         bounded_map.map <- Map.remove e bounded_map.map ;
         classes.parameters.on_discarded_operation e ;
         classes.in_mempool <- Map.remove e classes.in_mempool) ;
  (match classification with
  | `Refused _ | `Outdated _ -> classes.parameters.on_discarded_operation oph
  | `Branch_delayed _ | `Branch_refused _ -> ()) ;
  bounded_map.map <- Map.add oph (op, tztrace) bounded_map.map ;
  let classification : classification = (classification :> classification) in
  classes.in_mempool <- Map.add oph (op, classification) classes.in_mempool

let add_unparsable oph classes =
  classes.unparsable <- Operation_hash.Set.add oph classes.unparsable ;
  classes.parameters.on_discarded_operation oph

let add classification op classes =
  match classification with
  | `Applied -> handle_applied op.hash op classes
  | `Validated -> handle_validated op.hash op classes
  | (`Branch_refused _ | `Branch_delayed _ | `Refused _ | `Outdated _) as
    classification ->
      handle_error op.hash op classification classes

let to_map ~applied ~validated ~branch_delayed ~branch_refused ~refused
    ~outdated classes : 'protocol_data operation Map.t =
  let ( +> ) accum to_add =
    let merge_fun _k accum_v_opt to_add_v_opt =
      match (accum_v_opt, to_add_v_opt) with
      | Some accum_v, None -> Some accum_v
      | None, Some (to_add_v, _err) -> Some to_add_v
      | Some _accum_v, Some (to_add_v, _err) ->
          (* This case should not happen, because the different classes
             should be disjoint. However, if this invariant is broken,
             it is not critical, hence we do not raise an error.
             Because such part of the code is quite technical and
             the invariant is not critical,
             we don't advertise the node administrator either (no log). *)
          Some to_add_v
      | None, None -> None
    in
    Map.merge merge_fun accum to_add
  in
  Map.union
    (fun _oph op _ -> Some op)
    (if validated then Sized_map.to_map classes.validated else Map.empty)
  @@ (if applied then
      List.to_seq classes.applied_rev
      |> Seq.map (fun op -> (op.hash, op))
      |> Map.of_seq
     else Map.empty)
     +> (if branch_delayed then classes.branch_delayed.map else Map.empty)
     +> (if branch_refused then classes.branch_refused.map else Map.empty)
     +> (if refused then classes.refused.map else Map.empty)
     +> if outdated then classes.outdated.map else Map.empty

type 'block block_tools = {
  bhash : 'block -> Block_hash.t;
  operations : 'block -> Operation.t list list;
  all_operation_hashes : 'block -> Operation_hash.t list list;
}

type 'block chain_tools = {
  clear_or_cancel : Operation_hash.t -> unit;
  inject_operation : Operation_hash.t -> Operation.t -> unit Lwt.t;
  new_blocks :
    from_block:'block -> to_block:'block -> ('block * 'block list) Lwt.t;
  read_predecessor_opt : 'block -> 'block option Lwt.t;
}

(* There's detailed documentation in the mli *)
let handle_live_operations ~classes ~(block_store : 'block block_tools)
    ~(chain : 'block chain_tools) ~(from_branch : 'block) ~(to_branch : 'block)
    ~(is_branch_alive : Block_hash.t -> bool)
    ~(parse :
       Operation_hash.t -> Operation.t -> 'protocol_data operation option)
    old_mempool =
  let open Lwt_syntax in
  let rec pop_block ancestor (block : 'block) mempool =
    let hash = block_store.bhash block in
    if Block_hash.equal hash ancestor then Lwt.return mempool
    else
      let operations = block_store.operations block in
      let* mempool =
        List.fold_left_s
          (List.fold_left_s (fun mempool op ->
               let oph = Operation.hash op in
               let+ () = chain.inject_operation oph op in
               match parse oph op with
               | None ->
                   (* There are hidden invariants between the shell and
                      the economic protocol which should ensure this will
                      (almost) never happen in practice:

                          1. Decoding/encoding an operation only depends
                      on the protocol and not the current context.

                          2. It is not possible to have a reorganisation
                      where one branch is using one protocol and another
                      branch on another protocol.

                          3. Ok, actually there might be one case using
                      [user_activated_upgrades] where this could happen,
                      but this is quite rare.

                        If this happens, we classifies an operation as
                      unparsable and it is ok. *)
                   add_unparsable oph classes ;
                   mempool
               | Some parsed_op -> Operation_hash.Map.add oph parsed_op mempool))
          mempool
          operations
      in
      let* o = chain.read_predecessor_opt block in
      match o with
      | None ->
          (* Can this happen? If yes, there's nothing more to pop anyway,
             so returning the accumulator. It's not the mempool that
             should crash, should this case happen. *)
          let+ () = Event.(emit predecessor_less_block ancestor) in
          mempool
      | Some predecessor ->
          (* This is a tailcall, which is nice; that is why we annotate
             here. But it is not required for the code to be correct.
             Given the maximum size of possible reorgs, even if the call
             was not tail recursive; we wouldn't reach the runtime's stack
             limit. *)
          (pop_block [@tailcall]) ancestor predecessor mempool
  in
  let push_block mempool block =
    let operations = block_store.all_operation_hashes block in
    List.iter (List.iter chain.clear_or_cancel) operations ;
    List.fold_left
      (List.fold_left (fun mempool h -> Operation_hash.Map.remove h mempool))
      mempool
      operations
  in
  let* ancestor, path =
    chain.new_blocks ~from_block:from_branch ~to_block:to_branch
  in
  let+ mempool =
    pop_block (block_store.bhash ancestor) from_branch old_mempool
  in
  let new_mempool = List.fold_left push_block mempool path in
  let new_mempool, outdated =
    Map.partition
      (fun _oph op -> is_branch_alive op.raw.Operation.shell.branch)
      new_mempool
  in
  Map.iter (fun oph _op -> chain.clear_or_cancel oph) outdated ;
  new_mempool

let recycle_operations ~from_branch ~to_branch ~live_blocks ~classes ~parse
    ~pending ~(block_store : 'block block_tools) ~(chain : 'block chain_tools)
    ~handle_branch_refused =
  let open Lwt_syntax in
  let+ pending =
    handle_live_operations
      ~classes
      ~block_store
      ~chain
      ~from_branch
      ~to_branch
      ~is_branch_alive:(fun branch -> Block_hash.Set.mem branch live_blocks)
      ~parse
      (Map.union
         (fun _key v _ -> Some v)
         (to_map
            ~applied:true
            ~validated:true
            ~branch_delayed:true
            ~branch_refused:handle_branch_refused
            ~refused:false
            ~outdated:false
            classes)
         pending)
  in
  (* Non parsable operations that were previously included in a block
     will be removed by the call to [flush]. However, as explained in
     [handle_live_operations] it should never happen in practice. *)
  flush classes ~handle_branch_refused ;
  pending

module Internal_for_tests = struct
  (** [copy_bounded_map bm] returns a deep copy of [bm] *)
  let copy_bounded_map (bm : 'protocol_data bounded_map) :
      'protocol_data bounded_map =
    let copy_ring (ring : Operation_hash.t Ringo.Ring.t) =
      let result = Ringo.Ring.capacity ring |> Ringo.Ring.create in
      List.iter (Ringo.Ring.add result) (Ringo.Ring.elements ring) ;
      result
    in
    {map = bm.map; ring = copy_ring bm.ring}

  let copy (t : 'protocol_data t) : 'protocol_data t =
    (* Code could be shorter by doing a functional update thanks to
       the 'with' keyword. We rather list all the fields, so that
       the compiler emits a warning when a field is added. *)
    {
      parameters = t.parameters;
      refused = copy_bounded_map t.refused;
      outdated = copy_bounded_map t.outdated;
      branch_refused = copy_bounded_map t.branch_refused;
      branch_delayed = copy_bounded_map t.branch_delayed;
      applied_rev = t.applied_rev;
      validated = t.validated;
      unparsable = t.unparsable;
      in_mempool = t.in_mempool;
    }

  let[@coverage off] bounded_map_pp ppf bounded_map =
    bounded_map.map |> Map.bindings
    |> List.map (fun (key, _value) -> key)
    |> Format.fprintf ppf "%a" (Format.pp_print_list Operation_hash.pp)

  let[@coverage off] pp ppf
      {
        parameters;
        refused;
        outdated;
        branch_refused;
        branch_delayed;
        applied_rev;
        validated;
        unparsable;
        in_mempool;
      } =
    let applied_pp ppf applied =
      applied
      |> List.map (fun op -> op.hash)
      |> Format.fprintf ppf "%a" (Format.pp_print_list Operation_hash.pp)
    in
    let in_mempool_pp ppf in_mempool =
      in_mempool |> Map.bindings |> List.map fst
      |> Format.fprintf ppf "%a" (Format.pp_print_list Operation_hash.pp)
    in
    let validated_pp ppf validated =
      validated |> Sized_map.bindings |> List.map fst
      |> Format.fprintf ppf "%a" (Format.pp_print_list Operation_hash.pp)
    in
    let unparsable_pp ppf unparsable =
      unparsable |> Operation_hash.Set.elements
      |> Format.fprintf ppf "%a" (Format.pp_print_list Operation_hash.pp)
    in
    Format.fprintf
      ppf
      "Map_size_limit:@.%i@.On discarded operation: \
       <function>@.Refused:%a@.Outdated:%a@.Branch refused:@.%a@.Branch \
       delayed:@.%a@.Applied:@.%a@.Validated:@.%a@.Unparsable:@.%a@.In \
       Mempool:@.%a"
      parameters.map_size_limit
      bounded_map_pp
      refused
      bounded_map_pp
      outdated
      bounded_map_pp
      branch_refused
      bounded_map_pp
      branch_delayed
      applied_pp
      applied_rev
      validated_pp
      validated
      unparsable_pp
      unparsable
      in_mempool_pp
      in_mempool

  let set_of_bounded_map = set_of_bounded_map

  let[@coverage off] pp_t_sizes pp t =
    let show_bounded_map name bounded_map =
      Format.sprintf
        "%s map: %d, %s ring: %d"
        name
        (Map.cardinal bounded_map.map)
        name
        (Ringo.Ring.length bounded_map.ring)
    in
    let show_map name (map : 'a Sized_map.t) =
      Format.sprintf "%s map: %d" name (Sized_map.cardinal map)
    in
    Format.fprintf
      pp
      "map_size_limit: %d\n%s\n%s\n%s\n%s\n%sapplied_rev: %d\nin_mempool: %d"
      t.parameters.map_size_limit
      (show_bounded_map "refused" t.refused)
      (show_bounded_map "outdated" t.outdated)
      (show_bounded_map "branch_refused" t.branch_refused)
      (show_bounded_map "branch_delayed" t.branch_delayed)
      (show_map "validated" t.validated)
      (List.length t.applied_rev)
      (Map.cardinal t.in_mempool)

  let to_map = to_map

  let flush = flush

  let handle_live_operations = handle_live_operations
end
