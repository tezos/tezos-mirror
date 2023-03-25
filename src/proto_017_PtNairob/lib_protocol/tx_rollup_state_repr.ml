(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

open Tx_rollup_errors_repr

type range =
  | Interval of {
      oldest : Tx_rollup_level_repr.t;
      newest : Tx_rollup_level_repr.t;
    }
  | Empty of {next : Tx_rollup_level_repr.t}

let range_newest = function Interval {newest; _} -> Some newest | _ -> None

let range_oldest = function Interval {oldest; _} -> Some oldest | _ -> None

let extend = function
  | Empty {next} -> (Interval {oldest = next; newest = next}, next)
  | Interval {oldest; newest} ->
      let newest = Tx_rollup_level_repr.succ newest in
      (Interval {oldest; newest}, newest)

let shrink = function
  | Empty _ -> error (Internal_error "cannot shrink range")
  | Interval {oldest; newest} when Tx_rollup_level_repr.(oldest < newest) ->
      ok (Interval {oldest = Tx_rollup_level_repr.succ oldest; newest})
  | Interval {newest; oldest = _} ->
      ok (Empty {next = Tx_rollup_level_repr.succ newest})

let belongs_to range level =
  match range with
  | Empty _ -> false
  | Interval {oldest; newest} ->
      Tx_rollup_level_repr.(oldest <= level && level <= newest)

let right_cut range level =
  match Tx_rollup_level_repr.pred level with
  | None -> ok (Empty {next = Tx_rollup_level_repr.root})
  | Some predecessor -> (
      match range with
      | Interval {oldest; newest = _} when belongs_to range level ->
          if Tx_rollup_level_repr.(oldest <= predecessor) then
            ok (Interval {oldest; newest = predecessor})
          else ok (Empty {next = level})
      | _ -> error (Internal_error "cannot cut range"))

let left_extend range level =
  match range with
  | Interval {oldest = _; newest} -> ok (Interval {oldest = level; newest})
  | Empty {next} ->
      let newest =
        Option.value ~default:level (Tx_rollup_level_repr.pred next)
      in
      ok (Interval {oldest = level; newest})

let range_count = function
  | Empty _ -> 0
  | Interval {oldest; newest} ->
      Int32.(succ @@ Tx_rollup_level_repr.diff newest oldest |> to_int)

let range_encoding : range Data_encoding.t =
  Data_encoding.(
    union
      [
        case
          (Tag 0)
          ~title:"empty"
          (obj1 (req "next" Tx_rollup_level_repr.encoding))
          (function Empty {next} -> Some next | _ -> None)
          (fun next -> Empty {next});
        case
          (Tag 1)
          ~title:"interval"
          (obj2
             (req "newest" Tx_rollup_level_repr.encoding)
             (req "oldest" Tx_rollup_level_repr.encoding))
          (function
            | Interval {newest; oldest} -> Some (newest, oldest) | _ -> None)
          (fun (newest, oldest) -> Interval {newest; oldest});
      ])

let pp_range fmt = function
  | Empty {next} -> Format.(fprintf fmt "next: %a" Tx_rollup_level_repr.pp next)
  | Interval {oldest; newest} ->
      Format.(
        fprintf
          fmt
          "oldest: %a newest: %a"
          Tx_rollup_level_repr.pp
          oldest
          Tx_rollup_level_repr.pp
          newest)

type watermark = Tx_rollup_level_repr.t option

let is_above_watermark watermark level =
  match watermark with
  | Some watermark -> Tx_rollup_level_repr.(watermark < level)
  | None -> true

let make_watermark level = Some level

(** The state of a transaction rollup is composed of [burn_per_byte]
    and [inbox_ema] fields. [initial_state] introduces their initial
    values. Both values are updated by [update_burn_per_byte] as the
    rollup progresses.

    [burn_per_byte] state the cost of burn per byte to be paid for
    each byte submitted to a transaction rollup inbox. [inbox_ema]
    is a key factor to impact the update of [burn_per_byte].

    [inbox_ema] is the N-block EMA to react to recent N-inbox size
    changes. N-block EMA is an exponential moving average (EMA), that
    is a type of moving average that places a greater weight and
    significance on the most N data points. The purpose of [inbox_ema]
    is to get lessened volatility of burn, that is more resistant to
    spurious spikes of [burn_per_byte].

   The state of the transaction rollup also keeps track of four pointers
   to four different rollup levels.

    - The [commitment_oldest_level] is the level of the oldest
      finalized commitment still stored in the layer-1 storage.

    - The [commitment_newest_level] is the level of the most recent
      unfinalized commitment in the layer-1 storage.

    - The [oldest_inbox_level] is the level of the oldest inbox still stored
      in the layer-1 storage.

    - The [newest_level] is the level of the most recent inbox in the
      layer-1 storage.
*)
type t = {
  last_removed_commitment_hashes :
    (Tx_rollup_message_result_hash_repr.t * Tx_rollup_commitment_repr.Hash.t)
    option;
  finalized_commitments : range;
  unfinalized_commitments : range;
  uncommitted_inboxes : range;
  commitment_newest_hash : Tx_rollup_commitment_repr.Hash.t option;
  tezos_head_level : Raw_level_repr.t option;
  burn_per_byte : Tez_repr.t;
  inbox_ema : int;
  allocated_storage : Z.t;
  occupied_storage : Z.t;
  commitments_watermark : watermark;
}

(*

   The main use of a transaction rollup state is to keep track of four
   pointers to four different rollup levels (see above).

   When the rollup is created, these four pointers are initialized with
   the [None] value, because no inboxes or commitments have been created
   yet. Because inboxes and commitments can be removed from the layer-1
   context under certain circumstances, they can be reset to [None].

   The state allows us to keep track of three intervals: the finalized
   commitments (whose inboxes have been removed from the layer-1
   storage), the unfinalized commitments (whose inboxes are still in
   the layer-1 storage), and uncommitted inboxes (that is, inboxes
   which are still waiting for a commitment).

     finalized     uncommitted
       ^^^^^^        ^^^^^^^^
       [------------]            commitments
             [--------------]    inboxes
             ^^^^^^^^
            unfinalized

    Note that this layout is not the only one that we can witness in
    the layer-1 storage, even if it is the more common. It is possible
    for instance that there is no unfinalized commitments at a given
    time.

     finalized
       ^^^^^^
       [----]                    commitments
             [--------------]    inboxes
             ^^^^^^^^^^^^^^^^
                uncommitted

     Or that we have no more inboxes, but only finalized commitments.

     finalized
       ^^^^^^
      CT
       [-----]              commitments
                            inboxes

 *)

let initial_state ~pre_allocated_storage =
  {
    last_removed_commitment_hashes = None;
    finalized_commitments = Empty {next = Tx_rollup_level_repr.root};
    unfinalized_commitments = Empty {next = Tx_rollup_level_repr.root};
    uncommitted_inboxes = Empty {next = Tx_rollup_level_repr.root};
    commitment_newest_hash = None;
    tezos_head_level = None;
    burn_per_byte = Tez_repr.zero;
    inbox_ema = 0;
    allocated_storage = pre_allocated_storage;
    occupied_storage = Z.zero;
    commitments_watermark = None;
  }

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           last_removed_commitment_hashes;
           finalized_commitments;
           unfinalized_commitments;
           uncommitted_inboxes;
           commitment_newest_hash;
           tezos_head_level;
           burn_per_byte;
           allocated_storage;
           occupied_storage;
           inbox_ema;
           commitments_watermark;
         } ->
      ( ( last_removed_commitment_hashes,
          finalized_commitments,
          unfinalized_commitments,
          uncommitted_inboxes,
          commitment_newest_hash,
          tezos_head_level,
          burn_per_byte,
          allocated_storage,
          occupied_storage,
          inbox_ema ),
        commitments_watermark ))
    (fun ( ( last_removed_commitment_hashes,
             finalized_commitments,
             unfinalized_commitments,
             uncommitted_inboxes,
             commitment_newest_hash,
             tezos_head_level,
             burn_per_byte,
             allocated_storage,
             occupied_storage,
             inbox_ema ),
           commitments_watermark ) ->
      {
        last_removed_commitment_hashes;
        finalized_commitments;
        unfinalized_commitments;
        uncommitted_inboxes;
        commitment_newest_hash;
        tezos_head_level;
        burn_per_byte;
        allocated_storage;
        occupied_storage;
        inbox_ema;
        commitments_watermark;
      })
    (merge_objs
       (obj10
          (req
             "last_removed_commitment_hashes"
             (option
             @@ obj2
                  (req
                     "last_message_hash"
                     Tx_rollup_message_result_hash_repr.encoding)
                  (req
                     "commitment_hash"
                     Tx_rollup_commitment_repr.Hash.encoding)))
          (req "finalized_commitments" range_encoding)
          (req "unfinalized_commitments" range_encoding)
          (req "uncommitted_inboxes" range_encoding)
          (req
             "commitment_newest_hash"
             (option Tx_rollup_commitment_repr.Hash.encoding))
          (req "tezos_head_level" (option Raw_level_repr.encoding))
          (req "burn_per_byte" Tez_repr.encoding)
          (req "allocated_storage" n)
          (req "occupied_storage" n)
          (req "inbox_ema" int31))
       (obj1
          (req "commitments_watermark" @@ option Tx_rollup_level_repr.encoding)))

let pp fmt
    {
      last_removed_commitment_hashes;
      finalized_commitments;
      unfinalized_commitments;
      uncommitted_inboxes;
      commitment_newest_hash;
      tezos_head_level;
      burn_per_byte;
      allocated_storage;
      occupied_storage;
      inbox_ema;
      commitments_watermark;
    } =
  Format.(
    fprintf
      fmt
      "cost_per_byte: %a inbox_ema: %d finalized_commitments: %a \
       unfinalized_commitments: %a uncommitted_inboxes: %a \
       commitment_newest_hash: %a tezos_head_level: %a \
       last_removed_commitment_hashes: %a allocated_storage: %a \
       occupied_storage: %a commitments_watermark: %a"
      Tez_repr.pp
      burn_per_byte
      inbox_ema
      pp_range
      finalized_commitments
      pp_range
      unfinalized_commitments
      pp_range
      uncommitted_inboxes
      (pp_print_option Tx_rollup_commitment_repr.Hash.pp)
      commitment_newest_hash
      (pp_print_option Raw_level_repr.pp)
      tezos_head_level
      (pp_print_option (fun fmt (m, c) ->
           fprintf
             fmt
             "(message result: %a, commitment: %a)"
             Tx_rollup_message_result_hash_repr.pp
             m
             Tx_rollup_commitment_repr.Hash.pp
             c))
      last_removed_commitment_hashes
      Z.pp_print
      allocated_storage
      Z.pp_print
      occupied_storage
      (pp_print_option Tx_rollup_level_repr.pp)
      commitments_watermark)

let adjust_storage_allocation : t -> delta:Z.t -> (t * Z.t) tzresult =
 fun state ~delta ->
  if Z.(equal zero delta) then ok (state, Z.zero)
  else
    let occupied_storage' = Z.add state.occupied_storage delta in
    if Compare.Z.(occupied_storage' < Z.zero) then
      (* returns [Internal_error] if [delta < 0] and [| delta | > state.occupied_storage].
         This error should never happen. *)
      error
      @@ Internal_error
           "Storage size should be positive after occupied space is freed."
    else
      let diff = Z.sub occupied_storage' state.allocated_storage in
      if Compare.Z.(diff > Z.zero) then
        let state =
          {
            state with
            occupied_storage = occupied_storage';
            allocated_storage = occupied_storage';
          }
        in
        ok (state, diff)
      else
        let state = {state with occupied_storage = occupied_storage'} in
        ok (state, Z.zero)

let update_burn_per_byte_helper :
    t -> factor:int -> final_size:int -> hard_limit:int -> t =
 fun ({burn_per_byte; inbox_ema; _} as state) ~factor ~final_size ~hard_limit ->
  let threshold_increase = 90 in
  let threshold_decrease = 80 in
  let variation_factor = 5L in
  let smoothing = 2 in
  (* The formula of the multiplier of EMA :

       smoothing / (1 + N)

     Suppose the period we want to observe is given by the
     [factor]. The common choice of smoothing is 2.
  *)
  let inbox_ema =
    inbox_ema + ((final_size - inbox_ema) * smoothing / (1 + factor))
  in
  let percentage = inbox_ema * 100 / hard_limit in
  let computation =
    let open Compare.Int in
    if threshold_decrease < percentage && percentage <= threshold_increase then
      (* constant case *)
      ok burn_per_byte
    else
      Tez_repr.(burn_per_byte *? variation_factor >>? fun x -> x /? 100L)
      >>? fun variation ->
      let variation =
        if Tez_repr.(variation = zero) then Tez_repr.one_mutez else variation
      in
      (* increase case *)
      if threshold_increase < percentage then
        Tez_repr.(burn_per_byte +? variation)
      else if percentage < threshold_decrease && Tez_repr.(zero < burn_per_byte)
      then
        (* decrease case, and strictly positive burn *)
        Tez_repr.(burn_per_byte -? variation)
      else (* decrease case, and burn equals zero *)
        ok burn_per_byte
  in
  match computation with
  | Ok burn_per_byte -> {state with burn_per_byte; inbox_ema}
  (* In the (very unlikely) event of an overflow, we force the burn to
     be the maximum amount. *)
  | Error _ -> {state with burn_per_byte = Tez_repr.max_mutez; inbox_ema}

let rec update_burn_per_byte :
    t -> elapsed:int -> factor:int -> final_size:int -> hard_limit:int -> t =
 fun state ~elapsed ~factor ~final_size ~hard_limit ->
  (* factor is expected to be a low number ~ 100 *)
  if Compare.Int.(elapsed > factor) then
    (* We do not need to compute precisely the new state. *)
    {state with burn_per_byte = Tez_repr.zero; inbox_ema = 0}
  else if Compare.Int.(elapsed <= 0) then
    (* Base case, we take into a account the [final_size] once. *)
    update_burn_per_byte_helper state ~factor ~final_size ~hard_limit
  else
    (* For all the blocks that do not contain inboxes, we act as if
       the inbox size was [0]. *)
    let state' =
      update_burn_per_byte_helper state ~factor ~final_size:0 ~hard_limit
    in
    let elapsed = elapsed - 1 in
    update_burn_per_byte state' ~elapsed ~factor ~final_size ~hard_limit

let has_valid_commitment_at {finalized_commitments; unfinalized_commitments; _}
    level =
  belongs_to finalized_commitments level
  || belongs_to unfinalized_commitments level

let inboxes_count {unfinalized_commitments; uncommitted_inboxes; _} =
  range_count unfinalized_commitments + range_count uncommitted_inboxes

let uncommitted_inboxes_count {uncommitted_inboxes; _} =
  range_count uncommitted_inboxes

let commitments_count {finalized_commitments; unfinalized_commitments; _} =
  range_count unfinalized_commitments + range_count finalized_commitments

let record_inbox_creation t level =
  (match t.tezos_head_level with
  | Some tezos_lvl ->
      error_when
        Raw_level_repr.(level <= tezos_lvl)
        (Internal_error "Trying to create an inbox in the past")
  | None -> ok ())
  >>? fun () ->
  let uncommitted_inboxes, new_level = extend t.uncommitted_inboxes in
  adjust_storage_allocation t ~delta:Tx_rollup_inbox_repr.size
  >>? fun (t, diff) ->
  ok
    ( {t with tezos_head_level = Some level; uncommitted_inboxes},
      new_level,
      diff )

let next_commitment_predecessor state = state.commitment_newest_hash

let finalized_commitment_oldest_level state =
  range_oldest state.finalized_commitments

let next_commitment_level state current_level =
  match
    ( range_oldest state.uncommitted_inboxes,
      range_newest state.uncommitted_inboxes )
  with
  | Some oldest_level, Some newest_level -> (
      if
        (* We want to return an error if there is only one inbox in the
           storage, and this inbox has been created in the current
           block. *)
        Tx_rollup_level_repr.(oldest_level < newest_level)
      then
        (* If [oldest_level < newest_level], we know we are not in
           this setup, and we can safely return [oldest_level]. *)
        ok oldest_level
      else
        (* Otherwise, we know that [oldest_level = newest_level], and we
           need to check at which Tezos level is has been created. *)
        match state.tezos_head_level with
        | Some newest_inbox_creation ->
            error_when
              Raw_level_repr.(current_level <= newest_inbox_creation)
              No_uncommitted_inbox
            >>? fun () -> ok oldest_level
        | None -> error (Internal_error "tezos_head_level was not properly set")
      )
  | None, None -> error No_uncommitted_inbox
  | Some _, None | None, Some _ ->
      error (Internal_error "rollup state is inconsistent")

let next_commitment_to_finalize state =
  range_oldest state.unfinalized_commitments

let next_commitment_to_remove state = range_oldest state.finalized_commitments

let record_inbox_deletion state candidate =
  match range_oldest state.unfinalized_commitments with
  | Some level when Tx_rollup_level_repr.(candidate = level) ->
      shrink state.unfinalized_commitments >>? fun unfinalized_commitments ->
      let finalized_commitments, _ = extend state.finalized_commitments in
      ok {state with unfinalized_commitments; finalized_commitments}
  | _ -> error (Internal_error "Trying to delete the wrong inbox")

let record_commitment_creation state level hash =
  match range_oldest state.uncommitted_inboxes with
  | Some oldest ->
      error_unless
        Tx_rollup_level_repr.(level = oldest)
        (Internal_error "Trying to create the wrong commitment")
      >>? fun () ->
      shrink state.uncommitted_inboxes >>? fun uncommitted_inboxes ->
      let unfinalized_commitments, _ = extend state.unfinalized_commitments in
      let state =
        {
          state with
          uncommitted_inboxes;
          unfinalized_commitments;
          commitment_newest_hash = Some hash;
        }
      in
      if is_above_watermark state.commitments_watermark level then
        (* See {{Note inbox}} in [Tx_rollup_commitment_storage] for
           why it is safe to “free” the inbox storage when it is
           committed too. *)
        adjust_storage_allocation state ~delta:(Z.neg Tx_rollup_inbox_repr.size)
        >>? fun (state, _) ->
        ok {state with commitments_watermark = make_watermark level}
      else ok state
  | None ->
      error (Internal_error "Cannot create a commitment due to lack of inbox")

let record_commitment_rejection state level predecessor_hash =
  let unwrap_option msg = function
    | Some x -> ok x
    | _ -> error (Internal_error msg)
  in
  let check_none msg = function
    | None -> ok ()
    | Some _ -> error (Internal_error msg)
  in
  left_extend state.uncommitted_inboxes level >>? fun uncommitted_inboxes ->
  let state = {state with uncommitted_inboxes} in
  right_cut state.unfinalized_commitments level
  >>? fun unfinalized_commitments ->
  match Tx_rollup_level_repr.pred level with
  | Some pred_level
    when belongs_to state.unfinalized_commitments pred_level
         || belongs_to state.finalized_commitments pred_level ->
      (* Case 1. Predecessor level of the rejected commitments has a commitment in the storage *)
      unwrap_option "Missing predecessor commitment" predecessor_hash
      >>? fun predecessor_hash ->
      ok
        {
          state with
          unfinalized_commitments;
          commitment_newest_hash = Some predecessor_hash;
        }
  | Some _ ->
      (* Case 2. Predecessor level of the rejected commitments has its
         commitment removed from the storage *)
      check_none "Unexpected predecessor hash" predecessor_hash >>? fun () ->
      unwrap_option
        "Missing commitment hash"
        state.last_removed_commitment_hashes
      >>? fun (_, pred_hash) ->
      ok
        {
          state with
          unfinalized_commitments;
          commitment_newest_hash = Some pred_hash;
        }
  | None ->
      (* Case 3. The rejected commitment is the commitment of the root level *)
      ok {state with unfinalized_commitments; commitment_newest_hash = None}

let record_commitment_deletion state level hash message_hash =
  match range_oldest state.finalized_commitments with
  | Some oldest when Tx_rollup_level_repr.(level = oldest) ->
      shrink state.finalized_commitments >>? fun finalized_commitments ->
      ok
        {
          state with
          finalized_commitments;
          last_removed_commitment_hashes = Some (message_hash, hash);
        }
  | _ -> error (Internal_error "Trying to remove an incorrect commitment")

let burn_cost ~limit state size =
  Tez_repr.(state.burn_per_byte *? Int64.of_int size) >>? fun burn ->
  match limit with
  | Some limit when Tez_repr.(limit >= burn) ->
      error (Submit_batch_burn_exceeded {burn; limit})
  | _ -> ok burn

let finalized_commitments_range state =
  match
    ( range_oldest state.finalized_commitments,
      range_newest state.finalized_commitments )
  with
  | Some oldest, Some newest -> Some (oldest, newest)
  | _ -> None

let check_level_can_be_rejected state level =
  match
    ( range_oldest state.unfinalized_commitments,
      range_newest state.unfinalized_commitments )
  with
  | Some oldest, Some newest ->
      error_unless Tx_rollup_level_repr.(oldest <= level && level <= newest)
      @@ Cannot_reject_level
           {provided = level; accepted_range = Some (oldest, newest)}
  | _ -> error @@ Cannot_reject_level {provided = level; accepted_range = None}

let last_removed_commitment_hashes state = state.last_removed_commitment_hashes

let head_levels state =
  match (state.uncommitted_inboxes, state.tezos_head_level) with
  | Empty {next = l}, Some tz_level ->
      Option.map (fun l -> (l, tz_level)) (Tx_rollup_level_repr.pred l)
  | Interval {newest; _}, Some tz_level -> Some (newest, tz_level)
  | _ -> None

module Internal_for_tests = struct
  let make :
      ?burn_per_byte:Tez_repr.t ->
      ?inbox_ema:int ->
      ?last_removed_commitment_hashes:
        Tx_rollup_message_result_hash_repr.t * Tx_rollup_commitment_repr.Hash.t ->
      ?finalized_commitments:Tx_rollup_level_repr.t * Tx_rollup_level_repr.t ->
      ?unfinalized_commitments:Tx_rollup_level_repr.t * Tx_rollup_level_repr.t ->
      ?uncommitted_inboxes:Tx_rollup_level_repr.t * Tx_rollup_level_repr.t ->
      ?commitment_newest_hash:Tx_rollup_commitment_repr.Hash.t ->
      ?tezos_head_level:Raw_level_repr.t ->
      ?occupied_storage:Z.t ->
      ?commitments_watermark:Tx_rollup_level_repr.t ->
      allocated_storage:Z.t ->
      unit ->
      t =
   fun ?(burn_per_byte = Tez_repr.zero)
       ?(inbox_ema = 0)
       ?last_removed_commitment_hashes
       ?finalized_commitments
       ?unfinalized_commitments
       ?uncommitted_inboxes
       ?commitment_newest_hash
       ?tezos_head_level
       ?(occupied_storage = Z.zero)
       ?commitments_watermark
       ~allocated_storage
       () ->
    let to_range = function
      | Some (oldest, newest) ->
          assert (Tx_rollup_level_repr.(oldest <= newest)) ;
          Interval {oldest; newest}
      | _ -> Empty {next = Tx_rollup_level_repr.root}
    in

    let unfinalized_commitments = to_range unfinalized_commitments in
    let finalized_commitments = to_range finalized_commitments in
    let uncommitted_inboxes = to_range uncommitted_inboxes in

    {
      last_removed_commitment_hashes;
      burn_per_byte;
      occupied_storage;
      allocated_storage;
      inbox_ema;
      finalized_commitments;
      unfinalized_commitments;
      uncommitted_inboxes;
      commitment_newest_hash;
      tezos_head_level;
      commitments_watermark;
    }

  let get_inbox_ema : t -> int = fun {inbox_ema; _} -> inbox_ema

  let get_occupied_storage : t -> Z.t =
   fun {occupied_storage; _} -> occupied_storage

  let set_occupied_storage : Z.t -> t -> t =
   fun occupied_storage st -> {st with occupied_storage}

  let get_allocated_storage : t -> Z.t =
   fun {allocated_storage; _} -> allocated_storage

  let set_allocated_storage : Z.t -> t -> t =
   fun allocated_storage st -> {st with allocated_storage}

  let reset_commitments_watermark : t -> t =
   fun st -> {st with commitments_watermark = None}

  let get_commitments_watermark : t -> Tx_rollup_level_repr.t option =
   fun st -> st.commitments_watermark
end
