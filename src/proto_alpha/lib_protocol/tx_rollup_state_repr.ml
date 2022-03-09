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

    - The [commitment_tail_level] is the level of the oldest
      finalized commitment still stored in the layer-1 storage.

    - The [commitment_head_level] is the level of the most recent
      unfinalized commitment in the layer-1 storage.

    - The [oldest_inbox_level] is the level of the oldest inbox still stored
      in the layer-1 storage.

    - The [head_level] is the level of the most recent inbox in the
      layer-1 storage.
*)
type t = {
  last_removed_commitment_hashes :
    (Tx_rollup_commitment_repr.Message_result_hash.t
    * Tx_rollup_commitment_repr.Commitment_hash.t)
    option;
  commitment_tail_level : Tx_rollup_level_repr.t option;
  oldest_inbox_level : Tx_rollup_level_repr.t option;
  commitment_head_level :
    (Tx_rollup_level_repr.t * Tx_rollup_commitment_repr.Commitment_hash.t)
    option;
  head_level : (Tx_rollup_level_repr.t * Raw_level_repr.t) option;
  burn_per_byte : Tez_repr.t;
  inbox_ema : int;
}

(*

   The main use of a transaction rollup state is to keep track of four
   pointers to four different rollup levels (see above).

   When the rollup is created, these four pointers are initialized with
   the [None] value, because no inboxes or commitments have been created
   yet. Because inboxes and commitments can be removed from the layer-1
   context under certain circumstances, they can be reset to [None].

   Given [CT] the commitment tail, [OI] the oldest inbox, [CH] the
   commitment head, and [H] the head, a typical rollup state is

     finalized     uncommitted
       ^^^^^^        ^^^^^^^^
      CT           CH
       [------------]            commitments
             [--------------]    inboxes
            OI              H
             ^^^^^^^^
            unfinalized

   Note that the oldest inbox level is not always lesser than the
   commitment head. If the commitment head is equal to [None], it
   means all the inboxes currently stored in layer-1 are “uncommitted”,
   that is, no commitments have been submitted for them.

   In such a case, the rollup state is analoguous to

     finalized
       ^^^^^^
      CT
       [-----[                   commitments
             [--------------]    inboxes
            OI              H
             ^^^^^^^^^^^^^^^^
                uncommitted

   Similarly, it is possible to see the oldest inbox level and head level
   set to [None]. For instance, when each commitment in the
   layer-1 storage has been properly finalized. In this case, the
   layout will be

     finalized
       ^^^^^^
      CT
       [-----[              commitments
             [              inboxes


   When a pointer is reset to [None], its next value will be decided
   by its previous value. For instance, when the oldest inbox level is
   set from [Some l] to [None], its next value is the successor level
   of [l].

   To implement this behavior, this module relies on four fields and
   four functions which share the same name. Once set to a given
   value, the fields are never set back to [None]. It is the purpose
   of the functions to determine if a value is correct, or kept in
   memory for future use, and only the functions are exposed to other
   modules.

   Let’s take the [oldest_inbox_level], for instance. It is supposed
   to be [None] iff there is no “uncommitted” inbox in the context,
   and we can retreive the number of uncommitted inbox by computing
   the difference between the fields [head_level] and
   [oldest_inbox_level].

 *)

let initial_state =
  {
    last_removed_commitment_hashes = None;
    commitment_tail_level = None;
    oldest_inbox_level = None;
    commitment_head_level = None;
    head_level = None;
    burn_per_byte = Tez_repr.zero;
    inbox_ema = 0;
  }

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           last_removed_commitment_hashes;
           commitment_tail_level;
           oldest_inbox_level;
           commitment_head_level;
           head_level;
           burn_per_byte;
           inbox_ema;
         } ->
      ( last_removed_commitment_hashes,
        commitment_tail_level,
        oldest_inbox_level,
        commitment_head_level,
        head_level,
        burn_per_byte,
        inbox_ema ))
    (fun ( last_removed_commitment_hashes,
           commitment_tail_level,
           oldest_inbox_level,
           commitment_head_level,
           head_level,
           burn_per_byte,
           inbox_ema ) ->
      {
        last_removed_commitment_hashes;
        commitment_tail_level;
        oldest_inbox_level;
        commitment_head_level;
        head_level;
        burn_per_byte;
        inbox_ema;
      })
    (obj7
       (req
          "last_removed_commitment_hashes"
          (option
          @@ obj2
               (req
                  "last_message_hash"
                  Tx_rollup_commitment_repr.Message_result_hash.encoding)
               (req
                  "commitment_hash"
                  Tx_rollup_commitment_repr.Commitment_hash.encoding)))
       (req "commitment_tail_level" (option Tx_rollup_level_repr.encoding))
       (req "oldest_inbox_level" (option Tx_rollup_level_repr.encoding))
       (req
          "commitment_head_level"
          (option
             (obj2
                (req "level" Tx_rollup_level_repr.encoding)
                (req "hash" Tx_rollup_commitment_repr.Commitment_hash.encoding))))
       (req
          "head_level"
          (option
             (obj2
                (req "level" Tx_rollup_level_repr.encoding)
                (req "tezos_level" Raw_level_repr.encoding))))
       (req "burn_per_byte" Tez_repr.encoding)
       (req "inbox_ema" int31))

let pp fmt
    {
      last_removed_commitment_hashes;
      commitment_tail_level;
      oldest_inbox_level;
      commitment_head_level;
      head_level;
      burn_per_byte;
      inbox_ema;
    } =
  Format.(
    fprintf
      fmt
      "oldest_inbox_level: %a cost_per_byte: %a inbox_ema: %d head inbox: %a \
       commitment tail: %a commitment head: %a last_removed_commitment_hashes: \
       %a"
      (Format.pp_print_option Tx_rollup_level_repr.pp)
      oldest_inbox_level
      Tez_repr.pp
      burn_per_byte
      inbox_ema
      (pp_print_option (fun fmt (tx_lvl, tezos_lvl) ->
           fprintf
             fmt
             "(rollup level: %a, tezos level: %a)"
             Tx_rollup_level_repr.pp
             tx_lvl
             Raw_level_repr.pp
             tezos_lvl))
      head_level
      (Format.pp_print_option Tx_rollup_level_repr.pp)
      commitment_tail_level
      (pp_print_option (fun fmt (tx_lvl, tezos_lvl) ->
           fprintf
             fmt
             "(rollup level: %a, commitment: %a)"
             Tx_rollup_level_repr.pp
             tx_lvl
             Tx_rollup_commitment_repr.Commitment_hash.pp
             tezos_lvl))
      commitment_head_level
      (pp_print_option (fun fmt (m, c) ->
           fprintf
             fmt
             "(message result: %a, commitment: %a)"
             Tx_rollup_commitment_repr.Message_result_hash.pp
             m
             Tx_rollup_commitment_repr.Commitment_hash.pp
             c))
      last_removed_commitment_hashes)

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

let inboxes_count {head_level; oldest_inbox_level; _} =
  match (oldest_inbox_level, head_level) with
  | (Some oldest_level, Some (newest_level, _)) ->
      let delta =
        (* [Int32.succ] because the range is inclusive, i.e., [l; l]
           has one inbox at level [l]. *)
        Int32.succ @@ Tx_rollup_level_repr.diff newest_level oldest_level
      in
      Compare.Int32.max 0l delta |> Int32.to_int
  | _ -> 0

let committed_inboxes_count {oldest_inbox_level; commitment_head_level; _} =
  match (oldest_inbox_level, commitment_head_level) with
  | (Some oldest_level, Some (newest_level, _)) ->
      let delta =
        (* [Int32.succ] because the range is inclusive, i.e., [l; l]
           has one committed inbox at level [l]. *)
        Int32.succ @@ Tx_rollup_level_repr.diff newest_level oldest_level
      in
      Compare.Int32.max 0l delta |> Int32.to_int
  | _ -> 0

let uncommitted_inboxes_count {head_level; commitment_head_level; _} =
  match (commitment_head_level, head_level) with
  | (Some (oldest_level, _), Some (newest_level, _)) ->
      let delta =
        (* No [Int32.succ] because the range is not inclusive. More
           precisely, the [commitment_head_level] has one commitment,
           and therefore is not part of the count. *)
        Tx_rollup_level_repr.diff newest_level oldest_level
      in
      Compare.Int32.max 0l delta |> Int32.to_int
  | _ -> 0

let finalized_commitments_count {commitment_tail_level; oldest_inbox_level; _} =
  match (commitment_tail_level, oldest_inbox_level) with
  | (Some oldest_level, Some newest_level) ->
      let delta =
        (* No [Int32.succ] because the range is not inclusive. More
           precisely, the [oldest_inbox_level] has not yet been
           finalized. *)
        Tx_rollup_level_repr.diff newest_level oldest_level
      in
      Compare.Int32.max 0l delta |> Int32.to_int
  | _ -> 0

let commitment_head_level state =
  if Compare.Int.(committed_inboxes_count state = 0) then None
  else Option.map fst state.commitment_head_level

let commitment_tail_level state =
  if Compare.Int.(finalized_commitments_count state = 0) then None
  else state.commitment_tail_level

let head_level state =
  if Compare.Int.(inboxes_count state = 0) then None else state.head_level

let oldest_inbox_level state =
  if Compare.Int.(inboxes_count state = 0) then None
  else state.oldest_inbox_level

let next_commitment_level state =
  match (commitment_head_level state, oldest_inbox_level state) with
  | (Some commitment_head, Some _) ->
      ok (Tx_rollup_level_repr.succ commitment_head)
  | (None, Some oldest_inbox) -> ok oldest_inbox
  | (_, _) -> error No_uncommitted_inbox

let next_commitment_predecessor state =
  Option.map snd state.commitment_head_level

let record_inbox_creation t level =
  (match t.head_level with
  | Some (tx_lvl, tezos_lvl) ->
      (if Raw_level_repr.(level <= tezos_lvl) then
       error (Internal_error "Trying to create an inbox in the past")
      else ok ())
      >>? fun () -> ok (Tx_rollup_level_repr.succ tx_lvl)
  | None -> ok Tx_rollup_level_repr.root)
  >>? fun tx_level ->
  let oldest_inbox_level =
    Some (Option.value ~default:tx_level t.oldest_inbox_level)
  in
  ok ({t with head_level = Some (tx_level, level); oldest_inbox_level}, tx_level)

let record_inbox_deletion state candidate =
  match state.oldest_inbox_level with
  | Some level
    when Compare.Int.(0 < inboxes_count state)
         && Tx_rollup_level_repr.(candidate = level) ->
      let oldest_inbox_level = Some (Tx_rollup_level_repr.succ level) in
      ok {state with oldest_inbox_level}
  | _ -> error (Internal_error "Trying to delete the wrong inbox")

let record_commitment_creation state level hash =
  let commitment_tail_level =
    Some (Option.value ~default:level state.commitment_tail_level)
  in
  let state = {state with commitment_tail_level} in
  match (commitment_head_level state, oldest_inbox_level state) with
  | (Some commitment_head, Some _)
    when Tx_rollup_level_repr.(level = succ commitment_head) ->
      ok {state with commitment_head_level = Some (level, hash)}
  | (None, Some oldest) when Tx_rollup_level_repr.(level = oldest) ->
      ok {state with commitment_head_level = Some (level, hash)}
  | _ ->
      error
        (Internal_error "Trying to create a commitment at an incorrect level")

let record_commitment_rejection state level predecessor_hash =
  let unwrap_option msg = function
    | Some x -> ok x
    | _ -> error (Internal_error msg)
  in
  let check_none msg = function
    | None -> ok ()
    | Some _ -> error (Internal_error msg)
  in
  match (oldest_inbox_level state, commitment_head_level state) with
  | (Some inbox_tail, Some commitment_head)
    when Tx_rollup_level_repr.(inbox_tail <= level && level <= commitment_head)
    -> (
      match (commitment_tail_level state, Tx_rollup_level_repr.pred level) with
      | (Some commitment_tail, Some pred_level)
        when Tx_rollup_level_repr.(commitment_tail <= pred_level) ->
          unwrap_option "Missing predecessor commitment" predecessor_hash
          >>? fun pred_hash ->
          ok {state with commitment_head_level = Some (pred_level, pred_hash)}
      | (None, Some pred_level) ->
          check_none "Unexpected predecessor hash" predecessor_hash
          >>? fun () ->
          unwrap_option
            "Missing commitment hash"
            state.last_removed_commitment_hashes
          >>? fun (_, pred_hash) ->
          ok {state with commitment_head_level = Some (pred_level, pred_hash)}
      | (None, None) ->
          check_none
            "Unexpected last removed commitment"
            state.last_removed_commitment_hashes
          >>? fun () -> ok {state with commitment_head_level = None}
      | _ -> error (Internal_error "Machine state inconsistency"))
  | _ -> error (Internal_error "No commitment to reject")

let record_commitment_deletion state level hash message_hash =
  match commitment_tail_level state with
  | Some tail when Tx_rollup_level_repr.(level = tail) ->
      ok
        {
          state with
          commitment_tail_level = Some (Tx_rollup_level_repr.succ tail);
          last_removed_commitment_hashes = Some (message_hash, hash);
        }
  | _ -> error (Internal_error "Trying to remove an incorrect commitment")

let burn_cost ~limit state size =
  Tez_repr.(state.burn_per_byte *? Int64.of_int size) >>? fun burn ->
  match limit with
  | Some limit when Tez_repr.(limit >= burn) ->
      error (Submit_batch_burn_excedeed {burn; limit})
  | _ -> ok burn

let finalized_commitments_range state =
  if Compare.Int.(0 < finalized_commitments_count state) then
    match (state.commitment_tail_level, state.oldest_inbox_level) with
    | (Some commitment_tail, Some oldest_inbox) -> (
        match Tx_rollup_level_repr.pred oldest_inbox with
        | Some res -> ok (Some (commitment_tail, res))
        | None ->
            error
              (Internal_error
                 "oldest inbox has no predecessor, but commitments have been \
                  finalized"))
    | (_, _) ->
        error
          (Internal_error
             "unreachable code per definition of [finalized_commitments_count]")
  else ok None

let check_level_can_be_rejected state level =
  match (oldest_inbox_level state, commitment_head_level state) with
  | (Some oldest, Some newest) ->
      error_unless Tx_rollup_level_repr.(oldest <= level && level <= newest)
      @@ Cannot_reject_level
           {provided = level; accepted_range = Some (oldest, newest)}
  | (_, _) ->
      error @@ Cannot_reject_level {provided = level; accepted_range = None}

let last_removed_commitment_hashes state = state.last_removed_commitment_hashes

module Internal_for_tests = struct
  let make :
      ?burn_per_byte:Tez_repr.t ->
      ?inbox_ema:int ->
      ?last_removed_commitment_hashes:
        Tx_rollup_commitment_repr.Message_result_hash.t
        * Tx_rollup_commitment_repr.Commitment_hash.t ->
      ?commitment_tail_level:Tx_rollup_level_repr.t ->
      ?oldest_inbox_level:Tx_rollup_level_repr.t ->
      ?commitment_head_level:
        Tx_rollup_level_repr.t * Tx_rollup_commitment_repr.Commitment_hash.t ->
      ?head_level:Tx_rollup_level_repr.t * Raw_level_repr.t ->
      unit ->
      t =
   fun ?(burn_per_byte = Tez_repr.zero)
       ?(inbox_ema = 0)
       ?last_removed_commitment_hashes
       ?commitment_tail_level
       ?oldest_inbox_level
       ?commitment_head_level
       ?head_level
       () ->
    {
      last_removed_commitment_hashes;
      burn_per_byte;
      inbox_ema;
      commitment_tail_level;
      oldest_inbox_level;
      commitment_head_level;
      head_level;
    }

  let get_inbox_ema : t -> int = fun {inbox_ema; _} -> inbox_ema
end
