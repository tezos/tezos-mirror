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

type error +=
  | Tx_rollup_submit_batch_burn_excedeed of {
      burn : Tez_repr.t;
      limit : Tez_repr.t;
    }

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
*)
type t = {
  first_unfinalized_level : Raw_level_repr.t option;
  unfinalized_level_count : int;
  burn_per_byte : Tez_repr.t;
  inbox_ema : int;
  last_inbox_level : Raw_level_repr.t option;
}

let initial_state =
  {
    first_unfinalized_level = None;
    unfinalized_level_count = 0;
    burn_per_byte = Tez_repr.zero;
    inbox_ema = 0;
    last_inbox_level = None;
  }

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           first_unfinalized_level;
           unfinalized_level_count;
           burn_per_byte;
           inbox_ema;
           last_inbox_level;
         } ->
      ( first_unfinalized_level,
        unfinalized_level_count,
        burn_per_byte,
        inbox_ema,
        last_inbox_level ))
    (fun ( first_unfinalized_level,
           unfinalized_level_count,
           burn_per_byte,
           inbox_ema,
           last_inbox_level ) ->
      {
        first_unfinalized_level;
        unfinalized_level_count;
        burn_per_byte;
        inbox_ema;
        last_inbox_level;
      })
    (obj5
       (req "first_unfinalized_level" (option Raw_level_repr.encoding))
       (req "unfinalized_level_count" int16)
       (req "burn_per_byte" Tez_repr.encoding)
       (req "inbox_ema" int31)
       (req "last_inbox_level" (option Raw_level_repr.encoding)))

let pp fmt
    {
      first_unfinalized_level;
      unfinalized_level_count;
      burn_per_byte;
      inbox_ema;
      last_inbox_level;
    } =
  Format.fprintf
    fmt
    "first_unfinalized_level: %a unfinalized_level_count: %d cost_per_byte: %a \
     inbox_ema: %d newest inbox: %a"
    (Format.pp_print_option Raw_level_repr.pp)
    first_unfinalized_level
    unfinalized_level_count
    Tez_repr.pp
    burn_per_byte
    inbox_ema
    (Format.pp_print_option Raw_level_repr.pp)
    last_inbox_level

let update_burn_per_byte : t -> final_size:int -> hard_limit:int -> t =
 fun ({burn_per_byte; inbox_ema; _} as state) ~final_size ~hard_limit ->
  let threshold_increase = 90 in
  let threshold_decrease = 80 in
  let variation_factor = 5L in
  (* The formula of the multiplier of EMA :

       smoothing / (1 + N)

     Suppose the period we want to observe is an hour and
     producing a block takes 30 seconds, then, N is equal
     to 120. The common choice of smoothing is 2. Therefore,
     multiplier of EMA:

       2 / (1 + 120) ~= 0.0165 *)
  let inbox_ema_multiplier = 165 in
  let inbox_ema =
    ((final_size * inbox_ema_multiplier)
    + (inbox_ema * (10000 - inbox_ema_multiplier)))
    / 10000
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

let burn {burn_per_byte; _} size = Tez_repr.(burn_per_byte *? Int64.of_int size)

let last_inbox_level {last_inbox_level; _} = last_inbox_level

let append_inbox t level =
  {
    t with
    last_inbox_level = Some level;
    first_unfinalized_level =
      Some (Option.value ~default:level t.first_unfinalized_level);
    unfinalized_level_count = t.unfinalized_level_count + 1;
  }

let unfinalized_level_count {unfinalized_level_count; _} =
  unfinalized_level_count

let first_unfinalized_level {first_unfinalized_level; _} =
  first_unfinalized_level

let update_after_finalize state level count =
  {
    state with
    first_unfinalized_level = level;
    unfinalized_level_count = state.unfinalized_level_count - count;
  }

let burn ~limit state size =
  burn state size >>? fun burn ->
  match limit with
  | Some limit when Tez_repr.(limit >= burn) ->
      error (Tx_rollup_submit_batch_burn_excedeed {burn; limit})
  | _ -> ok burn

(* ------ Error registration ------------------------------------------------ *)

let () =
  (* Tx_rollup_submit_batch_burn_excedeed *)
  register_error_kind
    `Permanent
    ~id:"operation.tx_rollup_submit_batch_burn_excedeed"
    ~title:"Submit batch excedeed burn limit"
    ~description:
      "The submit batch would exceed the burn limit, we withdraw the submit."
    ~pp:(fun ppf (burn, limit) ->
      Format.fprintf
        ppf
        "Cannot submit the batch of L2 operations as the cost (%a) would \
         exceed the burn limit (%a)"
        Tez_repr.pp
        burn
        Tez_repr.pp
        limit)
    Data_encoding.(
      obj2 (req "burn" Tez_repr.encoding) (req "limit" Tez_repr.encoding))
    (function
      | Tx_rollup_submit_batch_burn_excedeed {burn; limit} -> Some (burn, limit)
      | _ -> None)
    (fun (burn, limit) -> Tx_rollup_submit_batch_burn_excedeed {burn; limit})

module Internal_for_tests = struct
  let make :
      burn_per_byte:Tez_repr.t ->
      inbox_ema:int ->
      last_inbox_level:Raw_level_repr.t option ->
      t =
   fun ~burn_per_byte ~inbox_ema ~last_inbox_level ->
    {
      burn_per_byte;
      inbox_ema;
      last_inbox_level;
      first_unfinalized_level = None;
      unfinalized_level_count = 0;
    }

  let get_inbox_ema : t -> int = fun {inbox_ema; _} -> inbox_ema
end
