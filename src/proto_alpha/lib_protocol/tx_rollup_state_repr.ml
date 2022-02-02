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
  | Tx_rollup_submit_batch_fees_excedeed of {
      fees : Tez_repr.t;
      limit : Tez_repr.t;
    }

(** The state of a transaction rollup is composed of [fees_per_byte]
    and [inbox_ema] fields. [initial_state] introduces their initial
    values. Both values are updated by [update_fees_per_byte] as the
    rollup progresses.

    [fees_per_byte] state the cost of fees per byte to be paid for
    each byte submitted to a transaction rollup inbox. [inbox_ema]
    is a key factor to impact the update of [fees_per_byte].

    [inbox_ema] is the N-block EMA to react to recent N-inbox size
    changes. N-block EMA is an exponential moving average (EMA), that
    is a type of moving average that places a greater weight and
    significance on the most N data points. The purpose of [inbox_ema]
    is to get lessened volatility of fees, that is more resistant to
    spurious spikes of [fees_per_byte].
*)
type t = {
  fees_per_byte : Tez_repr.t;
  inbox_ema : int;
  last_inbox_level : Raw_level_repr.t option;
}

let initial_state =
  {fees_per_byte = Tez_repr.zero; inbox_ema = 0; last_inbox_level = None}

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {last_inbox_level; fees_per_byte; inbox_ema} ->
      (last_inbox_level, fees_per_byte, inbox_ema))
    (fun (last_inbox_level, fees_per_byte, inbox_ema) ->
      {last_inbox_level; fees_per_byte; inbox_ema})
    (obj3
       (req "last_inbox_level" (option Raw_level_repr.encoding))
       (req "fees_per_byte" Tez_repr.encoding)
       (req "inbox_ema" int31))

let pp fmt {fees_per_byte; last_inbox_level; inbox_ema} =
  Format.fprintf
    fmt
    "Tx_rollup: fees_per_byte = %a; inbox_ema %d; last_inbox_level = %a"
    Tez_repr.pp
    fees_per_byte
    inbox_ema
    (Format.pp_print_option Raw_level_repr.pp)
    last_inbox_level

let update_fees_per_byte : t -> final_size:int -> hard_limit:int -> t =
 fun ({fees_per_byte; inbox_ema; _} as state) ~final_size ~hard_limit ->
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
      ok fees_per_byte
    else
      Tez_repr.(fees_per_byte *? variation_factor >>? fun x -> x /? 100L)
      >>? fun variation ->
      let variation =
        if Tez_repr.(variation = zero) then Tez_repr.one_mutez else variation
      in
      (* increase case *)
      if threshold_increase < percentage then
        Tez_repr.(fees_per_byte +? variation)
      else if percentage < threshold_decrease && Tez_repr.(zero < fees_per_byte)
      then
        (* decrease case, and strictly positive fees *)
        Tez_repr.(fees_per_byte -? variation)
      else (* decrease case, and fees equals zero *)
        ok fees_per_byte
  in
  match computation with
  | Ok fees_per_byte -> {state with fees_per_byte; inbox_ema}
  (* In the (very unlikely) event of an overflow, we force the fees to
     be the maximum amount. *)
  | Error _ -> {state with fees_per_byte = Tez_repr.max_mutez; inbox_ema}

let fees {fees_per_byte; _} size = Tez_repr.(fees_per_byte *? Int64.of_int size)

let last_inbox_level {last_inbox_level; _} = last_inbox_level

let append_inbox t level = {t with last_inbox_level = Some level}

let fees ~limit state size =
  let fees = fees state size in
  fees >>? fun fees ->
  match limit with
  | Some limit when Tez_repr.(limit >= fees) ->
      error (Tx_rollup_submit_batch_fees_excedeed {fees; limit})
  | _ -> ok fees

(* ------ Error registration ------------------------------------------------ *)

let () =
  (* Tx_rollup_submit_batch_fees_excedeed *)
  register_error_kind
    `Permanent
    ~id:"operation.tx_rollup_submit_batch_fees_excedeed"
    ~title:"Submit batch excedeed fees limit"
    ~description:
      "The submit batch would exceed the fees limit, we withdraw the submit."
    ~pp:(fun ppf (fees, limit) ->
      Format.fprintf
        ppf
        "Cannot submit the batch of L2 operations as the cost (%a) would \
         exceed the fees limit (%a)"
        Tez_repr.pp
        fees
        Tez_repr.pp
        limit)
    Data_encoding.(
      obj2 (req "fees" Tez_repr.encoding) (req "limit" Tez_repr.encoding))
    (function
      | Tx_rollup_submit_batch_fees_excedeed {fees; limit} -> Some (fees, limit)
      | _ -> None)
    (fun (fees, limit) -> Tx_rollup_submit_batch_fees_excedeed {fees; limit})

module Internal_for_tests = struct
  let make :
      fees_per_byte:Tez_repr.t ->
      inbox_ema:int ->
      last_inbox_level:Raw_level_repr.t option ->
      t =
   fun ~fees_per_byte ~inbox_ema ~last_inbox_level ->
    {fees_per_byte; inbox_ema; last_inbox_level}

  let get_inbox_ema : t -> int = fun {inbox_ema; _} -> inbox_ema
end
