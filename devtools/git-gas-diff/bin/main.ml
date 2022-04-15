(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(*
[git-gas-diff] is a tool that outputs a synthesis of gas changes from a git
diff on regression traces.

Typical usage is to pipe a git diff that contains gas changes with the tool:
  git diff HEAD^ HEAD | dune exec git-gas-diff

The script reads a file (or the standard input if no file is provided) line by
line in the [run] function.
It ignores lines that do not start with '+' or '-', and the ones that start with
"+++" and "---".

Other lines (the ones that start with '+' or '-', but not "+++" nor "---") are
parsed by recognizing sub-strings (function [Stats.categorize]).
For instance, a line that contains "Estimated gas: " followed by a decimal
number [n] will be parsed into [Stat.Estimated n]. Whether the line is deleted
or added in the diff is also recognized.

The deleted lines are added in a queue. When an added line is found, it is
compared with the oldest line in the queue. If they match (meaning that they
concern the same kind of gas change; both lines contain "Estimated gas: " for
example), then the synthesis ([Synth.synth]) for this kind is updated
([Synths.update]).

The reason for a queue of deleted lines instead of just a single deleted line
is that [git diff] does not always produce a sequence of a deletion followed by
an addition. Indeed, we can also find several lines being deleted and then,
after this batch, the corresponding added lines.
*)

let ( let* ) = Option.bind

let ( let+ ) f opt = Option.map opt f

let either l a = List.find_map (fun f -> f a) l

module Option = struct
  include Option

  let value_map default f opt = value ~default (map f opt)
end

module Decimal = struct
  module Big_int = struct
    include Big_int

    let ( * ) = mult_big_int

    let ( + ) = add_big_int
  end

  (* A decimal is represented by its value without decimals (an integer), and
     the number of decimals. *)
  type t = { value : Big_int.big_int; decimals : int }

  let of_int i = { value = Big_int.big_int_of_int i; decimals = 0 }

  let zero = of_int 0

  let one = of_int 1

  (* [scale r1 r2] returns a triple [(r1', r2', decimals)] where [r1'] (resp.
     [r2']) is mathematically equal to [r1] ([r2]), and [r1'] and [r2'] have
     the same number of decimals ([decimals]). It's just about adding the right
     number of zeros to the one with the fewest decimals.
     This eases some operations on decimal numbers (such as the sum). *)
  let scale r1 r2 =
    let { value = value1; decimals = decimals1 } = r1 in
    let { value = value2; decimals = decimals2 } = r2 in
    let decimals = max decimals1 decimals2 in
    let pow10 = abs (decimals1 - decimals2) in
    let scale_value v = Big_int.(v * power_int_positive_int 10 pow10) in
    let value1, value2 =
      if decimals1 >= decimals2 then (value1, scale_value value2)
      else (scale_value value1, value2)
    in
    (value1, value2, decimals)

  let add r1 r2 =
    let value1, value2, decimals = scale r1 r2 in
    { value = Big_int.add_big_int value1 value2; decimals }

  let opp r = { r with value = Big_int.minus_big_int r.value }

  let sub r1 r2 = add r1 (opp r2)

  let mul r1 r2 =
    let { value = value1; decimals = decimals1 } = r1 in
    let { value = value2; decimals = decimals2 } = r2 in
    {
      value = Big_int.mult_big_int value1 value2;
      decimals = decimals1 + decimals2;
    }

  let ge r1 r2 =
    let value1, value2, _decimals = scale r1 r2 in
    Big_int.ge_big_int value1 value2

  let gt r1 r2 =
    let value1, value2, _decimals = scale r1 r2 in
    Big_int.gt_big_int value1 value2

  let re = Re.Posix.re "([0-9]+)(\\.([0-9]*))?"

  let re_t = Re.compile re

  let of_string s =
    let open Re in
    let* group = exec_opt re_t s in
    let* int_part = Group.get_opt group 1 in
    let dec_part = Option.value ~default:"" (Group.get_opt group 3) in
    let+ value = Big_int.big_int_of_string_opt (int_part ^ dec_part) in
    let decimals = String.length dec_part in
    { value; decimals }

  let to_string { value; decimals } =
    let pow10 = Big_int.power_int_positive_int 10 decimals in
    let int_part, dec_part = Big_int.quomod_big_int value pow10 in
    let int_part = Big_int.string_of_big_int int_part in
    let dec_part =
      if Big_int.(eq_big_int dec_part zero_big_int) then ""
      else
        let dec_part = Big_int.string_of_big_int dec_part in
        "." ^ String.make (decimals - String.length dec_part) '0' ^ dec_part
    in
    int_part ^ dec_part

  let ( + ) = add

  let ( - ) = sub

  let ( * ) = mul

  let ( >= ) = ge

  let ( > ) = gt

  let max r1 r2 = if r1 >= r2 then r1 else r2

  (* [pct v ref_v] returns the percentage represented by [v] with regards to the
     reference value [ref_v], close to the lower percent. *)
  let pct v ref_v =
    let open Big_int in
    let v, ref_v, _decimals = scale v ref_v in
    let v = mult_big_int v (big_int_of_int 100) in
    try Some { value = div_big_int v ref_v; decimals = 0 }
    with Division_by_zero -> None
end

module Synth = struct
  (* All the kinds of lines that are recognized:
     - [Estimated] is for a line with "Estimated gas: " and has the amount for
       parameter;
     - [Consumed] is for a line with "Consumed gas: " and has the amount for
       parameter;
     - etc.
     (The recognized strings are defined further below.) *)
  type kind =
    | Estimated of Decimal.t
    | Consumed of Decimal.t
    | Gas_remaining of Decimal.t
    | Gas_limit of Decimal.t
    | Remaining_gas of Decimal.t
    | Baker_fee of Decimal.t
    | Payload_fee of Decimal.t
    | Fee of Decimal.t
    | Hash
    | Tezos_client
    | Operation_hash
    | New_contract
    | To
    | Parameter

  (* Whether a line is removed or added in the git diff. *)
  type diff = Removed | Added

  (* When a line is recognized in the git diff, it is characterized by its kind
     and whether it's deleted or added. *)
  type diff_kind = diff * kind

  (* Associated to a kind with an amount, a [dir] states whether it's better
     that the amount increases or decreases.
     For example, we'd like the estimated gas to decrease, while we want the gas
     remaining to increase. *)
  type dir = Inc | Dec

  (* The synthesis associated to a kind that has an amount for parameter. *)
  type synth = {
    old : Decimal.t;
    (* The accumulated amount from deleted lines. *)
    new_ : Decimal.t;
    (* The accumulated amount from added lines. *)
    (* The maximum amount loss on a line (with regards to [dir] below). *)
    max_loss : Decimal.t;
    (* The maximum amount loss in percentage on a line (with regards to [dir]
       below). *)
    max_loss_pct : Decimal.t option;
    (* The maximum amount saved on a line (with regards to [dir] below). *)
    max_gain : Decimal.t;
    (* The maximum amount saved in percentage on a line (with regards to [dir]
       below). *)
    max_gain_pct : Decimal.t option;
    (* The number of lines with a degradation (with regards to [dir] below).
      *)
    degradations : int;
    total : int;
    (* Total number of lines changed. *)
    (* Whether an amelioration is seen when the amount increases or decreases.
     *)
    win : dir;
    (* The sub-string to be matched by lines that will be included in this
       synthesis. *)
    str : string;
  }

  let estimated_str = "Estimated gas: "

  let consumed_str = "Consumed gas: "

  let gas_remaining_str = "Gas remaining: "

  let gas_limit_str = "Gas limit: "

  let remaining_gas_str = "remaining gas: "

  let baker_fee_str = "Fee to the baker: ꜩ"

  let payload_fee_str = "payload fees(the block proposer) ....... +ꜩ"

  let fee_str = "fee = "

  let hash_strs = [ "PUBLIC_KEY_HASH"; "CONTRACT_HASH" ]

  let operation_hash_strs = [ "Operation hash" ]

  let tezos_client_strs = [ "tezos-client" ]

  let new_contract_strs = [ "New contract" ]

  let to_strs = [ "To: " ]

  let parameter_strs = [ "Parameter: " ]

  let empty_synth str win =
    {
      old = Decimal.zero;
      new_ = Decimal.zero;
      max_loss = Decimal.zero;
      max_loss_pct = None;
      max_gain = Decimal.zero;
      max_gain_pct = None;
      degradations = 0;
      total = 0;
      win;
      str;
    }

  let show
      {
        old;
        new_;
        max_loss;
        max_loss_pct;
        max_gain;
        max_gain_pct;
        degradations;
        total;
        win;
        str;
      } =
    let open Decimal in
    let total_win = match win with Dec -> old - new_ | Inc -> new_ - old in
    let win_loss = if total_win >= zero then "gain" else "loss" in
    let total_win = if total_win >= zero then total_win else opp total_win in
    let string_of_max_pct = function
      | None -> ""
      | Some v -> " (~" ^ Decimal.to_string v ^ "%)"
    in
    let percent =
      Option.value_map "N/A" Decimal.to_string (pct total_win old)
    in
    Printf.printf
      "Lines with `%s`:\n\
      \  (Better means the value must %srease.)\n\
      \  Accumulated value before: %s\n\
      \  Accumulated value now:    %s\n\
      \  Total %s: ~%s%%\n\
      \  Maximum loss on a line: %s%s\n\
      \  Maximum gain on a line: %s%s\n\
      \  Number of lines with a change:      %d\n\
      \  Number of lines with a degradation: %d\n\n\
       %!"
      str
      (match win with Dec -> "dec" | Inc -> "inc")
      (to_string old) (to_string new_) win_loss percent (to_string max_loss)
      (string_of_max_pct max_loss_pct)
      (to_string max_gain)
      (string_of_max_pct max_gain_pct)
      total degradations

  let get_diff = function '-' -> Some Removed | '+' -> Some Added | _ -> None

  (* [get_dec str cstr line] looks for [str] in [line] and then parses a decimal
     number at the index following the occurrence of [str]. Finally, it applies
     [cstr] to the obtained decimal.
     The function is used to return the amount found on a line of a git diff for
     the various supported categories. *)
  let get_dec sub cstr =
    (* Lets's leave this line below out of the function so that the expression is
       compiled only once for each [get_*], and not every time we call
       [get_dec]. *)
    let re = Re.(compile (seq [ str sub; group Decimal.re ])) in
    fun line ->
      let* re = Re.exec_opt re line in
      let* dec_str = Re.Group.get_opt re 1 in
      let+ dec = Decimal.of_string dec_str in
      cstr dec

  let get_estimated = get_dec estimated_str (fun v -> Estimated v)

  let get_consumed = get_dec consumed_str (fun v -> Consumed v)

  let get_gas_remaining = get_dec gas_remaining_str (fun v -> Gas_remaining v)

  let get_gas_limit = get_dec gas_limit_str (fun v -> Gas_limit v)

  let get_remaining_gas = get_dec remaining_gas_str (fun v -> Remaining_gas v)

  let get_baker_fee = get_dec baker_fee_str (fun v -> Baker_fee v)

  let get_payload_fee = get_dec payload_fee_str (fun v -> Payload_fee v)

  let get_fee = get_dec fee_str (fun v -> Fee v)

  let get_discarded strs res =
    (* Lets's leave this line below out of the function so that the expression is
       compiled only once for each [get_*], and not every time we call
       [get_discarded]. *)
    let re_ts = List.map (fun sub -> Re.(compile (str sub))) strs in
    let contains line re_t = Re.execp re_t line in
    fun line -> if List.exists (contains line) re_ts then Some res else None

  let get_hash = get_discarded hash_strs Hash

  let get_tezos_client = get_discarded tezos_client_strs Tezos_client

  let get_operation_hash = get_discarded operation_hash_strs Operation_hash

  let get_new_contract = get_discarded new_contract_strs New_contract

  let get_to = get_discarded to_strs To

  let get_parameter = get_discarded parameter_strs Parameter

  let get_kind line =
    either
      [
        get_estimated;
        get_consumed;
        get_gas_remaining;
        get_gas_limit;
        get_remaining_gas;
        get_baker_fee;
        get_hash;
        get_payload_fee;
        get_fee;
        get_tezos_client;
        get_operation_hash;
        get_new_contract;
        get_to;
        get_parameter;
      ]
      line
end

module Synths = struct
  open Synth

  (* All the kinds of changes in a git diff for which we build statistics.

     [previous_kinds] is a queue of deleted parsed lines (with the unparsed line
     kept for error management). An element in this list will be consumed when a
     corresponding added line is found; the statistics are then updated
     accordingly. *)
  type t = {
    previous_kinds : (string * kind) list;
    estimated : synth;
    consumed : synth;
    gas_remaining : synth;
    gas_limit : synth;
    remaining_gas : synth;
    baker_fee : synth;
    payload_fee : synth;
    fee : synth;
  }

  let empty =
    {
      previous_kinds = [];
      estimated = empty_synth estimated_str Dec;
      consumed = empty_synth consumed_str Dec;
      gas_remaining = empty_synth gas_remaining_str Inc;
      gas_limit = empty_synth gas_limit_str Dec;
      remaining_gas = empty_synth remaining_gas_str Inc;
      baker_fee = empty_synth baker_fee_str Dec;
      payload_fee = empty_synth payload_fee_str Dec;
      fee = empty_synth fee_str Dec;
    }

  type category =
    (* Lines that don't start with a '+' or a '-', or that start with "+++" or
       "---". We are not interested in those. *)
    | Garbage
    (* Lines that start with a '+' or a '-' (but not "+++" nor "---"), but for
       which the script didn't recognize a kind. *)
    | Unsupported
    (* Lines that start with a '+' or a '-' (but not "+++" nor "---"), and that
       are correctly parsed by the script (i.e. the kind was recognized. *)
    | Diff of diff_kind

  let categorize =
    (* Lets's leave this line below out of the function so that the expression is
       compiled only once and for all, i.e. not every time we call [categorize].
    *)
    let re_t = Re.Posix.(compile (re "^(\\+\\+\\+)|(---)")) in
    fun line ->
      let length = String.length line in
      let is_git_garbage = Re.execp re_t line in
      if length = 0 then Garbage
      else
        match (get_diff line.[0], get_kind line) with
        | None, _ -> Garbage
        | Some _, _ when is_git_garbage -> Garbage
        | Some _, None -> Unsupported
        | Some diff, Some kind -> Diff (diff, kind)

  let same_kind kind1 kind2 =
    match (kind1, kind2) with
    | Estimated _, Estimated _
    | Consumed _, Consumed _
    | Gas_remaining _, Gas_remaining _
    | Gas_limit _, Gas_limit _
    | Remaining_gas _, Remaining_gas _
    | Baker_fee _, Baker_fee _
    | Payload_fee _, Payload_fee _
    | Fee _, Fee _
    | Hash, Hash ->
        true
    | Tezos_client, Tezos_client -> true
    | Operation_hash, Operation_hash -> true
    | New_contract, New_contract -> true
    | To, To -> true
    | Parameter, Parameter -> true
    | _ (* we shouldn't be using a joker here... *) -> false

  let builder = function
    | Estimated v ->
        Some
          ( v,
            (fun synth -> synth.estimated),
            fun estimated synths -> { synths with estimated } )
    | Consumed v ->
        Some
          ( v,
            (fun synth -> synth.consumed),
            fun consumed synths -> { synths with consumed } )
    | Gas_remaining v ->
        Some
          ( v,
            (fun synth -> synth.gas_remaining),
            fun gas_remaining synths -> { synths with gas_remaining } )
    | Gas_limit v ->
        Some
          ( v,
            (fun synth -> synth.gas_limit),
            fun gas_limit synths -> { synths with gas_limit } )
    | Remaining_gas v ->
        Some
          ( v,
            (fun synth -> synth.remaining_gas),
            fun remaining_gas synths -> { synths with remaining_gas } )
    | Baker_fee v ->
        Some
          ( v,
            (fun synth -> synth.baker_fee),
            fun baker_fee synths -> { synths with baker_fee } )
    | Payload_fee v ->
        Some
          ( v,
            (fun synth -> synth.payload_fee),
            fun payload_fee synths -> { synths with payload_fee } )
    | Fee v ->
        Some (v, (fun synth -> synth.fee), fun fee synths -> { synths with fee })
    | Hash | Tezos_client | Operation_hash | New_contract | To | Parameter ->
        None

  let extract_value kind =
    let+ v, _, _ = builder kind in
    v

  let builders old_kind new_kind =
    let* old_v, getter, setter = builder old_kind in
    let+ new_v = extract_value new_kind in
    (old_v, new_v, getter, setter)

  let update_max old_value max_diff old_max_diff_pct =
    let open Decimal in
    let max_diff_pct = pct max_diff old_value in
    match (old_max_diff_pct, max_diff_pct) with
    | None, _ -> max_diff_pct
    | _, None -> old_max_diff_pct
    | Some max_loss_pct, Some max_loss_pct' ->
        Some (max max_loss_pct max_loss_pct')

  let update synth old_v new_v =
    let open Decimal in
    let win = synth.win in
    let old = synth.old + old_v in
    let new_ = synth.new_ + new_v in
    let loss = match win with Dec -> new_v - old_v | Inc -> old_v - new_v in
    let max_loss = max synth.max_loss loss in
    let max_loss_pct = update_max old_v max_loss synth.max_loss_pct in
    let max_gain = max synth.max_gain (opp loss) in
    let max_gain_pct = update_max old_v max_gain synth.max_gain_pct in
    let open Stdlib in
    let degradations =
      synth.degradations + if Decimal.gt loss zero then 1 else 0
    in
    let total = synth.total + 1 in
    {
      old;
      new_;
      max_loss;
      max_loss_pct;
      max_gain;
      max_gain_pct;
      degradations;
      total;
      win;
      str = synth.str;
    }

  (* [consume_kind line synths kind] tries to match the added line [line], that
     was successfully parsed as [kind], with the first element of the queue of
     deleted lines [synths.previous_kinds]. If they indeed match, that synthesis
     in [synths] is updated accordingly. *)
  let consume_kind line synths kind =
    match synths.previous_kinds with
    | [] ->
        Printf.printf "* No line to consume `%s`.\n%!" line;
        synths
    | (_, previous_kind) :: previous_kinds when same_kind previous_kind kind ->
        let synths =
          match builders previous_kind kind with
          | None -> synths
          | Some (old_v, new_v, getter, setter) ->
              setter (update (getter synths) old_v new_v) synths
        in
        { synths with previous_kinds }
    | (line', _) :: _ ->
        Printf.printf "* Unmatched lines `%s` and `%s`.\n%!" line line';
        synths

  let add_line synths line =
    match categorize line with
    | Garbage -> synths
    | Unsupported ->
        Printf.printf "* Could not parse `%s`.\n%!" line;
        synths
    | Diff (Removed, k) ->
        { synths with previous_kinds = synths.previous_kinds @ [ (line, k) ] }
    | Diff (Added, k) -> consume_kind line synths k

  let show
      {
        previous_kinds;
        estimated;
        consumed;
        gas_remaining;
        gas_limit;
        remaining_gas;
        baker_fee;
        payload_fee;
        fee;
      } =
    List.iter
      (fun (line, _) -> Printf.printf "* Leftover line: `%s`.\n%!" line)
      previous_kinds;
    Printf.printf "\n%!";
    Synth.show estimated;
    Synth.show consumed;
    Synth.show gas_remaining;
    Synth.show gas_limit;
    Synth.show remaining_gas;
    Synth.show baker_fee;
    Synth.show payload_fee;
    Synth.show fee;
    Printf.printf "Lines with the following strings were ignored:\n%!";
    List.iter
      (Printf.printf "  `%s`\n%!")
      (hash_strs @ tezos_client_strs @ operation_hash_strs @ new_contract_strs
     @ to_strs @ parameter_strs)
end

let run file_opt =
  let ic = Option.value_map stdin open_in file_opt in
  let rec read_all line_number synths =
    try
      (* Printf.printf "%d\n%!" line_number ; *)
      let line = input_line ic in
      let synths = Synths.add_line synths line in
      read_all (succ line_number) synths
    with End_of_file -> synths
  in
  let synths = read_all 1 Synths.empty in
  if Option.is_some file_opt then close_in ic;
  Synths.show synths

let () = run (try Some Sys.argv.(1) with Invalid_argument _ -> None)
