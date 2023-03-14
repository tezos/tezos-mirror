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
parsed by recognizing sub-strings (function [Stats.categorize]), but this relies
on a high-level description of the categories (in [Category.all]).
For instance, a line that contains "Estimated gas: " followed by a decimal
number will be recognized. Whether the line is deleted or added in the diff is
also recorded.

The deleted lines are added in a queue. When an added line is found, it is
compared with the oldest line in the queue. If they match (meaning that they
concern the same kind of gas change; both lines contain "Estimated gas: " for
example), then the synthesis ([Synth.t]) for this kind is updated
([Synths.update]).

The reason for a queue of deleted lines instead of just a single deleted line
is that [git diff] by default does not always produce a sequence of a deletion
followed by an addition. Indeed, we can also find several lines being deleted
and then, after this batch, the corresponding added lines.
*)

let ( let* ) = Option.bind

let ( let+ ) f opt = Option.map opt f

(* [either l a] returns the first function [f] in [l] such that [f a <> None].
*)
let either l a = List.find_map (fun f -> f a) l

module Option = struct
  include Option

  let value_map default f opt = value ~default (map f opt)
end

module Decimal = struct
  module Big_int = struct
    include Big_int

    let ( * ) = mult_big_int
  end

  (* A decimal is represented by its value without decimals (an integer), and
     the number of decimals. *)
  type t = {value : Big_int.big_int; decimals : int}

  let of_int i = {value = Big_int.big_int_of_int i; decimals = 0}

  let zero = of_int 0

  (* [scale r1 r2] returns a triple [(r1', r2', decimals)] where [r1'] (resp.
     [r2']) is mathematically equal to [r1] ([r2]), and [r1'] and [r2'] have
     the same number of decimals ([decimals]). It's just about adding the right
     number of zeros to the one with the fewest decimals.
     This eases some operations on decimal numbers (such as the sum). *)
  let scale r1 r2 =
    let {value = value1; decimals = decimals1} = r1 in
    let {value = value2; decimals = decimals2} = r2 in
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
    {value = Big_int.add_big_int value1 value2; decimals}

  let opp r = {r with value = Big_int.minus_big_int r.value}

  let sub r1 r2 = add r1 (opp r2)

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
    {value; decimals}

  let to_string {value; decimals} =
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

  let ( >= ) = ge

  (* [pct v ref_v] returns the percentage represented by [v] with regards to the
     reference value [ref_v], close to the lower percent. *)
  let pct v ref_v =
    let open Big_int in
    let v, ref_v, _decimals = scale v ref_v in
    let v = mult_big_int v (big_int_of_int 100) in
    try Some {value = div_big_int v ref_v; decimals = 0}
    with Division_by_zero -> None
end

(* Each declared category in [Category.all] will have a dedicated section in the
   output. *)
module Category = struct
  (* Let's simply use the string to match as the representant of a category. *)
  type t = string

  let compare = String.compare

  (* Associated to a category with an amount, a [dir] states whether it's better
     that the amount increases or decreases.
     For example, we'd like the estimated gas to decrease, while we want the gas
     remaining to increase. *)
  type dir = Inc | Dec

  (* Categories have different payloads: a decimal for estimated gas, gas
     remaining, etc., and nothing for the public key hash for instance.
     Even if a category does not have a payload, we still recognize it as such.
  *)
  type case = Decimal_payload_case of dir | No_payload_case

  let increasing_payload = Decimal_payload_case Inc

  let decreasing_payload = Decimal_payload_case Dec

  let no_payload = No_payload_case

  (* All the kinds of lines that are recognized:
     - "Estimated gas: " with an amount (a decimal) that we'd like to decrease;
     - "Gas remaining: " with an amount (a decimal) that we'd like to increase;
     - "PUBLIC_KEY_HASH" with nothing else interesting to record;
     - etc.
     These declarations will be iterated on when initializing syntheses and
     categorizing a line. *)
  let all : (t * case) list =
    [
      ("Estimated gas: ", decreasing_payload);
      ("Estimated storage: ", decreasing_payload);
      ("Consumed gas: ", decreasing_payload);
      ("Gas remaining: ", increasing_payload);
      ("Storage size: ", decreasing_payload);
      ("Gas limit: ", decreasing_payload);
      ("Storage limit: ", decreasing_payload);
      ("just consumed gas: ", decreasing_payload);
      ("Fee to the baker: ꜩ", decreasing_payload);
      ("payload fees(the block proposer) ....... +ꜩ", decreasing_payload);
      ("storage fees ........................... +ꜩ", decreasing_payload);
      ("fee = ", decreasing_payload);
      ("PUBLIC_KEY_HASH", no_payload);
      ("CONTRACT_HASH", no_payload);
      ("Operation hash", no_payload);
      ("tezos-client", no_payload);
      ("New contract", no_payload);
      ("To: ", no_payload);
      ("Parameter: ", no_payload);
    ]
end

module Synth = struct
  (* The synthesis associated to a recognized category. What the synthesis
     contains depends on the type of payload. *)
  open Category

  type dir = Category.dir = Inc | Dec

  type payload = Decimal_payload of Decimal.t | No_payload

  (* When a line is recognized in a git diff, this means that a category was
     found with an expected payload. *)
  type kind = Category.t * payload

  (* Whether a line is removed or added in the git diff. *)
  type diff = Removed | Added

  (* When a line is recognized in the git diff, it is characterized by its kind
     and whether it's deleted or added. *)
  type diff_kind = diff * kind

  (* The synthesis associated to a kind that has an amount for parameter. *)
  type decimal_synth = {
    (* The accumulated amount from deleted lines. *)
    old : Decimal.t;
    (* The accumulated amount from added lines. *)
    new_ : Decimal.t;
    (* The maximum amount loss on a line (with regards to [dir] below). *)
    max_loss : Decimal.t;
    (* The maximum amount loss in percentage on a line (with regards to [dir]
       below), with the line number. *)
    max_loss_pct : (Decimal.t * int) option;
    (* The maximum amount saved on a line (with regards to [dir] below). *)
    max_gain : Decimal.t;
    (* The maximum amount saved in percentage on a line (with regards to [dir]
       below), with the line number. *)
    max_gain_pct : (Decimal.t * int) option;
    (* The number of lines with a degradation (with regards to [dir] below). *)
    degradations : int;
    (* Total number of lines changed. *)
    total : int;
    (* Whether an amelioration is seen when the amount increases or decreases.
    *)
    win : dir;
  }

  let empty_decimal_synth win =
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
    }

  type no_payload_synth = int (* total number of lines changed *)

  type t =
    | Decimal_synth of decimal_synth
    | No_payload_synth of no_payload_synth

  (* Initializing a synth from a category case. *)
  let make_synth = function
    | Decimal_payload_case dir -> Decimal_synth (empty_decimal_synth dir)
    | No_payload_case -> No_payload_synth 0

  (* Initializing all categories. *)
  let categories =
    List.map (fun (category, case) -> (category, make_synth case)) Category.all

  let show_it str
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
      } =
    let open Decimal in
    let total_win = match win with Dec -> old - new_ | Inc -> new_ - old in
    let win_loss = if total_win >= zero then "gain" else "loss" in
    let total_win = if total_win >= zero then total_win else opp total_win in
    let string_of_max_pct = function
      | None -> ""
      | Some (v, l) ->
          " (~" ^ Decimal.to_string v ^ "%, line " ^ string_of_int l ^ ")"
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
      (to_string old)
      (to_string new_)
      win_loss
      percent
      (to_string max_loss)
      (string_of_max_pct max_loss_pct)
      (to_string max_gain)
      (string_of_max_pct max_gain_pct)
      total
      degradations

  let show str = function
    | Decimal_synth synth -> if synth.total <> 0 then show_it str synth
    | No_payload_synth _ -> ()

  let show_unchanged str = function
    | Decimal_synth synth ->
        if synth.total = 0 then Printf.printf "  `%s`\n%!" str
    | No_payload_synth _ -> ()

  let show_ignored str = function
    | Decimal_synth _ -> ()
    | No_payload_synth _ -> Printf.printf "  `%s`\n%!" str

  let get_diff = function '-' -> Some Removed | '+' -> Some Added | _ -> None

  (* [get_dec str cstr line] looks for [str] in [line] and then parses a decimal
     number at the index following the occurrence of [str].
     The function is used to return the amount found on a line of a git diff for
     the various supported categories. *)
  let get_dec sub =
    (* Lets's leave this line below out of the function so that the expression
       is compiled only once for each category, and not every time we call
       [get_dec]. *)
    let re = Re.(compile (seq [str sub; group Decimal.re])) in
    fun line ->
      let* re = Re.exec_opt re line in
      let* dec_str = Re.Group.get_opt re 1 in
      let+ dec = Decimal.of_string dec_str in
      (sub, Decimal_payload dec)

  let get_discarded sub =
    (* Lets's leave this line below out of the function so that the expression
       is compiled only once for each category, and not every time we call
       [get_discarded]. *)
    let re = Re.(compile (str sub)) in
    let contains line re = Re.execp re line in
    fun line -> if contains line re then Some (sub, No_payload) else None

  (* Recognition of a line from a category case. *)
  let make_get_kind = function
    | Decimal_payload_case _dir -> get_dec
    | No_payload_case -> get_discarded

  (* The list of all ways to recognize a line. *)
  let get_kinds =
    List.map (fun (category, case) -> make_get_kind case category) Category.all

  (* [get_kind line] recognizes a line by the first kind-getter that matches it.
     Returns None otherwise. *)
  let get_kind = either get_kinds
end

module Synths = struct
  open Synth
  module CMap = Map.Make (Category)

  (* All the kinds of changes in a git diff for which we build syntheses.

     [previous_kinds] is a queue of deleted parsed lines (with the unparsed line
     kept for error management). An element in this list will be consumed when a
     corresponding added line is found; the syntheses are then updated
     accordingly. *)
  type t = {
    previous_kinds : (string * kind * int (* line number *)) list;
    categories : Synth.t CMap.t;
    total_lines : int;
    total_degradations : int;
  }

  let empty =
    let add_category map (category, synth) = CMap.add category synth map in
    let categories = List.fold_left add_category CMap.empty Synth.categories in
    {previous_kinds = []; categories; total_lines = 0; total_degradations = 0}

  type git_line =
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
    (* Lets's leave this line below out of the function so that the expression
       is compiled only once and for all, i.e. not every time we call
       [categorize]. *)
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

  (* The old and new value of a payload when a deleted and an added line match.
  *)
  type payload_diff =
    | Decimal_payload_diff of Decimal.t * Decimal.t
    | No_payload_diff

  (* [same_kind] returns the category and payload diff between two [diff_kind]s
     if their category is the same and the kind match, and [None] otherwise. *)
  let same_kind (category1, payload1) (category2, payload2) =
    if category1 <> category2 then None
    else
      let+ payload_diff =
        match (payload1, payload2) with
        | Decimal_payload v1, Decimal_payload v2 ->
            Some (Decimal_payload_diff (v1, v2))
        | No_payload, No_payload -> Some No_payload_diff
        | Decimal_payload _, No_payload | No_payload, Decimal_payload _ -> None
      in
      (category1, payload_diff)

  let update_max line_nb old_value old_max_diff old_max_diff_pct diff =
    let open Decimal in
    if diff >= old_max_diff then
      let diff_pct = pct diff old_value in
      let max_diff_pct = Option.map (fun v -> (v, line_nb)) diff_pct in
      (diff, max_diff_pct)
    else (old_max_diff, old_max_diff_pct)

  let update line_nb synth kind_diff =
    match (synth, kind_diff) with
    | Decimal_synth synth, Decimal_payload_diff (old_v, new_v) ->
        let open Decimal in
        let win = synth.win in
        let old = synth.old + old_v in
        let new_ = synth.new_ + new_v in
        let loss =
          match win with Dec -> new_v - old_v | Inc -> old_v - new_v
        in
        let max_loss, max_loss_pct =
          update_max line_nb old_v synth.max_loss synth.max_loss_pct loss
        in
        let max_gain, max_gain_pct =
          update_max line_nb old_v synth.max_gain synth.max_gain_pct (opp loss)
        in
        let open Stdlib in
        let is_degraded = Decimal.gt loss zero in
        let degradations = synth.degradations + if is_degraded then 1 else 0 in
        let total = synth.total + 1 in
        let new_synth =
          Decimal_synth
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
            }
        in
        (new_synth, is_degraded)
    | No_payload_synth total, No_payload_diff ->
        (No_payload_synth (total + 1), false)
    | Decimal_synth _, No_payload_diff
    | No_payload_synth _, Decimal_payload_diff _ ->
        (* Yes, we could avoid this with typing and GADTs for example, but hey,
           that's just a script, let's keep it simple. We're not even explaining
           how the invariant is maintained... *)
        assert false

  (* [consume_kind line line_number synths kind] tries to match the added [line]
     at [line_number], whose [kind] was successfully parsed, with the first
     element of the queue of deleted lines [synths.previous_kinds]. If they
     indeed match, that synthesis in [synths] is updated accordingly. *)
  let consume_kind line line_nb synths kind =
    match synths.previous_kinds with
    | [] ->
        Printf.printf "* No line to consume for line %d `%s`.\n%!" line_nb line ;
        synths
    | (line', previous_kind, line_nb) :: previous_kinds -> (
        match same_kind previous_kind kind with
        | Some (category, kind_diff) ->
            let old_synth = CMap.find category synths.categories in
            let new_synth, is_degraded = update line_nb old_synth kind_diff in
            let categories = CMap.add category new_synth synths.categories in
            let total_lines = synths.total_lines + 1 in
            let total_degradations =
              synths.total_degradations + if is_degraded then 1 else 0
            in
            {previous_kinds; categories; total_lines; total_degradations}
        | None ->
            Printf.printf
              "* At line %d, unmatched lines `%s` and `%s`.\n%!"
              line_nb
              line
              line' ;
            synths)

  let add_line synths line line_nb =
    match categorize line with
    | Garbage -> synths
    | Unsupported ->
        Printf.printf "* Could not parse line %d `%s`.\n%!" line_nb line ;
        synths
    | Diff (Removed, k) ->
        {
          synths with
          previous_kinds = synths.previous_kinds @ [(line, k, line_nb)];
        }
    | Diff (Added, k) -> consume_kind line line_nb synths k

  let show {previous_kinds; categories; total_lines; total_degradations} =
    let count_ignored _ synth total =
      match synth with
      | Decimal_synth _ -> total
      | No_payload_synth total' -> total + total'
    in
    let nb_ignored = CMap.fold count_ignored categories 0 in
    List.iter
      (fun (line, _, line_nb) ->
        Printf.printf "* Leftover at line %d: `%s`.\n%!" line_nb line)
      previous_kinds ;
    Printf.printf "\n%!" ;
    CMap.iter Synth.show categories ;
    Printf.printf "Total number of lines with a change: %d.\n" total_lines ;
    Printf.printf
      "Total number of lines with a degradation: %d.\n"
      total_degradations ;
    Printf.printf "\nLines with the following strings were not changed:\n%!" ;
    CMap.iter Synth.show_unchanged categories ;
    Printf.printf
      "\n%d line(s) with the following strings have changed:\n%!"
      nb_ignored ;
    CMap.iter Synth.show_ignored categories
end

let run file_opt =
  let ic = Option.value_map stdin open_in file_opt in
  let rec read_all line_nb synths =
    try
      let line = input_line ic in
      let synths = Synths.add_line synths line line_nb in
      read_all (succ line_nb) synths
    with End_of_file -> synths
  in
  let synths = read_all 1 Synths.empty in
  if Option.is_some file_opt then close_in ic ;
  Synths.show synths

let () = run (try Some Sys.argv.(1) with Invalid_argument _ -> None)
