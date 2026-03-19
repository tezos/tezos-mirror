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

type t = {
  level : Raw_level_repr.t;
  level_position : int32;
  cycle : Cycle_repr.t;
  cycle_position : int32;
  expected_commitment : bool;
}

module Ordered = struct
  type nonrec t = t

  let compare {level = l1; _} {level = l2; _} = Raw_level_repr.compare l1 l2
end

include Compare.Make (Ordered)
module Map = Map.Make (Ordered)
module Set = Set.Make (Ordered)

type level = t

let pp ppf {level; _} = Raw_level_repr.pp ppf level

let pp_full ppf l =
  Format.fprintf
    ppf
    "%a.%ld (cycle %a.%ld)"
    Raw_level_repr.pp
    l.level
    l.level_position
    Cycle_repr.pp
    l.cycle
    l.cycle_position

let encoding =
  let open Data_encoding in
  conv
    (fun {level; level_position; cycle; cycle_position; expected_commitment} ->
      (level, level_position, cycle, cycle_position, expected_commitment))
    (fun (level, level_position, cycle, cycle_position, expected_commitment) ->
      {level; level_position; cycle; cycle_position; expected_commitment})
    (obj5
       (req
          "level"
          ~description:
            "The level of the block relative to genesis. This is also the \
             Shell's notion of level."
          Raw_level_repr.encoding)
       (req
          "level_position"
          ~description:
            "The level of the block relative to the successor of the genesis \
             block. More precisely, it is the position of the block relative \
             to the block that starts the \"Alpha family\" of protocols, which \
             includes all protocols except Genesis (that is, from 001 \
             onwards)."
          int32)
       (req
          "cycle"
          ~description:
            "The current cycle's number. Note that cycles are a \
             protocol-specific notion. As a result, the cycle number starts at \
             0 with the first block of the Alpha family of protocols."
          Cycle_repr.encoding)
       (req
          "cycle_position"
          ~description:
            "The current level of the block relative to the first block of the \
             current cycle."
          int32)
       (req
          "expected_commitment"
          ~description:
            "Tells whether the baker of this block has to commit a seed nonce \
             hash."
          bool))

let diff {level = l1; _} {level = l2; _} =
  Int32.sub (Raw_level_repr.to_int32 l1) (Raw_level_repr.to_int32 l2)

type cycle_era = {
  first_level : Raw_level_repr.t;
  first_cycle : Cycle_repr.t;
  blocks_per_cycle : int32;
  blocks_per_commitment : int32;
}

type cycle_eras = cycle_era list

type error += Invalid_cycle_eras

let () =
  register_error_kind
    `Temporary
    ~id:"level_repr.invalid_cycle_eras"
    ~title:"Invalid cycle eras"
    ~description:
      "The cycles eras are not valid: empty list or non-decreasing first \
       levels or first cycles."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The cycles eras are not valid: empty list or non-decreasing first \
         levels or first cycles.")
    Data_encoding.empty
    (function Invalid_cycle_eras -> Some () | _ -> None)
    (fun () -> Invalid_cycle_eras)

let create_cycle_eras cycle_eras =
  let open Result_syntax in
  match cycle_eras with
  | [] -> tzfail Invalid_cycle_eras
  | newest_era :: older_eras ->
      let rec aux {first_level; first_cycle; _} older_eras =
        match older_eras with
        | ({
             first_level = first_level_of_previous_era;
             first_cycle = first_cycle_of_previous_era;
             _;
           } as previous_era)
          :: even_older_eras ->
            if
              Raw_level_repr.(first_level > first_level_of_previous_era)
              && Cycle_repr.(first_cycle > first_cycle_of_previous_era)
            then aux previous_era even_older_eras
            else tzfail Invalid_cycle_eras
        | [] -> return_unit
      in
      let* () = aux newest_era older_eras in
      return cycle_eras

let add_cycle_era new_era cycle_eras = create_cycle_eras (new_era :: cycle_eras)

let current_era = function [] -> assert false | cycle_era :: _ -> cycle_era

let update_cycle_eras cycle_eras ~(level : int32) ~prev_blocks_per_cycle
    ~new_blocks_per_cycle ~prev_blocks_per_commitment ~new_blocks_per_commitment
    =
  let previous_era = current_era cycle_eras in
  (* making sure that previous protocol's constants match the last cycle era *)
  assert (Compare.Int32.(prev_blocks_per_cycle = previous_era.blocks_per_cycle)) ;
  assert (
    Compare.Int32.(
      prev_blocks_per_commitment = previous_era.blocks_per_commitment)) ;
  let current_cycle =
    let level_position =
      Int32.sub level (Raw_level_repr.to_int32 previous_era.first_level)
    in
    Cycle_repr.add
      previous_era.first_cycle
      (Int32.to_int (Int32.div level_position prev_blocks_per_cycle))
  in
  let new_cycle_era =
    {
      first_level = Raw_level_repr.of_int32_exn (Int32.succ level);
      first_cycle = Cycle_repr.succ current_cycle;
      blocks_per_cycle = new_blocks_per_cycle;
      blocks_per_commitment = new_blocks_per_commitment;
    }
  in
  add_cycle_era new_cycle_era cycle_eras

let cycle_era_encoding =
  let open Data_encoding in
  conv
    (fun {first_level; first_cycle; blocks_per_cycle; blocks_per_commitment} ->
      (first_level, first_cycle, blocks_per_cycle, blocks_per_commitment))
    (fun (first_level, first_cycle, blocks_per_cycle, blocks_per_commitment) ->
      {first_level; first_cycle; blocks_per_cycle; blocks_per_commitment})
    (obj4
       (req
          "first_level"
          ~description:"The first level of a new cycle era."
          Raw_level_repr.encoding)
       (req
          "first_cycle"
          ~description:"The first cycle of a new cycle era."
          Cycle_repr.encoding)
       (req
          "blocks_per_cycle"
          ~description:
            "The value of the blocks_per_cycle constant used during the cycle \
             era starting with first_level."
          int32)
       (req
          "blocks_per_commitment"
          ~description:
            "The value of the blocks_per_commitment constant used during the \
             cycle era starting with first_level."
          int32))

let cycle_eras_encoding =
  Data_encoding.conv_with_guard
    (fun eras -> eras)
    (fun eras ->
      match create_cycle_eras eras with
      | Ok eras -> Ok eras
      | Error _ -> Error "Invalid cycle eras")
    (Data_encoding.list cycle_era_encoding)

let root_level cycle_eras =
  let first_era = List.last_opt cycle_eras in
  let first_era =
    match first_era with
    | Some first_era -> first_era
    | None ->
        (* {!create_cycle_eras} fails if the list is empty.
           {!cycle_eras_encoding} uses {!create_cycle_eras} and so fails on empty
           lists too. *)
        assert false
  in
  {
    level = first_era.first_level;
    level_position = 0l;
    cycle = Cycle_repr.root;
    cycle_position = 0l;
    expected_commitment = false;
  }

let level_zero_use_with_care =
  {
    level = Raw_level_repr.root;
    level_position = 0l;
    cycle = Cycle_repr.root;
    cycle_position = 0l;
    expected_commitment = false;
  }

(* This function returns the cycle era to which [level] belongs. *)
let era_of_level ~cycle_eras level =
  let rec aux = function
    | ({first_level; _} as era) :: previous_eras ->
        if Raw_level_repr.(level >= first_level) then era else aux previous_eras
    | [] -> assert false
  in
  aux cycle_eras

(* This function returns the cycle era to which [cycle] belongs. *)
let era_of_cycle ~cycle_eras cycle =
  let rec aux = function
    | ({first_cycle; _} as era) :: previous_eras ->
        if Cycle_repr.(cycle >= first_cycle) then era else aux previous_eras
    | [] -> assert false
  in
  aux cycle_eras

(* precondition: [level] belongs to [era] *)
let cycle_from_raw_with_era era level =
  let {first_level; first_cycle; blocks_per_cycle; _} = era in
  let level_position_in_era = Raw_level_repr.diff level first_level in
  assert (Compare.Int32.(level_position_in_era >= 0l)) ;
  let cycles_since_era_start =
    Int32.div level_position_in_era blocks_per_cycle
  in
  let cycle =
    Cycle_repr.add first_cycle (Int32.to_int cycles_since_era_start)
  in
  (cycle, level_position_in_era)

(* precondition: [level] belongs to [era] *)
let level_from_raw_with_era era ~first_level_in_alpha_family level =
  let cycle, level_position_in_era = cycle_from_raw_with_era era level in
  let cycle_position = Int32.rem level_position_in_era era.blocks_per_cycle in
  let level_position = Raw_level_repr.diff level first_level_in_alpha_family in
  let expected_commitment =
    Compare.Int32.(
      Int32.rem cycle_position era.blocks_per_commitment
      = Int32.pred era.blocks_per_commitment)
  in
  {level; level_position; cycle; cycle_position; expected_commitment}

let cycle_from_raw ~cycle_eras l =
  let era = era_of_level ~cycle_eras l in
  fst @@ cycle_from_raw_with_era era l

let level_from_raw_aux_exn ~cycle_eras level =
  let first_level_in_alpha_family =
    match List.rev cycle_eras with
    | [] -> assert false
    | {first_level; _} :: _ -> first_level
  in
  let era = era_of_level ~cycle_eras level in
  level_from_raw_with_era era ~first_level_in_alpha_family level

let level_from_raw ~cycle_eras l = level_from_raw_aux_exn ~cycle_eras l

type error += Level_not_in_alpha of Raw_level_repr.t

let () =
  register_error_kind
    `Permanent
    ~id:"level_not_in_alpha"
    ~title:"Level not in Alpha family"
    ~description:"Level not in Alpha family"
    ~pp:(fun ppf level ->
      Format.fprintf
        ppf
        "Level %a is not in the Alpha family of protocols."
        Raw_level_repr.pp
        level)
    Data_encoding.(obj1 (req "level" Raw_level_repr.encoding))
    (function Level_not_in_alpha level -> Some level | _ -> None)
    (fun level -> Level_not_in_alpha level)

let level_from_raw_aux ~cycle_eras level =
  let open Result_syntax in
  let first_level_in_alpha_family =
    match List.rev cycle_eras with
    | [] -> assert false
    | {first_level; _} :: _ -> first_level
  in
  let+ () =
    error_when
      Raw_level_repr.(level < first_level_in_alpha_family)
      (Level_not_in_alpha level)
  in
  let era = era_of_level ~cycle_eras level in
  level_from_raw_with_era era ~first_level_in_alpha_family level

type error += Negative_level_and_offset_sum of int32 * int32

let () =
  register_error_kind
    `Permanent
    ~id:"negative_level_and_offset_sum"
    ~title:"Negative sum of level and offset"
    ~description:"Negative sum of level and offset"
    ~pp:(fun ppf (level, offset) ->
      Format.fprintf
        ppf
        "Sum of level (%ld) and offset (%ld) is negative."
        level
        offset)
    Data_encoding.(obj2 (req "level" int32) (req "offset" int32))
    (function
      | Negative_level_and_offset_sum (level, offset) -> Some (level, offset)
      | _ -> None)
    (fun (level, offset) -> Negative_level_and_offset_sum (level, offset))

let level_from_raw_with_offset ~cycle_eras ~offset raw_level =
  let res = Raw_level_repr.(of_int32 (Int32.add (to_int32 raw_level) offset)) in
  match res with
  | Ok level -> level_from_raw_aux ~cycle_eras level
  | Error _ ->
      Result_syntax.tzfail
        (Negative_level_and_offset_sum
           (Raw_level_repr.to_int32 raw_level, offset))

let first_level_in_cycle_from_eras ~cycle_eras cycle =
  let first_level_in_alpha_family =
    match List.rev cycle_eras with
    | [] -> assert false
    | {first_level; _} :: _ -> first_level
  in
  let era = era_of_cycle ~cycle_eras cycle in
  let cycle_position = Cycle_repr.diff cycle era.first_cycle in
  let offset = Int32.mul era.blocks_per_cycle cycle_position in
  let first_level_in_cycle =
    Raw_level_repr.(of_int32_exn (Int32.add (to_int32 era.first_level) offset))
  in
  level_from_raw_with_era era ~first_level_in_alpha_family first_level_in_cycle

let last_of_cycle ~cycle_eras level =
  let era = era_of_level ~cycle_eras level.level in
  Compare.Int32.(Int32.succ level.cycle_position = era.blocks_per_cycle)

module Internal_for_tests = struct
  let add_level level n =
    let raw_level = level.level in
    let new_raw_level = Raw_level_repr.add raw_level n in
    {level with level = new_raw_level}

  let add_cycles ~blocks_per_cycle level n =
    {
      level with
      cycle = Cycle_repr.add level.cycle n;
      level = Raw_level_repr.add level.level (n * blocks_per_cycle);
      level_position =
        Int32.add level.level_position (Int32.of_int (n * blocks_per_cycle));
    }

  let root =
    {
      level = Raw_level_repr.root;
      level_position = 0l;
      cycle = Cycle_repr.root;
      cycle_position = 0l;
      expected_commitment = false;
    }

  let make_cycle_eras ~blocks_per_cycle ~blocks_per_commitment =
    let cycle_era =
      {
        first_level = Raw_level_repr.root;
        first_cycle = Cycle_repr.root;
        blocks_per_cycle;
        blocks_per_commitment;
      }
    in
    create_cycle_eras [cycle_era]

  let level_from_int32 ~cycle_eras l =
    let open Result_syntax in
    let* raw_level = Raw_level_repr.of_int32 l in
    return @@ level_from_raw ~cycle_eras raw_level
end
