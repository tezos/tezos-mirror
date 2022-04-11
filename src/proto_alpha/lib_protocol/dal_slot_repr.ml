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

module Header = struct
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3101

     Datatype is mocked for the moment while the cryptography is not
     provided by the environment. *)
  type t = int

  let encoding = Data_encoding.int31

  let pp = Format.pp_print_int
end

type index = int

type header = Header.t

type t = {level : Raw_level_repr.t; index : index; header : header}

type slot = t

let make ~level ~index ~header =
  if Compare.Int.(index < 0) then
    invalid_arg "dal_slot_repr.make: index should be a non-negative number" ;
  {level; index; header}

let encoding =
  let open Data_encoding in
  conv
    (fun {level; index; header} -> (level, index, header))
    (fun (level, index, header) -> {level; index; header})
    (obj3
       (req "level" Raw_level_repr.encoding)
       (req "index" Data_encoding.uint8)
       (req "header" Header.encoding))

let pp fmt {level; index; header} =
  Format.fprintf
    fmt
    "level: %a index: %a header: %a"
    Raw_level_repr.pp
    level
    Format.pp_print_int
    index
    Header.pp
    header

module Slot_market = struct
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3108

     Think harder about this data structure and whether it can be
     optimized. *)

  type t = (slot * Tez_repr.t) option list

  let init ~length =
    let l =
      List.init
        ~when_negative_length:
          "Dal_slot_repr.Slot_market.init: length cannot be negative"
        length
        (fun _ -> None)
    in
    match l with Error msg -> invalid_arg msg | Ok l -> l

  let current_fees candidates index =
    match List.nth candidates index with
    | None | Some None -> None
    | Some (Some ((_ : slot), tez)) -> Some tez

  let update candidates slot fees =
    let has_changed = ref false in
    let may_replace_candidate current_candidate =
      match current_candidate with
      | Some ((_slot : slot), current_fees) when Tez_repr.(current_fees >= fees)
        ->
          current_candidate
      | _ -> Some (slot, fees)
    in
    let candidates =
      List.mapi
        (fun i candidate ->
          if Compare.Int.(i = slot.index) then may_replace_candidate candidate
          else candidate)
        candidates
    in
    (candidates, !has_changed)

  let candidates candidates =
    List.filter_map
      (fun candidate ->
        Option.map (fun (slot, (_fee : Tez_repr.t)) -> slot) candidate)
      candidates
end
