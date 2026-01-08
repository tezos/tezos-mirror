(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Sc_rollup_repr

type t = {state_hash : State_hash.t option; tick : Sc_rollup_tick_repr.t}

let equal {state_hash; tick} chunk2 =
  Option.equal State_hash.equal state_hash chunk2.state_hash
  && Sc_rollup_tick_repr.equal tick chunk2.tick

let encoding =
  let open Data_encoding in
  conv
    (fun {state_hash; tick} -> (state_hash, tick))
    (fun (state_hash, tick) -> {state_hash; tick})
    (obj2
       (opt "state" State_hash.encoding)
       (req "tick" Sc_rollup_tick_repr.encoding))

type error +=
  | (* `Temporary *)
      Dissection_number_of_sections_mismatch of {
      expected : Z.t;
      given : Z.t;
    }
  | (* `Permanent *) Dissection_invalid_number_of_sections of Z.t
  | (* `Temporary *)
      Dissection_start_hash_mismatch of {
      expected : Sc_rollup_repr.State_hash.t option;
      given : Sc_rollup_repr.State_hash.t option;
    }
  | (* `Temporary *)
      Dissection_stop_hash_mismatch of
      Sc_rollup_repr.State_hash.t option
  | (* `Temporary *)
      Dissection_edge_ticks_mismatch of {
      dissection_start_tick : Sc_rollup_tick_repr.t;
      dissection_stop_tick : Sc_rollup_tick_repr.t;
      chunk_start_tick : Sc_rollup_tick_repr.t;
      chunk_stop_tick : Sc_rollup_tick_repr.t;
    }
  | (* `Permanent *) Dissection_ticks_not_increasing
  | (* `Permanent *) Dissection_invalid_distribution of Z.t
  | (* `Permanent *) Dissection_invalid_successive_states_shape

let pp_state_hash =
  let open Format in
  pp_print_option ~none:(fun ppf () -> fprintf ppf "None") State_hash.pp

let pp_hash_opt fmt = function
  | None -> Format.fprintf fmt "None"
  | Some x -> Sc_rollup_repr.State_hash.pp fmt x

let pp ppf {state_hash; tick} =
  let open Format in
  fprintf
    ppf
    "State hash:%a@ Tick: %a"
    pp_state_hash
    state_hash
    Sc_rollup_tick_repr.pp
    tick

let default_check_sections_number ~default_number_of_sections
    ~number_of_sections ~dist =
  let open Result_syntax in
  let number_of_sections = Z.of_int number_of_sections in
  let default_number_of_sections = Z.of_int default_number_of_sections in
  let should_be_equal_to expected =
    Dissection_number_of_sections_mismatch
      {expected; given = number_of_sections}
  in
  if Compare.Z.(default_number_of_sections <= dist) then
    error_unless
      Compare.Z.(number_of_sections = default_number_of_sections)
      (should_be_equal_to default_number_of_sections)
  else if Compare.Z.(dist > Z.one) then
    error_unless Compare.Z.(number_of_sections = dist) (should_be_equal_to dist)
  else tzfail (Dissection_invalid_number_of_sections number_of_sections)

let default_check ~section_maximum_size ~check_sections_number
    ~default_number_of_sections ~start_chunk ~stop_chunk dissection =
  let open Result_syntax in
  let number_of_sections = Compare.Int.max 0 (List.length dissection - 1) in
  let dist = Sc_rollup_tick_repr.distance start_chunk.tick stop_chunk.tick in
  let* () =
    check_sections_number ~default_number_of_sections ~number_of_sections ~dist
  in
  let* () =
    match (List.hd dissection, List.last_opt dissection) with
    | Some {state_hash = a; tick = a_tick}, Some {state_hash = b; tick = b_tick}
      ->
        let* () =
          error_unless
            (Option.equal State_hash.equal a start_chunk.state_hash
            && not (Option.is_none a))
            (Dissection_start_hash_mismatch
               {expected = start_chunk.state_hash; given = a})
        in
        let* () =
          error_unless
            (not (Option.equal State_hash.equal b stop_chunk.state_hash))
            ((* If the [b] state is equal to [stop_chunk], that means we
                agree on the after state of the section. But, we're trying
                to dispute it, it doesn't make sense. *)
               Dissection_stop_hash_mismatch
               stop_chunk.state_hash)
        in
        Sc_rollup_tick_repr.(
          error_unless
            (a_tick = start_chunk.tick && b_tick = stop_chunk.tick)
            (Dissection_edge_ticks_mismatch
               {
                 dissection_start_tick = a_tick;
                 dissection_stop_tick = b_tick;
                 chunk_start_tick = start_chunk.tick;
                 chunk_stop_tick = stop_chunk.tick;
               }))
    | _ ->
        (* This case is probably already handled by the
           [Dissection_invalid_number_of_sections] returned above *)
        tzfail
          (Dissection_invalid_number_of_sections (Z.of_int number_of_sections))
  in
  let rec traverse states =
    match states with
    | {state_hash = None; _} :: {state_hash = Some _; _} :: _ ->
        tzfail Dissection_invalid_successive_states_shape
    | {tick; _} :: ({tick = next_tick; state_hash = _} as next) :: others ->
        if Sc_rollup_tick_repr.(tick < next_tick) then
          let incr = Sc_rollup_tick_repr.distance tick next_tick in
          if Z.(leq incr section_maximum_size) then traverse (next :: others)
          else tzfail (Dissection_invalid_distribution section_maximum_size)
        else tzfail Dissection_ticks_not_increasing
    | _ -> return_unit
  in
  traverse dissection

let () =
  let description = "Mismatch in the number of sections in the dissection" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_dissection_number_of_sections_mismatch"
    ~title:description
    ~description
    ~pp:(fun ppf (expected, given) ->
      Format.fprintf
        ppf
        "The number of sections must be equal to %a instead of %a"
        Z.pp_print
        expected
        Z.pp_print
        given)
    Data_encoding.(obj2 (req "expected" n) (req "given" n))
    (function
      | Dissection_number_of_sections_mismatch {expected; given} ->
          Some (expected, given)
      | _ -> None)
    (fun (expected, given) ->
      Dissection_number_of_sections_mismatch {expected; given}) ;
  let description = "Invalid number of sections in the dissection" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_dissection_invalid_number_of_sections"
    ~title:description
    ~description
    ~pp:(fun ppf n ->
      Format.fprintf
        ppf
        "A dissection with %a sections can never be valid"
        Z.pp_print
        n)
    Data_encoding.(obj1 (req "value" n))
    (function Dissection_invalid_number_of_sections n -> Some n | _ -> None)
    (fun n -> Dissection_invalid_number_of_sections n) ;
  let description = "Mismatch in the start hash of the dissection" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_dissection_start_hash_mismatch"
    ~title:description
    ~description
    ~pp:(fun ppf (given, expected) ->
      match given with
      | None -> Format.fprintf ppf "The start hash must not be None"
      | Some _ ->
          Format.fprintf
            ppf
            "The start hash should be equal to %a, but the provided hash is %a"
            pp_hash_opt
            expected
            pp_hash_opt
            given)
    Data_encoding.(
      obj2
        (req "expected" (option Sc_rollup_repr.State_hash.encoding))
        (req "given" (option Sc_rollup_repr.State_hash.encoding)))
    (function
      | Dissection_start_hash_mismatch {expected; given} ->
          Some (expected, given)
      | _ -> None)
    (fun (expected, given) -> Dissection_start_hash_mismatch {expected; given}) ;
  let description = "Mismatch in the stop hash of the dissection" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_dissection_stop_hash_mismatch"
    ~title:description
    ~description
    ~pp:(fun ppf h ->
      Format.fprintf ppf "The stop hash should not be equal to %a" pp_hash_opt h)
    Data_encoding.(
      obj1 (req "hash" (option Sc_rollup_repr.State_hash.encoding)))
    (function Dissection_stop_hash_mismatch hopt -> Some hopt | _ -> None)
    (fun hopt -> Dissection_stop_hash_mismatch hopt) ;
  let description = "Mismatch in the edge ticks of the dissection" in
  register_error_kind
    `Temporary
    ~id:"smart_rollup_dissection_edge_ticks_mismatch"
    ~title:description
    ~description
    ~pp:(fun
        ppf
        ( dissection_start_tick,
          dissection_stop_tick,
          chunk_start_tick,
          chunk_stop_tick )
      ->
      Sc_rollup_tick_repr.(
        Format.fprintf
          ppf
          "We should have dissection_start_tick(%a) = %a and \
           dissection_stop_tick(%a) = %a"
          pp
          dissection_start_tick
          pp
          chunk_start_tick
          pp
          dissection_stop_tick
          pp
          chunk_stop_tick))
    Data_encoding.(
      obj4
        (req "dissection_start_tick" Sc_rollup_tick_repr.encoding)
        (req "dissection_stop_tick" Sc_rollup_tick_repr.encoding)
        (req "chunk_start_tick" Sc_rollup_tick_repr.encoding)
        (req "chunk_stop_tick" Sc_rollup_tick_repr.encoding))
    (function
      | Dissection_edge_ticks_mismatch e ->
          Some
            ( e.dissection_start_tick,
              e.dissection_stop_tick,
              e.chunk_start_tick,
              e.chunk_stop_tick )
      | _ -> None)
    (fun ( dissection_start_tick,
           dissection_stop_tick,
           chunk_start_tick,
           chunk_stop_tick )
       ->
      Dissection_edge_ticks_mismatch
        {
          dissection_start_tick;
          dissection_stop_tick;
          chunk_start_tick;
          chunk_stop_tick;
        }) ;
  let description = "Ticks should only increase in dissection" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_dissection_ticks_not_increasing"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.pp_print_string ppf description)
    Data_encoding.empty
    (function Dissection_ticks_not_increasing -> Some () | _ -> None)
    (fun () -> Dissection_ticks_not_increasing) ;
  register_error_kind
    `Permanent
    ~id:"smart_rollup_dissection_invalid_distribution"
    ~title:description
    ~description
    ~pp:(fun ppf max ->
      Format.fprintf
        ppf
        "Maximum tick increment in a section cannot be more than %a ticks"
        Z.pp_print
        max)
    Data_encoding.(obj1 (req "section_max_size" n))
    (function Dissection_invalid_distribution max -> Some max | _ -> None)
    (fun max -> Dissection_invalid_distribution max) ;
  let description = "Cannot recover from a blocked state in a dissection" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_dissection_invalid_successive_states_shape"
    ~title:description
    ~description
    ~pp:(fun ppf () -> Format.pp_print_string ppf description)
    Data_encoding.empty
    (function
      | Dissection_invalid_successive_states_shape -> Some () | _ -> None)
    (fun () -> Dissection_invalid_successive_states_shape)
