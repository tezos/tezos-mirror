(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol

(* This file contains unit tests pertaining to the computation of
   serialization and deserialization gas of Michelson terms. *)

module Tested_terms () = struct
  open Micheline

  let string = String (0, "this is a test string")

  let int = Int (0, Z.of_string "133713371337133713371337")

  let bytes = Bytes (0, Bytes.of_string "this is a value of type Bytes.t")

  (* We're not going to typecheck; the chosen primitive does not matter. *)
  let some_prim = Michelson_v1_primitives.D_Unit

  (* replace this by a proper prng if you feel like it *)
  let next_seed seed = seed + 1

  let rec dummy_but_large_term depth seed k =
    if depth = 0 then
      let kind = seed mod 3 in
      if kind = 0 then k string else if kind = 1 then k int else k bytes
    else
      let seed1 = next_seed seed in
      let seed2 = next_seed seed1 in
      dummy_but_large_term (depth - 1) seed1 (fun term1 ->
          dummy_but_large_term (depth - 1) seed2 (fun term2 ->
              let kind = seed mod 2 in
              if kind = 0 then k (Prim (0, some_prim, [term1; term2], []))
              else k (Seq (0, [term1; term2]))))

  let dummy_but_large_term ~depth ~seed =
    dummy_but_large_term depth seed (fun x -> x)

  let ( % ) g f x = g (f x)

  let terms : Script_repr.lazy_expr list =
    List.map (Script_repr.lazy_expr % Micheline.strip_locations)
    @@ [
         string;
         int;
         bytes;
         dummy_but_large_term ~depth:1 ~seed:1;
         dummy_but_large_term ~depth:5 ~seed:1;
         dummy_but_large_term ~depth:10 ~seed:1;
         dummy_but_large_term ~depth:15 ~seed:1;
       ]

  let bytes =
    List.map
      (Data_encoding.Binary.to_bytes_exn Script_repr.lazy_expr_encoding)
      terms

  let lazy_terms : Script_repr.lazy_expr list =
    try
      List.map
        (Data_encoding.Binary.of_bytes_exn Script_repr.lazy_expr_encoding)
        bytes
    with Data_encoding.Binary.Read_error err ->
      Format.eprintf "%a@." Data_encoding.Binary.pp_read_error err ;
      assert false

  let minimal_costs = List.map Script_repr.minimal_deserialize_cost lazy_terms

  let full_costs =
    List.map
      (fun lazy_term ->
        match Script_repr.force_decode lazy_term with
        | Error _ -> assert false
        | Ok (_term, cost) -> cost)
      lazy_terms

  let check_correctness () =
    List.iter2_e
      ~when_different_lengths:
        (TzTrace.make
           (Exn (Failure "min costs and full costs have different lengths")))
      (fun min full ->
        if Z.leq min full then ok_unit
        else
          generic_error
            "Script_repr: inconsistent costs %a vs %a@."
            Z.pp_print
            min
            Z.pp_print
            full)
      minimal_costs
      full_costs

  let check_correctness () = Lwt.return @@ check_correctness ()
end

let check_property () =
  let module T = Tested_terms () in
  T.check_correctness ()

let tests =
  [
    Test_services.tztest
      "Script_repr.minimal_deserialize_cost is a lower bound for full \
       deserialization cost"
      `Quick
      check_property;
  ]
