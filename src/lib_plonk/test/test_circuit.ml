(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

module SMap = Kzg.SMap
open Plompiler.Csir

let gates_equal = SMap.equal (Array.for_all2 Scalar.equal)

let ql_name = Plompiler.Csir.linear_selector_name 0

let qr_name = Plompiler.Csir.linear_selector_name 1

let qlg_name = Plompiler.Csir.add_next_wire_suffix ql_name

module Make = struct
  module Main = Plonk.Main_protocol
  module Helpers = Plonk_test.Helpers
  module Helpers_main = Helpers.Make (Main)

  let test_make_one_sel sel () =
    let open Plonk.Circuit in
    let wires =
      let a = [1] in
      let b = [1] in
      let c = [1] in
      [|a; b; c|]
    in
    let gates = SMap.add sel [Scalar.one] SMap.empty in
    let gates, tables =
      match sel with
      | "q_plookup" -> (SMap.add "q_table" [Scalar.one] gates, [[[||]]])
      | "q_table" -> (SMap.add "q_plookup" [Scalar.one] gates, [[[||]]])
      | _ -> (gates, [])
    in
    let c = make ~tables ~wires ~gates ~public_input_size:0 () in
    let wires = Array.map Array.of_list wires in
    let gates = Tables.map Array.of_list gates in
    assert (Array.sub c.wires 0 (Array.length wires) = wires) ;
    assert (gates_equal c.gates gates)

  let tests_one_sel =
    List.map
      (fun (s, _) ->
        Alcotest.test_case ("make " ^ s) `Quick (test_make_one_sel s))
      CS.all_selectors

  let test_empty () =
    let open Plonk.Circuit in
    let wires =
      let a = [1] in
      let b = [1] in
      let c = [1] in
      [|a; b; c|]
    in
    let gates = SMap.add "qc" [Scalar.one] SMap.empty in
    Helpers.must_fail (fun () ->
        ignore @@ make ~wires:[||] ~gates ~public_input_size:0 ()) ;
    Helpers.must_fail (fun () ->
        ignore @@ make ~wires ~gates:SMap.empty ~public_input_size:0 ())

  let test_different_size () =
    let open Plonk.Circuit in
    (* wires have different size wrt to gates *)
    let wires =
      let a = [1] in
      let b = [1] in
      let c = [1] in
      [|a; b; c|]
    in
    let gates = SMap.add "qc" Scalar.[one; one] SMap.empty in
    Helpers.must_fail (fun () ->
        ignore @@ make ~wires ~gates ~public_input_size:0 ()) ;
    (* wires have different sizes *)
    let wrong_wires =
      let a = [1; 1] in
      let b = [1] in
      let c = [1] in
      [|a; b; c|]
    in
    let gates = SMap.add "qc" Scalar.[one] SMap.empty in
    Helpers.must_fail (fun () ->
        ignore @@ make ~wires:wrong_wires ~gates ~public_input_size:0 ()) ;
    (* gates have different sizes *)
    let gates = SMap.of_list Scalar.[("qc", [one]); (ql_name, [one; one])] in
    Helpers.must_fail (fun () ->
        ignore @@ make ~wires ~gates ~public_input_size:0 ())

  (* Test that Plonk supports using qecc_ws_add and a q*g in the same circuit. *)
  let test_disjoint () =
    let open Plonk.Circuit in
    let x = Scalar.[|one; add one one; of_string "3"; of_string "4"|] in
    let wires =
      let a = [0; 2; 0] in
      let b = [0; 3; 3] in
      let c = [0; 1; 1] in
      [|a; b; c|]
    in
    let gates =
      SMap.of_list
        Scalar.
          [
            (ql_name, [one; zero; zero]);
            ("qecc_ws_add", [zero; one; zero]);
            (qlg_name, [one; zero; zero]);
            ("qc", [Scalar.(negate (of_string "4")); zero; zero]);
          ]
    in
    let c = make ~wires ~gates ~public_input_size:0 () in
    Helpers_main.test_circuit ~name:"test_disjoint" c x

  let test_wrong_selectors () =
    let open Plonk.Circuit in
    let wires =
      let a = [0; 2; 0] in
      let b = [0; 3; 3] in
      let c = [0; 1; 1] in
      [|a; b; c|]
    in
    let gates =
      SMap.of_list
        Scalar.
          [
            (ql_name, [one; zero; zero]);
            ("dummy", [zero; one; zero]);
            (qlg_name, [one; zero; zero]);
            ("qc", [Scalar.(negate (of_string "4")); zero; zero]);
          ]
    in
    try
      let _ = make ~wires ~gates ~public_input_size:0 () in
      ()
    with
    | Invalid_argument s when s = "Make Circuit: unknown gates." -> ()
    | _ ->
        failwith
          "Test_wrong_selector : Invalid_argument \"Make Circuit: unknown \
           gates.\" expected."

  let test_vector () =
    let open Plonk.Circuit in
    let wires =
      let a = [1; 1] in
      let b = [1; 1] in
      let c = [1; 1] in
      [|a; b; c|]
    in
    let gates =
      SMap.of_list Scalar.[("qc", [zero; one]); (qr_name, [zero; zero])]
    in
    let gates_expected =
      SMap.of_list Scalar.[("qc", [|zero; one|]); (ql_name, [|zero; zero|])]
    in
    let c = make ~wires ~gates ~public_input_size:1 () in
    let wires = Array.map Array.of_list wires in
    assert (Array.sub c.wires 0 (Array.length wires) = wires) ;
    assert (gates_equal c.gates gates_expected)

  (* TODO add more tests about lookup *)

  let test_table () =
    let zero, one = Scalar.(zero, one) in
    let table_or =
      Table.of_list
        [
          [|zero; zero; one; one|];
          [|zero; one; zero; one|];
          [|zero; one; one; one|];
          [|zero; zero; zero; zero|];
          [|zero; zero; zero; zero|];
        ]
    in
    let entry = ([|zero; zero; zero; zero; zero|] : Table.entry) in
    let input = [|Some zero; Some zero; None; None; None|] in
    assert (Table.size table_or = 4) ;
    assert (Table.mem entry table_or) ;
    Table.find input table_or |> Option.get |> fun res ->
    assert (
      Scalar.(
        eq entry.(0) res.(0) && eq entry.(1) res.(1) && eq entry.(2) res.(2))) ;
    ()
end

let tests =
  Make.tests_one_sel
  @ List.map
      (fun (n, f) -> Alcotest.test_case n `Quick f)
      [
        ("make empty", Make.test_empty);
        ("make different_size", Make.test_different_size);
        ("make vectors", Make.test_vector);
        ("make table", Make.test_table);
        ("make disjoint", Make.test_disjoint);
        ("to_plonk wrong selectors", Make.test_wrong_selectors);
      ]
