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

open Plompiler
module CS = Plonk.Circuit

module Gen = struct
  let read_int () =
    try input_binary_int stdin
    with End_of_file ->
      Printf.printf "insufficient input" ;
      exit 0

  let scalar () =
    (* Fr.random () *)
    S.of_z @@ Z.of_int @@ read_int ()
end

let test_afl () =
  let module P (L : LIB) = struct
    open L
    open L.Num

    let[@warning "-8"] t (x, y) =
      let* x = input x in
      let* y = input y in
      let* o = add x y in
      let* o = mul o y in
      div o y

    let ins = Input.(scalar S.zero, scalar S.one)
  end in
  let circuit, ins =
    let module E1 = P (LibCircuit) in
    (E1.t, E1.ins)
  in

  let LibCircuit.{cs; tables; solver; _} = LibCircuit.(get_cs (circuit ins)) in
  (* safety: sat => trace *)
  let private_inputs = Array.init solver.final_size (fun _ -> Gen.scalar ()) in
  if CS.sat cs tables private_inputs then (
    print_endline "satisfied" ;
    let initial, _ = LibCircuit.(get_inputs (circuit ins)) in
    let solved_pi = Solver.solve solver initial in
    assert (Array.for_all2 S.( = ) solved_pi private_inputs))
  else () ;
  (* soundness: trace => sat *)
  let trace =
    let x = LibCircuit.Input.scalar @@ Gen.scalar () in
    let y = LibCircuit.Input.scalar @@ Gen.scalar () in
    let initial, _ = LibCircuit.(get_inputs (circuit (x, y))) in
    try Solver.solve solver initial |> fun x -> Some x with _ -> None
  in
  match trace with
  | None -> ()
  | Some trace ->
      print_endline "found trace" ;
      assert (CS.sat cs tables trace)

let () = test_afl ()
