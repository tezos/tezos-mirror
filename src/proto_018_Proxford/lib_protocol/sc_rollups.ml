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

module PVM = struct
  type boot_sector = string

  module type S = sig
    val parse_boot_sector : string -> boot_sector option

    val pp_boot_sector : Format.formatter -> boot_sector -> unit

    include Sc_rollup_PVM_sig.S
  end

  type ('state, 'proof, 'output) implementation =
    (module S
       with type state = 'state
        and type proof = 'proof
        and type output_proof = 'output)

  type t = Packed : ('state, 'proof, 'output) implementation -> t [@@unboxed]
end

module Kind = struct
  (*
      Each time we add a data constructor to [t], we also need:
      - to extend [Sc_rollups.all] with this new constructor ;
      - to update [Sc_rollups.of_name] and [encoding] ;
      - to update [Sc_rollups.wrapped_proof] and [wrapped_proof_encoding].

  *)
  type t = Example_arith | Wasm_2_0_0

  let all = [Example_arith; Wasm_2_0_0]

  let to_string = function
    | Example_arith -> "arith"
    | Wasm_2_0_0 -> "wasm_2_0_0"

  let of_string = function
    | "arith" -> Some Example_arith
    | "wasm_2_0_0" -> Some Wasm_2_0_0
    | _ -> None

  let encoding =
    Data_encoding.string_enum @@ List.map (fun k -> (to_string k, k)) all

  let pp fmt = function
    | Example_arith -> Format.pp_print_string fmt "arith"
    | Wasm_2_0_0 -> Format.pp_print_string fmt "wasm_2_0_0"

  let equal x y =
    match (x, y) with
    | Example_arith, Example_arith -> true
    | Wasm_2_0_0, Wasm_2_0_0 -> true
    | _ -> false

  let example_arith_pvm =
    PVM.Packed (module Sc_rollup_arith.Protocol_implementation)

  let wasm_2_0_0_pvm =
    PVM.Packed (module Sc_rollup_wasm.V2_0_0.Protocol_implementation)

  let reference_initial_state_hash_of = function
    | Example_arith -> Sc_rollup_arith.reference_initial_state_hash
    | Wasm_2_0_0 -> Sc_rollup_wasm.V2_0_0.reference_initial_state_hash

  let pvm_of = function
    | Example_arith -> example_arith_pvm
    | Wasm_2_0_0 -> wasm_2_0_0_pvm

  let no_proof_machine_of : t -> (module Sc_rollup_machine_no_proofs.S) =
    function
    | Example_arith -> (module Sc_rollup_machine_no_proofs.Arith)
    | Wasm_2_0_0 -> (module Sc_rollup_machine_no_proofs.Wasm)
end

let genesis_state_hash_of ~boot_sector kind =
  let open Lwt_syntax in
  let (module Machine) = Kind.no_proof_machine_of kind in
  let empty = Sc_rollup_machine_no_proofs.empty_tree () in
  let* tree = Machine.initial_state ~empty in
  let* initial_hash = Machine.state_hash tree in
  assert (
    Sc_rollup_repr.State_hash.(
      initial_hash = Kind.reference_initial_state_hash_of kind)) ;
  let* tree = Machine.install_boot_sector tree boot_sector in
  Machine.state_hash tree
