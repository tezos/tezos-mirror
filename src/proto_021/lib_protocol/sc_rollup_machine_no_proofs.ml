(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type void = |

let void =
  Data_encoding.(
    conv_with_guard
      (function (_ : void) -> .)
      (fun _ -> Error "void has no inhabitant")
      unit)

type t = Context_binary.t

type tree = Context_binary.tree

let empty_tree () = Context_binary.(make_empty_context () |> Tree.empty)

module Context_no_proofs = struct
  module Tree = Context_binary.Tree

  type tree = Context_binary.tree

  type proof = void

  let verify_proof = function (_ : proof) -> .

  let produce_proof _context _state _step = assert false

  let proof_before = function (_ : proof) -> .

  let proof_after = function (_ : proof) -> .

  let proof_encoding = void
end

module type S = sig
  val parse_boot_sector : string -> string option

  val pp_boot_sector : Format.formatter -> string -> unit

  include
    Sc_rollup_PVM_sig.S
      with type context = Context_no_proofs.Tree.t
       and type state = Context_no_proofs.tree
       and type proof = void
end

module Arith : S = Sc_rollup_arith.Make (Context_no_proofs)

module Wasm : S =
  Sc_rollup_wasm.V2_0_0.Make (Wasm_2_0_0.Make) (Context_no_proofs)

module Riscv : S = struct
  let parse_boot_sector _ = None

  let pp_boot_sector _fmtr _bs = ()

  type state = tree

  let pp _state = Lwt.return (fun _ _ -> ())

  type context = t

  type hash = Smart_rollup.State_hash.t

  type proof = void

  let proof_encoding = void

  let elim_void = function (_ : void) -> .

  let proof_start_state = elim_void

  let proof_stop_state = elim_void

  let state_hash _state =
    Sc_rollup_riscv.(Protocol_implementation.state_hash (make_empty_state ()))

  let initial_state ~empty = Lwt.return empty

  let install_boot_sector state _bs = Lwt.return state

  let is_input_state ~is_reveal_enabled:_ _state =
    failwith "is_input_state: unimplemented"

  let set_input _input _state = failwith "set_input: unimplemented"

  let eval _state = failwith "eval: unimplemented"

  let verify_proof ~is_reveal_enabled:_ _input_opt = elim_void

  let produce_proof _ctxt ~is_reveal_enabled:_ _input_opt _state =
    failwith "produce_proof: unimplemented"

  type output_proof = void

  let output_proof_encoding = void

  let output_of_output_proof = elim_void

  let state_of_output_proof = elim_void

  let verify_output_proof = elim_void

  let produce_output_proof _ctxt _state _out =
    failwith "produce_output_proof: unimplemented"

  let check_dissection ~default_number_of_sections:_ ~start_chunk:_
      ~stop_chunk:_ _chunks =
    failwith "check_dissection: unimplemented"

  let get_current_level _state = failwith "get_current_level: unimplemented"

  module Internal_for_tests = struct
    let insert_failure _state = failwith "insert_failure: unimplemented"
  end
end
