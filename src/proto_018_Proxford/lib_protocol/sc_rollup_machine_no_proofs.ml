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
