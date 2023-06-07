(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <team@marigold.dev>                           *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Micheline sampling. *)

type width_function = depth:int -> int Base_samplers.sampler

(** [Base_samplers] specifies samplers for leaves, primitives and annotations. *)
module type Base_samplers = sig
  (** The type of primitives. *)
  type prim

  val sample_prim : prim Base_samplers.sampler

  val sample_annots : string list Base_samplers.sampler

  val sample_string : string Base_samplers.sampler

  val sample_bytes : Bytes.t Base_samplers.sampler

  val sample_z : Z.t Base_samplers.sampler

  val width_function : width_function
end

module type S = sig
  type prim

  val sample : (int, prim) Micheline.node Base_samplers.sampler
end

type node_kind = Int_node | String_node | Bytes_node | Seq_node | Prim_node

(* The distribution can be skewed towards non-leaf nodes by repeating their
   relevant kind in the array below. *)
let all_kinds = [|Int_node; String_node; Bytes_node; Seq_node; Prim_node|]

let sample_kind : node_kind Base_samplers.sampler =
 fun rng_state ->
  let i = Random.State.int rng_state (Array.length all_kinds) in
  all_kinds.(i)

let reasonable_width_function ~depth rng_state =
  (* Entirely ad-hoc *)
  Base_samplers.(
    sample_in_interval
      ~range:{min = 0; max = 20 / (Bits.numbits depth + 1)}
      rng_state)

module Make (P : Base_samplers) : S with type prim = P.prim = struct
  type prim = P.prim

  let sample (w : width_function) rng_state =
    let rec sample depth rng_state k =
      match sample_kind rng_state with
      | Int_node -> k (Micheline.Int (0, P.sample_z rng_state))
      | String_node -> k (Micheline.String (0, P.sample_string rng_state))
      | Bytes_node -> k (Micheline.Bytes (0, P.sample_bytes rng_state))
      | Seq_node ->
          let width = w ~depth rng_state in
          sample_list
            depth
            width
            []
            (fun terms -> k (Micheline.Seq (0, terms)))
            rng_state
      | Prim_node ->
          let prim = P.sample_prim rng_state in
          let annots = P.sample_annots rng_state in
          let width = w ~depth rng_state in
          sample_list
            depth
            width
            []
            (fun terms -> k (Micheline.Prim (0, prim, terms, annots)))
            rng_state
    and sample_list depth width acc k rng_state =
      if width < 0 then invalid_arg "sample_list: negative width"
      else if width = 0 then k (List.rev acc)
      else
        sample (depth + 1) rng_state (fun x ->
            sample_list depth (width - 1) (x :: acc) k rng_state)
    in
    sample 0 rng_state (fun x -> x)

  let sample rng_state = sample P.width_function rng_state
end
