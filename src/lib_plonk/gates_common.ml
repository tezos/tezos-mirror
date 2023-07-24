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

open Kzg.Bls
open Identities
module L = Plompiler.LibCircuit

type public = {public_inputs : Scalar.t array; input_coms_size : int}

let one = Scalar.one

let mone = Scalar.negate one

let two = Scalar.add one one

let wire_name = Plompiler.Csir.wire_name

let com_label = "com"

let tmp_buffers = ref [||]

type answers = {q : Scalar.t; wires : Scalar.t array; wires_g : Scalar.t array}

type witness = {q : Evaluations.t; wires : Evaluations.t array}

let get_buffers ~nb_buffers ~nb_ids =
  let precomputed = !tmp_buffers in
  let nb_precomputed = Array.length precomputed in
  let size_eval = Evaluations.length precomputed.(0) in
  let buffers =
    Array.init (max nb_buffers nb_precomputed) (fun i ->
        if i < nb_precomputed then precomputed.(i)
        else Evaluations.create size_eval)
  in
  tmp_buffers := buffers ;
  (buffers, Array.init nb_ids (fun _ -> Evaluations.create size_eval))

let get_answers ?(gx = false) ~q_label ~prefix ~prefix_common answers : answers
    =
  let dummy = Scalar.zero in
  let answer = get_answer answers in
  let answer_wire w =
    let w = wire_name w in
    let w' = prefix w in
    let x = answer X w' in
    let gx = if gx then answer GX w' else dummy in
    (x, gx)
  in
  let q = prefix_common q_label |> answer X in
  let wires, wires_g =
    Array.init Plompiler.Csir.nb_wires_arch answer_wire |> Array.split
  in
  {q; wires; wires_g}

let get_evaluations ~q_label ~prefix ~prefix_common evaluations =
  let find_wire w =
    Evaluations.find_evaluation evaluations (prefix @@ wire_name w)
  in
  {
    q = Evaluations.find_evaluation evaluations (prefix_common q_label);
    wires = Array.init Plompiler.Csir.nb_wires_arch find_wire;
  }

(* Block names to merge identities within, if identities are independent, use q_label instead.
   For instance, we want to have want AddLeft and Addright identities to be merged inside the Arithmetic block,
   we thus use "arith" as Map key for these gates identities.
   We also want to use the ECC point addition identity independently, as such we put ECCAdd gate's q_label as key. *)
let arith = "Arith"

let qadv_label = "qadv"

let map_singleton m =
  let map f t =
    let open L in
    let* x = t in
    ret (f x)
  in
  map (fun x -> [x]) m

module type Base_sig = sig
  val q_label : string

  val identity : string * int

  val index_com : int option

  val nb_advs : int

  val nb_buffers : int

  val gx_composition : bool

  val equations :
    q:Scalar.t ->
    wires:Scalar.t array ->
    wires_g:Scalar.t array ->
    ?precomputed_advice:Scalar.t SMap.t ->
    unit ->
    Scalar.t list

  val prover_identities :
    prefix_common:(string -> string) ->
    prefix:(string -> string) ->
    public:public ->
    domain:Domain.t ->
    prover_identities

  val verifier_identities :
    prefix_common:(string -> string) ->
    prefix:(string -> string) ->
    public:public ->
    generator:Scalar.t ->
    size_domain:int ->
    verifier_identities

  (* Give the size of the domain on which the identities
     of the gate need to have the evaluation of each of his polynomial
     divided by the size of the citcuit. *)
  val polynomials_degree : int SMap.t

  val cs :
    q:L.scalar L.repr ->
    wires:L.scalar L.repr array ->
    wires_g:L.scalar L.repr array ->
    ?precomputed_advice:L.scalar L.repr SMap.t ->
    unit ->
    L.scalar L.repr list L.t
end
