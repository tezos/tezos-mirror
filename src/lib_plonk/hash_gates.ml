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

open Bls
open Identities
module L = Plompiler.LibCircuit
open Gates_common

(* Anemoi 2 rounds
   Arith monomial
   degree : 6n
   advice selectors : kx1, kx2, ky1, ky2 (round constants)
   equations : x2, y2 = round(x1,y1) = round(round(x0, y0))
      1) q · [g · x1 + ky1 - y1)^2 · beta - beta · y0^2 - (g^2 + 1) · x1 + g · y1 + delta - gamma + kx1 + x0]
      2) q · [g · x1 + ky1 + y0 - y1)^5 + beta · y0^2 + gamma - x0]
      3) q · [g · x2 + ky2 - y2)^2 · beta - beta · y1^2 - (g^2 + 1) · x2 + g · y2 + delta - gamma + kx2 + x1]
      4) q · [g · x2 + ky2 + y1 - y2)^5 + beta · y1^2 + gamma - x1]
*)
module AnemoiDouble : Base_sig = struct
  module AnemoiPerm = Bls12_381_hash.Permutation.Anemoi

  let q_label = "q_anemoi"

  let identity = (q_label, 4)

  let index_com = None

  let nb_advs = 4

  let nb_buffers = 3

  let gx_composition = true

  let g = AnemoiPerm.Parameters.g

  let beta = AnemoiPerm.Parameters.beta

  let gamma = AnemoiPerm.Parameters.gamma

  let delta = AnemoiPerm.Parameters.delta

  let kx1_label = qadv_label ^ "0"

  let ky1_label = qadv_label ^ "1"

  let kx2_label = qadv_label ^ "2"

  let ky2_label = qadv_label ^ "3"

  let ( -@ ) a b = Scalar.sub a b

  let g2_p_1 = Scalar.((g * g) + one)

  (* Identities:
     (g · x1 + ky1 - y1)^2 · beta - beta · y0^2 - (g^2 + 1) · x1 + g · y1 + delta - gamma + kx1 + x0
     (g · x1 + ky1 + y0 - y1)^5 + beta · y0^2 + gamma - x0
     (g · x2 + ky2 - y2)^2 · beta - beta · y1^2 - (g^2 + 1) · x2 + g · y2 + delta - gamma + kx2 + x1
     (g · x2 + ky2 + y1 - y2)^5 + beta · y1^2 + gamma - x1

                     a    b    c    d    e
       wire #i:          x1   y1   x0   y0
       wire #i+1:                  x2   y2 *)
  let round_identities ~kx ~ky (x, y) (x', y') =
    let mv = Scalar.((g * x') + ky -@ y') in
    let w = Scalar.(mv + y) in
    let c = Scalar.((g * y') + delta + kx + x -@ ((g2_p_1 * x') + gamma)) in
    let beta_y2 = Scalar.(beta * y * y) in
    let id1 = Scalar.((beta * mv * mv) -@ beta_y2 + c) in
    let id2 = Scalar.(pow w (Z.of_int 5) + beta_y2 + gamma -@ x) in
    [id1; id2]

  let cs_round_identities ~kx ~ky (x, y) (x', y') =
    let open L in
    let open Num in
    let mg2_p_1 = Scalar.negate g2_p_1 in
    let* mv = add_list ~coeffs:[g; one; mone] (to_list [x'; ky; y']) in
    let* w = add mv y in
    let* c =
      add_list
        ~qc:(delta -@ gamma)
        ~coeffs:[g; one; one; mg2_p_1]
        (to_list [y'; kx; x; x'])
    in
    let* beta_y2 = mul ~qm:beta y y in
    let* beta_mv2_c = custom ~qx2b:beta ~ql:one c mv in
    let* id1 = add ~qr:mone beta_mv2_c beta_y2 in
    let* w5_x = custom ~qx5a:one ~qr:mone w x in
    let* id2 = add ~qc:gamma w5_x beta_y2 in
    ret [id1; id2]

  let evals_round_identities ~domain_size ~buffers ~selector ~id1_buffer
      ~id2_buffer ~kx ~ky ?(compose' = 0) (x, y) (x', y') =
    (* buffers.(0) <- g · x' - y' + ky *)
    let gx'_y'_ky =
      Evaluations.linear_c
        ~res:buffers.(0)
        ~evaluations:[x'; y'; ky]
        ~linear_coeffs:[g; mone; one]
        ~composition_gx:([compose'; compose'; 0], domain_size)
        ()
    in
    (* id1_buffer <- (g · x' + ky - y')^2 *)
    let id1_partial =
      Evaluations.mul_c ~res:id1_buffer ~evaluations:[gx'_y'_ky] ~powers:[2] ()
    in
    (* buffers.(1) <- (g · x' + ky - y') + y *)
    let w =
      Evaluations.linear_c ~res:buffers.(1) ~evaluations:[gx'_y'_ky; y] ()
    in
    (* id2_buffer <- (g · x' + ky + y - y')^5 *)
    let id2_partial =
      Evaluations.mul_c ~res:id2_buffer ~evaluations:[w] ~powers:[5] ()
    in
    (* buffers.(2) <- y^2 *)
    let y2 =
      Evaluations.mul_c ~res:buffers.(2) ~evaluations:[y] ~powers:[2] ()
    in
    (* buffer1 <- (g · x' + ky - y')^2 + y^2 + x' + y' + x + kx *)
    let id1 =
      Evaluations.linear_c
        ~res:buffers.(0)
        ~evaluations:[id1_partial; y2; x'; y'; x; kx]
        ~linear_coeffs:Scalar.[beta; negate beta; negate g2_p_1; g; one; one]
        ~composition_gx:([0; 0; compose'; compose'; 0; 0], domain_size)
        ~add_constant:(delta -@ gamma)
        ()
    in
    (* buffer2 <- (g · x' + ky + y - y')^5 + y^2 + x *)
    let id2 =
      Evaluations.linear_c
        ~res:buffers.(1)
        ~evaluations:[id2_partial; y2; x]
        ~linear_coeffs:[Scalar.one; beta; mone]
        ~add_constant:gamma
        ()
    in
    (* id1_buffer <- q · (g · x' + ky - y')^2 + y^2 + x' + y' + x + kx *)
    let qid1 =
      Evaluations.mul_c ~res:id1_buffer ~evaluations:[selector; id1] ()
    in
    (* id2_buffer <- q · (g · x' + ky + y - y')^5 + y^2 + x *)
    let qid2 =
      Evaluations.mul_c ~res:id2_buffer ~evaluations:[selector; id2] ()
    in
    (qid1, qid2)

  let equations ~q ~wires ~wires_g ?(precomputed_advice = SMap.empty) () =
    let x1 = wires.(1) in
    let y1 = wires.(2) in
    let x0 = wires.(3) in
    let y0 = wires.(4) in
    let x2 = wires_g.(3) in
    let y2 = wires_g.(4) in
    if Scalar.is_zero q then Scalar.[zero; zero; zero; zero]
    else
      let kx1 = SMap.find kx1_label precomputed_advice in
      let ky1 = SMap.find ky1_label precomputed_advice in
      let kx2 = SMap.find kx2_label precomputed_advice in
      let ky2 = SMap.find ky2_label precomputed_advice in
      let ids12 = round_identities ~kx:kx1 ~ky:ky1 (x0, y0) (x1, y1) in
      let ids34 = round_identities ~kx:kx2 ~ky:ky2 (x1, y1) (x2, y2) in
      ids12 @ ids34

  let blinds =
    SMap.of_list
      [
        (left, [|0; 0|]);
        (right, [|1; 0|]);
        (output, [|1; 0|]);
        (top, [|1; 1|]);
        (bottom, [|1; 1|]);
      ]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain :
      prover_identities =
   fun evaluations ->
    let domain_size = Domain.length domain in
    let buffers, ids = get_buffers ~nb_buffers ~nb_ids:(snd identity) in
    let ({q; b; c; d; e; _} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in
    let selector, x1, y1, x0, y0, x2, y2 = (q, b, c, d, e, d, e) in

    let kx1, ky1, kx2, ky2 =
      ( Evaluations.find_evaluation evaluations (prefix_common kx1_label),
        Evaluations.find_evaluation evaluations (prefix_common ky1_label),
        Evaluations.find_evaluation evaluations (prefix_common kx2_label),
        Evaluations.find_evaluation evaluations (prefix_common ky2_label) )
    in
    let id1, id2 =
      evals_round_identities
        ~domain_size
        ~buffers
        ~selector
        ~id1_buffer:ids.(0)
        ~id2_buffer:ids.(1)
        ~kx:kx1
        ~ky:ky1
        (x0, y0)
        (x1, y1)
    in
    let id3, id4 =
      evals_round_identities
        ~domain_size
        ~buffers
        ~selector
        ~id1_buffer:ids.(2)
        ~id2_buffer:ids.(3)
        ~kx:kx2
        ~ky:ky2
        ~compose':1
        (x1, y1)
        (x2, y2)
    in
    SMap.of_list
      [
        (prefix @@ q_label ^ ".0", id1);
        (prefix @@ q_label ^ ".1", id2);
        (prefix @@ q_label ^ ".2", id3);
        (prefix @@ q_label ^ ".3", id4);
      ]

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let {q; b; c; d; e; dg; eg; _} =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let x0, y0, x1, y1, x2, y2 = (d, e, b, c, dg, eg) in

    let kx1 = get_answer answers X @@ prefix_common kx1_label in
    let ky1 = get_answer answers X @@ prefix_common ky1_label in
    let kx2 = get_answer answers X @@ prefix_common kx2_label in
    let ky2 = get_answer answers X @@ prefix_common ky2_label in
    let precomputed_advice =
      SMap.of_list
        [(kx1_label, kx1); (ky1_label, ky1); (kx2_label, kx2); (ky2_label, ky2)]
    in
    let identities =
      equations
        ~q
        ~wires:Scalar.[|zero; x1; y1; x0; y0|]
        ~wires_g:Scalar.[|zero; zero; zero; x2; y2|]
        ~precomputed_advice
        ()
      |> List.map (Scalar.mul q)
    in
    SMap.of_list
    @@ List.mapi
         (fun i id -> (prefix @@ q_label ^ "." ^ string_of_int i, id))
         identities

  let polynomials_degree =
    SMap.of_list [(right, 6); (output, 6); (top, 6); (bottom, 6); (q_label, 6)]

  let cs ~q ~wires ~wires_g ?(precomputed_advice = SMap.empty) () =
    let x1 = wires.(1) in
    let y1 = wires.(2) in
    let x0 = wires.(3) in
    let y0 = wires.(4) in
    let x2 = wires_g.(3) in
    let y2 = wires_g.(4) in
    let open L in
    let kx1 = SMap.find kx1_label precomputed_advice in
    let ky1 = SMap.find ky1_label precomputed_advice in
    let kx2 = SMap.find kx2_label precomputed_advice in
    let ky2 = SMap.find ky2_label precomputed_advice in
    let* ids12 = cs_round_identities ~kx:kx1 ~ky:ky1 (x0, y0) (x1, y1) in
    let* ids34 = cs_round_identities ~kx:kx2 ~ky:ky2 (x1, y1) (x2, y2) in
    mapM (fun id -> Num.mul q id) (ids12 @ ids34)
end
