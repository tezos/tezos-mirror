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

(* Weierstrass elliptic curve addition : checks that (a, ag) + (b, bg) = (c, cg)
   Non Arith
   degree : 4n
   nb identities : 2
   advice selectors : None
   equations : with λ = (bg - ag) / (b - a),
     1) q·[ (a + b + c)·(b - a)² - (bg - ag)² ] = 0
     2) q·[ (ag + cg)·(b - a) - (a - c)·(bg - ag) ] = 0
   /!\ q must be 0 or 1
*)
module AddWeierstrass : Base_sig = struct
  let q_label = "qecc_ws_add"

  let identity = (q_label, 2)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 3

  let gx_composition = true

  let equations ~q ~a ~b ~c ~d:_ ~e:_ ~ag ~bg ~cg ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    if Scalar.is_zero q then Scalar.[zero; zero]
    else if not (Scalar.(is_one) q) then
      failwith "AddWeierstrass.equations : qecc_ws_add must be zero or one."
    else
      let lambda = Scalar.(div_exn (sub bg ag) (sub b a)) in
      let x = Scalar.(sub (lambda * lambda) (a + b)) in
      let y = Scalar.(sub (lambda * sub a x) ag) in
      Scalar.[sub x c; sub y cg]

  let blinds =
    SMap.of_list [(right, [|1; 1|]); (left, [|1; 1|]); (output, [|1; 1|])]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain :
      prover_identities =
   fun evaluations ->
    (* lambda:
       numerator =  (bg - ag) ;
       denominator = (b - a) *)
    (* identity on new point's x coordinate:
       (c + b + a)·(b - a)^2 - (bg - ag)^2 = 0 *)
    let domain_size = Domain.length domain in
    let tmps, ids = get_buffers ~nb_buffers ~nb_ids:(snd identity) in
    let ({q; a; b; c; _} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in

    (* tmps.(2) <- (b - a) *)
    let b_minus_a =
      Evaluations.linear_c
        ~res:tmps.(2)
        ~evaluations:[b; a]
        ~linear_coeffs:[one; mone]
        ()
    in
    (* tmps.(1) <- (b - a)^2 *)
    let b_minus_a_sqr =
      Evaluations.mul_c ~res:tmps.(1) ~evaluations:[b_minus_a] ~powers:[2] ()
    in
    (* ids.(1) <- (a + b + c) *)
    let a_plus_b_plus_c =
      Evaluations.linear_c ~res:ids.(1) ~evaluations:[a; b; c] ()
    in
    (* tmps.(0) <- (a + b + c)·(b - a)^2 *)
    let left_term =
      Evaluations.mul_c
        ~res:tmps.(0)
        ~evaluations:[a_plus_b_plus_c; b_minus_a_sqr]
        ()
    in
    (* ids.(1) <- (a + b + c)·(b - a)^2 - (bg - ag)^2 *)
    let first_identity =
      Evaluations.linear_c
        ~res:ids.(1)
        ~evaluations:[left_term; b_minus_a_sqr]
        ~composition_gx:([0; 1], domain_size)
        ~linear_coeffs:[one; mone]
        ()
    in
    let first_identity =
      Evaluations.mul_c ~res:ids.(0) ~evaluations:[q; first_identity] ()
    in
    (* identity on new point's y coordinate:
       (cg + ag)·(b - a) - (bg - ag)·(a - c) = 0 *)
    (* ids.(1) <- (cg + ag) *)
    let cg_plus_ag =
      Evaluations.linear_c
        ~res:ids.(1)
        ~evaluations:[c; a]
        ~composition_gx:([1; 1], domain_size)
        ()
    in
    (* tmps.(0) <- (cg + ag)·(b - a) *)
    let left_term_2 =
      Evaluations.mul_c ~res:tmps.(0) ~evaluations:[cg_plus_ag; b_minus_a] ()
    in
    (* ids.(1) <- (a - c) *)
    let a_minus_c =
      Evaluations.linear_c
        ~res:ids.(1)
        ~evaluations:[a; c]
        ~linear_coeffs:[one; mone]
        ()
    in
    (* tmps.(1) <- (bg - ag)·(a - c) *)
    let right_term_2 =
      Evaluations.mul_c
        ~res:tmps.(1)
        ~evaluations:[b_minus_a; a_minus_c]
        ~composition_gx:([1; 0], domain_size)
        ()
    in
    (* tmps.(2) <- (cg + ag)·(b - a) - (bg - ag)·(a - c) *)
    let second_identity =
      Evaluations.linear_c
        ~res:tmps.(2)
        ~evaluations:[left_term_2; right_term_2]
        ~linear_coeffs:[one; mone]
        ()
    in
    let second_identity =
      Evaluations.mul_c ~res:ids.(1) ~evaluations:[q; second_identity] ()
    in
    SMap.of_list
      [
        (prefix @@ q_label ^ ".0", first_identity);
        (prefix @@ q_label ^ ".1", second_identity);
      ]

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let {q; a; b; c; ag; bg; cg; _} =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let num_lambda = Scalar.(sub bg ag) in
    let den_lambda = Scalar.(sub b a) in
    (* identity on new point's x coordinate:
       (c + b + a)·(b - a)^2 - (bg - ag)^2 = 0 *)
    let first_identity =
      let num_lambda2 = Scalar.mul num_lambda num_lambda in
      let den_lambda2 = Scalar.mul den_lambda den_lambda in
      let id = Scalar.(sub ((c + b + a) * den_lambda2) num_lambda2) in
      Scalar.mul q id
    in
    (* identity on new point's y coordinate:
       (cg + ag)·(b - a) - (bg - ag)·(a - c) = 0 *)
    let second_identity =
      let id = Scalar.(sub ((cg + ag) * den_lambda) (num_lambda * sub a c)) in
      Scalar.mul q id
    in
    SMap.of_list
      [
        (prefix @@ q_label ^ ".0", first_identity);
        (prefix @@ q_label ^ ".1", second_identity);
      ]

  let polynomials_degree =
    SMap.of_list [(left, 4); (right, 4); (output, 4); (q_label, 4)]

  let cs ~q:qec ~a ~b ~c ~d:_ ~e:_ ~ag ~bg ~cg ~dg:_ ~eg:_ ?precomputed_advice:_
      () =
    let open L in
    let open Num in
    let sub = add ~qr:mone in
    let* lambda_num = sub bg ag in
    let* lambda_denom = sub b a in
    let* lambda_num2 = mul lambda_num lambda_num in
    let* cba = add_list (to_list [c; b; a]) in
    let* fst_term = mul_list (to_list [cba; lambda_denom; lambda_denom]) in
    let* fst = sub fst_term lambda_num2 in
    let* fst = mul qec fst in
    let* cgag = add cg ag in
    let* ac = sub a c in
    let* fst_term = mul cgag lambda_denom in
    let* snd_term = mul lambda_num ac in
    let* snd = sub fst_term snd_term in
    let* snd = mul qec snd in
    ret [fst; snd]
end

(* Edwards elliptic curve addition : checks that P + Q = R
   Non Arith
   degree : 6n
   nb identities : 2
   advice selectors : None
   equations :
     1) q · [  r ·(1 + param_d · px · qx · py · qy) - (px · bg +           qx · py) ] = 0
     2) q · [ ry ·(1 - param_d · px · qx · py · qy) - (qy · py - param_a · qx · px) ] = 0
   /!\ q must be 0 or 1
*)
module AddEdwards : Base_sig = struct
  let q_label = "qecc_ed_add"

  let identity = (q_label, 2)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 2

  let gx_composition = true

  (* JubJub curve parameters *)
  let param_a = mone

  let param_d =
    Scalar.of_string
      "19257038036680949359750312669786877991949435402254120286184196891950884077233"

  let equations ~q ~a:px ~b:qx ~c:rx ~d:_ ~e:_ ~ag:py ~bg:qy ~cg:ry ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    if Scalar.is_zero q then Scalar.[zero; zero]
    else if not (Scalar.is_one q) then
      failwith "AddEdwards.equations : qecc_ed_add must be zero or one."
    else
      let pxqy = Scalar.(px * qy) in
      let qxpy = Scalar.(qx * py) in
      let pyqy = Scalar.(py * qy) in
      let pxqx = Scalar.(px * qx) in
      let rx' = Scalar.((pxqy + qxpy) / (one + (param_d * pxqy * qxpy))) in
      let ry' =
        Scalar.(
          (pyqy + (negate param_a * pxqx))
          / (one + (negate param_d * pxqy * qxpy)))
      in
      Scalar.[rx' + negate rx; ry' + negate ry]

  let blinds =
    SMap.of_list [(right, [|1; 1|]); (left, [|1; 1|]); (output, [|1; 1|])]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain :
      prover_identities =
   fun evaluations ->
    let domain_size = Domain.length domain in
    let tmps, ids = get_buffers ~nb_buffers ~nb_ids:(snd identity) in
    let ({q; a; b; c; _} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in
    let s, p, q, r = (q, a, b, c) in

    (* identity on new point's x coordinate:
       q · [r_x · (1 + Params_d · p_x · q_x · p_y · q_y) - (q_x · q_y + p_y · q_x)] = 0 *)
    (* tmps.(0) <- p_x · q_y *)
    let px_mul_qy =
      Evaluations.mul_c
        ~res:tmps.(0)
        ~evaluations:[p; q]
        ~composition_gx:([0; 1], domain_size)
        ()
    in
    (* tmps.(1) <- p_y · q_x *)
    let py_mul_qx =
      Evaluations.mul_c
        ~res:tmps.(1)
        ~evaluations:[p; q]
        ~composition_gx:([1; 0], domain_size)
        ()
    in
    (* ids.(0) <- px · py · qx · qy *)
    let px_mul_py_mul_qx_mul_qy =
      Evaluations.mul_c ~res:ids.(0) ~evaluations:[px_mul_qy; py_mul_qx] ()
    in
    (* ids.(1) <- 1 + Params_d · px · py · qx · qy *)
    let one_plus_d_mul_px_mul_py_mul_qx_mul_qy =
      Evaluations.linear_c
        ~res:ids.(1)
        ~evaluations:[px_mul_py_mul_qx_mul_qy]
        ~linear_coeffs:[param_d]
        ~add_constant:one
        ()
    in
    (* ids.(0) <- px · qy + py · qx *)
    let px_mul_qy_plus_py_mul_qx =
      Evaluations.linear_c ~res:ids.(0) ~evaluations:[px_mul_qy; py_mul_qx] ()
    in
    (* tmps.(0) <- rx · (1 + Params_d · px · py · qx · qy) *)
    let rx_mul_one_plus_d_mul_px_mul_py_mul_qx_mul_qy =
      Evaluations.mul_c
        ~res:tmps.(0)
        ~evaluations:[one_plus_d_mul_px_mul_py_mul_qx_mul_qy; r]
        ()
    in
    (* tmps.(1) <- rx · (1 + Params_d · px · py · qx · qy) - (px · qy + py · qx) *)
    let first_identity =
      Evaluations.linear_c
        ~res:tmps.(1)
        ~evaluations:
          [
            rx_mul_one_plus_d_mul_px_mul_py_mul_qx_mul_qy;
            px_mul_qy_plus_py_mul_qx;
          ]
        ~linear_coeffs:[one; mone]
        ()
    in
    let first_identity =
      Evaluations.mul_c ~res:ids.(0) ~evaluations:[s; first_identity] ()
    in
    (* identity on new point's y coordinate:
       q * [ry * (1 - Params_d * p_x * q_x * p_y * q_y) - (p_y * q_y - Params_a * p_x * q_x)]  = 0 *)
    (* tmps.(1) <- (1 - Params_d · px · py · qx · qy) *)
    let one_minus_d_mul_px_mul_py_mul_qx_mul_qy =
      Evaluations.linear_c
        ~res:tmps.(1)
        ~evaluations:[one_plus_d_mul_px_mul_py_mul_qx_mul_qy]
        ~linear_coeffs:[mone]
        ~add_constant:two
        ()
    in
    (* tmps.(0) <- ry · (1 - Params_d · px · py · qx · qy) *)
    let ry_mul_one_minus_d_mul_px_mul_py_mul_qx_mul_qy =
      Evaluations.mul_c
        ~res:tmps.(0)
        ~evaluations:[one_minus_d_mul_px_mul_py_mul_qx_mul_qy; r]
        ~composition_gx:([0; 1], domain_size)
        ()
    in
    (* ids.(1) <- px · qx *)
    let px_mul_qx = Evaluations.mul_c ~res:ids.(1) ~evaluations:[p; q] () in
    (* tmps.(1) <- ry · (1 - Params_d · px · py · qx · qy) - py · qy + Params_a · px · qx *)
    let second_identity =
      Evaluations.linear_c
        ~res:tmps.(1)
        ~evaluations:
          [ry_mul_one_minus_d_mul_px_mul_py_mul_qx_mul_qy; px_mul_qx; px_mul_qx]
        ~composition_gx:([0; 1; 0], domain_size)
        ~linear_coeffs:[one; mone; param_a]
        ()
    in
    let second_identity =
      Evaluations.mul_c ~res:ids.(1) ~evaluations:[s; second_identity] ()
    in
    SMap.of_list
      [
        (prefix @@ q_label ^ ".0", first_identity);
        (prefix @@ q_label ^ ".1", second_identity);
      ]

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ : verifier_identities =
   fun _ answers ->
    let {q; a; b; c; ag; bg; cg; _} =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let px, py, qx, qy, rx, ry = (a, ag, b, bg, c, cg) in
    let pxqx = Scalar.mul px qx in
    let pyqy = Scalar.mul py qy in
    let den_common = Scalar.(param_d * pxqx * pyqy) in
    (* q·[x3·(1 + d·x1·x2·y1·y2) - (x1·y2 + y1·x2)] = 0 *)
    let first_identity =
      let num = Scalar.((px * qy) + (py * qx)) in
      let den = Scalar.(one + den_common) in
      let id = Scalar.(sub (rx * den) num) in
      Scalar.mul q id
    in
    (* q · [ry · (1 - d · px · qx · py · qy) - (py · qy - a · px · qx)] = 0 *)
    let second_identity =
      let num = Scalar.(sub pyqy (param_a * pxqx)) in
      let den = Scalar.(sub one den_common) in
      let id = Scalar.(sub (ry * den) num) in
      Scalar.mul q id
    in
    SMap.of_list
      [
        (prefix @@ q_label ^ ".0", first_identity);
        (prefix @@ q_label ^ ".1", second_identity);
      ]

  let polynomials_degree =
    SMap.of_list [(left, 6); (right, 6); (output, 6); (q_label, 6)]

  let cs ~q:qec ~a:px ~b:qx ~c:rx ~d:_ ~e:_ ~ag:py ~bg:qy ~cg:ry ~dg:_ ~eg:_
      ?precomputed_advice:_ () =
    let open L in
    let open Num in
    let sub = add ~qr:mone in
    let* pxqy = mul px qy in
    let* qxpy = mul qx py in
    (* 1 + d · p_x · q_x · p_y · q_y *)
    let* den = custom ~qc:Scalar.one ~qm:param_d pxqy qxpy in
    let* fst_term = mul rx den in
    let* snd_term = add pxqy qxpy in
    let* fst_id = sub fst_term snd_term in
    let* fst_id = mul qec fst_id in
    (* 1 - d · p_x · q_x · p_y · q_y *)
    let* den' = add_constant two ~ql:mone den in
    (* r_y · (1 - d · p_x · q_x · p_y · q_y) *)
    let* fst_term = mul ry den' in
    (* p_y · q_y - a · p_x · q_x *)
    let* snd_term =
      let* pyqy = mul py qy in
      let* apxqx = mul ~qm:param_a px qx in
      sub pyqy apxqx
    in
    let* snd_id = sub fst_term snd_term in
    let* snd_id = mul qec snd_id in
    ret [fst_id; snd_id]
end

(* Edwards elliptic curve conditional addition : checks that P(d, e) + a · Q = R
   Non Arith
   degree : 7n
   nb identities : 2
   advice selectors : None
   equations :
      · q · [ rx · (1 + param_d · bit · qx · qy · px · py) - (px + bit · (px · qy +           qx · py - px)) ] = 0
      · q · [ ry · (1 - param_d · bit · qx · qy · px · py) - (py + bit · (py · qy - param_a · qx · px - py)) ] = 0
*)
module ConditionalAddEdwards : Base_sig = struct
  let q_label = "qecc_ed_cond_add"

  let identity = (q_label, 2)

  let index_com = None

  let nb_advs = 0

  let nb_buffers = 3

  let gx_composition = true

  (* JubJub curve parameters *)
  let param_a = mone

  let param_d =
    Scalar.of_string
      "19257038036680949359750312669786877991949435402254120286184196891950884077233"

  (* Let P = (p_x; p_y), Q = (q_x; q_y), R = (r_x; r_y), b in {0,1}.
     This gate asserts that R = P + b * Q.
     b * Q = Q if b = 1 and (0, 1) if b = 0, so b * Q can be written as: (b * q_x; b * q_y + 1 - b)
     Let a et d the Edwards curve parameters.
     We thus have the following identities:
        r_x =     (p_x * (b * q_y + 1 - b) + b * q_x * p_y) / (1 + d * p_x * b * q_x * p_y * (b * q_y + 1 - b))
        r_y = (p_y * (b * q_y + 1 - b) - a * p_x * b * q_x) / (1 - d * p_x * b * q_x * p_y * (b * q_y + 1 - b))
      We put in the wires a, b and c the point coordinates in this order:
                    a     b     c     d     e
      wire #i:    bit   q_x   q_y   p_x   p_y
      wire #i+1:                    r_x   r_y

     Simplifying the equations, we get:
        r_x =     (p_x * (b * q_y + 1 - b) + b * q_x * p_y) / (1 + b * d * p_x * q_x * p_y * q_y)
        r_y = (p_y * (b * q_y + 1 - b) - b * a * p_x * q_x) / (1 - b * d * p_x * q_x * p_y * q_y)
  *)
  let equations ~q ~a:bit ~b:qx ~c:qy ~d:px ~e:py ~ag:_ ~bg:_ ~cg:_ ~dg:rx
      ~eg:ry ?precomputed_advice:_ () =
    if Scalar.is_zero q then Scalar.[zero; zero]
    else
      let qx' = Scalar.(bit * qx) in
      let qy' = Scalar.((bit * qy) + sub one bit) in
      let pxqy' = Scalar.(px * qy') in
      let qx'py = Scalar.(qx' * py) in
      let pyqy' = Scalar.(py * qy') in
      let pxqx' = Scalar.(px * qx') in
      let rx' = Scalar.((pxqy' + qx'py) / (one + (param_d * pxqy' * qx'py))) in
      let ry' =
        Scalar.(
          (pyqy' + (negate param_a * pxqx'))
          / (one + (negate param_d * pxqy' * qx'py)))
      in
      Scalar.[rx' + negate rx; ry' + negate ry]

  let blinds =
    SMap.of_list
      [
        (left, [|1; 0|]);
        (right, [|1; 0|]);
        (output, [|1; 0|]);
        (top, [|1; 1|]);
        (bottom, [|1; 1|]);
      ]

  let prover_identities ~prefix_common ~prefix ~public:_ ~domain evaluations =
    let domain_size = Domain.length domain in
    let tmps, ids = get_buffers ~nb_buffers ~nb_ids:(snd identity) in
    let ({q; a; b; c; d; e} : witness) =
      get_evaluations ~q_label ~blinds ~prefix ~prefix_common evaluations
    in
    let b, qx, qy, px, py, rx, ry = (a, b, c, d, e, d, e) in

    (* identity on new point's x coordinate:
       q · [r_x · (1 + d · b · p_x · q_x · p_y · q_y)
            - (b · p_x · q_y + p_x - b · p_x + b · q_x · p_y) = 0] *)
    (* tmps.(0) <- b · px · qy *)
    let b_mul_px_mul_qy =
      Evaluations.mul_c ~res:tmps.(0) ~evaluations:[b; px; qy] ()
    in
    (* tmps.(1) <- py · qx *)
    let py_mul_qx = Evaluations.mul_c ~res:tmps.(1) ~evaluations:[py; qx] () in
    (* tmps.(2) <- b · px *)
    let b_mul_px = Evaluations.mul_c ~res:tmps.(2) ~evaluations:[b; px] () in
    (* ids.(0) <- b · px · py · qx · qy *)
    let b_mul_px_mul_py_mul_qx_mul_qy =
      Evaluations.mul_c
        ~res:ids.(0)
        ~evaluations:[b_mul_px_mul_qy; py_mul_qx]
        ()
    in
    (* ids.(1) <- 1 + Params_d · b · px · py · qx · qy *)
    let den1 =
      Evaluations.linear_c
        ~res:ids.(1)
        ~evaluations:[b_mul_px_mul_py_mul_qx_mul_qy]
        ~linear_coeffs:[param_d]
        ~add_constant:one
        ()
    in
    (* tmps.(1) <- b · py · qx *)
    let b_mul_py_mul_qx =
      Evaluations.mul_c ~res:tmps.(1) ~evaluations:[b; py; qx] ()
    in
    (* ids.(0) <- px - b · px + b · px · qy +  b · py · qx *)
    let rhs =
      Evaluations.linear_c
        ~res:ids.(0)
        ~evaluations:[px; b_mul_px; b_mul_px_mul_qy; b_mul_py_mul_qx]
        ~linear_coeffs:[one; mone; one; one]
        ()
    in
    (* tmps.(0) <- rx · (1 + Params_d · b · px · py · qx · qy) *)
    let lhs =
      Evaluations.mul_c
        ~res:tmps.(0)
        ~evaluations:[den1; rx]
        ~composition_gx:([0; 1], domain_size)
        ()
    in
    (* tmps.(1) <-
         rx · (1 + Params_d · b · px · py · qx · qy)
       - (px - b · px + b · px · qy +  b · py · qx) *)
    let first_identity =
      Evaluations.linear_c
        ~res:tmps.(1)
        ~evaluations:[lhs; rhs]
        ~linear_coeffs:[one; mone]
        ()
    in
    let first_identity =
      Evaluations.mul_c ~res:ids.(0) ~evaluations:[q; first_identity] ()
    in
    (* identity on new point's y coordinate:
       q · [r_y · (1 - d · b · p_x · q_x · p_y · q_y)
        - (b · p_y · q_y + p_y - p_y · b - a · p_x · b · q_x)] = 0 *)
    (* tmps.(1) <- (1 - Params_d · b · px · py · qx · qy) *)
    let den2 =
      Evaluations.linear_c
        ~res:tmps.(1)
        ~evaluations:[den1]
        ~linear_coeffs:[mone]
        ~add_constant:two
        ()
    in
    (* tmps.(0) <- ry · (1 - Params_d · px · py · qx · qy) *)
    let lhs =
      Evaluations.mul_c
        ~res:tmps.(0)
        ~evaluations:[den2; ry]
        ~composition_gx:([0; 1], domain_size)
        ()
    in
    (* ids.(1) <- px · qx *)
    let px_mul_qx = Evaluations.mul_c ~res:ids.(1) ~evaluations:[px; qx] () in
    (* tmps.(1) <- py · qy *)
    let py_mul_qy = Evaluations.mul_c ~res:tmps.(1) ~evaluations:[py; qy] () in
    let minus_a_mul_px_mul_qx_plus_py_mul_qy_minus_py =
      Evaluations.linear_c
        ~res:tmps.(2)
        ~evaluations:[px_mul_qx; py_mul_qy; py]
        ~linear_coeffs:[Scalar.negate param_a; one; mone]
        ()
    in
    let minus_b_times_a_mul_px_mul_qx_plus_py_mul_qy_minus_py =
      Evaluations.mul_c
        ~res:ids.(1)
        ~evaluations:[b; minus_a_mul_px_mul_qx_plus_py_mul_qy_minus_py]
        ()
    in
    let rhs =
      Evaluations.linear_c
        ~res:tmps.(2)
        ~evaluations:[minus_b_times_a_mul_px_mul_qx_plus_py_mul_qy_minus_py; py]
        ()
    in
    (* tmps.(1) <-
       ry · (1 - Params_d · px · py · qx · qy)
       - (b · py · qy + py - py · b - Params_a · px · b · qx) *)
    let second_identity =
      Evaluations.linear_c
        ~res:tmps.(1)
        ~evaluations:[lhs; rhs]
        ~linear_coeffs:[one; mone]
        ()
    in
    let second_identity =
      Evaluations.mul_c ~res:ids.(1) ~evaluations:[q; second_identity] ()
    in
    SMap.of_list
      [
        (prefix @@ q_label ^ ".0", first_identity);
        (prefix @@ q_label ^ ".1", second_identity);
      ]

  let verifier_identities ~prefix_common ~prefix ~public:_ ~generator:_
      ~size_domain:_ _ answers =
    let {q; a; b; c; d; e; dg; eg; _} =
      get_answers ~q_label ~blinds ~prefix ~prefix_common answers
    in
    let b, qx, qy, px, py, rx, ry = (a, b, c, d, e, dg, eg) in
    let bpxqy = Scalar.(b * px * qy) in
    let pyqx = Scalar.(py * qx) in
    let den_common = Scalar.(param_d * bpxqy * pyqx) in
    (* q · [rx · (1 + d · b · px · qx · py · qy)
         - (b · px · qy + px - b · px + b · qx · py) = 0] *)
    let first_identity =
      let num = Scalar.(sub px (b * px) + bpxqy + (b * pyqx)) in
      let den = Scalar.(one + den_common) in
      let id = Scalar.(sub (rx * den) num) in
      Scalar.mul q id
    in
    (* q · [ry · (1 - d · b · px · qx · py · qy)
        - (b · py · qy + py - py · b - a · px · b · qx)] = 0 *)
    let second_identity =
      let num =
        Scalar.(sub (b * py * qy) (param_a * px * b * qx) + sub py (py * b))
      in
      let den = Scalar.(sub one den_common) in
      let id = Scalar.(sub (ry * den) num) in
      Scalar.mul q id
    in
    SMap.of_list
      [
        (prefix @@ q_label ^ ".0", first_identity);
        (prefix @@ q_label ^ ".1", second_identity);
      ]

  let polynomials_degree =
    SMap.of_list
      [(left, 7); (right, 7); (output, 7); (top, 7); (bottom, 7); (q_label, 7)]

  let cs ~q:qec ~a:bit ~b:qx ~c:qy ~d:px ~e:py ~ag:_ ~bg:_ ~cg:_ ~dg:rx ~eg:ry
      ?precomputed_advice:_ () =
    let open L in
    let open Num in
    let sub x y = add ~qr:mone x y in
    let* px_qy = mul px qy in
    let* py_qx = mul py qx in
    let* pqs = mul px_qy py_qx in
    let* denom_first = custom ~qm:param_d bit pqs ~qc:one in
    (* q · [rx · (1 + d · b · px · qx · py · qy)
         - (b · px · qy + b · py · qx - b · px + px)] = 0 *)
    let* first_identity =
      (* left  = rx · (1 + d · b · px · qx · py · qy) *)
      let* left = mul denom_first rx in
      (* right = b · (px · qy + py · qx - px) + px    *)
      let* px_qy_plus_py_qx = add px_qy py_qx in
      let* px_qy_plus_py_qx_minux_px = sub px_qy_plus_py_qx px in
      let* right_b = mul bit px_qy_plus_py_qx_minux_px in
      let* right = add px right_b in
      (* all = left - right
             = rx · (1 + d · b · px · qx · py · qy)
               - (b · px · qy + b · py · qx - b · px + px) *)
      let* all = sub left right in
      mul qec all
    in
    (* q · [ry · (1 - d · b · px · qx · py · qy)
        - (b · py · qy - a · b · px · qx - b · py + py )] = 0 *)
    let* second_identity =
      (* left  = ry · (1 - d · b · px · qx · py · qy)  *)
      let* denom_second = add_constant ~ql:mone two denom_first in
      let* left = mul ry denom_second in
      (* right = b · (py · qy - a · px · qx - py) + py *)
      let* py_qy = mul py qy in
      let* a_px_qx = mul ~qm:param_a px qx in
      let* py_qy_plus_a_px_qx = sub py_qy a_px_qx in
      let* py_qy_plus_a_px_qx_minus_py = sub py_qy_plus_a_px_qx py in
      let* right_b = mul bit py_qy_plus_a_px_qx_minus_py in
      let* right = add right_b py in
      (* all = left - right
             = y · (1 - d · b · px · qx · py · qy)
               - (b · py · qy - a · b · px · qx - b · py + py ) *)
      let* all = sub left right in
      mul qec all
    in
    ret [first_identity; second_identity]
end
