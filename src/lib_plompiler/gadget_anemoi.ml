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

open Lang_core
open Lang_stdlib
module AnemoiPerm = Bls12_381_hash.Permutation.Anemoi

module Make (L : LIB) = struct
  open L

  (* FIXME: should be removed when
     https://gitlab.com/nomadic-labs/cryptography/privacy-team/-/merge_requests/387
     is merged *)
  let parameters = AnemoiPerm.Parameters.security_128_state_size_2

  let nb_rounds = AnemoiPerm.Parameters.get_number_of_rounds parameters

  let matrix = AnemoiPerm.Parameters.get_matrix parameters

  let rc = AnemoiPerm.Parameters.get_round_constants parameters

  let rc =
    Array.init
      (Array.length rc + 2)
      (fun i ->
        (* We pad with two zeroes *)
        if i / 2 = nb_rounds then Bls12_381.Fr.zero
        else if i mod 2 = 0 then rc.(i / 2)
        else rc.(nb_rounds + (i / 2)))

  let rec repeat : n:int -> ('a -> 'a t) -> 'a -> 'a t =
   fun ~n f e ->
    if n <= 0 then ret e
    else
      let* x = f e in
      repeat ~n:(n - 1) f x

  let round :
      scalar repr * scalar repr * int -> (scalar repr * scalar repr * int) t =
   fun (xi, yi, i) ->
    let kx, ky = (rc.(i), rc.(i + 1)) in
    with_label ~label:"Anemoi.round"
    @@ let* res = Anemoi.anemoi_round ~kx ~ky (xi, yi) in
       let xj, yj = of_pair res in
       ret @@ (xj, yj, i + 2)

  let init_state_for_rounds x0 y0 =
    let* x00 =
      Num.add
        ~ql:matrix.(0).(0)
        ~qr:matrix.(0).(1)
        ~qc:S.((matrix.(0).(0) * rc.(0)) + (matrix.(0).(1) * rc.(1)))
        x0
        y0
    in
    let* y00 =
      Num.add
        ~ql:matrix.(1).(0)
        ~qr:matrix.(1).(1)
        ~qc:S.((matrix.(1).(0) * rc.(0)) + (matrix.(1).(1) * rc.(1)))
        x0
        y0
    in
    ret (x00, y00)

  let compress : scalar repr -> scalar repr -> scalar repr t =
   fun x0 y0 ->
    with_label ~label:"Anemoi.compress"
    @@ let* x00, y00 = init_state_for_rounds x0 y0 in
       let* xn, yn, _i = repeat ~n:nb_rounds round (x00, y00, 2) in
       Num.add_list (to_list [x0; y0; xn; yn])

  let double_round :
      scalar repr * scalar repr * int -> (scalar repr * scalar repr * int) t =
   fun (xi, yi, i) ->
    let kx1, ky1 = (rc.(i), rc.(i + 1)) in
    let kx2, ky2 = (rc.(i + 2), rc.(i + 3)) in
    with_label ~label:"Anemoi.double_round"
    @@ let* res = Anemoi.anemoi_double_round ~kx1 ~ky1 ~kx2 ~ky2 (xi, yi) in
       let xj, yj = of_pair res in
       ret @@ (xj, yj, i + 4)

  let compress_two : scalar repr -> scalar repr -> scalar repr t =
   fun x0 y0 ->
    with_label ~label:"Anemoi.compress_two"
    @@ let* x00, y00 = init_state_for_rounds x0 y0 in
       let* xn, yn, n = repeat ~n:(nb_rounds / 2) double_round (x00, y00, 2) in
       let* xn, yn, _ =
         if nb_rounds mod 2 = 0 then ret (xn, yn, 0) else round (xn, yn, n)
       in
       Num.add_list (to_list [x0; y0; xn; yn])

  let custom_round :
      scalar repr * scalar repr * int -> (scalar repr * scalar repr * int) t =
   fun (xi, yi, i) ->
    let kx1, ky1 = (rc.(i), rc.(i + 1)) in
    let kx2, ky2 = (rc.(i + 2), rc.(i + 3)) in
    with_label ~label:"Anemoi.custom_round"
    @@ let* res = Anemoi.anemoi_custom ~kx1 ~ky1 ~kx2 ~ky2 (xi, yi) in
       let xj, yj = of_pair res in
       ret @@ (xj, yj, i + 4)

  let compress_custom : scalar repr -> scalar repr -> scalar repr t =
   fun x0 y0 ->
    with_label ~label:"Anemoi.compress_custom"
    @@ let* x00, y00 = init_state_for_rounds x0 y0 in
       let* xn, yn, n = repeat ~n:(nb_rounds / 2) custom_round (x00, y00, 2) in
       let* xn, yn, _ =
         if nb_rounds mod 2 = 0 then ret (xn, yn, 0) else round (xn, yn, n)
       in
       Num.add_list (to_list [x0; y0; xn; yn])

  let compress_19_5 : scalar repr -> scalar repr -> scalar repr t =
   fun x0 y0 ->
    (* Anemoi of 20 rounds without the first linear layer *)
    (* let* x1, y1 = init_state_for_rounds x0 y0 in *)
    with_label ~label:"Anemoi.19.5"
    @@ let* xn, yn, _ = repeat ~n:10 custom_round (x0, y0, 0) in
       Num.add_list (to_list [x0; y0; xn; yn])

  let digest : ?input_length:int -> scalar list repr -> scalar repr t =
   fun ?input_length:_ inputs ->
    match of_list inputs with
    | [] -> Num.constant (AnemoiPerm.jive128_1 S.zero S.zero)
    | [x] ->
        let* zero = Num.zero in
        compress_19_5 zero x
    | x :: rest -> foldM compress_19_5 x rest
end

module Anemoi128 = struct
  module P : Hash_sig.P_HASH = struct
    type scalar = S.t

    let anemoi_instance =
      (* 19 rounds *)
      AnemoiPerm.(allocate_ctxt Parameters.security_128_state_size_2)

    let jive x y =
      let state =
        AnemoiPerm.set_state anemoi_instance [|x; y|] ;
        AnemoiPerm.apply_flystel anemoi_instance ;
        AnemoiPerm.apply_permutation anemoi_instance ;
        AnemoiPerm.get_state anemoi_instance
      in
      S.(state.(0) + state.(1) + x + y)

    let direct ?input_length:_ inputs =
      match Array.to_list inputs with
      | [] -> jive S.zero S.zero
      | [x] -> jive S.zero x
      | x :: rest -> List.fold_left jive x rest

    type ctxt = S.t

    let init ?input_length:_ () = S.zero

    let digest ctxt inputs =
      ignore ctxt ;
      direct inputs

    let get ctxt = ctxt
  end

  (* compute one round and return intermediate values *)
  let compute_one_round x0 y0 kx ky =
    let beta = AnemoiPerm.Parameters.beta in
    let gamma = AnemoiPerm.Parameters.gamma in
    let delta = AnemoiPerm.Parameters.delta in
    let g = AnemoiPerm.Parameters.g in
    let alpha_inv = AnemoiPerm.Parameters.alpha_inv in
    let g2_p_1 = S.((g * g) + one) in
    (* -> Sbox *)
    (* w^5 = x0 - (beta y0^2 + gamma) *)
    let w_5 = S.(sub x0 ((beta * y0 * y0) + gamma)) in
    (* Computing w *)
    let w = S.(pow w_5 (to_z alpha_inv)) in
    (* v = y0 - w *)
    let v = S.sub y0 w in
    (* u = w^5 + beta * v^2 + delta *)
    let u = S.(w_5 + ((beta * v * v) + delta)) in
    (* -> Linear layer + rc *)
    (* x1 = (u + kx) + g * (v + ky) *)
    let x1 = S.(u + kx + (g * (v + ky))) in
    (* y1 = (g * (u + kx) + (g^2 + 1) * (v + ky) *)
    let y1 = S.((g * (u + kx)) + (g2_p_1 * (v + ky))) in
    (w_5, w, v, u, x1, y1)

  module V : Hash_sig.HASH = Make
end
