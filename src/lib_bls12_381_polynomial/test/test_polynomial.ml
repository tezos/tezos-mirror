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

module Fr = Bls12_381.Fr
module G1 = Bls12_381.G1
module Fr_generation = Octez_bls12_381_polynomial.Fr_carray
module Poly = Polynomial.MakeUnivariate (Fr)
module Domain = Octez_bls12_381_polynomial.Domain.Domain_unsafe
module Poly_c = Octez_bls12_381_polynomial.Polynomial.Polynomial_impl

let p_of_c : Poly_c.t -> Poly.t =
 fun poly -> Poly_c.to_sparse_coefficients poly |> Poly.of_coefficients

let test_copy_polynomial () =
  let poly = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  assert (Poly_c.equal poly (Poly_c.copy poly))

let test_erase () =
  let poly = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  Poly_c.erase poly ;
  assert (Poly_c.equal poly Poly_c.zero)

let test_get_zero () =
  let p = Poly_c.zero in
  assert (Fr.(Poly_c.get p 0 = zero)) ;
  Helpers.must_fail (fun () -> ignore @@ Poly_c.get p (-1)) ;
  Helpers.must_fail (fun () -> ignore @@ Poly_c.get p 1)

let test_get_one () =
  let p = Poly_c.one in
  assert (Fr.(Poly_c.get p 0 = one)) ;
  Helpers.must_fail (fun () -> ignore @@ Poly_c.get p (-1)) ;
  Helpers.must_fail (fun () -> ignore @@ Poly_c.get p 1)

let test_get_random () =
  let module Poly_c = Octez_bls12_381_polynomial.Polynomial.Polynomial_impl in
  let module C_array = Octez_bls12_381_polynomial.Fr_carray in
  let degree = 1 + Random.int 100 in
  let p = Poly_c.of_coefficients [(Fr.one, degree)] in
  assert (Poly_c.length p = degree + 1) ;
  Helpers.repeat
    10
    (fun () ->
      assert (Fr.eq (Poly_c.get p (Random.int (Poly_c.length p - 1))) Fr.zero))
    () ;
  Helpers.(
    repeat
      10
      (fun () ->
        let i = Random.int 10 in
        must_fail (fun () -> ignore @@ Poly_c.get p (-i - 1)) ;
        must_fail (fun () -> ignore @@ Poly_c.get p (Poly_c.length p + i)))
      ())

let test_one () =
  let p = Poly_c.one in
  let expected = Poly.one in
  assert (Poly.equal (p_of_c p) expected)

let test_degree () =
  let p1 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  assert (Poly_c.degree p1 = Poly.degree_int (p_of_c p1))

let test_add () =
  let p1 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let p2 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let res = Poly_c.add p1 p2 in
  let expected_res = Poly.add (p_of_c p1) (p_of_c p2) in
  assert (Poly.equal (p_of_c res) expected_res)

let test_add_inplace () =
  let p1 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let p2 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let expected_res = Poly.add (p_of_c p1) (p_of_c p2) in
  let res = Poly_c.(allocate (max (length p1) (length p2))) in
  Poly_c.add_inplace res p1 p2 ;
  assert (Poly.equal (p_of_c res) expected_res)

let test_sub () =
  let p1 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let p2 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let res = Poly_c.sub p1 p2 in
  let expected_res = Poly.sub (p_of_c p1) (p_of_c p2) in
  assert (Poly.equal (p_of_c res) expected_res)

let test_sub_inplace () =
  let p1 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let p2 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let expected_res = Poly.sub (p_of_c p1) (p_of_c p2) in
  let res = Poly_c.(allocate (max (length p1) (length p2))) in
  Poly_c.sub_inplace res p1 p2 ;
  assert (Poly.equal (p_of_c res) expected_res)

let test_mul () =
  let p1 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let p2 = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let res = Poly_c.mul p1 p2 in
  let expected_res = Poly.polynomial_multiplication (p_of_c p1) (p_of_c p2) in
  assert (Poly.equal (p_of_c res) expected_res) ;
  let dres = Poly_c.degree res in
  let d1 = Poly_c.degree p1 in
  let d2 = Poly_c.degree p2 in
  if d1 = -1 || d2 = -1 then assert (dres = -1) else assert (dres = d1 + d2)

let test_opposite () =
  let p = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let res = Poly_c.opposite p in
  let expected_res = Poly.opposite (p_of_c p) in
  assert (Poly.equal (p_of_c res) expected_res)

let test_opposite_inplace () =
  let p = Poly_c.generate_biased_random_polynomial (Random.int 100) in
  let expected_res = Poly.opposite (p_of_c p) in
  Poly_c.opposite_inplace p ;
  assert (Poly.equal (p_of_c p) expected_res)

let test_mul_by_scalar () =
  let p = Poly_c.generate_biased_random_polynomial 100 in
  let s = Fr.random () in
  let res = Poly_c.mul_by_scalar s p in
  let expected_res = Poly.mult_by_scalar s (p_of_c p) in
  assert (Poly.equal (p_of_c res) expected_res)

let test_mul_by_scalar_inplace () =
  let p = Poly_c.generate_biased_random_polynomial 100 in
  let s = Fr.random () in
  let expected_res = Poly.mult_by_scalar s (p_of_c p) in
  Poly_c.mul_by_scalar_inplace p s p ;
  assert (Poly.equal (p_of_c p) expected_res)

let test_is_zero () =
  let size = Random.int 100 in
  let poly_zero =
    Poly_c.of_coefficients (List.init size (fun i -> (Fr.copy Fr.zero, i)))
  in
  assert (Poly_c.is_zero poly_zero)

let test_evaluate () =
  let p = Poly_c.generate_biased_random_polynomial 10 in
  let s = Fr.random () in
  let expected_res = Poly.evaluation (p_of_c p) s in
  let res = Poly_c.evaluate p s in
  assert (Fr.eq res expected_res)

(* division by (X^n + c) *)
let test_division_xn_plus_const poly n const =
  let poly_caml = p_of_c poly in
  let poly_xn_plus_const_caml =
    Poly.of_coefficients [(Fr.one, n); (const, 0)]
  in
  let res_q, res_r = Poly_c.division_xn poly n const in
  let res_q_caml = p_of_c res_q in
  let res_r_caml = p_of_c res_r in
  let expected_quotient, expected_reminder =
    Poly.euclidian_division_opt poly_caml poly_xn_plus_const_caml |> Option.get
  in
  assert (Poly.equal res_q_caml expected_quotient) ;
  assert (Poly.equal res_r_caml expected_reminder)

(* exact division by (X^n + c) *)
let test_division_exact_xn_plus_const poly_non_divisible n const =
  let poly_non_divisible_caml = p_of_c poly_non_divisible in
  let poly_xn_plus_const_caml =
    Poly.of_coefficients [(Fr.one, n); (const, 0)]
  in
  let poly_divisible_caml =
    Poly.(poly_non_divisible_caml * poly_xn_plus_const_caml)
  in
  let poly_divisible =
    Poly_c.of_coefficients (Poly.get_list_coefficients poly_divisible_caml)
  in
  let res_q, res_r = Poly_c.division_xn poly_divisible n const in
  let res_q_caml = p_of_c res_q in
  let res_r_caml = p_of_c res_r in
  let expected_quotient, expected_reminder =
    Poly.euclidian_division_opt poly_divisible_caml poly_xn_plus_const_caml
    |> Option.get
  in
  let poly_xn_plus_const = Poly_c.(of_coefficients [(Fr.one, n); (const, 0)]) in
  assert (Poly.equal res_q_caml expected_quotient) ;
  assert (Poly.equal res_r_caml expected_reminder) ;
  assert (Poly.equal Poly.zero expected_reminder) ;
  assert (Poly_c.(mul_xn res_q n const = poly_divisible)) ;
  assert (Poly_c.(mul res_q poly_xn_plus_const = poly_divisible))

(* division by (X - z) *)
let test_division_x_z () =
  let poly_non_divisible =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 100 in
      if Poly_c.degree p > 0 then p else generate ()
    in
    generate ()
  in
  let z = Fr.random () in
  let minus_z = Fr.negate z in
  test_division_exact_xn_plus_const poly_non_divisible 1 minus_z ;
  test_division_xn_plus_const poly_non_divisible 1 minus_z

(* division by (X^n - 1), degree a random poly >= 2 * n *)
let test_division_xn_minus_one () =
  let n = 10 in
  let poly_non_divisible =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 100 in
      if Poly_c.degree p >= 2 * n then p else generate ()
    in
    generate ()
  in
  let minus_one = Fr.(negate one) in
  test_division_exact_xn_plus_const poly_non_divisible n minus_one ;
  test_division_xn_plus_const poly_non_divisible n minus_one

(* division by (X^n - 1), degree a random poly = 2 * n *)
let test_division_xn_minus_one_limit_case () =
  (* We test the limit case in which the size of the polynomial is equal to 2*n
     ie. the degree is equal to 2*n. *)
  let n = 16 in
  let poly_non_divisible = Poly_c.of_coefficients [(Fr.random (), n - 1)] in
  let poly_non_divisible =
    Poly_c.add
      poly_non_divisible
      (Poly_c.generate_biased_random_polynomial (n - 2))
  in
  let minus_one = Fr.(negate one) in
  test_division_exact_xn_plus_const poly_non_divisible n minus_one ;
  test_division_xn_plus_const poly_non_divisible n minus_one

(* division by (X^n - 1), degree a random poly >= n and < 2 * n *)
let test_division_xn_minus_one_lt_2n () =
  let n = 10 in
  let poly_non_divisible =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 20 in
      let poly_degree = Poly_c.degree p in
      if n <= poly_degree && poly_degree < 2 * n then p else generate ()
    in
    generate ()
  in
  let minus_one = Fr.(negate one) in
  test_division_exact_xn_plus_const poly_non_divisible n minus_one ;
  test_division_xn_plus_const poly_non_divisible n minus_one

(* division by (X^n + 1), degree a random poly >= 2 * n *)
let test_division_xn_plus_one () =
  let n = 10 in
  let poly_non_divisible =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 100 in
      if Poly_c.degree p >= 2 * n then p else generate ()
    in
    generate ()
  in
  test_division_exact_xn_plus_const poly_non_divisible n Fr.one ;
  test_division_xn_plus_const poly_non_divisible n Fr.one

(* division by (X^n + 1), degree a random poly >= n and < 2 * n *)
let test_division_xn_plus_one_lt_2n () =
  let n = 10 in
  let poly_non_divisible =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 20 in
      let poly_degree = Poly_c.degree p in
      if n <= poly_degree && poly_degree < 2 * n then p else generate ()
    in
    generate ()
  in
  test_division_exact_xn_plus_const poly_non_divisible n Fr.one ;
  test_division_xn_plus_const poly_non_divisible n Fr.one

(* division by (X^n + c), degree a random poly >= 2 * n *)
let test_division_xn_plus_c () =
  let n = 10 in
  let poly_non_divisible =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 100 in
      if Poly_c.degree p >= 2 * n then p else generate ()
    in
    generate ()
  in
  let c = Fr.random () in
  test_division_exact_xn_plus_const poly_non_divisible n c ;
  test_division_xn_plus_const poly_non_divisible n c

(* division by (X^n + c), degree a random poly >= n and < 2 * n *)
let test_division_xn_plus_c_lt_2n () =
  let n = 10 in
  let poly_non_divisible =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 20 in
      let poly_degree = Poly_c.degree p in
      if n <= poly_degree && poly_degree < 2 * n then p else generate ()
    in
    generate ()
  in
  let c = Fr.random () in
  test_division_exact_xn_plus_const poly_non_divisible n c ;
  test_division_xn_plus_const poly_non_divisible n c

let test_linear () =
  let n = 4 in
  let polys =
    List.init n (fun i ->
        if i = 2 then Poly_c.zero
        else Poly_c.generate_biased_random_polynomial (Random.int 100))
  in
  let coeffs = List.init n (fun _i -> Fr.random ()) in
  let res = Poly_c.linear polys coeffs in
  let expected_res =
    List.fold_left2
      (fun acc coeff poly ->
        Poly.add acc @@ Poly.mult_by_scalar coeff (p_of_c poly))
      Poly.zero
      coeffs
      polys
  in
  assert (Poly.equal (p_of_c res) expected_res)

let test_linear_with_powers () =
  let n = 4 in
  let polys =
    List.init n (fun i ->
        if i = 2 then Poly_c.zero
        else Poly_c.generate_biased_random_polynomial (Random.int 100))
  in
  let coeff = Fr.random () in
  let coeffs = Fr_generation.powers n coeff |> Array.to_list in
  let res1 = Poly_c.linear polys coeffs in
  let res2 = Poly_c.linear_with_powers polys coeff in
  let expected_res =
    List.fold_left2
      (fun acc coeff poly ->
        Poly.add acc @@ Poly.mult_by_scalar coeff (p_of_c poly))
      Poly.zero
      coeffs
      polys
  in
  assert (Poly.equal (p_of_c res1) expected_res) ;
  assert (Poly.equal (p_of_c res2) expected_res)

let test_linear_with_powers_equal_length () =
  let n = 4 in
  let p =
    let rec generate () =
      let p = Poly_c.generate_biased_random_polynomial 100 in
      if Poly_c.degree p > 10 then p else generate ()
    in
    generate ()
  in
  let polys =
    List.init n (fun _ ->
        Poly_c.add p (Poly_c.generate_biased_random_polynomial 10))
  in
  let coeff = Fr.random () in
  let coeffs = Fr_generation.powers n coeff |> Array.to_list in
  let res = Poly_c.linear_with_powers polys coeff in
  let expected_res =
    List.fold_left2
      (fun acc coeff poly ->
        Poly.add acc @@ Poly.mult_by_scalar coeff (p_of_c poly))
      Poly.zero
      coeffs
      polys
  in
  assert (Poly.equal (p_of_c res) expected_res)

let test_of_sparse_coefficients () =
  let test_vectors =
    [
      [|(Fr.random (), 0)|];
      (* FIXME: repr for zero polynomial *)
      (* [|(Fr.(copy zero), 0)|] *)
      [|(Fr.random (), 0); (Fr.(copy one), 1)|];
      Array.init (1 + Random.int 100) (fun i -> (Fr.random (), i));
      (* with zero coefficients somewhere *)
      [|(Fr.random (), 1)|];
      (let n = 1 + Random.int 100 in
       Array.init n (fun i -> (Fr.random (), (i * 100) + Random.int 100)));
      (* Not in order *)
      [|(Fr.random (), 1); (Fr.random (), 0)|];
      (* random polynomial where we shuffle randomly the coefficients *)
      (let n = 1 + Random.int 100 in
       let p =
         Array.init n (fun i -> (Fr.random (), (i * 100) + Random.int 100))
       in
       Array.fast_sort (fun _ _ -> if Random.bool () then 1 else -1) p ;
       p);
    ]
  in
  List.iter
    (fun coefficients ->
      let polynomial_c = Poly_c.of_coefficients (Array.to_list coefficients) in
      let polynomial = Poly_c.to_sparse_coefficients polynomial_c in
      Array.fast_sort (fun (_, i1) (_, i2) -> Int.compare i1 i2) coefficients ;
      List.iter2
        (fun (x1, i1) (x2, i2) ->
          if not (Fr.eq x1 x2 && i1 = i2) then
            Alcotest.failf
              "Expected output (%s, %d), computed (%s, %d)"
              (Fr.to_string x1)
              i1
              (Fr.to_string x2)
              i2)
        (Array.to_list coefficients)
        polynomial)
    test_vectors

let test_vectors_fft_aux =
  List.iter (fun (points, expected_fft_results, root, n) ->
      let log = Z.(log2up @@ of_int n) in
      let primitive_root = Fr.of_string root in
      let points =
        Array.mapi (fun i v -> (Fr.of_string v, i)) points |> Array.to_list
      in
      let expected_fft_results = Array.map Fr.of_string expected_fft_results in
      let polynomial =
        Octez_bls12_381_polynomial.Polynomial.Polynomial_unsafe.of_coefficients
          points
      in
      let domain = Domain.build_power_of_two ~primitive_root log in
      let evaluation =
        Octez_bls12_381_polynomial.Evaluations.evaluation_fft domain polynomial
      in
      assert (
        Array.for_all2
          Fr.eq
          expected_fft_results
          (Octez_bls12_381_polynomial.Evaluations.to_array evaluation)) ;
      let ifft_results : Octez_bls12_381_polynomial.Polynomial.t =
        Octez_bls12_381_polynomial.Evaluations.interpolation_fft
          domain
          evaluation
      in
      assert (
        Octez_bls12_381_polynomial.Polynomial.equal polynomial ifft_results))

let test_vectors_fft () =
  (* Generated using {{: https://github.com/dannywillems/ocaml-polynomial} https://github.com/dannywillems/ocaml-polynomial},
     commit 8351c266c4eae185823ab87d74ecb34c0ce70afe with the following program:
     {[
     module Fr = Ff.MakeFp (struct
       let prime_order = Z.of_string "52435875175126190479447740508185965837690552500527637822603658699938581184513"
     end)

     module Poly = Polynomial.MakeUnivariate (Fr)

     let () = Random.self_init ()

     let n = 16 in
     let root = Fr.of_string "16624801632831727463500847948913128838752380757508923660793891075002624508302" in
     let domain = Polynomial.generate_evaluation_domain (module Fr) n root in
     let coefficients = List.init n (fun i -> (Fr.random (), i)) in
     let result_fft = Poly.evaluation_fft ~domain (Poly.of_coefficients coefficients) in
     Printf.printf "Random generated points: [%s]\n"
     (String.concat "; " (List.map (fun s -> Printf.sprintf "\"%s\"" (Fr.to_string s)) (List.map fst coefficients)))

     Printf.printf "Results FFT: [%s]\n" (String.concat "; " (List.map (fun s -> Printf.sprintf "\"%s\"" (Fr.to_string s)) result_fft))
     ]} *)
  [
    ( [|
        "27368034540955591518185075247638312229509481411752400387472688330662143761856";
        "19540886853600136773806888540031779652697522926951761090609474934921975120659";
        "26220624956959285725992525915931330099055855809419283071707941601749666540606";
        "35989465088288326521015971876164012326945860638993195438436634989311471699248";
        "40768987516446156628891831412618502769603720088125758542973088262778427031409";
        "37502582815775325591532709222383633732324588493894705918379415386493138186217";
        "36239410834198262339129557342130251912163884537720472258419319596614475181710";
        "30052101390787354520041305700295264045780224441445621717858661812094895346346";
        "21995102202891872269281007631877407834312363335297399149197261878759964569557";
        "2080131691710661742168916043433037277601198784836429928476841594059118749370";
        "42808036164195249275280963312025828986508786508614910971333518929197538998773";
        "5416726269860450738839660947415682809504169447200045382722531315472176837580";
        "35385358739760726215042915512119186540038118703012759099153921687310544107033";
        "27079498335589470388559429012071885383086029562052523482197446658383111072774";
        "14990895240018155420388973968063729735246061536340426651548494721699138380965";
        "44309562300548454609435401663522908994502966824080187553903158603409727800199";
      |],
      [|
        "28260403540575956442011209282235027627356413045516778063561130703408863908198";
        "17993250808843485497593981839623744555167943862527528485216257064528703538115";
        "28899493105874864372514833666497043025570659782554515014837391083549224357663";
        "51337256534645387155350783214024321835908156339022153213532792354442520429110";
        "36105366491256902485994568622814429160585987748654956293511966452904096000801";
        "43254842638818221613648051676914453096533481207151298911620538414624271037642";
        "30137483332361623247450591551673456707141812995305992129418088848770541392731";
        "51832887160148947731754609316481314302061191268958147833796630151024005898627";
        "43805495449265118506792567337086345883995710810828939619222069714626283759516";
        "43490811501888359185770575458084504215306703326085936961313644380351136142796";
        "45353492906491645466864766616545361422166823184893464158187486307135027299228";
        "25825572114166843629678422732449048165822431962411503253462472518376902250644";
        "26847540293236075734670790417576073958082755044059129980667062867535005919314";
        "27595037266661293341555794625370636649048746138720916959755452489901838331803";
        "50624068481465143084533854525326091050293490363529345388747088297570661644827";
        "43833176554968168233119024604069041530181053009400713400523917741660961832220";
      |],
      "16624801632831727463500847948913128838752380757508923660793891075002624508302",
      16 );
    ( [|
        "47158772775073549946552793332529610487862373214948589019241181027783090564927";
        "6969718530264830180568846645175610175808087246047159746318756753094302772749";
        "9686871355228041364920849477160310252453883661104177346095519526469628056477";
        "23701529205156171747824936169986903488212555374755529649712750610781499462060";
        "51871556544025649087958327041446036456025707894309969231504981069404849216674";
        "49777423642698315996233125696731960180992156573003897035030679033255179062106";
        "45193193026211585427447988721783950037458564208700328346100038403604928063434";
        "22127747185998681281737634883536464039523214813021737348626571438746574042602";
        "2436430336755214848244332120262587151205846315495966767110243193396303772169";
        "36030568404692126810397267528533249629542998741265626260816144992520627327404";
        "9733396005656647923887929901290339938184031472478829155648784527262943967802";
        "9232698019619721561468579430575787343679427145493378862256989115824088269321";
        "14667337314275170563143674471965744229406746565668692173766020639206764616232";
        "4730122363727714359627443666953284727545363839021842492384814113239394248107";
        "37600263337451873049904059627388200632297313939920669755923850829944793593331";
        "45293309035642456591052566417557511757964418893263771110015399573101864564094";
      |],
      [|
        "49159810856594417384836171575575789664328822394806699542327113948066763307898";
        "19513654523362036572945532378832190324412992065939315076003556719650225069528";
        "12400187722036186964464702144585831794106028402975104166770049464367024965836";
        "12729164750455352417868976767915518872789618782856391788646411468569638526867";
        "33894355416182050171353418403150653912631032966615091567798695189037336420208";
        "48124615888501419045682889152193022416530216136751831272743260607287479387889";
        "2671821870190640507797010788564244592122722767639756300432356882625084132990";
        "5747205261848282327286719985435091052484794664574217516630541561287563721611";
        "20484704306877713683149554254776007841626244646754279290228513586509772102603";
        "27810669340447093284494524394980216672115899286964546160004040217206390970278";
        "43245957846359230019664483425272837146730091593909167243373849997985752696625";
        "22694735552917590450918366121541571341777153373371464847736029264056462779996";
        "46382266250107013667570920582197666853273280950350971430513428795918673742221";
        "51480236715740128219868641008506780736289153092278122714177218949428874124399";
        "31214895100904294521197520923656651794655875018922463281556409805109003774848";
        "12370831947896207029058818364173897763780730291302175173295467787791916207957";
      |],
      "16624801632831727463500847948913128838752380757508923660793891075002624508302",
      16 );
  ]
  |> test_vectors_fft_aux

let test_big_vectors_fft () =
  (* Vectors generated with the following program:
     {[
     module Poly = Polynomial.MakeUnivariate (Bls12_381.Fr)

     let fft_polynomial () = Random.self_init ()

     let n = 16 in
     let root = Bls12_381.Fr.of_string "16624801632831727463500847948913128838752380757508923660793891075002624508302" in
     let domain = Array.init n (fun i -> Bls12_381.Fr.pow root (Z.of_int i)) in
     let pts = List.init (1 + Random.int (n - 1)) (fun _ -> Bls12_381.Fr.random ()) in
     let polynomial = Poly.of_coefficients (List.mapi (fun i a -> (a, i)) pts) in
     let result_fft = Poly.evaluation_fft ~domain polynomial in
     Printf.printf "Random generated points: [%s]\n"
     (String.concat "; " (List.map (fun s -> Printf.sprintf "\"%s\"" (Bls12_381.Fr.to_string s)) pts))

     Printf.printf "Results FFT: [%s]\n"
     (String.concat "; " (List.map (fun s -> Printf.sprintf "\"%s\"" (Bls12_381.Fr.to_string s)) result_fft))
     ]} *)
  [
    ( [|
        "9094991653442636551690401718409437467203667404120465574859260125376376539450";
        "36784955550505906583992321485589046801627651823699631865063763788121474956423";
        "23827544216835540859131494346185922491907194146637931126659231165131053381546";
        "10176016683576441255472989519285808950044942050632563738283008320151081557329";
        "41261344119924552928659481894133335263574984487635963901785259507577321585518";
        "28108264987024051584932616695427507902278417309474112669437967601423863612687";
        "21663005828102287486879023285429676198163987257708847741409763610604537443435";
        "18912970628341929012261641271508381962649936522242143993694370697397857088913";
        "33177098286124931552384105142324342074093558966750206832606157530164930555990";
        "41779089735219182784224021378358463745524240595217520312794088473702250244182";
        "24771759634084311767096550664156494017951139598922779731418065985892886762579";
        "18834636798335127532549678052953254168490017021191169393716309390907400251275";
        "37065057460533301959533800444892921530478486771898638531540967318564763301424";
        "18938863825307662380373110832265488863727283022825056414953618706445868152394";
      |],
      [|
        "49780348356600721362494793681804286411572191975791204892599880021830178326067";
        "8549591071530141239261747244017543764022520298682086923116546417177857002039";
        "35886012007479519208161417657801891880299072602521153742960206336995970034979";
        "36879757027451463267869158639366365984684566292751757860296441614566288944016";
        "25552627061385412172712093185426062975326634130990223990807408609971848562932";
        "40849721284704094903185176454822507982303428910642899972182407608731263436679";
        "40262217027396163892755703349196195510233017307031851974427648769362539590762";
        "4024657785152272655575160397887740541765239374805874835569295765424969048069";
        "17326002990737261971568478260144176649030530288392635052335578265162073706739";
        "47515927815717320642495388213183809977298704534265933353706448858555977417195";
        "52298143452904716702579823336245852729554594072189474112975263645940258094478";
        "38128262572060415006531869596459654355503839891826916597932054131315998785692";
        "22683861445494963106161608114363858441639566622753570669198100130199399042199";
        "42340678072800669141416985973284322598393757133909156380080108708870479614576";
        "41951631999415595936714396792701945895031651466767627001815596764929608220851";
        "8541552710134786973696810155146544642433231066298546595970787256557383095518";
      |],
      "16624801632831727463500847948913128838752380757508923660793891075002624508302",
      16 );
    ( [|
        "20366504166179122431562501467558245272875051944994165565786399718324788856679";
        "40707817767410470154858828539330531184811806360720222380433027956764139746466";
        "356740997816546714538836356735076115722082850319975887507567537568199256047";
        "48855616550298959913298962949425933402033459692306408089491507478720529722094";
        "50541121720442637012670577014433112674281325316959751220046011112849321159679";
        "23560938154727269982745286190878568921692353327526898815237861835948424192124";
        "30355630669939243314704291156412040070582169882207211757125280380204423737785";
        "15505400703222858077649159431058027041296078925582612030937675335604082942698";
        "9586508907273909539462890684309186991235869304827055764285226746962615270470";
        "21709482882717689551729218263314753077681455523073051228049601241465109175016";
      |],
      [|
        "51802261819523944775429590020711611401449443126406801448485524544657309321006";
        "29550508720460856371179136000468622846061053475009563533950956229272926799427";
        "34504932105265771677001237289210566670578056002926384303520505022799649248166";
        "4643308580964924754290358292327975630897331781437501325565301584135504508458";
        "17730606024639651178329906540477536898037036267737442452479452878455969345160";
        "24297582300913880282598264556022644683498819864163783595116300237736738839190";
        "45214789002547697621229629104795392415050372172770523100077751708945973280784";
        "38763628634933901441181735457381723197057249525798600597931200723695018553421";
        "13303125578400401812105381813625813334871897970626605473204470347345643686775";
        "32546955097754011854531536998637015836635047626862902049591688741227100748732";
        "43371134287888091869973665347576016158409006343795943923125773302177689132719";
        "977886516424948761804141006390103804220304457123540866565332209651749412953";
        "29397045052513916251128036257643354768448398900242489535886467742333654056319";
        "42811280985553165680233963683840751931239235399282560432833035292503761934416";
        "4300210716844780583005690840899166466043159214063580403793066175583344946263";
        "17520561584488394949873231287295559998885523992713701655662886152551750262101";
      |],
      "16624801632831727463500847948913128838752380757508923660793891075002624508302",
      16 );
    ( [|
        "32801662691963427514267894674769063086591208663284771582446704033458906830071";
        "24619295659426029871924262808019119093071500634075223399719630241414416329188";
        "30281390693300077132916380864478514127955276426883373365519750642923776606198";
        "18360093270499580872004343235099023022053747412073668894932667384067918433774";
        "29963087933945554223059107694526533492795491013596481736225165935107527885595";
        "11751763604609984739794412901449568244757509926148834238756168445779385705986";
        "12053604641353691632646194412265923075808946261200908259159925010321955112554";
        "18754076052090553735885674487706501053759862205295654431460792297041333363001";
        "47468650442554572799432426443366889486432535695490121813022148744472031223598";
        "14440572519495178160314880615884126543409017897541889779007038203125763034294";
      |],
      [|
        "30750696808733888764454616104821397875872886133480376209835356137958689786207";
        "49483516902638449411010713773222171172110676754879303430247846257920008023265";
        "50001076338014749759889979935035920868691846963545175128696820779919918368493";
        "29272587126301051909677034081892875172511198246651372545679439519796830186976";
        "47427833951319342460150948284104794355462688464805550185990729120932799600905";
        "49998526798262076618938116969936636882951238460630346414173801769033033742691";
        "25521743570973710093283381420987544378598602802917257652549357440730406563564";
        "45895319943762993960416590175848277115358075420167900587796337144240772114664";
        "12206720121869805442950689533062619474841267484792748189893739094916799607260";
        "21183683010445370049002762862965083896160882035130570824969727228122839130361";
        "33920383903205043356875140927108958078964986378193302844915854434108615373905";
        "31857510768286257012272964035049915872502891931888264780127736321339350249744";
        "35933102341174038602795018279545337530956784403240999005434298298714087655606";
        "14290588518514609077405374097049833967459854487008550973527796654640168619796";
        "39349821814970090673068610903119287156967024735530273188209056016596119181821";
        "7733491152943363036094373412554355586048433909694353357099368316372071075878";
      |],
      "16624801632831727463500847948913128838752380757508923660793891075002624508302",
      16 );
  ]
  |> test_vectors_fft_aux

let test_fft_evaluate_common length inner =
  let module Domain = Octez_bls12_381_polynomial.Domain in
  let module Poly = Octez_bls12_381_polynomial.Polynomial in
  let polynomial = Poly.generate_biased_random_polynomial length in
  let primitive_root = Fr_generation.primitive_root_of_unity length in
  let domain = Domain.build ~primitive_root length in
  let expected_result =
    Array.init length (fun i -> Poly.evaluate polynomial (Domain.get domain i))
  in
  let result = inner ~length ~primitive_root ~domain ~polynomial in
  assert (Array.for_all2 Fr.eq result expected_result)

let test_fft_evaluate () =
  let module Evaluations = Octez_bls12_381_polynomial.Evaluations in
  test_fft_evaluate_common
    16
    (fun ~length:_ ~primitive_root:_ ~domain ~polynomial ->
      Evaluations.(evaluation_fft domain polynomial |> to_array))

let test_fft_pfa_evaluate () =
  let module Evaluations = Octez_bls12_381_polynomial.Evaluations in
  let module Domain = Octez_bls12_381_polynomial.Domain in
  test_fft_evaluate_common
    (128 * 11)
    (fun ~length:_ ~primitive_root ~domain:_ ~polynomial ->
      let primroot1 = Fr.pow primitive_root (Z.of_int 11) in
      let primroot2 = Fr.pow primitive_root (Z.of_int 128) in
      let domain1 = Domain.build_power_of_two ~primitive_root:primroot1 7 in
      let domain2 = Domain.build ~primitive_root:primroot2 11 in
      Evaluations.(
        evaluation_fft_prime_factor_algorithm ~domain1 ~domain2 polynomial
        |> to_array))

let test_dft_evaluate () =
  let module Evaluations = Octez_bls12_381_polynomial.Evaluations in
  let module Domain = Octez_bls12_381_polynomial.Domain in
  let module Poly = Octez_bls12_381_polynomial.Polynomial in
  test_fft_evaluate_common
    (3 * 11 * 19)
    (fun ~length:_ ~primitive_root:_ ~domain ~polynomial ->
      Evaluations.(dft domain polynomial |> to_array))

let test_fft_interpolate () =
  let module Domain = Octez_bls12_381_polynomial.Domain in
  let module Poly_c = Octez_bls12_381_polynomial.Polynomial in
  let module Evaluations = Octez_bls12_381_polynomial.Evaluations in
  let n = 16 in
  let log = Z.(log2up @@ of_int n) in
  let domain = Domain.build_power_of_two log in
  let scalars = Array.init n (fun _ -> Fr.random ()) in
  let polynomial = Evaluations.interpolation_fft2 domain scalars in
  Array.iteri
    (fun i x ->
      assert (Fr.eq x (Poly_c.evaluate polynomial (Domain.get domain i))))
    scalars

let test_fft_interpolate_common length inner =
  let module Poly = Octez_bls12_381_polynomial.Polynomial in
  let primitive_root = Fr_generation.primitive_root_of_unity length in
  let polynomial = Poly.generate_biased_random_polynomial length in
  let result = inner ~length ~primitive_root ~polynomial in
  assert (Poly.equal polynomial result)

let test_ifft_random () =
  let module Domain = Octez_bls12_381_polynomial.Domain in
  let module Poly_c = Octez_bls12_381_polynomial.Polynomial in
  let module Evaluations = Octez_bls12_381_polynomial.Evaluations in
  test_fft_interpolate_common 16 (fun ~length ~primitive_root ~polynomial ->
      let domain = Domain.build_power_of_two ~primitive_root length in
      Evaluations.(evaluation_fft domain polynomial |> interpolation_fft domain))

let test_fft_pfa_interpolate () =
  let module Evaluations = Octez_bls12_381_polynomial.Evaluations in
  let module Domain = Octez_bls12_381_polynomial.Domain in
  test_fft_interpolate_common
    (4 * 3)
    (fun ~length:_ ~primitive_root ~polynomial ->
      let primroot1 = Fr.pow primitive_root (Z.of_int 3) in
      let primroot2 = Fr.pow primitive_root (Z.of_int 4) in
      let domain1 = Domain.build_power_of_two ~primitive_root:primroot1 2 in
      let domain2 = Domain.build ~primitive_root:primroot2 3 in
      Evaluations.(
        evaluation_fft_prime_factor_algorithm ~domain1 ~domain2 polynomial
        |> interpolation_fft_prime_factor_algorithm ~domain1 ~domain2))

let test_dft_interpolate () =
  let module Evaluations = Octez_bls12_381_polynomial.Evaluations in
  let module Domain = Octez_bls12_381_polynomial.Domain in
  let module Poly = Octez_bls12_381_polynomial.Polynomial in
  test_fft_interpolate_common
    (11 * 19)
    (fun ~length ~primitive_root ~polynomial ->
      let domain = Domain.build ~primitive_root length in
      Evaluations.(dft domain polynomial |> idft domain))

let test_of_dense () =
  let array = Array.init 10 (fun _ -> Fr.random ()) in
  let array_res = Poly_c.of_dense array |> Poly_c.to_dense_coefficients in
  assert (Array.for_all2 Fr.eq array array_res)

let test_of_carray_does_not_allocate_a_full_new_carray () =
  let n = 1 + Random.int 1_000 in
  let array = Array.init n (fun _ -> Fr.random ()) in
  let carray = Octez_bls12_381_polynomial.Fr_carray.of_array array in
  let polynomial =
    Octez_bls12_381_polynomial.Polynomial.Polynomial_unsafe.of_carray carray
  in
  let two = Fr.of_string "2" in
  Octez_bls12_381_polynomial.Polynomial.mul_by_scalar_inplace
    polynomial
    two
    polynomial ;
  let poly_values =
    Octez_bls12_381_polynomial.Polynomial.to_dense_coefficients polynomial
  in
  let carray_values = Octez_bls12_381_polynomial.Fr_carray.to_array carray in
  assert (Array.for_all2 Fr.eq poly_values carray_values)

let tests =
  let repetitions = 100 in
  List.map
    (fun (name, f) ->
      Alcotest.test_case name `Quick (Helpers.repeat repetitions f))
    [
      ("build_erase", test_erase);
      ("vectors_fft", test_vectors_fft);
      ("big_vectors_fft", test_big_vectors_fft);
      ("ifft_random", test_ifft_random);
      ("get_sparse_coefficients", test_of_sparse_coefficients);
      ("copy", test_copy_polynomial);
      ("get_zero", test_get_zero);
      ("get_one", test_get_one);
      ("get_random", test_get_random);
      ("one", test_one);
      ("degree", test_degree);
      ("add", test_add);
      ("add_inplace", test_add_inplace);
      ("sub", test_sub);
      ("sub_inplace", test_sub_inplace);
      ("mul", test_mul);
      ("opposite", test_opposite);
      ("opposite_inplace", test_opposite_inplace);
      ("mul_by_scalar", test_mul_by_scalar);
      ("mul_by_scalar_inplace", test_mul_by_scalar_inplace);
      ("is_zero", test_is_zero);
      ("evaluate", test_evaluate);
      ("division_x_z", test_division_x_z);
      ("division_xn_minus_one", test_division_xn_minus_one);
      ("division_xn_minus_one_limit_case", test_division_xn_minus_one_limit_case);
      ("division_xn_minus_one_lt_2n", test_division_xn_minus_one_lt_2n);
      ("division_xn_plus_one", test_division_xn_plus_one);
      ("division_xn_plus_one_lt_2n", test_division_xn_plus_one_lt_2n);
      ("division_xn_plus_c", test_division_xn_plus_c);
      ("division_xn_plus_c_lt_2n", test_division_xn_plus_c_lt_2n);
      ("test_linear", test_linear);
      ("test_linear_with_powers", test_linear_with_powers);
      ( "test_linear_with_powers_equal_length",
        test_linear_with_powers_equal_length );
      ("fft_evaluate", test_fft_evaluate);
      ("fft_interpolate", test_fft_interpolate);
      ("fft_pfa_evaluate", test_fft_pfa_evaluate);
      ("fft_pfa_interpolate", test_fft_pfa_interpolate);
      ("fft_dft_evaluate", test_dft_evaluate);
      ("fft_dft_interpolate", test_dft_interpolate);
      ("of_dense", test_of_dense);
      ( "of_carray does not copy the carray",
        test_of_carray_does_not_allocate_a_full_new_carray );
    ]
