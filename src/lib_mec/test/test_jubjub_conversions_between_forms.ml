open Mec.Curve.Jubjub

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f)

let test_generation () =
  let pt_ed1 = AffineEdwards.random () in
  let pt_mt1 = from_affine_edwards_to_affine_montgomery pt_ed1 in
  assert (Option.(is_some pt_mt1)) ;
  let pt_wt1 = from_affine_edwards_to_affine_weierstrass pt_ed1 in
  assert (Option.(is_some pt_wt1)) ;
  let pt_mt2 = AffineMontgomery.random () in
  let pt_wt2 = from_affine_montgomery_to_affine_weierstrass pt_mt2 in
  assert (Option.(is_some pt_wt2)) ;
  let pt_ed2 = from_affine_montgomery_to_affine_edwards pt_mt2 in
  assert (Option.(is_some pt_ed2))

let test_addition () =
  let pt_ed1 = AffineEdwards.random () in
  let pt_ed2 = AffineEdwards.random () in
  let pt_ed1_add = AffineEdwards.add pt_ed1 pt_ed2 in
  (* Twisted -> Montgomery *)
  let pt_mt1 = from_affine_edwards_to_affine_montgomery pt_ed1 |> Option.get in
  let pt_mt2 = from_affine_edwards_to_affine_montgomery pt_ed2 |> Option.get in
  let pt_mt1_add = AffineMontgomery.add pt_mt1 pt_mt2 in
  let pt_ed2_add =
    from_affine_montgomery_to_affine_edwards pt_mt1_add |> Option.get
  in
  let pt_mt2_add =
    from_affine_edwards_to_affine_montgomery pt_ed1_add |> Option.get
  in
  assert (AffineMontgomery.eq pt_mt1_add pt_mt2_add) ;
  assert (AffineEdwards.eq pt_ed1_add pt_ed2_add) ;
  (* Twisted -> Weierstrass *)
  let pt_wt1 = from_affine_edwards_to_affine_weierstrass pt_ed1 |> Option.get in
  let pt_wt2 = from_affine_edwards_to_affine_weierstrass pt_ed2 |> Option.get in
  let pt_wt1_add = AffineWeierstrass.add pt_wt1 pt_wt2 in
  let pt_wt2_add =
    from_affine_edwards_to_affine_weierstrass pt_ed1_add |> Option.get
  in
  assert (AffineWeierstrass.eq pt_wt1_add pt_wt2_add) ;
  (* Montgomery -> Twisted *)
  let pt_mta = AffineMontgomery.random () in
  let pt_mtb = AffineMontgomery.random () in
  let pt_mta_add = AffineMontgomery.add pt_mta pt_mtb in
  let pt_eda = from_affine_montgomery_to_affine_edwards pt_mta |> Option.get in
  let pt_edb = from_affine_montgomery_to_affine_edwards pt_mtb |> Option.get in
  let pt_eda_add = AffineEdwards.add pt_eda pt_edb in
  let pt_mtb_add =
    from_affine_edwards_to_affine_montgomery pt_eda_add |> Option.get
  in
  let pt_edb_add =
    from_affine_montgomery_to_affine_edwards pt_mta_add |> Option.get
  in
  assert (AffineEdwards.eq pt_eda_add pt_edb_add) ;
  assert (AffineMontgomery.eq pt_mta_add pt_mtb_add)

let test_multiplication () =
  let pt_ed1 = AffineEdwards.random () in
  let alpha = AffineEdwards.Scalar.random () in
  let pt_ed1_mul = AffineEdwards.mul pt_ed1 alpha in
  (* Twisted -> Montgomery *)
  let pt_mt1 = from_affine_edwards_to_affine_montgomery pt_ed1 |> Option.get in
  let alpha = AffineMontgomery.Scalar.of_z (AffineEdwards.Scalar.to_z alpha) in
  let pt_mt1_mul = AffineMontgomery.mul pt_mt1 alpha in
  let pt_ed2_mul =
    from_affine_montgomery_to_affine_edwards pt_mt1_mul |> Option.get
  in
  let pt_mt2_mul =
    from_affine_edwards_to_affine_montgomery pt_ed1_mul |> Option.get
  in
  assert (AffineMontgomery.eq pt_mt1_mul pt_mt2_mul) ;
  assert (AffineEdwards.eq pt_ed1_mul pt_ed2_mul) ;
  (* Twisted -> Weierstrass *)
  let pt_wt1 = from_affine_edwards_to_affine_weierstrass pt_ed1 |> Option.get in
  let alpha =
    AffineWeierstrass.Scalar.of_z (AffineMontgomery.Scalar.to_z alpha)
  in
  let pt_wt1_mul = AffineWeierstrass.mul pt_wt1 alpha in
  let pt_wt2_mul =
    from_affine_edwards_to_affine_weierstrass pt_ed1_mul |> Option.get
  in
  assert (AffineWeierstrass.eq pt_wt1_mul pt_wt2_mul) ;
  (* Montgomery -> Twisted *)
  let pt_mta = AffineMontgomery.random () in
  let beta = AffineMontgomery.Scalar.random () in
  let pt_mta_mul = AffineMontgomery.mul pt_mta beta in
  let pt_eda = from_affine_montgomery_to_affine_edwards pt_mta |> Option.get in
  let beta = AffineEdwards.Scalar.of_z (AffineMontgomery.Scalar.to_z beta) in
  let pt_eda_mul = AffineEdwards.mul pt_eda beta in
  let pt_mtb_mul =
    from_affine_edwards_to_affine_montgomery pt_eda_mul |> Option.get
  in
  let pt_edb_mul =
    from_affine_montgomery_to_affine_edwards pt_mta_mul |> Option.get
  in
  assert (AffineEdwards.eq pt_eda_mul pt_edb_mul) ;
  assert (AffineMontgomery.eq pt_mta_mul pt_mtb_mul)

let get_tests () =
  let open Alcotest in
  ( "Affine Edwards <-> Affine Montgomery ; Affine Edwards -> Affine Weierstrass",
    [
      test_case "Points are on both curves" `Quick (repeat 100 test_generation);
      test_case "Addition" `Quick (repeat 100 test_addition);
      test_case "Scalar multiplication" `Quick (repeat 100 test_multiplication);
    ] )

let () =
  let open Alcotest in
  run ~__FILE__ "JubJub" [get_tests ()]
