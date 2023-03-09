let test_b_cannot_be_null_for_weierstrass_form () =
  let module Fq = Ff.MakeFp (struct
    let prime_order = Z.of_int 3
  end) in
  try
    let module E =
      Mec.Curve.Utils.Functor.MakeProjectiveWeierstrass (Fq) (Fq)
        (struct
          let a = Fq.random ()

          let b = Fq.zero

          let cofactor = Z.one

          let bytes_generator = Bytes.make (Fq.size_in_bytes * 3) '\000'
        end)
    in
    assert false
  with _ -> assert true

let () =
  Alcotest.run
    ~verbose:true
    "Curve functors"
    [
      ( "Check initialisation strengthen conditions",
        [
          Alcotest.test_case
            "b cannot be null"
            `Quick
            test_b_cannot_be_null_for_weierstrass_form;
        ] );
    ]
