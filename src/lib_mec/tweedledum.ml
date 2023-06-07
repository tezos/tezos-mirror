(**

  https://github.com/daira/tweedle

  Base field: 2^254 + 4707489545178046908921067385359695873 = 28948022309329048855892746252171976963322203655955319056773317069363642105857 (254 bits - 32 bytes)
  Scalar field: 2^254 + 4707489544292117082687961190295928833 = 28948022309329048855892746252171976963322203655954433126947083963168578338817 (254 bits - 32 bytes)

  Base field multiplication subgroup decomposition:
    2^33 * 3 * 5179 * 216901160674121772178243990852639108850176422522235334586122689
  Scalar field multiplicative subgroup decomposition:
    2^34 * 3 * 561665555565638329055562814312908972367531846121311209609791868583
*)

let two_z = Z.succ Z.one

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "4707489545178046908921067385359695873")
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.((two_z ** 254) + Z.of_string "4707489544292117082687961190295928833")
end)

module Jacobian =
  Ec.MakeJacobianWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 5)

      let cofactor = Z.one

      (* x = -1
         y = 2
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(to_bytes (negate (of_string "1")));
            Fq.(to_bytes (of_string "2"));
            Fq.(to_bytes one);
          ]
    end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 5)

      let cofactor = Z.one

      (* x = -1
         y = 2
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(to_bytes (negate (of_string "1")));
            Fq.(to_bytes (of_string "2"));
            Fq.(to_bytes one);
          ]
    end)

module Affine =
  Ec.MakeAffineWeierstrass (Fq) (Fp)
    (struct
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 5)

      let cofactor = Z.one

      (* x = -1
         y = 2
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(to_bytes (negate (of_string "1"))); Fq.(to_bytes (of_string "2"));
          ]
    end)

let from_affine_weierstrass_to_jacobian_weierstrass p =
  Ec.from_affine_weierstrass_to_jacobian_weierstrass
    (module Affine)
    (module Jacobian)
    p

let from_affine_weierstrass_to_projective_weierstrass p =
  Ec.from_affine_weierstrass_to_projective_weierstrass
    (module Affine)
    (module Projective)
    p

let from_jacobian_weierstrass_to_affine_weierstrass p =
  Ec.from_jacobian_weierstrass_to_affine_weierstrass
    (module Jacobian)
    (module Affine)
    p

let from_projective_weierstrass_to_affine_weierstrass p =
  Ec.from_projective_weierstrass_to_affine_weierstrass
    (module Projective)
    (module Affine)
    p
