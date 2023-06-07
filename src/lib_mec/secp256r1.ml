(**
  https://www.secg.org/SEC2-Ver-1.0.pdf, page 16

  Base field: 115792089210356248762697446949407573530086143415290314195533631308867097853951 (255 bits - 32 bytes)
  Scalar field: 115792089210356248762697446949407573529996955224135760342422259061068512044369 (255 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    2 * 3 * 5^2 * 17 * 257 * 641 * 1531 * 65537 * 490463 * 6700417 * 835945042244614951780389953367877943453916927241

  Prime field multiplication subgroup decomposition:
   2^4 * 3 * 71 * 131 * 373 * 3407 * 17449 * 38189 * 306279700557793653483863914058668676200382493196645316621
*)

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "115792089210356248762697446949407573530086143415290314195533631308867097853951"
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "115792089210356248762697446949407573529996955224135760342422259061068512044369"
end)

module Jacobian =
  Ec.MakeJacobianWeierstrass (Fq) (Fp)
    (struct
      let a =
        Fq.of_string
          "115792089210356248762697446949407573530086143415290314195533631308867097853948"

      let b =
        Fq.of_string
          "41058363725152142129326129780047268409114441015993725554835256314039467401291"

      let cofactor = Z.one

      (* x = 511607918243171233453336525287549545615842888188604004153674416748734923195030
         y = 730886786380646968340011037971715329076231014666828399308622702301047192244725
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(
              to_bytes
                (of_string
                   "48439561293906451759052585252797914202762949526041747995844080717082404635286"));
            Fq.(
              to_bytes
                (of_string
                   "36134250956749795798585127919587881956611106672985015071877198253568414405109"));
            Fq.(to_bytes one);
          ]
    end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      let a =
        Fq.of_string
          "115792089210356248762697446949407573530086143415290314195533631308867097853948"

      let b =
        Fq.of_string
          "41058363725152142129326129780047268409114441015993725554835256314039467401291"

      let cofactor = Z.one

      (* x = 511607918243171233453336525287549545615842888188604004153674416748734923195030
         y = 730886786380646968340011037971715329076231014666828399308622702301047192244725
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(
              to_bytes
                (of_string
                   "48439561293906451759052585252797914202762949526041747995844080717082404635286"));
            Fq.(
              to_bytes
                (of_string
                   "36134250956749795798585127919587881956611106672985015071877198253568414405109"));
            Fq.(to_bytes one);
          ]
    end)

module Affine =
  Ec.MakeAffineWeierstrass (Fq) (Fp)
    (struct
      let a =
        Fq.of_string
          "115792089210356248762697446949407573530086143415290314195533631308867097853948"

      let b =
        Fq.of_string
          "41058363725152142129326129780047268409114441015993725554835256314039467401291"

      let cofactor = Z.one

      (* x = 511607918243171233453336525287549545615842888188604004153674416748734923195030
         y = 730886786380646968340011037971715329076231014666828399308622702301047192244725
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(
              to_bytes
                (of_string
                   "48439561293906451759052585252797914202762949526041747995844080717082404635286"));
            Fq.(
              to_bytes
                (of_string
                   "36134250956749795798585127919587881956611106672985015071877198253568414405109"));
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
