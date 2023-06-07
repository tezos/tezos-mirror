(**
  Base field: 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1 = 115792089237316195423570985008687907853269984665640564039457584007908834671663 (255 bits - 32 bytes)
  Scalar field: 115792089237316195423570985008687907852837564279074904382605163141518161494337 (255 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    2 * 3 * 7 * 13441 * 205115282021455665897114700593932402728804164701536103180137503955397371

  Prime field multiplication subgroup decomposition:
    2^6 * 3 * 149 * 631 * 6414488540731361226607730496888035255996436684289152125202372832747357
*)

let two_z = Z.succ Z.one

module Fq = Ff.MakeFp (struct
  let prime_order =
    Z.(
      (two_z ** 256) - (two_z ** 32) - (two_z ** 9) - (two_z ** 8)
      - (two_z ** 7) - (two_z ** 6) - (two_z ** 4) - one)
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "115792089237316195423570985008687907852837564279074904382605163141518161494337"
end)

module Projective =
  Ec.MakeProjectiveWeierstrass (Fq) (Fp)
    (struct
      (* See https://en.bitcoin.it/wiki/Secp256k1 *)
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 7)

      let cofactor = Z.one

      (* x = 55066263022277343669578718895168534326250603453777594175500187360389116729240
         y = 32670510020758816978083085130507043184471273380659243275938904335757337482424
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(
              to_bytes
                (of_string
                   "55066263022277343669578718895168534326250603453777594175500187360389116729240"));
            Fq.(
              to_bytes
                (of_string
                   "32670510020758816978083085130507043184471273380659243275938904335757337482424"));
            Fq.(to_bytes one);
          ]
    end)

module Jacobian =
  Ec.MakeJacobianWeierstrass (Fq) (Fp)
    (struct
      (* See https://en.bitcoin.it/wiki/Secp256k1 *)
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 7)

      let cofactor = Z.one

      (* x = 55066263022277343669578718895168534326250603453777594175500187360389116729240
         y = 32670510020758816978083085130507043184471273380659243275938904335757337482424
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(
              to_bytes
                (of_string
                   "55066263022277343669578718895168534326250603453777594175500187360389116729240"));
            Fq.(
              to_bytes
                (of_string
                   "32670510020758816978083085130507043184471273380659243275938904335757337482424"));
            Fq.(to_bytes one);
          ]
    end)

module Affine =
  Ec.MakeAffineWeierstrass (Fq) (Fp)
    (struct
      (* See https://en.bitcoin.it/wiki/Secp256k1 *)
      let a = Fq.zero

      let b = Fq.of_z (Z.of_int 7)

      let cofactor = Z.one

      (* x = 55066263022277343669578718895168534326250603453777594175500187360389116729240
         y = 32670510020758816978083085130507043184471273380659243275938904335757337482424
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Fq.(
              to_bytes
                (of_string
                   "55066263022277343669578718895168534326250603453777594175500187360389116729240"));
            Fq.(
              to_bytes
                (of_string
                   "32670510020758816978083085130507043184471273380659243275938904335757337482424"));
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
