(**
  https://ed25519.cr.yp.to/
  https://ed25519.cr.yp.to/python/ed25519.py
  https://ed25519.cr.yp.to/eddsa-20150704.pdf (page 4, examples)


  Base field: 2^255 - 19 = 57896044618658097711785492504343953926634992332820282019728792003956564819949 (254 bits - 32 bytes)
  Scalar field: 2^252 + 27742317777372353535851937790883648493 = 7237005577332262213973186563042994240857116359379907606001950938285454250989 (252 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    2^2 * 3 * 65147 * 74058212732561358302231226437062788676166966415465897661863160754340907

  Prime field multiplication subgroup decomposition:
    2^2 * 3 * 11 * 54825799828274713742221110326083289703463002722575057621226901047617077659
*)

module Base = Ff.MakeFp (struct
  let prime_order = Z.(pow (of_int 2) 255 - of_int 19)
end)

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.(pow (of_int 2) 252 + of_string "27742317777372353535851937790883648493")
end)

module AffineEdwards =
  Ec.MakeAffineEdwards (Base) (Scalar)
    (struct
      let a = Base.(negate (of_string "1"))

      let d = Base.(negate (of_string "121665" / of_string "121666"))

      (* https://ed25519.cr.yp.to/eddsa-20150704.pdf (page 4, examples).
         c = 3 -> cofactor 8
      *)
      let cofactor = Z.of_string "8"

      (* https://ed25519.cr.yp.to/eddsa-20150704.pdf (page 4, examples).
         > B is the point (... 202, 4/5). Calculed using:
         ```
         let f a d x = Base.(sqrt_opt ((one + negate (x * x)) / (a + (negate d) * x * x)));;
         ```
         And use the result of:
         ```
         Base.to_string @@ Base.negate @@ Option.get @@ f (Base.(negate one))
         (Base.(negate (of_string "121665" / of_string "121666")))
         (Base.(of_string "4" / of_string "5"));;
         ```
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Base.(
              to_bytes
                (of_string
                   "15112221349535400772501151409588531511454012693041857206046113283949847762202"));
            Base.(to_bytes (of_string "4" / of_string "5"));
          ]

      (* 4/5 = 463168356949264781694283940034751631413079938662562256157830336
         03165251855960 *)
    end)

module AffineMontgomery =
  Ec.MakeAffineMontgomery (Base) (Scalar)
    (struct
      (* Parameters generated with function to_montgomery_curve_parameters ().
         The RFC (https://www.rfc-editor.org/rfc/rfc7748#section-4.1) uses a different mapping,
         the Montgomery v coordinate being multiplied by sqrt(-486664), to get "Edwards25519" *)

      let a = Base.of_string "486662"

      let b =
        Base.of_string
          "57896044618658097711785492504343953926634992332820282019728792003956564333285"

      let cofactor = Z.of_string "8"

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Base.(to_bytes (of_string "9"));
            Base.(
              to_bytes
                (of_string
                   "46155036877857898950720737868668298259344786430663990124372813544693780678454"));
          ]
    end)

let from_affine_edwards_to_affine_montgomery p =
  Ec.from_affine_edwards_to_affine_montgomery
    (module AffineEdwards)
    (module AffineMontgomery)
    p

let from_affine_montgomery_to_affine_edwards p =
  Ec.from_affine_montgomery_to_affine_edwards
    (module AffineMontgomery)
    (module AffineEdwards)
    p
