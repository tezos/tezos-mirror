(**
  https://eprint.iacr.org/2021/1152.pdf

  Base field: 52435875175126190479447740508185965837690552500527637822603658699938581184513 (254 bits - 32 bytes)
  Scalar field: 13108968793781547619861935127046491459309155893440570251786403306729687672801 (253 bits - 32 bytes)

  Base field multiplicative subgroup decomposition:
    2^32 * 3 * 11 * 19 * 10177 * 125527 * 859267 * 906349^2 * 2508409 * 2529403 * 52437899 * 254760293^2
  Prime field multiplication subgroup decomposition:
    2^5 * 3 * 5^2 * 5462070330742311508275806302936038108045481622266904271577668044470703197
*)

module Base = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "13108968793781547619861935127046491459309155893440570251786403306729687672801"
end)

(* Parameters found here: https://github.com/zhenfeizhang/bandersnatch/blob/65823a69d6c4c7612d2f2d3d21ce87f2b319fcf0/bandersnatch/src/curves/mod.rs *)
module AffineEdwards =
  Ec.MakeAffineEdwards (Base) (Scalar)
    (struct
      let a = Base.(negate (of_string "5"))

      let d =
        Base.(
          of_string "138827208126141220649022263972958607803"
          / of_string "171449701953573178309673572579671231137")

      let cofactor = Z.of_string "4"

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Base.(
              to_bytes
                (of_string
                   "18886178867200960497001835917649091219057080094937609519140440539760939937304"));
            Base.(
              to_bytes
                (of_string
                   "19188667384257783945677642223292697773471335439753913231509108946878080696678"));
          ]
    end)

module AffineMontgomery =
  Ec.MakeAffineMontgomery (Base) (Scalar)
    (struct
      let a =
        Base.of_string
          "0x4247698f4e32ad45a293959b4ca17afa4a2d2317e4c6ce5023e1fd63d1b5de98"

      let b =
        Base.of_string
          "0x300c3385d13bedb7c9e229e185c4ce8b1dd3b71366bb97c30855c0aa41d62727"

      let cofactor = Z.of_int 4

      (* Computing the y-coordinates using

         ```OCaml
         let x_m =
           B.of_string "0x67c5b5fed18254e8acb66c1e38f33ee0975ae6876f9c5266a883f4604024b3b8"
         in
         let a =
           B.of_string "0x4247698f4e32ad45a293959b4ca17afa4a2d2317e4c6ce5023e1fd63d1b5de98"
         in
         let b =
           B.of_string "0x300c3385d13bedb7c9e229e185c4ce8b1dd3b71366bb97c30855c0aa41d62727"
         in
         let tmp =
           B.(x_m * x_m * x_m + a * x_m * x_m + x_m)
         in
         let tmp = B.(tmp / b) in
         B.to_string (Option.get (B.sqrt_opt tmp))
         ```
      *)
      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Base.(
              to_bytes
                (of_string
                   "0x67c5b5fed18254e8acb66c1e38f33ee0975ae6876f9c5266a883f4604024b3b8"));
            Base.(
              to_bytes
                (of_string
                   "16568979071064374131063468616246789985515579410958029861283004835918472727623"));
          ]
    end)

module AffineWeierstrass =
  Ec.MakeAffineWeierstrass (Base) (Scalar)
    (struct
      let a = Base.(negate (of_string "3763200000"))

      let b = Base.(negate (of_string "78675968000000"))

      let cofactor = Z.of_int 4

      let bytes_generator =
        Bytes.concat
          Bytes.empty
          [
            Base.(
              to_bytes
                (of_string
                   "0xa76451786f95a802c0982bbd0abd68e41b92adc86c8859b4f44679b21658710"));
            Base.(
              to_bytes
                (of_string
                   "0x44d150c8b4bd14f79720d021a839e7b7eb4ee43844b30243126a72ac2375490a"));
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

let from_affine_montgomery_to_affine_weierstrass p =
  Ec.from_affine_montgomery_to_affine_weierstrass
    (module AffineMontgomery)
    (module AffineWeierstrass)
    p

let from_affine_edwards_to_affine_weierstrass p =
  let p = from_affine_edwards_to_affine_montgomery p in
  Option.bind p (fun opt ->
      Ec.from_affine_montgomery_to_affine_weierstrass
        (module AffineMontgomery)
        (module AffineWeierstrass)
        opt)
