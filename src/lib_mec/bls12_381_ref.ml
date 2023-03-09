(**
  The equation of G1 is x^3 + 4 = y^2
*)

module Fq = Ff.MakeFp (struct
  let prime_order =
    (* 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787 in decimal *)
    Z.of_string
      "0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab"
end)

module Fp = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001"
end)

module G1 = struct
  module Projective =
    Ec.MakeProjectiveWeierstrass (Fq) (Fp)
      (struct
        let a = Fq.zero

        let b = Fq.of_string "4"

        (* See https://github.com/zkcrypto/bls12_381/blob/main/src/notes/design.rs
           The value has been obtained by running the following code in SAGE
           ```
           def g1_h(x):
           	 return ((x-1)**2) // 3
           param = -0xd201000000010000
           g1_h(param)
           ```
        *)
        let cofactor = Z.of_string "76329603384216526031706109802092473003"

        (*
           https://github.com/zkcrypto/bls12_381/blob/main/src/g1.rs#L572
           ```
           x: Fp::from_raw_unchecked([
                0x5cb3_8790_fd53_0c16,
                0x7817_fc67_9976_fff5,
                0x154f_95c7_143b_a1c1,
                0xf0ae_6acd_f3d0_e747,
                0xedce_6ecc_21db_f440,
                0x1201_7741_9e0b_fb75,
            ]),
            y: Fp::from_raw_unchecked([
                0xbaac_93d5_0ce7_2271,
                0x8c22_631a_7918_fd8e,
                0xdd59_5f13_5707_25ce,
                0x51ac_5829_5040_5194,
                0x0e1c_8c3f_ad00_59c0,
                0x0bbc_3efc_5008_a26a,
            ]),
            z: Fp::one(),
            ```
            Using decimal representation, computed using
            ```
            Z.(to_string (of_string "0x..."))
            ```
        *)
        let bytes_generator =
          Bytes.concat
            Bytes.empty
            [
              Fq.(
                to_bytes
                  (of_string
                     "3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507"));
              Fq.(
                to_bytes
                  (of_string
                     "1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569"));
              Fq.(to_bytes one);
            ]
      end)

  module Affine =
    Ec.MakeAffineWeierstrass (Fq) (Fp)
      (struct
        let a = Fq.zero

        let b = Fq.of_string "4"

        (* See https://github.com/zkcrypto/bls12_381/blob/main/src/notes/design.rs
           The value has been obtained by running the following code in SAGE
           ```
           def g1_h(x):
           	 return ((x-1)**2) // 3
           param = -0xd201000000010000
           g1_h(param)
           ```
        *)
        let cofactor = Z.of_string "76329603384216526031706109802092473003"

        (*
           https://github.com/zkcrypto/bls12_381/blob/main/src/g1.rs#L572
           ```
           x: Fp::from_raw_unchecked([
                0x5cb3_8790_fd53_0c16,
                0x7817_fc67_9976_fff5,
                0x154f_95c7_143b_a1c1,
                0xf0ae_6acd_f3d0_e747,
                0xedce_6ecc_21db_f440,
                0x1201_7741_9e0b_fb75,
            ]),
            y: Fp::from_raw_unchecked([
                0xbaac_93d5_0ce7_2271,
                0x8c22_631a_7918_fd8e,
                0xdd59_5f13_5707_25ce,
                0x51ac_5829_5040_5194,
                0x0e1c_8c3f_ad00_59c0,
                0x0bbc_3efc_5008_a26a,
            ]),
            ```
            Using decimal representation, computed using
            ```
            Z.(to_string (of_string "0x..."))
            ```
        *)
        let bytes_generator =
          Bytes.concat
            Bytes.empty
            [
              Fq.(
                to_bytes
                  (of_string
                     "3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507"));
              Fq.(
                to_bytes
                  (of_string
                     "1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569"));
            ]
      end)
end

module Fq2 =
  Ff.MakeFp2
    (Fq)
    (struct
      let nsr = Fq.of_string "4"
    end)

(* TODO: require to implement sqrt_opt for Fq2. Not yet available in ocaml-ff *)

(* module G2 = struct
 *   module Projective =
 *     Ec.MakeProjectiveWeierstrass (Fq2) (Fp)
 *       (struct
 *         let a = Fq2.zero
 * 
 *         let b = Fq2.of_string "4"
 * 
 *         let cofactor =
 *           Z.of_string_base
 *             16
 *             "5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5"
 * 
 *         (\*
 *            https://github.com/zkcrypto/bls12_381/blob/main/src/g1.rs#L572
 *            ```
 *            x: Fp::from_raw_unchecked([
 *                 0x5cb3_8790_fd53_0c16,
 *                 0x7817_fc67_9976_fff5,
 *                 0x154f_95c7_143b_a1c1,
 *                 0xf0ae_6acd_f3d0_e747,
 *                 0xedce_6ecc_21db_f440,
 *                 0x1201_7741_9e0b_fb75,
 *             ]),
 *             y: Fp::from_raw_unchecked([
 *                 0xbaac_93d5_0ce7_2271,
 *                 0x8c22_631a_7918_fd8e,
 *                 0xdd59_5f13_5707_25ce,
 *                 0x51ac_5829_5040_5194,
 *                 0x0e1c_8c3f_ad00_59c0,
 *                 0x0bbc_3efc_5008_a26a,
 *             ]),
 *             z: Fp::one(),
 *             ```
 *             Using decimal representation, computed using
 *             ```
 *             Z.(to_string (of_string "0x..."))
 *             ```
 *         *\)
 *         let bytes_generator =
 *           Bytes.concat
 *             Bytes.empty
 *             [ Fq.(
 *                 to_bytes
 *                   (of_string
 *                      "3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507"));
 *               Fq.(
 *                 to_bytes
 *                   (of_string
 *                      "1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569"));
 *               Fq.(to_bytes one) ]
 *       end)
 * 
 *   module Affine =
 *     Ec.MakeAffineWeierstrass (Fq2) (Fp)
 *       (struct
 *         let a = Fq2.zero
 * 
 *         let b = Fq2.of_string "4"
 * 
 *         let cofactor =
 *           Z.of_string_base
 *             16
 *             "5d543a95414e7f1091d50792876a202cd91de4547085abaa68a205b2e5a7ddfa628f1cb4d9e82ef21537e293a6691ae1616ec6e786f0c70cf1c38e31c7238e5"
 * 
 *         let bytes_generator =
 *           Bytes.concat
 *             Bytes.empty
 *             [ Fq.(
 *                 to_bytes
 *                   (of_string
 *                      "3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507"));
 *               Fq.(
 *                 to_bytes
 *                   (of_string
 *                      "1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569"))
 *             ]
 *       end)
 * end *)
