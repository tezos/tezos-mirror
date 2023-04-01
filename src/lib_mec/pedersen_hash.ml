module MakePedersenHash
    (Ec : Ec_sig.BASE) (Params : sig
      val generators : Ec.t list

      val chunks_per_generator : int
    end) =
struct
  let hash iterator =
    let rec compute_m_j acc j =
      if j = Params.chunks_per_generator + 1 then (acc, false)
      else
        let a_opt = Iterator.Bit.next iterator in
        (* end of the bit stream, we do not continue, the boolean means we
           processed the entire bitstring
        *)
        if Option.is_none a_opt then (acc, true)
        else
          let a = Option.value a_opt ~default:0 in
          let b = Option.value (Iterator.Bit.next iterator) ~default:0 in
          let c = Option.value (Iterator.Bit.next iterator) ~default:0 in
          let enc_m_j = (1 - (2 * c)) * (1 + a + (2 * b)) in
          (* <M_i> accumulated *)
          let enc_m_j = Ec.Scalar.of_z (Z.of_int enc_m_j) in
          let power =
            Ec.Scalar.pow Ec.Scalar.(one + one) (Z.of_int (4 * (j - 1)))
          in
          let acc = Ec.Scalar.add acc (Ec.Scalar.mul enc_m_j power) in
          compute_m_j acc (j + 1)
    in
    let rec compute_segment_i_incremental acc_res generators =
      match generators with
      | [] -> raise (Invalid_argument "Not enough generators")
      | generator :: generators ->
          let sum_m_j, fully_processed = compute_m_j Ec.Scalar.zero 1 in
          (* compute segment i *)
          let m_i = Ec.(mul generator sum_m_j) in
          let acc_res = Ec.(add m_i acc_res) in
          if fully_processed then acc_res
          else compute_segment_i_incremental acc_res generators
    in
    compute_segment_i_incremental Ec.zero Params.generators
end

module Zcash =
  MakePedersenHash
    (Jubjub.AffineEdwards)
    (struct
      (* generators from
         https://github.com/zcash/librustzcash/blob/de1345a1c2f10b0843ab1f1ea6463b2330fdc673/zcash_primitives/src/constants.rs#L146 *)
      let generators =
        let tmp =
          [
            ( "0x73c016a42ded9578b5ea25de7ec0e3782f0c718f6f0fbadd194e42926f661b51",
              "0x289e87a2d3521b5779c9166b837edc5ef9472e8bc04e463277bfabd432243cca"
            );
            ( "0x15a36d1f0f390d8852a35a8c1908dd87a361ee3fd48fdf77b9819dc82d90607e",
              "0x015d8c7f5b43fe33f7891142c001d9251f3abeeb98fad3e87b0dc53c4ebf1891"
            );
            ( "0x664321a58246e2f6eb69ae39f5c84210bae8e5c46641ae5c76d6f7c2b67fc475",
              "0x362e1500d24eee9ee000a46c8e8ce8538bb22a7f1784b49880ed502c9793d457"
            );
            ( "0x323a6548ce9d9876edc5f4a9cff29fd57d02d50e654b87f24c767804c1c4a2cc",
              "0x2f7ee40c4b56cad891070acbd8d947b75103afa1a11f6a8584714beca33570e9"
            );
            ( "0x3bd2666000b5479689b64b4e03362796efd5931305f2f0bf46809430657f82d1",
              "0x494bc52103ab9d0a397832381406c9e5b3b9d8095859d14c99968299c3658aef"
            );
            ( "0x63447b2ba31bb28ada049746d76d3ee51d9e5ca21135ff6fcb3c023258d32079",
              "0x64ec4689e8bfb6e564cdb1070a136a28a80200d2c66b13a7436082119f8d629a"
            );
          ]
        in
        List.map
          (fun (u, v) ->
            Jubjub.AffineEdwards.from_coordinates_exn
              ~u:(Jubjub.AffineEdwards.Base.of_string u)
              ~v:(Jubjub.AffineEdwards.Base.of_string v))
          tmp

      let chunks_per_generator = 63
    end)
