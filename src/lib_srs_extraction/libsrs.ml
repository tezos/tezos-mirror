open Bls12_381
module G1 = G1
module G2 = G2
module G1_carray = Bls12_381_polynomial.G1_carray
module G2_carray = Bls12_381_polynomial.G2_carray

module Checks = struct
  let equality (g1_elements : Bls12_381_polynomial.G1_carray.t)
      (g2_elements : Bls12_381_polynomial.G2_carray.t) =
    let g1 = G1_carray.get g1_elements 0 in
    let g2 = G2_carray.get g2_elements 0 in
    Bls12_381_polynomial.G1_carray.iteri_copy_elt
      (fun i g1i ->
        let g2i = G2_carray.get g2_elements i in
        let gt_element1 = Pairing.pairing g1i g2 in
        let gt_element2 = Pairing.pairing g1 g2i in
        assert (GT.eq gt_element1 gt_element2))
      g1_elements

  let incrementation_g1 g1_elements g2_elements =
    let g2 = G2_carray.get g2_elements 0 in
    let g2x = G2_carray.get g2_elements 1 in
    for i = 0 to G1_carray.length g1_elements - 2 do
      let g1i = G1_carray.get g1_elements i in
      let g1iplus = G1_carray.get g1_elements (i + 1) in
      let gt_element1 = Pairing.pairing g1i g2x in
      let gt_element2 = Pairing.pairing g1iplus g2 in
      assert (GT.eq gt_element1 gt_element2)
    done

  let incrementation_g2 g1_elements g2_elements =
    let g1 = G1_carray.get g1_elements 0 in
    let g1x = G1_carray.get g1_elements 1 in
    for i = 0 to G2_carray.length g2_elements - 2 do
      let g2i = G2_carray.get g2_elements i in
      let g2iplus = G2_carray.get g2_elements (i + 1) in
      let gt_element1 = Pairing.pairing g1x g2i in
      let gt_element2 = Pairing.pairing g1 g2iplus in
      assert (GT.eq gt_element1 gt_element2)
    done

  let pairings g1_elements g2_elements =
    equality g1_elements g2_elements ;
    incrementation_g1 g1_elements g2_elements ;
    incrementation_g2 g1_elements g2_elements
end

let exact_log2 p =
  if p <= 0 then raise @@ Invalid_argument (string_of_int p) ;
  let rec aux res p =
    if p = 1 then res
    else
      let p' = p lsr 1 in
      if p' = 1 then
        if p mod 2 <> 0 then raise @@ Invalid_argument "not a power of 2"
        else res + 1
      else aux (res + 1) p'
  in
  aux 0 p

(** The binary format of our Srs is a sequence of G1 or G2 elements. *)
module Srs = struct
  let to_file gs file to_compressed_bytes iter =
    let oc = open_out_bin file in
    try
      iter (fun g1 -> output_bytes oc (to_compressed_bytes g1)) gs ;
      close_out oc
    with e ->
      close_out oc ;
      raise e

  let gs1_to_file gs output_file =
    to_file
      gs
      output_file
      G1.to_compressed_bytes
      Bls12_381_polynomial.G1_carray.iter_copy_elt

  let gs2_to_file gs output_file =
    to_file
      gs
      output_file
      G2.to_compressed_bytes
      Bls12_381_polynomial.G2_carray.iter_copy_elt
end

(** This module handles the file format of the result of powers-of-tau ceremony.

  The layout of the file can be found at the bottom of this file
  https://github.com/ebfull/powersoftau/blob/master/src/bin/verify.rs
  to be
  let tau_powers_length = 1 lsl power in
    G1.size_in_bytes (* alpha in g1 *)
  + G1.size_in_bytes (* beta in g1 *)
  + G2.size_in_bytes (* beta in g2 *)
  + (tau_powers_length * G1.size_in_bytes) (* g1_coeffs *)
  + (tau_powers_length * G2.size_in_bytes) (* g2_coeffs *)
  + (tau_powers_length * G1.size_in_bytes) (* g1_alpha_coeffs *)
  + (tau_powers_length * G1.size_in_bytes) (* g1_beta_coeffs *)
  + ((tau_powers_length - 1) * G1.size_in_bytes) (* h *)
*)
module Powers_of_tau = struct
  let generate_domain power =
    let size = 1 lsl power in
    let rec get_omega limit =
      if limit < 32 then Fr.square (get_omega (limit + 1))
      else
        Fr.of_string
          "0x16a2a19edfe81f20d09b681922c813b4b63683508c2280b93829971f439f0d2b"
    in
    let omega = get_omega power in
    let rec encoded_pow_x acc xi i =
      if i = 0 then List.rev acc
      else encoded_pow_x (xi :: acc) (Fr.mul xi omega) (i - 1)
    in
    Array.of_list @@ encoded_pow_x [] Fr.one size

  (* Derives the power that generated the file from the total size of the
     file using this formula:
     let tau_powers_length = (total - g1 - g2) / (4g1+g2)
     let power = log2 tau_powers_length
  *)
  let power_of_radixfile radixfile =
    let ic = open_in_bin radixfile in
    try
      let g1 = G1.size_in_bytes in
      let g2 = G2.size_in_bytes in
      let filesize = in_channel_length ic in
      close_in ic ;
      let nb_elements = (filesize - g1 - g2) / ((4 * g1) + g2) in
      exact_log2 nb_elements
    with e ->
      close_in ic ;
      raise e

  let to_gs radixfile skip size_in_bytes of_bytes_exn evaluation_ecfft of_array
      =
    let power = power_of_radixfile radixfile in
    let buf = Bytes.create size_in_bytes in
    let read ic =
      Stdlib.really_input ic buf 0 size_in_bytes ;
      of_bytes_exn buf
    in
    let ic = open_in_bin radixfile in
    try
      Stdlib.seek_in ic skip ;
      let size = 1 lsl power in
      let points = Array.init size (fun _i -> read ic) |> of_array in
      close_in ic ;
      let domain = Bls12_381_polynomial.Domain.build_power_of_two power in
      let points = evaluation_ecfft ~domain ~points in
      points
    with e ->
      close_in ic ;
      raise e

  let to_g1s radixfile =
    let skip_header =
      (* Skip alpha_g1, beta_g1 and beta_g2 *)
      (2 * G1.size_in_bytes) + G2.size_in_bytes
    in
    to_gs
      radixfile
      skip_header
      G1.size_in_bytes
      G1.of_bytes_exn
      Bls12_381_polynomial.G1_carray.evaluation_ecfft
      Bls12_381_polynomial.G1_carray.of_array

  let to_g2s radixfile =
    let power = power_of_radixfile radixfile in
    let size = 1 lsl power in
    let skip_header =
      (* Skip alpha_g1, beta_g1 and beta_g2 *)
      (2 * G1.size_in_bytes) + G2.size_in_bytes
    in
    let skip_g2s =
      let skip_g1s = size * G1.size_in_bytes in
      skip_header + skip_g1s
    in
    to_gs
      radixfile
      skip_g2s
      G2.size_in_bytes
      G2.of_bytes_exn
      Bls12_381_polynomial.G2_carray.evaluation_ecfft
      Bls12_381_polynomial.G2_carray.of_array
end
