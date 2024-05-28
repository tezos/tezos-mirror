module Poly = Polynomial
module Pairing = Bls12_381.Pairing
module Srs = Srs
module Evals = Evaluations

module Scalar = struct
  include Bls12_381.Fr

  type scalar = t

  let mone = negate one

  let string_of_scalar s =
    if String.length (to_string s) < 10 then to_string s
    else if String.length (to_string (negate s)) < 10 then
      "-" ^ to_string (negate s)
    else "H" ^ (to_z s |> Z.hash |> string_of_int)

  let equal a b = Bytes.equal (to_bytes a) (to_bytes b)

  (* TODO https://gitlab.com/nomadic-labs/privacy-team/-/issues/183
     Duplicated in plonk/bls.ml *)
  let t : t Repr.t =
    Repr.(map (bytes_of (`Fixed size_in_bytes)) of_bytes_exn to_bytes)

  let encoding = conv to_bytes of_bytes_exn (Fixed.bytes size_in_bytes)
end

module Scalar_map = Map.Make (Scalar)

module G (G : Bls12_381.CURVE) (Srs : Srs_sig with type elt = G.t) = struct
  module Srs = Srs
  include G

  let t : t Repr.t =
    Repr.(
      map
        (bytes_of (`Fixed (size_in_bytes / 2)))
        of_compressed_bytes_exn
        to_compressed_bytes)

  let encoding =
    conv
      to_compressed_bytes
      of_compressed_bytes_exn
      (Fixed.bytes (size_in_bytes / 2))
end

module Srs_g1 = Srs.Srs_g1
module Srs_g2 = Srs.Srs_g2
module G1 = G (Bls12_381.G1) (Srs_g1)
module G2 = G (Bls12_381.G2) (Srs_g2)

module type G_sig = sig
  include Bls12_381.CURVE

  module Srs : Srs_sig with type elt = t

  val t : t Repr.ty

  val encoding : t encoding
end

module GT = struct
  include Bls12_381.GT

  let t : t Repr.t =
    Repr.(map (bytes_of (`Fixed size_in_bytes)) of_bytes_exn to_bytes)
end

let to_encoding repr =
  let of_string repr bs =
    Stdlib.Result.map_error (fun (`Msg msg) -> msg)
    @@ Repr.(unstage @@ of_bin_string repr) bs
  in
  let to_string repr e = Repr.(unstage @@ to_bin_string repr) e in
  let data_encoding_of_repr repr =
    Data_encoding.conv_with_guard
      (to_string repr)
      (of_string repr)
      (Data_encoding.string' Hex)
  in
  data_encoding_of_repr repr

module Domain = struct
  include Domain

  let encoding = to_encoding Domain.t
end

module G1_carray = struct
  include G1_carray

  let encoding = to_encoding G1_carray.t
end
