open Bls

exception SRS_too_short of string

module Commit = struct
  (* This function is a wrapper for pippenger and used to raise a more helpful
     error message *)
  let pippenger pippenger ps ss =
    try pippenger ?start:None ?len:None ps ss
    with Invalid_argument s ->
      raise (Invalid_argument ("Utils.pippenger : " ^ s))

  (* Using affine array is more efficient than using regular G1 array, even
     with conversion, but slightly less efficient than with Srs. We need this
     function to perform pippenger with non-SRS arrays of points *)
  let with_affine_array_1 g =
    pippenger G1.pippenger_with_affine_array (G1.to_affine_array g)

  (* This is a wrapper for pippenger, dedicated to committing polynomials *)
  let commit_single pippenger zero srs_size srs p =
    let p_size = 1 + Poly.degree p in
    if p_size = 0 then zero
    else if p_size > srs_size then
      raise
        (SRS_too_short
           (Printf.sprintf
              "commit : Polynomial degree, %i, exceeds srs length, %i."
              p_size
              srs_size))
    else pippenger srs p

  (* This is the most efficient version of Pippenger *)
  let with_srs1 srs p =
    commit_single Srs_g1.pippenger G1.zero (Srs_g1.size srs) srs p

  let with_srs2 srs p =
    commit_single Srs_g2.pippenger G2.zero (Srs_g2.size srs) srs p
end

(* This module handles the base KZG commitment type, for G1 & G2 *)
module Single (G : Bls.G_sig) = struct
  (* The type for a commitment is an elliptic curve point *)
  type t = G.t [@@deriving repr]

  (* The public parameters for KZG is the Structured Reference String *)
  type public_parameters = G.Srs.t

  (* Commitments are computed over polynomials *)
  type secret = Poly.t

  let zero = G.zero

  let random = G.random

  (* this fuction is used for testing purposes *)
  let alter_proof proof = G.(add proof one)

  let encoding = G.encoding

  let equal = G.eq

  let compare a b =
    if G.eq a b then 0 else Bytes.compare (G.to_bytes a) (G.to_bytes b)

  (* The argument [shift] to skip the first coefficients of the polynomial.
     This is useful when the polynomial to commit has a lot of zeros as
     lower-order coefficients ; this is used to save time when committing to the
     proof in Degree_check *)
  let commit ?(shift = 0) srs secret =
    Commit.commit_single
      (G.Srs.pippenger ~offset:shift)
      G.zero
      (G.Srs.size srs)
      srs
      secret

  let size = G.compressed_size_in_bytes

  let commitment_of_bytes_exn bytes =
    match G.of_compressed_bytes_opt bytes with
    | None ->
        Format.kasprintf Stdlib.failwith "Unexpected data (KZG commitment)"
    | Some commitment -> commitment
  [@@coverage off]

  let to_string commitment = G.to_compressed_bytes commitment |> Bytes.to_string
  [@@coverage off]

  let of_string_opt str = G.of_compressed_bytes_opt (String.to_bytes str)
  [@@coverage off]
end

(* This module handles KZG commitments in a batch, using a String map. *)
module Make (G : G_sig) = struct
  module Single = Single (G)

  type public_parameters = Single.public_parameters

  (* The secret is a map of polynomials with their names *)
  type secret = Single.secret SMap.t

  (* The commitment is a map of commitment with their names *)
  type t = Single.t SMap.t [@@deriving repr]

  (* Auxiliary information needed by the prover for further computation, that
     are computed during the commit process but not part of the actual
     commitment.
     This type is not useful in this module, but may be used in other forms of
     commitments that follow the same interface *)
  type prover_aux = unit [@@deriving repr]

  let commit_single = Single.commit

  (* Returns a map of single commitments, each commitment binded with the name
     of the secret polynomial in [f_map] ; the argument [all_key] may be used
     for other forms of commitments with distribution *)
  let commit ?all_keys:_ srs f_map =
    let cmt = SMap.map (commit_single srs) f_map in
    let prover_aux = () in
    (cmt, prover_aux)

  let cardinal cmt = SMap.cardinal cmt

  let rename f cmt =
    SMap.fold (fun key x acc -> SMap.add (f key) x acc) cmt SMap.empty

  (* Merges a list of commitments ; this function assumes that two commitments
     with the same name are the same, otherwise only one of the same-named
     commitments will be kept *)
  let recombine cmt_list =
    List.fold_left
      (SMap.union (fun _k x _ -> Some x))
      (List.hd cmt_list)
      (List.tl cmt_list)

  let recombine_prover_aux _ = ()

  let empty = SMap.empty

  let empty_prover_aux = ()

  (* [of_list _ ~name [c1 ; c2 ; … cn]] returns a commitment of the form
     {"0…01~name" -> c1 ; "0…02~name" -> c2 ; …}. The indexes that prefix
     [name] in the keys is padded with 0s such that each key until the n-th
     has the same number of characters *)
  let of_list _ ~name l =
    let n = List.length l in
    ( SMap.(
        of_list
          (List.mapi (fun i c -> (Aggregation.add_prefix ~n ~i "" name, c)) l)),
      () )

  let to_map cm = cm
end

module Single_G1 = Single (G1)
module Commitment_G1 = Make (G1)
