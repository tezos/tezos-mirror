open Bls

exception SRS_too_short of string

module Commit = struct
  (* This function is used to raise a more helpful error message *)
  let pippenger pippenger ps ss =
    try pippenger ?start:None ?len:None ps ss
    with Invalid_argument s ->
      raise (Invalid_argument ("Utils.pippenger : " ^ s))

  let with_affine_array_1 g =
    pippenger G1.pippenger_with_affine_array (G1.to_affine_array g)

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

  let with_srs1 srs p =
    commit_single Srs_g1.pippenger G1.zero (Srs_g1.size srs) srs p

  let with_srs2 srs p =
    commit_single Srs_g2.pippenger G2.zero (Srs_g2.size srs) srs p
end

module Single = struct
  type t = G1.t [@@deriving repr]

  type public_parameters = Srs_g1.t

  type secret = Poly.t

  let zero = G1.zero

  let random = G1.random

  let alter_proof proof = G1.(add proof one)

  let encoding = G1.encoding

  let equal = G1.eq

  let compare a b =
    if G1.eq a b then 0 else Bytes.compare (G1.to_bytes a) (G1.to_bytes b)

  let commit = Commit.with_srs1

  let size = G1.compressed_size_in_bytes

  let commitment_of_bytes_exn bytes =
    match G1.of_compressed_bytes_opt bytes with
    | None ->
        Format.kasprintf Stdlib.failwith "Unexpected data (KZG commitment)"
    | Some commitment -> commitment
    [@@coverage off]

  let to_string commitment =
    G1.to_compressed_bytes commitment |> Bytes.to_string
    [@@coverage off]

  let of_string_opt str = G1.of_compressed_bytes_opt (String.to_bytes str)
    [@@coverage off]
end

type public_parameters = Single.public_parameters

type secret = Single.secret SMap.t

type t = Single.t SMap.t [@@deriving repr]

type prover_aux = unit [@@deriving repr]

let commit_single = Single.commit

let commit ?all_keys:_ srs f_map =
  let cmt = SMap.map (commit_single srs) f_map in
  let prover_aux = () in
  (cmt, prover_aux)

let cardinal cmt = SMap.cardinal cmt

let rename f cmt =
  SMap.fold (fun key x acc -> SMap.add (f key) x acc) cmt SMap.empty

let recombine cmt_list =
  List.fold_left
    (SMap.union (fun _k x _ -> Some x))
    (List.hd cmt_list)
    (List.tl cmt_list)

let recombine_prover_aux _ = ()

let empty = SMap.empty

let empty_prover_aux = ()

let of_list _ ~name l =
  let n = List.length l in
  ( SMap.(
      of_list
        (List.mapi (fun i c -> (Aggregation.add_prefix ~n ~i "" name, c)) l)),
    () )

let to_map cm = cm
