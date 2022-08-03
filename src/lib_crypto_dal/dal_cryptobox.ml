(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad
include Dal_cryptobox_intf
module Base58 = Tezos_crypto.Base58

type error +=
  | Failed_to_load_trusted_setup of string
  | No_trusted_setup of string list
  | Trusted_setup_too_small of
      int (* FIXME:  "SRS asked (%d) too big for %s" d srsfile *)

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.trusted_setup_loading_failed"
    ~title:"Trusted setup loading failed"
    ~description:"Trusted setup failed to load"
    ~pp:(fun ppf msg ->
      Format.fprintf ppf "Trusted setup failed to load: %s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Failed_to_load_trusted_setup parameter -> Some parameter | _ -> None)
    (fun parameter -> Failed_to_load_trusted_setup parameter) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.trusted_setup_not_found"
    ~title:"No trusted setup found"
    ~description:"No trusted setup found in the explored paths"
    ~pp:(fun ppf locations ->
      Format.fprintf
        ppf
        "@[<v>cannot find Trusted setup in any of:@,%a@]@."
        (Format.pp_print_list (fun fmt -> Format.fprintf fmt "- %s"))
        locations)
    Data_encoding.(obj1 (req "paths" (list string)))
    (function No_trusted_setup parameter -> Some parameter | _ -> None)
    (fun parameter -> No_trusted_setup parameter)

module Inner = struct
  (* Scalars are elements of the prime field Fr from BLS. *)
  module Scalar = Bls12_381.Fr
  module Polynomial = Bls12_381_polynomial.Polynomial

  (* Operations on vector of scalars *)
  module Evaluations = Polynomial.Evaluations

  (* Domains for the Fast Fourier Transform (FTT). *)
  module Domains = Polynomial.Domain
  module Polynomials = Polynomial.Polynomial
  module IntMap = Tezos_error_monad.TzLwtreslib.Map.Make (Int)

  type slot = bytes

  type scalar = Scalar.t

  type polynomial = Polynomials.t

  type commitment = Bls12_381.G1.t

  type shard_proof = Bls12_381.G1.t

  type commitment_proof = Bls12_381.G1.t

  type _proof_single = Bls12_381.G1.t

  type segment_proof = Bls12_381.G1.t

  type segment = {index : int; content : bytes}

  type share = Scalar.t array

  type _shards_map = share IntMap.t

  type shard = {index : int; share : share}

  type shards_proofs_precomputation = Scalar.t array * segment_proof array array

  type trusted_setup_files = {
    srs_g1_file : string;
    srs_g2_file : string;
    logarithm_size : int;
  }

  module Encoding = struct
    open Data_encoding

    let fr_encoding = conv Bls12_381.Fr.to_bytes Bls12_381.Fr.of_bytes_exn bytes

    (* FIXME https://gitlab.com/tezos/tezos/-/issues/3391

       The commitment is not bounded. *)
    let g1_encoding =
      conv
        Bls12_381.G1.to_compressed_bytes
        Bls12_381.G1.of_compressed_bytes_exn
        bytes

    let _proof_shards_encoding = g1_encoding

    let commitment_proof_encoding = g1_encoding

    let _proof_single_encoding = g1_encoding

    let segment_proof_encoding = g1_encoding

    let share_encoding = array fr_encoding

    let shard_encoding =
      conv
        (fun {index; share} -> (index, share))
        (fun (index, share) -> {index; share})
        (tup2 int31 share_encoding)

    let shards_encoding =
      conv
        IntMap.bindings
        (fun bindings -> IntMap.of_seq (List.to_seq bindings))
        (list (tup2 int31 share_encoding))

    let shards_proofs_precomputation_encoding =
      tup2 (array fr_encoding) (array (array g1_encoding))
  end

  include Encoding

  module Commitment = struct
    type t = commitment

    type Base58.data += Data of t

    let zero = Bls12_381.G1.zero

    let commitment_to_bytes = Bls12_381.G1.to_compressed_bytes

    let commitment_of_bytes_opt = Bls12_381.G1.of_compressed_bytes_opt

    let commitment_of_bytes_exn bytes =
      match Bls12_381.G1.of_compressed_bytes_opt bytes with
      | None ->
          Format.kasprintf Stdlib.failwith "Unexpected data (DAL commitment)"
      | Some commitment -> commitment

    (* We divide by two because we use the compressed representation. *)
    let commitment_size = Bls12_381.G1.size_in_bytes / 2

    let to_string commitment = commitment_to_bytes commitment |> Bytes.to_string

    let of_string_opt str = commitment_of_bytes_opt (String.to_bytes str)

    let b58check_encoding =
      Base58.register_encoding
        ~prefix:Base58.Prefix.slot_header
        ~length:commitment_size
        ~to_raw:to_string
        ~of_raw:of_string_opt
        ~wrap:(fun x -> Data x)

    let raw_encoding =
      let open Data_encoding in
      conv
        commitment_to_bytes
        commitment_of_bytes_exn
        (Fixed.bytes commitment_size)

    include Tezos_crypto.Helpers.Make (struct
      type t = commitment

      let name = "DAL_commitment"

      let title = "Commitment representation for the DAL"

      let b58check_encoding = b58check_encoding

      let raw_encoding = raw_encoding

      let compare = compare

      let equal = ( = )

      let hash _ =
        (* The commitment is not hashed. This is ensured by the
           function exposed. We only need the Base58 encoding and the
           rpc_arg. *)
        assert false

      let seeded_hash _ _ =
        (* Same argument. *)
        assert false
    end)
  end

  include Commitment

  (* Number of bytes fitting in a Scalar.t. Since scalars are integer modulo
     r~2^255, we restrict ourselves to 248-bit integers (31 bytes). *)
  let scalar_bytes_amount = Scalar.size_in_bytes - 1

  (* Builds group of nth roots of unity, a valid domain for the FFT. *)
  let make_domain n = Domains.build ~log:Z.(log2up (of_int n))

  type t = {
    redundancy_factor : int;
    slot_size : int;
    segment_size : int;
    number_of_shards : int;
    k : int;
    n : int;
    (* k and n are the parameters of the erasure code. *)
    domain_k : Domains.t;
    (* Domain for the FFT on slots as polynomials to be erasure encoded. *)
    domain_2k : Domains.t;
    domain_n : Domains.t;
    (* Domain for the FFT on erasure encoded slots (as polynomials). *)
    shard_size : int;
    (* Length of a shard in terms of scalar elements. *)
    nb_segments : int;
    (* Number of slot segments. *)
    segment_len : int;
    remaining_bytes : int;
    evaluations_log : int;
    (* Log of the number of evaluations that constitute an erasure encoded
       polynomial. *)
    evaluations_per_proof_log : int;
    (* Log of the number of evaluations contained in a shard. *)
    proofs_log : int; (* Log of th number of shards proofs. *)
  }

  let check_params t =
    let is_pow_of_two x =
      let logx = Z.(log2 (of_int x)) in
      1 lsl logx = x
    in
    if
      not
        (is_pow_of_two t.slot_size
        && is_pow_of_two t.segment_size
        && is_pow_of_two t.n)
    then
      (* According to the specification the lengths of a slot a slot segment are
         in MiB *)
      invalid_arg "Wrong slot size: expected MiB"
    else if not (Z.(log2 (of_int t.n)) <= 32 && is_pow_of_two t.k && t.n > t.k)
    then
      (* n must be at most 2^32, the biggest subgroup of 2^i roots of unity in the
         multiplicative group of Fr, because the FFTs operate on such groups. *)
      invalid_arg "Wrong computed size for n"
    else if not (is_pow_of_two t.number_of_shards && t.n > t.number_of_shards)
    then invalid_arg "Shards not containing at least two elements"
    else ()
  (* Shards must contain at least two elements. *)

  let make ~redundancy_factor ~slot_size ~segment_size ~number_of_shards =
    let k = 1 lsl Z.(log2up (of_int slot_size / of_int scalar_bytes_amount)) in
    let n = redundancy_factor * k in
    let shard_size = n / number_of_shards in
    let evaluations_log = Z.(log2 (of_int n)) in
    let evaluations_per_proof_log = Z.(log2 (of_int shard_size)) in
    let t =
      {
        redundancy_factor;
        slot_size;
        segment_size;
        number_of_shards;
        k;
        n;
        domain_k = make_domain k;
        domain_2k = make_domain (2 * k);
        domain_n = make_domain n;
        shard_size;
        nb_segments = slot_size / segment_size;
        segment_len = Int.div segment_size scalar_bytes_amount + 1;
        remaining_bytes = segment_size mod scalar_bytes_amount;
        evaluations_log;
        evaluations_per_proof_log;
        proofs_log = evaluations_log - evaluations_per_proof_log;
      }
    in
    check_params t ;
    t

  (* The srs is an initialisation setup required by many primitives of
     the cryptobox. For production code, this initialisation setup is
     provided via files of size ~300MB. Loading those files can be done
     once by the shell. However, the [srs] value depends on the
     [slot_size] which is determined by the protocol.

     What we provide here is a mechanism to store at most two different
     [srs] depending on two different slot sizes. using the [Ringo]
     library (see {!val:srs_ring}). We also provide a cache mechanism to
     avoid recomputing the [srs] if it was already computed once. *)
  type srs = {
    srs_g1 : Bls12_381.G1.t array;
    srs_g2 : Bls12_381.G2.t array;
    kate_amortized_srs_g2_shards : Bls12_381.G2.t;
    kate_amortized_srs_g2_segments : Bls12_381.G2.t;
  }

  let srs t : srs =
    let module Scalar = Bls12_381.Fr in
    let build_array init next len =
      let xi = ref init in
      Array.init len (fun _ ->
          let i = !xi in
          xi := next !xi ;
          i)
    in
    let create_srs :
        type t.
        (module Bls12_381.CURVE with type t = t) -> int -> Scalar.t -> t array =
     fun (module G) d x -> build_array G.(copy one) (fun g -> G.(mul g x)) d
    in
    let secret =
      Scalar.of_string
        "20812168509434597367146703229805575690060615791308155437936410982393987532344"
    in
    let srs_g1 = create_srs (module Bls12_381.G1) t.k secret in
    let srs_g2 = create_srs (module Bls12_381.G2) t.k secret in
    {
      srs_g1;
      srs_g2;
      kate_amortized_srs_g2_shards =
        Array.get srs_g2 (1 lsl t.evaluations_per_proof_log);
      kate_amortized_srs_g2_segments =
        Array.get srs_g2 (1 lsl Z.(log2up (of_int t.segment_len)));
    }

  module SRS_ring =
    (val Ringo.(
           map_maker ~replacement:FIFO ~overflow:Strong ~accounting:Precise))
      (struct
        include Int

        let hash = Hashtbl.hash
      end)

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/3408

     We use [3] because a priori, we only need to support [2] protocols
     at the same time. In case of an hard fork, we give one protocol
     more for security. *)
  let srs_ring = SRS_ring.create 3

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/3410

     This function should be factored out with the one of sapling. *)
  let find_trusted_setup ?(getenv_opt = Sys.getenv_opt) ?(getcwd = Sys.getcwd)
      ?(file_exists = Sys.file_exists) () =
    let ( // ) = Filename.concat in
    let env ?split name path =
      match getenv_opt name with
      | None -> []
      | Some value -> (
          match split with
          | None -> [Filename.concat value path]
          | Some char ->
              List.map
                (fun dir -> dir // path)
                (String.split_on_char char value))
    in
    let cwd path = try [getcwd () // path] with Sys_error _ -> [] in
    let candidate_directories =
      env "XDG_DATA_HOME" ".local/share/dal-trusted-setup"
      @ env ~split:':' "XDG_DATA_DIRS" "dal-trusted-setup"
      @ env "OPAM_SWITCH_PREFIX" "share/dal-trusted-setup"
      @ env "PWD" "_opam/share/dal-trusted-setup"
      @ cwd "_opam/share/dal-trusted-setup"
      @ env "HOME" ".dal-trusted-setup"
      @ env "HOME" ".local/share/dal-trusted-setup"
      @ env "HOMEBREW_PREFIX" "share/dal-trusted-setup"
      @ ["/usr/local/share/dal-trusted-setup"; "/usr/share/dal-trusted-setup"]
    in
    (* Files we are looking for. *)
    let srs_g1 = "srs_zcash_g1" in
    let srs_g2 = "srs_zcash_g2" in

    (* Find the first candidate directory that contains the expected files. *)
    let contains_trusted_setup_files directory =
      file_exists (directory // srs_g1) && file_exists (directory // srs_g2)
    in
    match List.find_opt contains_trusted_setup_files candidate_directories with
    | None -> Error [No_trusted_setup candidate_directories]
    | Some directory ->
        let srs_g1_file = directory // srs_g1 in
        let srs_g2_file = directory // srs_g2 in
        (* FIXME https://gitlab.com/tezos/tezos/-/issues/3409

           An integrity check should ensure that only one SRS file is
           expected. The `21` constant is the logarithmic size of this
           file. A refactorisation, should ensure that this constant
           is not needed or could be computed. *)
        Ok {srs_g1_file; srs_g2_file; logarithm_size = 21}

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/3400

     An integrity check is run to ensure the validity of the files. *)
  let build_trusted_setup_instance t ~srs_g1_file ~srs_g2_file ~logarithm_size =
    assert (t.k < 1 lsl logarithm_size) ;
    let srs_g1 =
      Bls12_381_polynomial.Srs.M.(to_array (load_from_file srs_g1_file t.k))
    in
    let g2_size_compressed = Bls12_381.G2.size_in_bytes / 2 in
    let buf = Bytes.create g2_size_compressed in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/3416

       The reading is not in `Lwt`. Hence it can be an issue that this
       reading is blocking. *)
    let read ic =
      Stdlib.really_input ic buf 0 g2_size_compressed ;
      Bls12_381.G2.of_compressed_bytes_exn buf
    in
    let ic = open_in srs_g2_file in
    let file_size = in_channel_length ic in
    if file_size < t.k * g2_size_compressed then (
      close_in ic ;
      Error [Trusted_setup_too_small file_size])
    else
      let srs_g2 = Array.init t.k (fun _ -> read ic) in
      close_in ic ;
      Ok
        {
          srs_g1;
          srs_g2;
          kate_amortized_srs_g2_shards =
            Array.get srs_g2 (1 lsl t.evaluations_per_proof_log);
          kate_amortized_srs_g2_segments =
            Array.get srs_g2 (1 lsl Z.(log2up (of_int t.segment_len)));
        }

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/3399

           The reading of the files should be done beforehand. This
     would ease the assumptions made by the protocol, and especially
     avoid the issue that [load_srs] may fail because the file was not
     found which is the responsibility of the shell. *)
  let load_srs_from_file t =
    match find_trusted_setup () with
    | Ok {srs_g1_file; srs_g2_file; logarithm_size} ->
        build_trusted_setup_instance t ~srs_g1_file ~srs_g2_file ~logarithm_size
    | Error err -> Error err

  let load_srs t =
    match SRS_ring.find_opt srs_ring t.k with
    | None -> (
        match load_srs_from_file t with
        | Ok srs ->
            SRS_ring.replace srs_ring t.k srs ;
            Ok srs
        | Error err -> Error err)
    | Some k -> Ok k

  let srs = srs

  let polynomial_degree = Polynomials.degree

  let polynomial_evaluate = Polynomials.evaluate

  let fft_mul d ps =
    let open Evaluations in
    let evaluations = List.map (evaluation_fft d) ps in
    interpolation_fft d (mul_c ~evaluations ())

  (* We encode by segments of [segment_size] bytes each.  The segments
     are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let polynomial_from_bytes' t slot =
    if Bytes.length slot <> t.slot_size then
      Error
        (`Slot_wrong_size
          (Printf.sprintf "message must be %d bytes long" t.slot_size))
    else
      let offset = ref 0 in
      let res = Array.init t.k (fun _ -> Scalar.(copy zero)) in
      for segment = 0 to t.nb_segments - 1 do
        for elt = 0 to t.segment_len - 1 do
          if !offset > t.slot_size then ()
          else if elt = t.segment_len - 1 then (
            let dst = Bytes.create t.remaining_bytes in
            Bytes.blit slot !offset dst 0 t.remaining_bytes ;
            offset := !offset + t.remaining_bytes ;
            res.((elt * t.nb_segments) + segment) <- Scalar.of_bytes_exn dst)
          else
            let dst = Bytes.create scalar_bytes_amount in
            Bytes.blit slot !offset dst 0 scalar_bytes_amount ;
            offset := !offset + scalar_bytes_amount ;
            res.((elt * t.nb_segments) + segment) <- Scalar.of_bytes_exn dst
        done
      done ;
      Ok res

  let polynomial_from_slot t slot =
    let open Result_syntax in
    let* data = polynomial_from_bytes' t slot in
    Ok (Evaluations.interpolation_fft2 t.domain_k data)

  let eval_coset t eval slot offset segment =
    for elt = 0 to t.segment_len - 1 do
      let idx = (elt * t.nb_segments) + segment in
      let coeff = Scalar.to_bytes (Array.get eval idx) in
      if elt = t.segment_len - 1 then (
        Bytes.blit coeff 0 slot !offset t.remaining_bytes ;
        offset := !offset + t.remaining_bytes)
      else (
        Bytes.blit coeff 0 slot !offset scalar_bytes_amount ;
        offset := !offset + scalar_bytes_amount)
    done

  (* The segments are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let polynomial_to_bytes t p =
    let eval = Evaluations.(evaluation_fft t.domain_k p |> to_array) in
    let slot = Bytes.init t.slot_size (fun _ -> '0') in
    let offset = ref 0 in
    for segment = 0 to t.nb_segments - 1 do
      eval_coset t eval slot offset segment
    done ;
    slot

  let encode t p = Evaluations.(evaluation_fft t.domain_n p |> to_array)

  (* The shards are arranged in cosets to evaluate in batch with Kate
     amortized. *)
  let shards_from_polynomial t p =
    let codeword = encode t p in
    let len_shard = t.n / t.number_of_shards in
    let rec loop i map =
      if i = t.number_of_shards then map
      else
        let shard = Array.init len_shard (fun _ -> Scalar.(copy zero)) in
        for j = 0 to len_shard - 1 do
          shard.(j) <- codeword.((t.number_of_shards * j) + i)
        done ;
        loop (i + 1) (IntMap.add i shard map)
    in
    loop 0 IntMap.empty

  (* Computes the polynomial N(X) := \sum_{i=0}^{k-1} n_i x_i^{-1} X^{z_i}. *)
  let compute_n t eval_a' shards =
    let w = Domains.get t.domain_n 1 in
    let n_poly = Array.init t.n (fun _ -> Scalar.(copy zero)) in
    let open Result_syntax in
    let c = ref 0 in
    let* () =
      IntMap.iter_e
        (fun z_i arr ->
          if !c >= t.k then Ok ()
          else
            let rec loop j =
              match j with
              | j when j = Array.length arr -> Ok ()
              | _ -> (
                  let c_i = arr.(j) in
                  let z_i = (t.number_of_shards * j) + z_i in
                  let x_i = Scalar.pow w (Z.of_int z_i) in
                  let tmp = Evaluations.get eval_a' z_i in
                  Scalar.mul_inplace tmp tmp x_i ;
                  match Scalar.inverse_exn_inplace tmp tmp with
                  | exception _ -> Error (`Invert_zero "can't inverse element")
                  | () ->
                      Scalar.mul_inplace tmp tmp c_i ;
                      n_poly.(z_i) <- tmp ;
                      c := !c + 1 ;
                      loop (j + 1))
            in
            loop 0)
        shards
    in
    Ok n_poly

  let polynomial_from_shards t shards =
    let open Result_syntax in
    if t.k > IntMap.cardinal shards * t.shard_size then
      Error
        (`Not_enough_shards
          (Printf.sprintf
             "there must be at least %d shards to decode"
             (t.k / t.shard_size)))
    else
      (* 1. Computing A(x) = prod_{i=0}^{k-1} (x - w^{z_i}).
         Let w be a primitive nth root of unity and
         Ω_0 = {w^{number_of_shards j}}_{j=0 to (n/number_of_shards)-1}
         be the (n/number_of_shards)-th roots of unity and Ω_i = w^i Ω_0.

         Together, the Ω_i's form a partition of the subgroup of the n-th roots
         of unity: 𝕌_n = disjoint union_{i ∈ {0, ..., number_of_shards-1}} Ω_i.

         Let Z_j := Prod_{w ∈ Ω_j} (x − w). For a random set of shards
         S⊆{0, ..., number_of_shards-1} of length k/shard_size, we reorganize the
         product A(x) = Prod_{i=0}^{k-1} (x − w^{z_i}) into
         A(x) = Prod_{j ∈ S} Z_j.

         Moreover, Z_0 = x^|Ω_0| - 1 since x^|Ω_0| - 1 contains all roots of Z_0
         and conversely. Multiplying each term of the polynomial by the root w^j
         entails Z_j = x^|Ω_0| − w^{j*|Ω_0|}.

         The intermediate products Z_j have a lower Hamming weight (=2) than
         when using other ways of grouping the z_i's into shards.

         This also reduces the depth of the recursion tree of the poly_mul
         function from log(k) to log(number_of_shards), so that the decoding time
         reduces from O(k*log^2(k) + n*log(n)) to O(n*log(n)). *)
      let split = List.fold_left (fun (l, r) x -> (x :: r, l)) ([], []) in
      let f1, f2 =
        IntMap.bindings shards
        (* We always consider the first k codeword vector components. *)
        |> Tezos_stdlib.TzList.take_n (t.k / t.shard_size)
        |> split
      in
      let f11, f12 = split f1 in
      let f21, f22 = split f2 in

      let prod =
        List.fold_left
          (fun acc (i, _) ->
            Polynomials.mul_xn
              acc
              t.shard_size
              (Scalar.negate (Domains.get t.domain_n (i * t.shard_size))))
          Polynomials.one
      in
      let p11 = prod f11 in
      let p12 = prod f12 in
      let p21 = prod f21 in
      let p22 = prod f22 in

      let a_poly = fft_mul t.domain_2k [p11; p12; p21; p22] in

      (* 2. Computing formal derivative of A(x). *)
      let a' = Polynomials.derivative a_poly in

      (* 3. Computing A'(w^i) = A_i(w^i). *)
      let eval_a' = Evaluations.evaluation_fft t.domain_n a' in

      (* 4. Computing N(x). *)
      let* n_poly = compute_n t eval_a' shards in

      (* 5. Computing B(x). *)
      let b = Evaluations.interpolation_fft2 t.domain_n n_poly in
      let b = Polynomials.copy ~len:t.k b in
      Polynomials.mul_by_scalar_inplace b (Scalar.of_int t.n) b ;

      (* 6. Computing Lagrange interpolation polynomial P(x). *)
      let p = fft_mul t.domain_2k [a_poly; b] in
      let p = Polynomials.copy ~len:t.k p in
      Polynomials.opposite_inplace p ;
      Ok p

  let commit' :
      type t.
      (module Bls12_381.CURVE with type t = t) ->
      scalar array ->
      t array ->
      (t, [> `Degree_exceeds_srs_length of string]) Result.t =
   fun (module G) p srs ->
    if p = [||] then Ok G.(copy zero)
    else if Array.(length p > length srs) then
      Error
        (`Degree_exceeds_srs_length
          (Printf.sprintf
             "polynomial degree, %i, exceeds  srs’ length, %i."
             (Array.length p)
             (Array.length srs)))
    else Ok (G.pippenger ~start:0 ~len:(Array.length p) srs p)

  let commit trusted_setup p =
    commit'
      (module Bls12_381.G1)
      (Polynomials.to_dense_coefficients p)
      trusted_setup.srs_g1

  (* p(X) of degree n. Max degree that can be committed: d, which is also the
     SRS's length - 1. We take d = k - 1 since we don't want to commit
     polynomials with degree greater than polynomials to be erasure-encoded.

     We consider the bilinear groups (G_1, G_2, G_T) with G_1=<g> and G_2=<h>.
     - Commit (p X^{d-n}) such that deg (p X^{d-n}) = d the max degree
     that can be committed
     - Verify: checks if e(commit(p), commit(X^{d-n})) = e(commit(p X^{d-n}), h)
     using the commitments for p and p X^{d-n}, and computing the commitment for
     X^{d-n} on G_2.*)

  let prove_commitment trusted_setup p =
    commit'
      (module Bls12_381.G1)
      Polynomials.(
        to_dense_coefficients
          (mul (Polynomials.of_coefficients [(Scalar.(copy one), 0)]) p))
      trusted_setup.srs_g1

  (* FIXME https://gitlab.com/tezos/tezos/-/issues/3389

     Generalize this function to pass the degree in parameter. *)
  let verify_commitment trusted_setup cm proof =
    let open Bls12_381 in
    let check =
      match Array.get trusted_setup.srs_g2 0 with
      | exception Invalid_argument _ -> false
      | commit_xk ->
          Pairing.pairing_check
            [(cm, commit_xk); (proof, G2.(negate (copy one)))]
    in
    Ok check

  let inverse domain =
    let n = Array.length domain in
    Array.init n (fun i ->
        if i = 0 then Bls12_381.Fr.(copy one) else Array.get domain (n - i))

  let diff_next_power_of_two x =
    let logx = Z.log2 (Z.of_int x) in
    if 1 lsl logx = x then 0 else (1 lsl (logx + 1)) - x

  let is_pow_of_two x =
    let logx = Z.log2 (Z.of_int x) in
    1 lsl logx = x

  (* Implementation of fast amortized Kate proofs
     https://github.com/khovratovich/Kate/blob/master/Kate_amortized.pdf). *)

  (* Precompute first part of Toeplitz trick, which doesn't depends on the
     polynomial’s coefficients. *)
  let preprocess_multi_reveals ~chunk_len ~degree srs1 =
    let open Bls12_381 in
    let l = 1 lsl chunk_len in
    let k =
      let ratio = degree / l in
      let log_inf = Z.log2 (Z.of_int ratio) in
      if 1 lsl log_inf < ratio then log_inf else log_inf + 1
    in
    let domain = Domains.build ~log:k |> Domains.inverse |> inverse in
    let precompute_srsj j =
      let quotient = (degree - j) / l in
      let padding = diff_next_power_of_two (2 * quotient) in
      let points =
        Array.init
          ((2 * quotient) + padding)
          (fun i ->
            if i < quotient then G1.copy srs1.(degree - j - ((i + 1) * l))
            else G1.(copy zero))
      in
      G1.fft_inplace ~domain ~points ;
      points
    in
    (domain, Array.init l precompute_srsj)

  (** Generate proofs of part 3.2.
  n, r are powers of two, m = 2^(log2(n)-1)
  coefs are f polynomial’s coefficients [f₀, f₁, f₂, …, fm-1]
  domain2m is the set of 2m-th roots of unity, used for Toeplitz computation
  (domain2m, precomputed_srs_part) = preprocess_multi_reveals r n m srs1
   *)
  let multiple_multi_reveals ~chunk_len ~chunk_count ~degree
      ~preprocess:(domain2m, precomputed_srs_part) coefs =
    let open Bls12_381 in
    let n = chunk_len + chunk_count in
    assert (2 <= chunk_len) ;
    assert (chunk_len < n) ;
    assert (is_pow_of_two degree) ;
    assert (1 lsl chunk_len < degree) ;
    assert (degree <= 1 lsl n) ;
    let l = 1 lsl chunk_len in
    (* We don’t need the first coefficient f₀. *)
    let compute_h_j j =
      let rest = (degree - j) mod l in
      let quotient = (degree - j) / l in
      (* Padding in case quotient is not a power of 2 to get proper fft in
         Toeplitz matrix part. *)
      let padding = diff_next_power_of_two (2 * quotient) in
      (* fm, 0, …, 0, f₁, f₂, …, fm-1 *)
      let points =
        Array.init
          ((2 * quotient) + padding)
          (fun i ->
            if i <= quotient + (padding / 2) then Scalar.(copy zero)
            else Scalar.copy coefs.(rest + ((i - (quotient + padding)) * l)))
      in
      if j <> 0 then points.(0) <- Scalar.copy coefs.(degree - j) ;
      Scalar.fft_inplace ~domain:domain2m ~points ;
      Array.map2 G1.mul precomputed_srs_part.(j) points
    in
    let sum = compute_h_j 0 in
    let rec sum_hj j =
      if j = l then ()
      else
        let hj = compute_h_j j in
        (* sum.(i) <- sum.(i) + hj.(i) *)
        Array.iteri (fun i hij -> sum.(i) <- G1.add sum.(i) hij) hj ;
        sum_hj (j + 1)
    in
    sum_hj 1 ;

    (* Toeplitz matrix-vector multiplication *)
    G1.ifft_inplace ~domain:(inverse domain2m) ~points:sum ;
    let hl = Array.sub sum 0 (Array.length domain2m / 2) in

    let phidomain = Domains.build ~log:chunk_count in
    let phidomain = inverse (Domains.inverse phidomain) in
    (* Kate amortized FFT *)
    G1.fft ~domain:phidomain ~points:hl

  (* h = polynomial such that h(y×domain[i]) = zi. *)
  let interpolation_h_poly y domain z_list =
    Scalar.ifft_inplace ~domain:(Domains.inverse domain) ~points:z_list ;
    let inv_y = Scalar.inverse_exn y in
    Array.fold_left_map
      (fun inv_yi h -> Scalar.(mul inv_yi inv_y, mul h inv_yi))
      Scalar.(copy one)
      z_list
    |> snd

  (* Part 3.2 verifier : verifies that f(w×domain.(i)) = evaluations.(i). *)
  let verify cm_f (srs1, srs2l) domain (w, evaluations) proof =
    let open Bls12_381 in
    let open Result_syntax in
    let h = interpolation_h_poly w domain evaluations in
    let* cm_h = commit' (module G1) h srs1 in
    let l = Domains.length domain in
    let sl_min_yl =
      G2.(add srs2l (negate (mul (copy one) (Scalar.pow w (Z.of_int l)))))
    in
    let diff_commits = G1.(add cm_h (negate cm_f)) in
    Ok
      (Pairing.pairing_check
         [(diff_commits, G2.(copy one)); (proof, sl_min_yl)])

  let precompute_shards_proofs t trusted_setup =
    preprocess_multi_reveals
      ~chunk_len:t.evaluations_per_proof_log
      ~degree:t.k
      trusted_setup.srs_g1

  let _save_precompute_shards_proofs (preprocess : shards_proofs_precomputation)
      filename =
    let chan = Out_channel.open_bin filename in
    Out_channel.output_bytes
      chan
      (Data_encoding.Binary.to_bytes_exn
         Encoding.shards_proofs_precomputation_encoding
         preprocess) ;
    Out_channel.close_noerr chan

  let _load_precompute_shards_proofs filename =
    let chan = In_channel.open_bin filename in
    let len = Int64.to_int (In_channel.length chan) in
    let data = Bytes.create len in
    let (_ : unit option) = In_channel.really_input chan data 0 len in
    let precomp =
      Data_encoding.Binary.of_bytes_exn
        Encoding.shards_proofs_precomputation_encoding
        data
    in
    In_channel.close_noerr chan ;
    precomp

  let prove_shards t srs p =
    let preprocess = precompute_shards_proofs t srs in
    multiple_multi_reveals
      ~chunk_len:t.evaluations_per_proof_log
      ~chunk_count:t.proofs_log
      ~degree:t.k
      ~preprocess
      (Polynomials.to_dense_coefficients p)

  let verify_shard t trusted_setup cm
      {index = shard_index; share = shard_evaluations} proof =
    let d_n = Domains.build ~log:t.evaluations_log in
    let domain = Domains.build ~log:t.evaluations_per_proof_log in
    verify
      cm
      (trusted_setup.srs_g1, trusted_setup.kate_amortized_srs_g2_shards)
      domain
      (Domains.get d_n shard_index, shard_evaluations)
      proof

  let _prove_single trusted_setup p z =
    let q, _ =
      Polynomials.(
        division_xn (p - constant (evaluate p z)) 1 (Scalar.negate z))
    in
    commit'
      (module Bls12_381.G1)
      (Polynomials.to_dense_coefficients q)
      trusted_setup.srs_g1

  let _verify_single trusted_setup cm ~point ~evaluation proof =
    let h_secret = Array.get trusted_setup.srs_g2 1 in
    Bls12_381.(
      Pairing.pairing_check
        [
          ( G1.(add cm (negate (mul (copy one) evaluation))),
            G2.(negate (copy one)) );
          (proof, G2.(add h_secret (negate (mul (copy one) point))));
        ])

  let prove_segment t trusted_setup p segment_index =
    if segment_index < 0 || segment_index >= t.nb_segments then
      Error `Segment_index_out_of_range
    else
      let l = 1 lsl Z.(log2up (of_int t.segment_len)) in
      let wi = Domains.get t.domain_k segment_index in
      let quotient, _ =
        Polynomials.(division_xn p l Scalar.(negate (pow wi (Z.of_int l))))
      in
      commit trusted_setup quotient

  (* Parses the [slot_segment] to get the evaluations that it contains. The
     evaluation points are given by the [slot_segment_index]. *)
  let verify_segment t trusted_setup cm
      {index = slot_segment_index; content = slot_segment} proof =
    if slot_segment_index < 0 || slot_segment_index >= t.nb_segments then
      Error `Segment_index_out_of_range
    else
      let domain = Domains.build ~log:Z.(log2up (of_int t.segment_len)) in
      let slot_segment_evaluations =
        Array.init
          (1 lsl Z.(log2up (of_int t.segment_len)))
          (function
            | i when i < t.segment_len - 1 ->
                let dst = Bytes.create scalar_bytes_amount in
                Bytes.blit
                  slot_segment
                  (i * scalar_bytes_amount)
                  dst
                  0
                  scalar_bytes_amount ;
                Scalar.of_bytes_exn dst
            | i when i = t.segment_len - 1 ->
                let dst = Bytes.create t.remaining_bytes in
                Bytes.blit
                  slot_segment
                  (i * scalar_bytes_amount)
                  dst
                  0
                  t.remaining_bytes ;
                Scalar.of_bytes_exn dst
            | _ -> Scalar.(copy zero))
      in
      verify
        cm
        (trusted_setup.srs_g1, trusted_setup.kate_amortized_srs_g2_segments)
        domain
        (Domains.get t.domain_k slot_segment_index, slot_segment_evaluations)
        proof
end

include Inner
module Verifier = Inner
