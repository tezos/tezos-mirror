(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
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

module Hash : sig
  type state

  val init : unit -> state

  val update : state -> bytes -> unit

  val finish : state -> bytes

  val hash_bytes : bytes list -> bytes

  val bytes_to_seed : bytes -> int array * bytes
end = struct
  type state = Hacl_star.EverCrypt.Hash.t

  let init () =
    Hacl_star.EverCrypt.Hash.init ~alg:Hacl_star.SharedDefs.HashDefs.BLAKE2b

  let update st msg = Hacl_star.EverCrypt.Hash.update ~st ~msg

  let finish st = Hacl_star.EverCrypt.Hash.finish ~st

  let hash_bytes bytes =
    (* select the appropriate BLAKE2b function depending on platform and
     * always produce a 32 byte digest *)
    let blake2b msg =
      let digest_size = 32 in
      let open Hacl_star in
      if AutoConfig2.(has_feature VEC256) then
        Hacl.Blake2b_256.hash msg digest_size
      else Hacl.Blake2b_32.hash msg digest_size
    in
    blake2b (Bytes.concat Bytes.empty bytes)

  (* generate a seed for Random.full_init from hash of b bytes
     Also returns the hash of the bytes*)
  let bytes_to_seed b =
    let hashed_b = hash_bytes [b] in
    assert (Bytes.length hashed_b = 32) ;
    let sys_int_size = Sys.int_size - 1 in
    let modulo = Z.pow (Z.of_int 2) sys_int_size in
    (* seed generation based on four int, computed from hashed_b sub_byte ;
       each ni is Bytes.sub hashed_b i 8 modulo 2**sys.int_size, in order to avoid
       Z.Overflow when ni is converted to int *)
    let n0_raw = Z.of_bits (Bytes.sub_string hashed_b 0 8) in
    let n0 = Z.to_int (Z.erem n0_raw modulo) in
    let n1_raw = Z.of_bits (Bytes.sub_string hashed_b 8 8) in
    let n1 = Z.to_int (Z.erem n1_raw modulo) in
    let n2_raw = Z.of_bits (Bytes.sub_string hashed_b 16 8) in
    let n2 = Z.to_int (Z.erem n2_raw modulo) in
    let n3_raw = Z.of_bits (Bytes.sub_string hashed_b 24 8) in
    let n3 = Z.to_int (Z.erem n3_raw modulo) in
    ([|n0; n1; n2; n3|], hashed_b)
end

module Transcript = struct
  (* expand a transcript with the elements of a list *)
  let list_expand repr list transcript =
    let open Hash in
    let st = init () in
    update st transcript ;
    List.iter (fun a -> update st (Plompiler.Utils.to_bytes repr a)) list ;
    finish st

  let expand : 'a Repr.t -> 'a -> bytes -> bytes =
   fun repr x transcript -> list_expand repr [x] transcript
end

module Array = struct
  include Array

  (* Pad array to given size with the last element of the array *)
  let pad array final_size =
    let size = Array.length array in
    Array.init final_size (fun i ->
        if i < size then array.(i) else array.(size - 1))

  (* Resize array: return the array, subarray or pad it with its last element *)
  let resize array final_size =
    let size = Array.length array in
    if size = final_size then array
    else if size > final_size then Array.sub array 0 final_size
    else pad array final_size

  let build init next len =
    let xi = ref init in
    Array.init len (fun _ ->
        let i = !xi in
        xi := next !xi ;
        i)
end

(* This function converts answers to a list of scalars. If [nb_proofs] < [nb_max_proofs], the missing answers will be added as zero, in an order that is suitable for aPlonK’s switches *)
let pad_answers nb_max_proofs nb_rc_wires nb_proofs
    (answers : S.t SMap.t SMap.t list) =
  let answers = List.map (SMap.map SMap.values) answers in
  (* We want to work on the 'a map list because it’s the only way to find the wires in the answers without knowing if there is ultra or next wire *)
  let answers_padded =
    List.map_end
      (SMap.map (fun w_list ->
           w_list
           @ List.init
               ((nb_max_proofs - nb_proofs)
               * (Plompiler.Csir.nb_wires_arch + nb_rc_wires))
               (Fun.const S.zero)))
      answers
  in
  answers_padded |> List.concat_map SMap.values |> List.flatten

module Fr_generation : sig
  (* computes [| 1; x; x²; x³; ...; xᵈ⁻¹ |] *)
  val powers : int -> Bls.Scalar.t -> Bls.Scalar.t array

  (* [batch x l] adds the elements of l scaled by ascending powers of x *)
  val batch : Bls.Scalar.t -> Bls.Scalar.t list -> Bls.Scalar.t

  (* quadratic non-residues for Sid *)
  val build_quadratic_non_residues : int -> Bls.Scalar.t array

  (* generate several scalars based on seed transcript *)
  val random_fr_list : Bytes.t -> int -> Bls.Scalar.t list * Bytes.t

  (* generate a single scalars based on seed transcript *)
  val random_fr : Bytes.t -> Bls.Scalar.t * Bytes.t

  (* Evaluates L1 on x, where L1 is the minimal (monic) polynomial that
     satisfies L1(generator) = 1 and L1(generator^i) = 0
     for all i = 2, ..., domain_size. *)
  val evaluate_l1 :
    domain_size:int -> generator:Bls.Scalar.t -> Bls.Scalar.t -> Bls.Scalar.t

  (* Evaluates Ln_p_1 on x, where Ln_p_1 is the minimal (monic) polynomial that
     satisfies Ln_p_1(1) = 1 and Ln_p_1(generator^i) = 0
     for all i = 1, ..., domain_size. *)
  val evaluate_l0 : domain_size:int -> Bls.Scalar.t -> Bls.Scalar.t
end = struct
  open Bls

  let powers d x = Array.build Scalar.one Scalar.(mul x) d

  let batch x l =
    List.fold_left
      (fun acc y -> Scalar.((x * acc) + y))
      Scalar.zero
      (List.rev l)

  let build_quadratic_non_residues len =
    let is_nonresidue n = Z.(equal (Scalar.legendre_symbol n) Z.(-one)) in
    let rec next n =
      Scalar.(n + one) |> fun n -> if is_nonresidue n then n else next n
    in
    Array.build Scalar.one next len

  (* a is the element to hash
   * to_bytes_func, add, one is the function of conversion to_bytes, the function of addition, the one compatible with a type
   * returns x ∈ F built from the hash of a
   * if hash a not in F, returns hash (a+1) until its value belongs to F
   *)
  let rec hash_to_Fr a =
    let b = Z.to_bits a |> Bytes.of_string in
    let hashed_b = Hash.hash_bytes [b] in
    assert (Bytes.length hashed_b = 32) ;
    let x_fr = Scalar.of_bytes_opt hashed_b in
    match x_fr with
    | Some a -> a (* x_fr can be converted *)
    | None -> hash_to_Fr (Z.succ a)

  let generate_random_fr ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let n0 = Z.of_int64 @@ Random.int64 Int64.max_int in
    let n1 = Z.of_int64 @@ Random.int64 Int64.max_int in
    let n2 = Z.of_int64 @@ Random.int64 Int64.max_int in
    let n3 = Z.of_int64 @@ Random.int64 Int64.max_int in
    let n1_64 = Z.(n1 lsl 64) in
    let n2_128 = Z.(n2 lsl 128) in
    let n3_192 = Z.(n3 lsl 192) in
    let gamma_z = Z.(n0 + n1_64 + n2_128 + n3_192) in
    let gamma_fr = hash_to_Fr gamma_z in
    gamma_fr

  (* generate nb_values scalar of Fr based on seed transcript *)
  let random_fr_list transcript nb_values =
    let transcript_array, hashed_transcript = Hash.bytes_to_seed transcript in
    Random.full_init transcript_array ;
    (List.init nb_values (fun _ -> generate_random_fr ()), hashed_transcript)

  let random_fr transcript =
    let l, hashed_transcript = random_fr_list transcript 1 in
    (List.hd l, hashed_transcript)

  let evaluate_l1 ~domain_size ~generator x =
    let n = Z.of_int domain_size in
    let l1_num = Scalar.(generator * sub (pow x n) one) in
    let l1_den = Scalar.(of_z n * sub x generator) in
    Scalar.div_exn l1_num l1_den

  let evaluate_l0 ~domain_size x =
    let n = Z.of_int domain_size in
    let l0_num = Scalar.(sub (pow x n) one) in
    let l0_den = Scalar.(of_z n * sub x one) in
    Scalar.div_exn l0_num l0_den
end
