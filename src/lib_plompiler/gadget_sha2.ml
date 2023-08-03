(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Lang_stdlib
open Sha2_variants

(** Gadget implementing SHA2.
    Specification can be found at
    https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf

    Precious test vectors can be found at
    https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values
    under "Secure hashing".
  *)

module MAKE
    (L : LIB)
    (V : VARIANT)
    (Op : Limb_list
            with type scalar = L.scalar
             and type 'a repr = 'a L.repr
             and type 'a t = 'a L.t
             and type 'a input = 'a L.Input.t) =
struct
  open L
  module M64 = Gadget_mod_arith.ArithMod64 (L)

  (* Utils *)
  let split_exactly array size_chunk nb_chunks =
    assert (Bytes.length array = size_chunk * nb_chunks) ;
    let res =
      List.init nb_chunks (fun i ->
          let array = Array.of_list (of_list array) in
          let array = Array.sub array (i * size_chunk) size_chunk in
          to_list (Array.to_list array))
    in
    Array.of_list (List.rev res)

  let debug_toggle = false

  let debug msg v = if debug_toggle then debug msg v else ret unit

  let debug_array s a =
    if debug_toggle then debug s unit >* iterM (debug "") (Array.to_list a)
    else ret unit

  (* For now, we mainly focus on optimising SHA-512, so we have a special
     case for a 64-bit word, where a modular addition gadget is used.
     In the future, the same optimisation can be done for a 32-bit word. *)
  let add_list (lb : Op.tl repr list) : Op.tl repr t =
    with_label ~label:"Sha2.add_list"
    @@
    if V.word_size = 64 then
      let mod_int_of_bytes (x : Op.tl repr) : M64.mod_int repr t =
        let* sx = Op.to_scalar x in
        M64.mod_int_of_scalars (to_list [sx])
      in
      let* lm = mapM mod_int_of_bytes lb in
      let* mres = foldM M64.add (List.hd lm) (List.tl lm) in
      let* sres = M64.scalars_of_mod_int mres in
      Op.of_scalar ~total_nb_bits:V.word_size (List.hd (of_list sres))
    else
      let* (lb : Bytes.tl repr list) = mapM Op.to_bool_list lb in
      let* res =
        foldM (Bytes.add ~ignore_carry:true) (List.hd lb) (List.tl lb)
      in
      Op.of_bool_list res

  let add a b = add_list [a; b]

  let xor_list lb = foldM Op.xor (List.hd lb) (List.tl lb)

  (* Section 4.1.2
     use six logical functions, where each function operates on 32-bit words,
     which are represented as x, y, and z. The result of each function is a
     new 32-bit word. For SHA-512, 64-bit operations are used. *)

  (* Ch(x, y, z) = (x && y) XOR ( !x && z) *)
  let ch x y z =
    with_label ~label:"Sha2.Ch"
    @@ let* x_and_y = Op.band x y in
       let* not_x = Op.not x in
       let* not_x_and_z = Op.band not_x z in
       Op.xor x_and_y not_x_and_z

  (* Maj(x, y, z) = (x && y) XOR (x && z) XOR (y && z) *)
  let maj x y z =
    with_label ~label:"Sha2.Maj"
    @@ let* x_and_y = Op.band x y in
       let* x_and_z = Op.band x z in
       let* y_and_z = Op.band y z in
       xor_list [x_and_y; x_and_z; y_and_z]

  (* Sum_0(x) = ROTR^{c0}(x) XOR ROTR^{c1}(x) XOR ROTR^{c2}(x) *)
  let sum_0 x =
    with_label ~label:"Sha2.Sum0"
    @@ let* x0 = Op.rotate_right x V.sum_constants.(0) in
       let* x1 = Op.rotate_right x V.sum_constants.(1) in
       let* x2 = Op.rotate_right x V.sum_constants.(2) in
       xor_list [x0; x1; x2]

  (* Sum_1(x) = ROTR^{c3}(x) XOR ROTR^{c4}(x) XOR ROTR^{c5}(x) *)
  let sum_1 x =
    with_label ~label:"Sha2.Sum1"
    @@ let* x0 = Op.rotate_right x V.sum_constants.(3) in
       let* x1 = Op.rotate_right x V.sum_constants.(4) in
       let* x2 = Op.rotate_right x V.sum_constants.(5) in
       xor_list [x0; x1; x2]

  (* Sigma_0(x) = ROTR^{d0}(x) XOR ROTR^{d1}(x) XOR SHR^{d2}(x) *)
  let sigma_0 x =
    with_label ~label:"Sha2.Sigma0"
    @@ let* x0 = Op.rotate_right x V.sigma_constants.(0) in
       let* x1 = Op.rotate_right x V.sigma_constants.(1) in
       let* x2 = Op.shift_right x V.sigma_constants.(2) in
       xor_list [x0; x1; x2]

  (* Sigma_1(x) = ROTR^{d3}(x) XOR ROTR^{d4}(x) XOR SHR^{d5}(x) *)
  let sigma_1 x =
    with_label ~label:"Sha2.Sigma1"
    @@ let* x0 = Op.rotate_right x V.sigma_constants.(3) in
       let* x1 = Op.rotate_right x V.sigma_constants.(4) in
       let* x2 = Op.shift_right x V.sigma_constants.(5) in
       xor_list [x0; x1; x2]

  (* Section 4.2.2 constants *)
  let ks : Op.tl repr array t =
    with_label ~label:"Sha2.ks"
    @@ let* a =
         mapM
           (fun s -> Op.constant ~le:false @@ Utils.bytes_of_hex s)
           (Array.to_list V.round_constants)
       in
       ret @@ Array.of_list a

  (* Section 5.3 *)
  let initial_hash : Op.tl repr array t =
    let* a =
      mapM
        (fun s -> Op.constant ~le:false @@ Utils.bytes_of_hex s)
        (Array.to_list V.init_hash)
    in
    ret @@ Array.of_list a

  (* Section 5.1.1 *)
  let padding : Bytes.tl repr -> Bytes.tl repr t =
   fun msg ->
    with_label ~label:"Sha2.padding"
    @@
    let l = Bytes.length msg in
    let k =
      let k = (V.block_size - (2 * V.word_size) - (l + 1)) mod V.block_size in
      if k > 0 then k else k + V.block_size
    in
    let* padding =
      let bitlist = List.(init k (Fun.const false) @ [true]) in
      Bytes.constant ~le:false @@ Utils.of_bitlist ~le:false bitlist
    in
    let* binary_l =
      let ocaml_bytes = Z.of_int l |> Z.to_bits |> Stdlib.Bytes.of_string in
      let ocaml_bytes =
        let len = Stdlib.Bytes.length ocaml_bytes in
        let len_padded = V.word_size / 4 in
        if len = len_padded then ocaml_bytes
        else
          let bytes_padded = Stdlib.Bytes.make len_padded '\000' in
          Stdlib.Bytes.blit ocaml_bytes 0 bytes_padded 0 len ;
          bytes_padded
      in
      Bytes.constant ~le:true ocaml_bytes
    in
    ret @@ Bytes.concat [|msg; padding; binary_l|]

  (* Section 5.2 *)
  let parsing : Bytes.tl repr -> Bytes.tl repr array array =
   fun msg ->
    let nb_blocks = Bytes.length msg / V.block_size in
    (* Split in blocks of V.block_size bits *)
    let blocks = split_exactly msg V.block_size nb_blocks in
    (* Split each block into 16 words of V.word_size bits *)
    Array.map (fun block -> split_exactly block V.word_size 16) blocks

  (* Section 6.2.2 step 1 *)
  let schedule : Op.tl repr array -> Op.tl repr array t =
   fun message_block ->
    assert (Array.length message_block = 16) ;
    with_label ~label:"Sha2.schedule"
    @@ let* rest =
         let* res =
           mapM
             (fun _ -> Op.constant ~le:false Stdlib.Bytes.empty)
             (List.init (V.loop_bound - 16) Fun.id)
         in
         ret @@ Array.of_list res
       in
       let ws = Array.append message_block rest in
       let rec aux t =
         if t = V.loop_bound then ret ()
         else
           (* res = sigma_1 ws.(t - 2) + ws.(t - 7) + sigma_0 ws.(t - 15) + ws.(t - 16) *)
           let* res =
             let* tmp1 = sigma_1 ws.(t - 2) in
             let* tmp2 = sigma_0 ws.(t - 15) in
             add_list [tmp1; ws.(t - 7); tmp2; ws.(t - 16)]
           in
           ws.(t) <- res ;
           aux (succ t)
       in
       let* () = aux 16 in
       ret ws

  type vars =
    Op.tl repr
    * Op.tl repr
    * Op.tl repr
    * Op.tl repr
    * Op.tl repr
    * Op.tl repr
    * Op.tl repr
    * Op.tl repr

  let assign_variables : Op.tl repr array -> vars =
   fun hs ->
    let a = hs.(0) in
    let b = hs.(1) in
    let c = hs.(2) in
    let d = hs.(3) in
    let e = hs.(4) in
    let f = hs.(5) in
    let g = hs.(6) in
    let h = hs.(7) in
    (a, b, c, d, e, f, g, h)

  (* Section 6.2.2 step 3. *)
  let step3_one_iteration t : vars -> Op.tl repr array -> vars t =
   fun (a, b, c, d, e, f, g, h) ws ->
    let ( + ) = add in
    with_label ~label:"Sha2.step3_one_iteration"
    @@ let* ks in
       (* t1 <- h + tmp_sum + tmp_ch + ks.(t) + ws.(t) *)
       let* t1 =
         let* tmp_sum = sum_1 e in
         let* tmp_ch = ch e f g in
         add_list [h; tmp_sum; tmp_ch; ks.(t); ws.(t)]
       in
       let* t1_plus_t2 =
         let* tmp_sum = sum_0 a in
         let* tmp_maj = maj a b c in
         add_list [t1; tmp_sum; tmp_maj]
       in
       let h = g in
       let g = f in
       let f = e in
       let* e = d + t1 in
       let d = c in
       let c = b in
       let b = a in
       let a = t1_plus_t2 in
       ret (a, b, c, d, e, f, g, h)

  let step3 : vars -> Op.tl repr array -> vars t =
   fun vars ws ->
    with_label ~label:"Sha2.step3"
    @@
    let rec aux (acc : vars) t =
      if t = V.loop_bound then ret acc
      else
        let* acc = step3_one_iteration t acc ws in
        aux acc (t + 1)
    in
    aux vars 0

  (* Section 6.2.2 step 4 *)
  let compute_intermediate_hash : vars -> Op.tl repr array -> Op.tl repr array t
      =
   fun (a, b, c, d, e, f, g, h) hs ->
    with_label ~label:"Sha2.compute_intermediate_hash"
    @@
    let vars = [a; b; c; d; e; f; g; h] in
    let hs = Array.to_list hs in
    let* res = map2M add vars hs in
    ret @@ Array.of_list res

  let process_one_block :
      Op.tl repr array -> Op.tl repr array -> Op.tl repr array t =
   fun block hs ->
    with_label ~label:"Sha2.process_one_block"
    @@ let* ws = schedule block in
       let vars = assign_variables hs in
       let* vars = step3 vars ws in
       compute_intermediate_hash vars hs

  let digest : Bytes.tl repr -> L.Bytes.tl repr t =
   fun blocks ->
    assert (Bytes.length blocks mod 8 = 0) ;
    with_label ~label:"Sha2.digest"
    @@ let* blocks = padding blocks in
       let* _ = debug "padding" blocks in
       let blocks = parsing blocks in
       let* _ = debug_array "parsing" blocks.(0) in
       let* initial_hash in
       let rec process_blocks (acc : Op.tl repr array) i =
         if i = Array.length blocks then ret acc
         else
           let* blocks_i = mapM Op.of_bool_list (Array.to_list blocks.(i)) in
           let* acc = process_one_block (Array.of_list blocks_i) acc in
           process_blocks acc (i + 1)
       in
       let* res = process_blocks initial_hash 0 in
       let* res = mapM Op.to_bool_list (Array.to_list res) in
       let res = Array.sub (Array.of_list res) 0 V.digest_blocks in
       ret @@ Bytes.concat res
end

module type SHA2 = functor (L : LIB) -> sig
  open L

  val digest : Bytes.tl repr -> Bytes.tl repr t
end

module SHA224 : SHA2 = functor (L : LIB) -> MAKE (L) (Sha224) (L.Bytes)

module SHA256 : SHA2 = functor (L : LIB) -> MAKE (L) (Sha256) (L.Bytes)

module SHA384 : SHA2 =
functor
  (L : LIB)
  ->
  MAKE (L) (Sha384)
    (L.Limbs (struct
      let nb_bits = 4
    end))

module SHA512 : SHA2 =
functor
  (L : LIB)
  ->
  MAKE (L) (Sha512)
    (L.Limbs (struct
      let nb_bits = 4
    end))
