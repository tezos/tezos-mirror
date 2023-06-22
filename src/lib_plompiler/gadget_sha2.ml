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

(* This file was partially generalized to handle more variants of SHA2, in
   reality only SHA256 is properly tested, so there are probably some SHA256
   constants lurking around. *)

module MAKE (V : VARIANT) =
functor
  (L : LIB)
  ->
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

    let bytes_add_list ?(ignore_carry = true) lb =
      if V.word_size = 64 then
        let mod_int_of_bytes x =
          let* sx = Num.scalar_of_bytes x in
          M64.mod_int_of_scalars (to_list [sx])
        in
        let* lm = mapM mod_int_of_bytes lb in
        let* mres = foldM M64.add (List.hd lm) (List.tl lm) in
        let* sres = M64.scalars_of_mod_int mres in
        bits_of_scalar ~nb_bits:V.word_size (List.hd (of_list sres))
      else foldM (Bytes.add ~ignore_carry) (List.hd lb) (List.tl lb)

    let bytes_add ?(ignore_carry = true) a b =
      bytes_add_list ~ignore_carry [a; b]

    (* Section 4.1.2
       use six logical functions, where each function operates on 32-bit words,
       which are represented as x, y, and z. The result of each function is a
       new 32-bit word. *)

    (* Ch(x, y, z) = (x && y) XOR ( !x && z) *)
    let ch x y z =
      with_label ~label:"Sha2.Ch"
      @@ let* x_and_y = Bytes.band x y in
         let* not_x = Bytes.not x in
         let* not_x_and_z = Bytes.band not_x z in
         let* res = Bytes.xor x_and_y not_x_and_z in
         ret res

    (* Maj(x, y, z) = (x && y) XOR (x && z) XOR (y && z) *)
    let maj x y z =
      with_label ~label:"Sha2.Maj"
      @@ let* x_and_y = Bytes.band x y in
         let* x_and_z = Bytes.band x z in
         let* y_and_z = Bytes.band y z in
         let* tmp = Bytes.xor x_and_y x_and_z in
         let* res = Bytes.xor tmp y_and_z in
         ret res

    (* Sum_0(x) = ROTR^{c0}(x) XOR ROTR^{c1}(x) XOR ROTR^{c2}(x) *)
    let sum_0 x =
      with_label ~label:"Sha2.Sum0"
      @@
      let x0 = Bytes.rotate_right x V.sum_constants.(0) in
      let x1 = Bytes.rotate_right x V.sum_constants.(1) in
      let x2 = Bytes.rotate_right x V.sum_constants.(2) in
      let* tmp = Bytes.xor x0 x1 in
      let* res = Bytes.xor tmp x2 in
      ret res

    (* Sum_1(x) = ROTR^{c3}(x) XOR ROTR^{c4}(x) XOR ROTR^{c5}(x) *)
    let sum_1 x =
      with_label ~label:"Sha2.Sum1"
      @@
      let x0 = Bytes.rotate_right x V.sum_constants.(3) in
      let x1 = Bytes.rotate_right x V.sum_constants.(4) in
      let x2 = Bytes.rotate_right x V.sum_constants.(5) in
      let* tmp = Bytes.xor x0 x1 in
      let* res = Bytes.xor tmp x2 in
      ret res

    (* Sigma_0(x) = ROTR^{d0}(x) XOR ROTR^{d1}(x) XOR SHR^{d2}(x) *)
    let sigma_0 x =
      with_label ~label:"Sha2.Sigma0"
      @@
      let x0 = Bytes.rotate_right x V.sigma_constants.(0) in
      let x1 = Bytes.rotate_right x V.sigma_constants.(1) in
      let* x2 = Bytes.shift_right x V.sigma_constants.(2) in
      let* tmp = Bytes.xor x0 x1 in
      let* res = Bytes.xor tmp x2 in
      ret res

    (* Sigma_1(x) = ROTR^{d3}(x) XOR ROTR^{d4}(x) XOR SHR^{d5}(x) *)
    let sigma_1 x =
      with_label ~label:"Sha2.Sigma1"
      @@
      let x0 = Bytes.rotate_right x V.sigma_constants.(3) in
      let x1 = Bytes.rotate_right x V.sigma_constants.(4) in
      let* x2 = Bytes.shift_right x V.sigma_constants.(5) in
      let* tmp = Bytes.xor x0 x1 in
      let* res = Bytes.xor tmp x2 in
      ret res

    (* Section 4.2.2 constants *)
    let ks : Bytes.bl repr array t =
      with_label ~label:"Sha2.ks"
      @@ let* a =
           mapM
             (fun s -> Bytes.constant ~le:false @@ Utils.bytes_of_hex s)
             (Array.to_list V.round_constants)
         in
         ret @@ Array.of_list a

    (* Section 5.3 *)
    let initial_hash : Bytes.bl repr array t =
      let* a =
        mapM
          (fun s -> Bytes.constant ~le:false @@ Utils.bytes_of_hex s)
          (Array.to_list V.init_hash)
      in
      ret @@ Array.of_list a

    (* Section 5.1.1 *)
    let padding : Bytes.bl repr -> Bytes.bl repr t =
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
    let parsing : Bytes.bl repr -> Bytes.bl repr array array =
     fun msg ->
      let nb_blocks = Bytes.length msg / V.block_size in
      (* Split in blocks of V.block_size bits *)
      let blocks = split_exactly msg V.block_size nb_blocks in
      (* Split each block into 16 words of V.word_size bits *)
      Array.map (fun block -> split_exactly block V.word_size 16) blocks

    (* Section 6.2.2 step 1 *)
    let schedule : Bytes.bl repr array -> Bytes.bl repr array t =
     fun message_block ->
      assert (Array.length message_block = 16) ;
      with_label ~label:"Sha2.schedule"
      @@ let* rest =
           let* res =
             mapM
               (fun _ -> Bytes.constant ~le:false Stdlib.Bytes.empty)
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
               bytes_add_list [tmp1; ws.(t - 7); tmp2; ws.(t - 16)]
             in
             ws.(t) <- res ;
             aux (succ t)
         in
         let* () = aux 16 in
         ret ws

    type vars =
      Bytes.bl repr
      * Bytes.bl repr
      * Bytes.bl repr
      * Bytes.bl repr
      * Bytes.bl repr
      * Bytes.bl repr
      * Bytes.bl repr
      * Bytes.bl repr

    let assign_variables : Bytes.bl repr array -> vars =
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
    let step3_one_iteration t : vars -> Bytes.bl repr array -> vars t =
     fun (a, b, c, d, e, f, g, h) ws ->
      let ( + ) = bytes_add in
      with_label ~label:"Sha2.step3_one_iteration"
      @@ let* ks in
         (* t1 <- h + tmp_sum + tmp_ch + ks.(t) + ws.(t) *)
         let* t1 =
           let* tmp_sum = sum_1 e in
           let* tmp_ch = ch e f g in
           bytes_add_list [h; tmp_sum; tmp_ch; ks.(t); ws.(t)]
         in
         let* t1_plus_t2 =
           let* tmp_sum = sum_0 a in
           let* tmp_maj = maj a b c in
           bytes_add_list [t1; tmp_sum; tmp_maj]
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

    let step3 : vars -> L.Bytes.bl repr array -> vars t =
     fun vars ws ->
      with_label ~label:"Sha2.step3"
      @@
      let rec aux acc t =
        (let a, b, c, d, e, f, g, h = acc in
         let tmp = [|a; b; c; d; e; f; g; h|] in
         debug_array ("t" ^ string_of_int t) tmp)
        >*
        if t = V.loop_bound then ret acc
        else
          let* acc = step3_one_iteration t acc ws in
          aux acc (t + 1)
      in
      aux vars 0

    (* Section 6.2.2 step 4 *)
    let compute_intermediate_hash :
        vars -> Bytes.bl repr array -> Bytes.bl repr array t =
     fun (a, b, c, d, e, f, g, h) hs ->
      with_label ~label:"Sha2.compute_intermediate_hash"
      @@
      let vars = [a; b; c; d; e; f; g; h] in
      let hs = Array.to_list hs in
      let* res = map2M bytes_add vars hs in
      ret @@ Array.of_list res

    let process_one_block :
        L.Bytes.bl repr array ->
        L.Bytes.bl repr array ->
        L.Bytes.bl repr array t =
     fun block hs ->
      with_label ~label:"Sha2.process_one_block"
      @@ let* ws = schedule block in
         let vars = assign_variables hs in
         let* vars = step3 vars ws in
         compute_intermediate_hash vars hs

    let digest : Bytes.bl repr -> L.Bytes.bl repr t =
     fun blocks ->
      assert (Bytes.length blocks mod 8 = 0) ;
      with_label ~label:"Sha2.digest"
      @@ let* blocks = padding blocks in
         let* _ = debug "padding" blocks in
         let blocks = parsing blocks in
         let* _ = debug_array "parsing" blocks.(0) in
         let* initial_hash in
         let rec process_blocks acc i =
           let* _ = debug_array ("ih" ^ string_of_int i) acc in
           if i = Array.length blocks then ret acc
           else
             let* acc = process_one_block blocks.(i) acc in
             process_blocks acc (i + 1)
         in
         let* res = process_blocks initial_hash 0 in
         let res = Array.sub res 0 V.digest_blocks in
         ret @@ Bytes.concat res
  end

module type SHA2 = functor (L : LIB) -> sig
  open L

  val digest : Bytes.bl repr -> Bytes.bl repr t
end

module SHA224 : SHA2 = MAKE (Sha224)

module SHA256 : SHA2 = MAKE (Sha256)

module SHA384 : SHA2 = MAKE (Sha384)

module SHA512 : SHA2 = MAKE (Sha512)
