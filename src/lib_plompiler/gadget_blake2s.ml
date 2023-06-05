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

open Lang_stdlib

module type BLAKE2S = functor (L : LIB) -> sig
  open L

  val mixing_g :
    Bytes.bl repr Array.t ->
    int ->
    int ->
    int ->
    int ->
    Bytes.bl repr ->
    Bytes.bl repr ->
    Bytes.bl repr Array.t t

  val compression :
    Bytes.bl repr Array.t ->
    Bytes.bl repr Array.t ->
    Stdint.Uint64.t ->
    bool ->
    Bytes.bl repr Array.t t

  val blake2s : Bytes.bl repr -> bytes -> Bytes.bl repr t
end

module Blake2s : BLAKE2S =
functor
  (L : LIB)
  ->
  struct
    open L

    let mixing_g v a b c d x y =
      with_label ~label:"Blake2s.mixing_g"
      @@
      let r1 = 16 in
      let r2 = 12 in
      let r3 = 8 in
      let r4 = 7 in
      let* tmp = Bytes.add ~ignore_carry:true v.(a) v.(b) in
      let* tmp = Bytes.add ~ignore_carry:true tmp x in
      v.(a) <- tmp ;
      let* tmp = Bytes.xor v.(d) v.(a) in
      let tmp = Bytes.rotate_right tmp r1 in
      v.(d) <- tmp ;
      let* tmp = Bytes.add ~ignore_carry:true v.(c) v.(d) in
      v.(c) <- tmp ;
      let* tmp = Bytes.xor v.(b) v.(c) in
      let tmp = Bytes.rotate_right tmp r2 in
      v.(b) <- tmp ;
      let* tmp = Bytes.add ~ignore_carry:true v.(a) v.(b) in
      let* tmp = Bytes.add ~ignore_carry:true tmp y in
      v.(a) <- tmp ;
      let* tmp = Bytes.xor v.(d) v.(a) in
      let tmp = Bytes.rotate_right tmp r3 in
      v.(d) <- tmp ;
      let* tmp = Bytes.add ~ignore_carry:true v.(c) v.(d) in
      v.(c) <- tmp ;
      let* tmp = Bytes.xor v.(b) v.(c) in
      let tmp = Bytes.rotate_right tmp r4 in
      v.(b) <- tmp ;
      ret v

    let sigma =
      [|
        [|0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15|];
        [|14; 10; 4; 8; 9; 15; 13; 6; 1; 12; 0; 2; 11; 7; 5; 3|];
        [|11; 8; 12; 0; 5; 2; 15; 13; 10; 14; 3; 6; 7; 1; 9; 4|];
        [|7; 9; 3; 1; 13; 12; 11; 14; 2; 6; 5; 10; 4; 0; 15; 8|];
        [|9; 0; 5; 7; 2; 4; 10; 15; 14; 1; 11; 12; 6; 8; 3; 13|];
        [|2; 12; 6; 10; 0; 11; 8; 3; 4; 13; 7; 5; 15; 14; 1; 9|];
        [|12; 5; 1; 15; 14; 13; 4; 10; 0; 7; 6; 3; 9; 2; 8; 11|];
        [|13; 11; 7; 14; 12; 1; 3; 9; 5; 0; 15; 4; 8; 6; 2; 10|];
        [|6; 15; 14; 9; 11; 3; 0; 8; 12; 2; 13; 7; 1; 4; 10; 5|];
        [|10; 2; 8; 4; 7; 6; 1; 5; 15; 11; 9; 14; 3; 12; 13; 0|];
      |]

    let compression h m t f =
      assert (Array.length h = 8) ;
      assert (Array.length m = 16) ;
      with_label ~label:"Blake2s.compression"
      @@ let* constants =
           mapM
             (fun c -> Bytes.constant (Utils.bytes_of_hex c))
             [
               "6A09E667";
               "BB67AE85";
               "3C6EF372";
               "A54FF53A";
               "510E527F";
               "9B05688C";
               "1F83D9AB";
               "5BE0CD19";
             ]
         in
         let constants = Array.of_list constants in
         let v = Array.append h constants in
         let* tmp = Bytes.constant_uint32 Stdint.Uint64.(to_uint32 t) in
         let* tmp = Bytes.xor v.(12) tmp in
         v.(12) <- tmp ;
         let* tmp =
           Bytes.constant_uint32 Stdint.Uint64.(to_uint32 (shift_right t 32))
         in
         let* tmp = Bytes.xor v.(13) tmp in
         v.(13) <- tmp ;
         (* TODO here we assign many times the same constant *)
         let* tmp = Bytes.constant_uint32 Stdint.Uint32.max_int in
         let* tmp = if f then Bytes.xor v.(14) tmp else ret v.(14) in
         v.(14) <- tmp ;
         let* v =
           foldiM
             (fun v i ->
               let s = sigma.(i mod 10) in
               let* v = mixing_g v 0 4 8 12 m.(s.(0)) m.(s.(1)) in
               let* v = mixing_g v 1 5 9 13 m.(s.(2)) m.(s.(3)) in
               let* v = mixing_g v 2 6 10 14 m.(s.(4)) m.(s.(5)) in
               let* v = mixing_g v 3 7 11 15 m.(s.(6)) m.(s.(7)) in
               let* v = mixing_g v 0 5 10 15 m.(s.(8)) m.(s.(9)) in
               let* v = mixing_g v 1 6 11 12 m.(s.(10)) m.(s.(11)) in
               let* v = mixing_g v 2 7 8 13 m.(s.(12)) m.(s.(13)) in
               mixing_g v 3 4 9 14 m.(s.(14)) m.(s.(15)))
             v
             10
         in
         foldiM
           (fun h i ->
             let* tmp = Bytes.xor h.(i) v.(i) in
             h.(i) <- tmp ;
             let* tmp = Bytes.xor h.(i) v.(i + 8) in
             h.(i) <- tmp ;
             ret h)
           h
           8

    let blake2s input personalization =
      assert (Stdlib.Bytes.length personalization = 8) ;
      assert (List.length (of_list input) mod 8 = 0) ;
      with_label ~label:"Blake2s.blake2s"
      @@
      let fist_iv =
        Stdint.Uint32.(
          let c = of_bytes_big_endian (Utils.bytes_of_hex "6A09E667") 0 in
          let mask = of_bytes_big_endian (Utils.bytes_of_hex "01010000") 0 in
          let c = logxor (logxor c mask) (of_int 32) in
          let b = Stdlib.Bytes.create 4 in
          to_bytes_big_endian c b 0 ;
          let (`Hex s) = Hex.of_bytes b in
          s)
      in
      let* h =
        foldM
          (fun acc c ->
            let* w = Bytes.constant (Utils.bytes_of_hex c) in
            ret (w :: acc))
          []
          [
            fist_iv;
            "BB67AE85";
            "3C6EF372";
            "A54FF53A";
            "510E527F";
            "9B05688C";
            (* TODO these last two should also contain personalization *)
            "1F83D9AB";
            "5BE0CD19";
          ]
      in
      let h = Array.of_list (List.rev h) in
      let* z = Bool.constant false in

      let rec loop (blocks, acc_block, acc_u32) (nblock, nu32) rest =
        let blocks, acc_block, acc_u32, nblock, nu32 =
          if nu32 = 32 then
            (blocks, List.rev acc_u32 :: acc_block, [], nblock + 1, 0)
          else (blocks, acc_block, acc_u32, nblock, nu32)
        in
        let blocks, acc_block, acc_u32, nblock, nu32 =
          if nblock = 16 then (
            assert (nu32 = 0) ;
            (Array.of_list (List.rev acc_block) :: blocks, [], [], 0, 0))
          else (blocks, acc_block, acc_u32, nblock, nu32)
        in
        match rest with
        | [] ->
            if nblock = 0 && nu32 = 0 then blocks
            else loop (blocks, acc_block, z :: acc_u32) (nblock, nu32 + 1) []
        | bit :: rest ->
            loop (blocks, acc_block, bit :: acc_u32) (nblock, nu32 + 1) rest
      in
      let blocks = loop ([], [], []) (0, 0) (of_list input) in
      match blocks with
      | [] -> assert false
      | last :: rest_inverted ->
          let* _ =
            foldM
              (fun i b ->
                let t = Stdint.Uint64.(succ (of_int i) * of_int 64) in
                let* _ = compression h b t false in
                ret (i + 1))
              0
              (List.rev @@ List.map (Array.map to_list) rest_inverted)
          in
          let t = Stdint.Uint64.of_int (List.length (of_list input) / 8) in
          let* h = compression h (Array.map to_list last) t true in
          let res = List.(map of_list (Array.to_list h) |> flatten) in
          ret @@ to_list res
  end
