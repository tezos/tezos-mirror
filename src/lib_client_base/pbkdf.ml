(* Copyright (c) 2016, Alfredo Beaumont, Sonia Meruelo  All rights reserved. *)
(* Distributed under the BSD2 license, see terms at the end of the file.     *)

let xorbuf a b =
  let alen = Bytes.length a in
  if Bytes.length b <> alen then
    invalid_arg "xor: both buffers must be of same size" ;
  for i = 0 to alen - 1 do
    Bytes.(set a i Char.(chr (code (get a i) lxor code (get b i))))
  done ;
  a

let cdiv x y =
  (* This is lifted from Nocrypto.Uncommon.(//)
     (formerly known as [cdiv]). It is part of the documented, publicly
     exposed _internal_ utility library not for public consumption, hence
     the API break that prompted this copy-pasted function. *)
  if y < 1 then raise Division_by_zero
  else if x > 0 then 1 + ((x - 1) / y)
  else 0
  [@@inline]

module type S = sig
  val pbkdf2 :
    password:Bytes.t -> salt:Bytes.t -> count:int -> dk_len:int32 -> Bytes.t
end

module Make (H : Hacl.Hash.S) : S = struct
  let pbkdf2 ~password ~salt ~count ~dk_len =
    if count <= 0 then invalid_arg "count must be a positive integer" ;
    if dk_len <= 0l then
      invalid_arg "derived key length must be a positive integer" ;
    let h_len = H.size and dk_len = Int32.to_int dk_len in
    let l = cdiv dk_len h_len in
    let r = dk_len - ((l - 1) * h_len) in
    let block i =
      let rec f u xor = function
        | 0 -> xor
        | j ->
            let u = H.HMAC.digest ~key:password ~msg:u in
            f u (xorbuf xor u) (j - 1)
      in
      let int_i = Bytes.create 4 in
      TzEndian.set_int32 int_i 0 (Int32.of_int i) ;
      let u_1 = H.HMAC.digest ~key:password ~msg:(Bytes.cat salt int_i) in
      f u_1 u_1 (count - 1)
    in
    let rec loop blocks = function
      | 0 -> blocks
      | i -> loop (block i :: blocks) (i - 1)
    in
    Bytes.concat Bytes.empty (loop [Bytes.sub (block l) 0 r] (l - 1))
end

module SHA256 = Make (Hacl.Hash.SHA256)
module SHA512 = Make (Hacl.Hash.SHA512)

(* Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
   AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
   IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
   FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
   SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
   OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
