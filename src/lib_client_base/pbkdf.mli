(* Copyright (c) 2016, Alfredo Beaumont, Sonia Meruelo  All rights reserved. *)
(* Distributed under the BSD2 license, see terms at the end of the file.     *)

(** {{:https://tools.ietf.org/html/rfc2898}RFC 2898} specifies two password-based
    key derivation functions (PBKDF1 and PBKDF2), which are abstracted over
    a specific hash/pseudorandom function. *)
module type S = sig
  (** [pbkdf2 password salt count dk_len] is [dk], the derived key of [dk_len] octets.
      @raise Invalid_argument when either [count] or [dk_len] are not valid *)
  val pbkdf2 :
    password:Bytes.t -> salt:Bytes.t -> count:int -> dk_len:int32 -> Bytes.t
end

(** Given a Hash/pseudorandom function, get the PBKDF *)
module Make (H : Hacl.Hash.S) : S

module SHA256 : S

module SHA512 : S

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
