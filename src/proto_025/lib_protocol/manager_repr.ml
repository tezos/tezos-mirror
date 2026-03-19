(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(* Tezos Protocol Implementation - Low level Repr. of Managers' keys *)

(* In S, the type of the [Manager] storage has been changed to [manager_key_storage_type].
   This change is purely internal. Previously revealed keys are determined to be valid
   or not at encoding time. This is necessary because the tz4 addresses that have been
   revealed before S should be revealed again. After S, the revealed keys are
   registered separately (tag 2).

   To keep consistency, this type is projected on [manager_key], which has the same
   types as before, and verifies the same invariants. This means that any member of
   [Hash] needs to be revealed (regardless of if it was already revealed before S or not),
   and member of [Public_key] are considered revealed.
*)
type manager_key_storage_type =
  | Hash of Signature.Public_key_hash.t
  | Public_key_before_S of Signature.Public_key.t
  | Public_key_after_S of Signature.Public_key.t

open Data_encoding

let hash_case tag =
  case
    tag
    ~title:"Public_key_hash"
    Signature.Public_key_hash.encoding
    (function Hash hash -> Some hash | _ -> None)
    (fun hash -> Hash hash)

let pubkey_case tag =
  case
    tag
    ~title:"Public_key"
    Signature.Public_key.encoding
    (function Public_key_before_S pk -> Some pk | _ -> None)
    (fun pk -> Public_key_before_S pk)

let pubkey_ok_case tag =
  case
    tag
    ~title:"Public_key_after_S"
    Signature.Public_key.encoding
    (function Public_key_after_S pk -> Some pk | _ -> None)
    (fun pk -> Public_key_after_S pk)

let encoding_legacy =
  union [hash_case (Tag 0); pubkey_case (Tag 1); pubkey_ok_case (Tag 2)]

type manager_key =
  | Hash of Signature.Public_key_hash.t
  | Public_key of Signature.Public_key.t

type t = manager_key

let encoding =
  conv
    (function
      | Hash hash -> (Hash hash : manager_key_storage_type)
      (* This encoding change only takes effect after S, so newly registered public keys
         are stored as [Public_key_after_S] *)
      | Public_key pk -> Public_key_after_S pk)
    (function
      | Hash hash -> Hash hash
      | Public_key_before_S (Bls _ as pk) -> Hash (Signature.Public_key.hash pk)
      | Public_key_before_S pk -> Public_key pk
      | Public_key_after_S pk -> Public_key pk)
    encoding_legacy
