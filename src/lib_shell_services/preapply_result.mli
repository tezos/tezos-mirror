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

(** A container for classified operations *)
type 'error t = {
  applied : (Operation_hash.t * Operation.t) list;  (** Applied operations *)
  refused : (Operation.t * 'error list) Operation_hash.Map.t;
      (** Refused operations, for example because of an invalid signature *)
  outdated : (Operation.t * 'error list) Operation_hash.Map.t;
      (** Outdated operations, for example a late endorsement *)
  branch_refused : (Operation.t * 'error list) Operation_hash.Map.t;
      (** Branch refused operations, for example because of insufficient balance *)
  branch_delayed : (Operation.t * 'error list) Operation_hash.Map.t;
      (** Branch delayed operations, for example because of a timestamp in the future *)
}

(** [empty t] returns an empty instance of {!t}: all collections
 *  are initialized empty *)
val empty : 'error t

(** [encoding err_enc t], where [err_enc] is the encoding for errors,
 *  returns an encoding for [t] *)
val encoding : 'error list Data_encoding.t -> 'error t Data_encoding.t
