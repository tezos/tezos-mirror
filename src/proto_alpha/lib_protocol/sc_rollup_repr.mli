(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The basic components of an optimistic rollup for smart-contracts. *)

(**

   An optimistic rollup for smart-contracts is made of two main
   components:

   - a proof generating virtual machine (PVM), which provides the
   essential semantics for the rollup operations to be validated by
   the layer 1 in case of dispute about a commitment ;

   - a database which maintains the finalized operations of the rollup
   as well as the potentially-disputed operations.

*)
module PVM : sig
  (** A PVM instance can be initialized by setting a boot sector. *)
  type boot_sector

  val boot_sector_encoding : boot_sector Data_encoding.t

  val boot_sector_of_string : string -> boot_sector
end

(** A smart-contract rollup has an address starting with "scr1". *)
module Address : sig
  include S.HASH

  (** [from_nonce nonce] produces an address completely determined by
     an operation hash and an origination counter. *)
  val from_nonce : Origination_nonce.t -> t tzresult

  (** [encoded_size] is the number of bytes needed to represent an address. *)
  val encoded_size : int
end

(** A smart contract rollup is identified by its address. *)
type t = Address.t

val encoding : t Data_encoding.t

val rpc_arg : t RPC_arg.t

(** The data model uses an index of these addresses. *)
module Index : Storage_description.INDEX with type t = Address.t

(** A smart contract rollup has a kind, which assigns meaning to
   rollup operations. *)
module Kind : sig
  type t = unit

  val encoding : t Data_encoding.t
end
