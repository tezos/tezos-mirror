(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Annotated manager operations are wrappers used to accumulate information
    (especially about limits) on the operation prior to the injection. *)

open Protocol
open Alpha_context

type _ t =
  | Manager_info : {
      source : Alpha_context.public_key_hash option;
      fee : Tez.t Limit.t;
      gas_limit : Gas.Arith.integral Limit.t;
      storage_limit : Z.t Limit.t;
      counter : Manager_counter.t option;
      operation : 'kind manager_operation;
    }
      -> 'kind t

type packed = Annotated_manager_operation : 'kind t -> packed

(** The [annotated_list] type helps making
    [contents_list] from a list of [manager_operation]s.
    Its construction mimics [contents_list] in order to keep
    consistent types when calling [inject_manager_operation]
    and [inject_operation].*)
type _ annotated_list =
  | Single_manager : 'kind t -> 'kind annotated_list
  | Cons_manager :
      'kind t * 'rest annotated_list
      -> ('kind * 'rest) annotated_list

type packed_annotated_list =
  | Manager_list : 'kind annotated_list -> packed_annotated_list

(** Convert a list of annotated operations to a list of packed annotated
    operations *)
val manager_to_list : packed_annotated_list -> packed list

(** Converse of [manager_to_list] *)
val manager_of_list : packed list -> packed_annotated_list

(** [join_fee fee op] updates [op.fee] to [Limit.join op.fee fee] and
    fails if the join fails *)
val join_fee : Tez.t Limit.t -> 'a t -> 'a t tzresult

(** [set_fee fee op] updates [op.fee] to [fee] *)
val set_fee : Tez.t Limit.t -> 'a t -> 'a t

(** See [join_fee] *)
val join_gas_limit : Gas.Arith.integral Limit.t -> 'a t -> 'a t tzresult

(** See [set_fee] *)
val set_gas_limit : Gas.Arith.integral Limit.t -> 'a t -> 'a t

(** See [join_fee] *)
val join_storage_limit : Z.t Limit.t -> 'a t -> 'a t tzresult

(** See [set_fee] *)
val set_storage_limit : Z.t Limit.t -> 'a t -> 'a t

(** Set the counter of the annotated operation. Fail if the counter
    is already set. *)
val set_counter : Manager_counter.t -> 'a t -> 'a t tzresult

(** Set the source of the operation. Fail if the source is already set. *)
val set_source : public_key_hash -> 'a t -> 'a t tzresult

(** Convert an annotated manager operation to a proper manager operation.
    Fail if some fields in the annotated operation are not set. *)
val manager_from_annotated : 'a t -> 'a Kind.manager contents tzresult

val manager_list_from_annotated :
  'kind annotated_list -> 'kind Kind.manager contents_list tzresult
