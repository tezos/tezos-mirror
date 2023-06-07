(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error += Tx_rollup_negative_input_size

(** A generic helper to hash an input *)
val hash :
  hash_f:(bytes list -> 'b) ->
  Raw_context.t ->
  'a Data_encoding.t ->
  'a ->
  (Raw_context.t * 'b) tzresult

(** [hash_cost size] returns the cost of gas for hashing a buffer of
    [size] bytes.

    Raises [Tx_rollup_negative_input_size] iff [size < 0]. *)
val hash_cost : int -> Gas_limit_repr.cost tzresult

val consume_check_path_inbox_cost : Raw_context.t -> Raw_context.t tzresult

val consume_check_path_commitment_cost : Raw_context.t -> Raw_context.t tzresult

(** [consume_add_message_cost ctxt] consume the gas cost of adding a
   message to an inbox and return the new context. *)
val consume_add_message_cost : Raw_context.t -> Raw_context.t tzresult

val consume_compact_commitment_cost :
  Raw_context.t -> int -> Raw_context.t tzresult
