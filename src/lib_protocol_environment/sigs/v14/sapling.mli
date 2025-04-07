(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

module Ciphertext : sig
  type t

  val encoding : t Data_encoding.t

  val get_memo_size : t -> int
end

module Commitment : sig
  type t

  val encoding : t Data_encoding.t

  val valid_position : int64 -> bool
end

module CV : sig
  type t

  val encoding : t Data_encoding.t
end

module Hash : sig
  type t

  val compare : t -> t -> int

  val encoding : t Data_encoding.t

  val to_bytes : t -> Bytes.t

  val of_bytes_exn : Bytes.t -> t

  val uncommitted : height:int -> t

  val merkle_hash : height:int -> t -> t -> t

  val of_commitment : Commitment.t -> t

  val to_commitment : t -> Commitment.t
end

module Nullifier : sig
  type t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int
end

module UTXO : sig
  type rk

  type spend_proof

  type spend_sig

  type output_proof

  type input = {
    cv : CV.t;
    nf : Nullifier.t;
    rk : rk;
    proof_i : spend_proof;
    signature : spend_sig;
  }

  val input_encoding : input Data_encoding.t

  type output = {
    cm : Commitment.t;
    proof_o : output_proof;
    ciphertext : Ciphertext.t;
  }

  val output_encoding : output Data_encoding.t

  type binding_sig

  type transaction = {
    inputs : input list;
    outputs : output list;
    binding_sig : binding_sig;
    balance : Int64.t;
    root : Hash.t;
    bound_data : string;
  }

  val transaction_encoding : transaction Data_encoding.t

  val binding_sig_encoding : binding_sig Data_encoding.t

  module Legacy : sig
    type transaction_new = transaction

    type transaction = {
      inputs : input list;
      outputs : output list;
      binding_sig : binding_sig;
      balance : Int64.t;
      root : Hash.t;
    }

    val transaction_encoding : transaction Data_encoding.t

    val cast : transaction -> transaction_new
  end
end

module Verification : sig
  type t

  val with_verification_ctx : (t -> 'a) -> 'a

  val check_spend : t -> UTXO.input -> Hash.t -> string -> bool

  val check_output : t -> UTXO.output -> bool

  val final_check : t -> UTXO.transaction -> string -> bool
end
