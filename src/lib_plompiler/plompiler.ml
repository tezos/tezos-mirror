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

include Lang_core
include Lang_stdlib

module LibResult : sig
  include LIB

  val get_result : 'a repr t -> 'a Input.t
end = struct
  include Result
  include Lib (Result)
end

module LibCircuit : sig
  include LIB

  val deserialize : S.t array -> 'a Input.t -> 'a Input.t

  val get_inputs : 'a repr t -> S.t array * int

  type cs_result = {
    nvars : int;
    free_wires : int list;
    cs : Csir.CS.t;
    public_input_size : int;
    input_com_sizes : int list;
    tables : Csir.Table.t list;
    range_checks : (string * (int * int) list) list;
    solver : Solver.t;
  }
  [@@deriving repr]

  val get_cs : ?optimize:bool -> 'a repr t -> cs_result
end = struct
  include Circuit
  include Lib (Circuit)
end

module Gadget = struct
  module type HASH = Hash_sig.HASH

  module Anemoi128 = Gadget_anemoi.Anemoi128
  module AnemoiJive_128_1 = Gadget_anemoi.Make
  module Poseidon128 = Gadget_poseidon.Poseidon128
  module Poseidon252 = Gadget_poseidon.Poseidon252
  module PoseidonFull = Gadget_poseidon.PoseidonFull
  module Merkle = Gadget_merkle.Make
  module Merkle_narity = Gadget_merkle_narity
  module JubjubEdwards = Gadget_edwards.Jubjub
  module JubjubWeierstrass = Gadget_weierstrass.Jubjub
  module Schnorr = Gadget_schnorr.Make
  module Blake2s = Gadget_blake2s.Blake2s
end

include Gadget
module Utils = Utils
module Linear_algebra = Linear_algebra
module Optimizer = Optimizer
module Solver = Solver
module Encodings = Encoding.Encodings
module Bounded = Bounded
module Csir = Csir
