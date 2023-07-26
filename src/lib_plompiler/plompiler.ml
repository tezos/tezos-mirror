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

(** Plompiler is an OCaml eDSL for writing PlonK arithmetic circuits
    (see {!Lib_plonk.Main_protocol} for an introduction to PlonK circuits).

    The eDSL is implemented following a tagless-final approach and
    is split into a core language and a standard library, each
    described as a module type. The core is defined by {!Lang_core.COMMON}
    while the standard library is defined by {!Lang_stdlib.LIB}.

    The Plompiler library is defined around three main parameterized types.
    First, ['a t] is a monadic type that represents a Plompiler computation
    returning a value of type ['a]. Second, ['a repr] is the Plompiler
    representation of a value of type ['a]. Finally, ['a input] represents
    an input to a circuit of type ['a]. These two final types are related:
    an ['a input] will become an ['a repr] once inputted into a circuit.

    A Plompiler program, then, will be a functor over [LIB]. The general
    structure of of a Plompiler program is:

    {[
      module Program (L : LIB) = struct
        open L

        let logic : 'a repr -> 'b repr -> unit repr t = ...

        let prog : 'a input -> 'b input -> unit repr t =
          fun a b ->
            let* a = input ~kind:'Public a in
            let* b = input b in
            logic a b
      end
    ]}

    Here, the first function defines the [logic] of the program,
    while [prog] declares the inputs, the first of which is public.

    A module implementing the `LIB` signature can be seen as an interpreter of
    the language. Concretely, two such interpreters are defined: {!LibResult}
    and {!LibCircuit}. The first runs a Plompiler program with pure values,
    i.e. without generating a circuit. This means that, for this interpreter,
    ['a repr] is essentially ['a]. The second, {!LibCircuit}, is the actual
    circuit backend. Here, ['a repr] will represent a PlonK wire carrying
    a value of type ['a].

    The rest of the library is implemented through the following modules:

    {ul
      {li {!Csir}: defines the Constraint System intermediate representation,
      which is an abstract representation for PlonK constraint systems.
      This is the target of the {!LibCircuit} interpreter, and can be converted
      into a PlonK circuit.
      }
      {li {!Encoding}: encoders/decoders for usage of structured data types
      in circuits. Simplifies data manipulation and the definition of inputs.
      }
      {li {!Optimizer}: low-level generic optimizer of [Csir] constraint
      systems.
      }
      {li {!Solver}: description and interpretation of the programs needed to
      populate the PlonK witness for a Plompiler circuit given the initial
      inputs.}
      {li [Gadget_X]: building blocks for circuits, mainly implementing
      cryptographic primitives.}
    }
*)

(** Pure-value backend. Used to check the semantics of {!LibCircuit} in the
    tests. *)
module LibResult : sig
  include LIB

  (** [get_result prog] runs the Plompiler computation [prog] and returns
      its result. The result is wrapped in the [Input.t] structure as this
      describes the possible values that Plompiler can deal with. *)
  val get_result : 'a repr t -> 'a Input.t
end = struct
  include Result
  include Lib (Result)
end

(** Circuit producing backend. *)
module LibCircuit : sig
  include LIB

  (** [deserialize scalars inpt] populates the structure of
      [inpt] with the values from [scalars]. *)
  val deserialize : S.t array -> 'a Input.t -> 'a Input.t

  (** [get_inputs c] returns the initial inputs for the computation [c],
      together with the number of those inputs that are public.
      This fuction is useful when the inputs used for the circuit definition
      have meaningful values (in contrast to the often used dummy inputs, as
      explained in {!Lang_core.COMMON.Input}).
  *)
  val get_inputs : 'a repr t -> S.t array * int

  (** Constraint system and auxiliary data resulting from a
      Plompiler program. *)
  type cs_result = {
    nvars : int;  (** Number of variables in the [cs] witness *)
    free_wires : int list;
        (** Wire indices that are not used in the [cs],
            they have been freed by the optimizer. *)
    cs : Csir.CS.t;  (** Constraint system *)
    public_input_size : int;
    input_com_sizes : int list;  (** Sizes for input commitments *)
    tables : Csir.Table.t list;  (** Tables for lookups *)
    range_checks : (int * (int * int) list) list;
        (** Range checks following the format:
         index of wire * (index in wire * bound) *)
    range_checks_labels : (string list * int) list;
        (** label trace that creates a range-check and
            the size of the range-check *)
    solver : Solver.t;  (** Solver for the [cs] *)
  }
  [@@deriving repr]

  (** [get_cs ?optimize c] runs the computation [c] generating a
      constraint system. If [optimize] is [true], it will run the
      optimizer on the produced CS. The optimized CS will be
      cached in {!Utils.circuit_dir}, using the [TMPDIR] environment
      variable. *)
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
  module JubjubEdwards =
    Gadget_edwards.MakeAffine (Mec.Curve.Jubjub.AffineEdwards)
  module JubjubWeierstrass =
    Gadget_weierstrass.MakeAffine (Mec.Curve.Jubjub.AffineWeierstrass)
  module Schnorr = Gadget_schnorr.Make (Mec.Curve.Jubjub.AffineEdwards)
  module Edwards25519 = Gadget_edwards25519.MakeEdwards25519
  module Ed25519 = Gadget_ed25519.Ed25519
  module Blake2s = Gadget_blake2s.Blake2s
  module ArithMod25519 = Gadget_mod_arith.ArithMod25519
  module ArithMod64 = Gadget_mod_arith.ArithMod64
  module Sha256 = Gadget_sha2.SHA256
  module Sha512 = Gadget_sha2.SHA512
end

include Gadget
module Utils = Utils
module Linear_algebra = Linear_algebra
module Optimizer = Optimizer
module Solver = Solver
module Bounded = Bounded
module Csir = Csir
