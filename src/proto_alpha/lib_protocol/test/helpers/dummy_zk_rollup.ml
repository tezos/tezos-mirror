(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Dummy ZK Rollup for testing the ZKRU integration in the protocol.
    The library Plompiler is used to build the circuits (in a module V as
    verifier) and the corresponding functions to produce the inputs for the
    circuits (in a module P as prover).

    The state of this rollup is a boolean value, which will be
    represented with a scalar value of [zero] for [false] and
    [one] for [true].

    This RU has only one operation, with [op_code] 0. In addition to the
    common header (see {!Zk_rollup_operation_repr}), this operation has
    as payload one scalar representing a boolean value.

    The transition function [f] for this rollup is:

    {[
      f : operation -> state -> state
      f (Op b) s = if b = s then not s else s
    ]}

    That is, the state bool is flipped only if the operation's payload is
    equal to the current state.

    The operation can be used publicly or in a private batch. The circuits
    that describe the RU are:
    - ["op"]: for a single public operation.
    - ["batch-"[N]]: for a batch of [N] private operations. [N] is determined
      by the [batch_size] parameter to the [Operator] functor.
    - ["fee"]: the trivial fees circuit, since this RU has no concept of fees.

    NB: the "op" circuit does not add any constraints over the operation's
    [exit_validity] other than it being in [{0, 1}]. This means that the dummy
    rollup can be used to test deposits/withdrawals, but the rollup will not
    perform any monetary bookkeeping.
*)

open Plompiler

(** Helper types and modules *)

(** Empty types to represent bounds *)

type balance

type amount

type fee

type op_code

(** Bounds required for the dummy rollup.  *)
module Bound : sig
  type 'a t = private Z.t

  val bound_balance : balance t

  val bound_amount : amount t

  val bound_fee : fee t

  val bound_op_code : op_code t

  val v : 'a t -> Z.t
end = struct
  type 'a t = Z.t

  (** These bounds are exclusive. *)

  (** Upper bound for ticket balance, as found in the price field of an
      operation's header *)
  let bound_balance = Z.(shift_left one 20)

  (** Upper bound for ticket amount, used for fee circuit *)
  let bound_amount = Z.(shift_left one 20)

  (** Upper bound for fee amount of one public operation *)
  let bound_fee = Z.(shift_left one 10)

  (** Upper bound for op code *)
  let bound_op_code = Z.one

  let v x = x
end

(** Modules to manipulate bounded integers, both as OCaml values and in circuit
    representation.
*)
module Bounded = Bounded.Make (Bound)

(** Types used for the Dummy Rollup circuits.
    This module is split into:
    - P: concrete OCaml version of the types,
    - V: Plompiler's circuit representation for P's types, and
    - Encodings: conversion between P and V.
*)
module Types = struct
  module P = struct
    type state = bool

    module Bounded = Bounded.P

    type 'a ticket = {id : S.t; amount : 'a Bounded.t}

    type tezos_pkh = Environment.Signature.Public_key_hash.t

    type header = {
      op_code : op_code Bounded.t;
      price : balance ticket;
      l1_dst : tezos_pkh;
      rollup_id : tezos_pkh;
    }

    type op = {header : header; payload : bool}

    (** Dummy values for these types. Useful to get the circuit without having
        the actual inputs. *)
    module Dummy = struct
      let op_code = Bounded.make ~bound:Bound.bound_op_code Z.zero

      let balance = Bounded.make ~bound:Bound.bound_balance Z.zero

      let tezos_pkh = Environment.Signature.Public_key_hash.zero

      let ticket_balance = {id = S.zero; amount = balance}

      let header =
        {
          op_code;
          price = ticket_balance;
          l1_dst = tezos_pkh;
          rollup_id = tezos_pkh;
        }
    end
  end

  module V (L : LIB) = struct
    open L
    module Bounded_u = Bounded.V (L)

    type 'a ticket_u = {id : scalar repr; amount : 'a Bounded_u.t}

    type tezos_pkh_u = scalar repr

    type header_u = {
      op_code : op_code Bounded_u.t;
      price : balance ticket_u;
      l1_dst : tezos_pkh_u;
      rollup_id : tezos_pkh_u;
    }

    type op_u = {header : header_u; payload : bool repr}
  end

  module Encodings (L : LIB) = struct
    module Bounded_e = Bounded.Encoding (L)
    open P

    open V (L)

    open Encodings (L)

    let op_code_encoding ~safety =
      Bounded_e.encoding ~safety Bound.bound_op_code

    let encoding_to_scalar e x =
      let bs = Data_encoding.Binary.to_bytes_exn e x in
      let z = Z.of_bits @@ Bytes.to_string bs in
      Bls12_381.Fr.of_z z

    let encoding_of_scalar e x =
      let z = Bls12_381.Fr.to_z x in
      let bs = Bytes.of_string @@ Z.to_bits z in
      Data_encoding.Binary.of_bytes_exn e bs

    let tezos_pkh_encoding : (tezos_pkh, tezos_pkh_u, _) encoding =
      conv
        (fun pkhu -> pkhu)
        (fun w -> w)
        (encoding_to_scalar Signature.Public_key_hash.encoding)
        (encoding_of_scalar Signature.Public_key_hash.encoding)
        scalar_encoding

    let amount_encoding ~safety = Bounded_e.encoding ~safety Bound.bound_amount

    let fee_encoding ~safety = Bounded_e.encoding ~safety Bound.bound_fee

    let ticket_encoding ~safety (bound : 'a Bound.t) :
        ('a ticket, 'a ticket_u, _) encoding =
      conv
        (fun {id; amount} -> (id, amount))
        (fun (id, amount) -> {id; amount})
        (fun ({id; amount} : 'a ticket) -> (id, amount))
        (fun (id, amount) -> {id; amount})
        (obj2_encoding scalar_encoding (Bounded_e.encoding ~safety bound))

    let ticket_balance_encoding ~safety =
      ticket_encoding ~safety Bound.bound_balance

    let header_encoding ~safety : (header, header_u, _) encoding =
      conv
        (fun {op_code; price; l1_dst; rollup_id} ->
          (op_code, (price, (l1_dst, rollup_id))))
        (fun (op_code, (price, (l1_dst, rollup_id))) ->
          {op_code; price; l1_dst; rollup_id})
        (fun ({op_code; price; l1_dst; rollup_id} : header) ->
          (op_code, (price, (l1_dst, rollup_id))))
        (fun (op_code, (price, (l1_dst, rollup_id))) ->
          {op_code; price; l1_dst; rollup_id})
        (obj4_encoding
           (op_code_encoding ~safety)
           (* We use an Unsafe Bounded scalar encoding here to be able to
              detect that an out-of-range value has been passed.
              This encoding is unsafe in the sense that such value will cause
              a failure in proving, instead of a circuit that can prove that
              the argument is out-of-range.
              This is enough for Protocol testing purposes, while keeping
              the dummy circuit small.
           *)
           (ticket_balance_encoding ~safety:Unsafe)
           tezos_pkh_encoding
           tezos_pkh_encoding)

    let op_encoding : (op, op_u, _) encoding =
      conv
        (fun {header; payload} -> (header, payload))
        (fun (header, payload) -> {header; payload})
        (fun ({header; payload} : op) -> (header, payload))
        (fun (header, payload) -> {header; payload})
        (obj2_encoding (header_encoding ~safety:NoCheck) bool_encoding)
  end
end

(** Plompiler circuits for the dummy rollup  *)
module V (L : LIB) = struct
  open L
  module E = Types.Encodings (L)
  module Encodings = Encodings (L)
  open Encodings

  open Types.V (L)

  let coerce (type a) (x : a Bounded_u.t) =
    fst (x : a Bounded_u.t :> scalar repr * Z.t)

  (** Common logic for the state transition function *)
  let logic_op ~old_state ~rollup_id op =
    ignore rollup_id ;
    let* valid = equal old_state op.payload in
    let* new_state = Bool.bnot old_state in
    let* expected_new_state = Bool.ifthenelse valid new_state old_state in
    Num.assert_eq_const (coerce op.header.op_code) S.zero
    (* >* assert_equal rollup_id op.header.rollup_id *)
    >* ret expected_new_state

  (** Circuit definition for one public operation *)
  let predicate_op ?(kind = `Public) ~old_state ~new_state ~fee ~exit_validity
      ~rollup_id op =
    let* old_state = input ~kind:`Public @@ Input.bool old_state in
    let* new_state = input ~kind:`Public @@ Input.bool new_state in
    let* (_fee : scalar repr) =
      input ~kind:`Public
      @@ E.((fee_encoding ~safety:Bounded_e.Unsafe).input) fee
    in
    let* (_exit_validity : bool repr) =
      input ~kind:`Public @@ Input.bool exit_validity
    in
    let* rollup_id =
      input ~kind:`Public @@ E.(tezos_pkh_encoding.input) rollup_id
    in
    let* op = input ~kind @@ E.op_encoding.input op in
    let op = E.op_encoding.decode op in
    let* expected_new_state = logic_op ~old_state ~rollup_id op in
    assert_equal expected_new_state new_state

  (** Circuit definition for a batch of private operations *)
  let predicate_batch ~old_state ~new_state ~fees ~rollup_id ops =
    let* old_state = input ~kind:`Public @@ Input.bool old_state in
    let* new_state = input ~kind:`Public @@ Input.bool new_state in
    let* (_fees : scalar repr) =
      input ~kind:`Public
      @@ E.((amount_encoding ~safety:Bounded_e.Unsafe).input) fees
    in
    let* rollup_id =
      input ~kind:`Public @@ E.(tezos_pkh_encoding.input) rollup_id
    in
    let* ops = input @@ (Encodings.list_encoding E.op_encoding).input ops in
    let ops = (Encodings.list_encoding E.op_encoding).decode ops in
    let* computed_final_state =
      foldM
        (fun old_state op -> logic_op ~old_state ~rollup_id op)
        old_state
        ops
    in
    assert_equal computed_final_state new_state

  (** Fee circuit *)
  let predicate_fees ~old_state ~new_state ~fees =
    let* old_state = input ~kind:`Public @@ Input.bool old_state in
    let* new_state = input ~kind:`Public @@ Input.bool new_state in
    let* (_fees : scalar repr) =
      input ~kind:`Public
      @@ E.((amount_encoding ~safety:Bounded_e.Unsafe).input) fees
    in
    assert_equal old_state new_state
end

(** Basic rollup operator for generating Updates.  *)
module Operator (Params : sig
  val batch_size : int
end) : sig
  open Protocol.Alpha_context

  (** Initial state of the rollup  *)
  val init_state : Zk_rollup.State.t

  (** Map associating every circuit identifier to its kind *)
  val circuits : [`Public | `Private | `Fee] Plonk.SMap.t

  (** Commitment to the circuits  *)
  val lazy_pp :
    (Plonk.Main_protocol.prover_public_parameters
    * Plonk.Main_protocol.verifier_public_parameters)
    lazy_t

  (** [craft_update state ~zk_rollup ?private_ops ?exit_validities public_ops]
      will apply first the [public_ops], then the [private_ops]. While doing so,
      the public inputs for every circuit will be collected. A Plonk proof of
      correctness of the application these operations is created. *)
  val craft_update :
    Zk_rollup.State.t ->
    zk_rollup:Zk_rollup.t ->
    ?private_ops:Zk_rollup.Operation.t list list ->
    ?exit_validities:bool list ->
    Zk_rollup.Operation.t list ->
    Zk_rollup.State.t * Zk_rollup.Update.t

  module Internal_for_tests : sig
    val true_op : Zk_rollup.Operation.t

    val false_op : Zk_rollup.Operation.t

    val pending : Zk_rollup.Operation.t list

    val private_ops : Zk_rollup.Operation.t list list

    val lazy_update_data : Zk_rollup.Update.t lazy_t
  end
end = struct
  open Protocol.Alpha_context
  module SMap = Plonk.SMap
  module Dummy = Types.P.Dummy
  module T = Types.P
  module VC = V (LibCircuit)

  let lazy_srs =
    lazy
      (let open Octez_bls12_381_polynomial.Bls12_381_polynomial in
      (Srs.generate_insecure 9 1, Srs.generate_insecure 1 1))

  let dummy_l1_dst =
    Hex.to_bytes_exn (`Hex "0002298c03ed7d454a101eb7022bc95f7e5f41ac78")

  let dummy_rollup_id =
    let address =
      Zk_rollup.Address.of_b58check_exn "epx18RJJqrYuJQqhB636BWvukU3XBNQGbtm8C"
    in
    Data_encoding.Binary.to_bytes_exn Zk_rollup.Address.encoding address

  let dummy_ticket_hash = Bytes.make 32 '0'

  let of_proto_state : Zk_rollup.State.t -> Types.P.state =
   fun s -> Bls12_381.Fr.is_one s.(0)

  let to_proto_state : Types.P.state -> Zk_rollup.State.t =
   fun s -> if s then [|Bls12_381.Fr.one|] else [|Bls12_381.Fr.zero|]

  let dummy_op = T.{header = Dummy.header; payload = false}

  let batch_name = "batch-" ^ string_of_int Params.batch_size

  (* Circuits that define the rollup, alongside their public input size and
     solver *)
  let circuit_map =
    let get_circuit _name c =
      let r = LibCircuit.get_cs ~optimize:true c in
      (Plonk.Circuit.to_plonk r, r.public_input_size, r.solver)
    in
    SMap.of_list
    @@ List.map
         (fun (n, c) -> (n, get_circuit n c))
         [
           ( "op",
             VC.predicate_op
               ~old_state:false
               ~new_state:true
               ~fee:(T.Bounded.make ~bound:Bound.bound_fee Z.zero)
               ~exit_validity:false
               ~rollup_id:Dummy.tezos_pkh
               dummy_op );
           ( batch_name,
             VC.predicate_batch
               ~old_state:false
               ~new_state:true
               ~fees:(T.Bounded.make ~bound:Bound.bound_amount Z.zero)
               ~rollup_id:Dummy.tezos_pkh
               (Stdlib.List.init Params.batch_size (Fun.const dummy_op)) );
           ( "fee",
             VC.predicate_fees
               ~old_state:false
               ~new_state:false
               ~fees:(T.Bounded.make ~bound:Bound.bound_amount Z.zero) );
         ]

  let circuits =
    SMap.(add "op" `Public @@ add batch_name `Private @@ add "fee" `Fee empty)

  let lazy_pp =
    lazy
      (let srs = Lazy.force lazy_srs in
       Plonk.Main_protocol.setup
         ~zero_knowledge:false
         (SMap.map (fun (a, b, _) -> (a, b)) circuit_map)
         ~srs)

  let insert s x m =
    match SMap.find_opt s m with
    | None -> SMap.add s [x] m
    | Some l -> SMap.add s (x :: l) m

  let craft_update :
      Zk_rollup.State.t ->
      zk_rollup:Zk_rollup.t ->
      ?private_ops:Zk_rollup.Operation.t list list ->
      ?exit_validities:bool list ->
      Zk_rollup.Operation.t list ->
      Zk_rollup.State.t * Zk_rollup.Update.t =
   fun s ~zk_rollup ?(private_ops = []) ?exit_validities pending ->
    let prover_pp, public_parameters = Lazy.force lazy_pp in
    let s = of_proto_state s in
    let rev_inputs = SMap.empty in
    let exit_validities =
      match exit_validities with
      | None -> List.map (Fun.const true) pending
      | Some l ->
          assert (List.length l = List.length pending) ;
          l
    in
    let _circ, _pi_size, op_solver = SMap.find "op" circuit_map in
    (* Process the public operations *)
    let s, rev_inputs, rev_pending_pis =
      Stdlib.List.fold_left2
        (fun (s, rev_inputs, rev_pending_pis) op exit_validity ->
          let new_state =
            if s = of_proto_state Zk_rollup.Operation.(op.payload) then not s
            else s
          in
          let fee = Bls12_381.Fr.zero in
          let pi_to_send =
            Zk_rollup.Update.
              {new_state = to_proto_state new_state; fee; exit_validity}
          in
          let exit_validity_s =
            if exit_validity then Bls12_381.Fr.one else Bls12_381.Fr.zero
          in
          let public_inputs =
            Array.concat
              [
                to_proto_state s;
                to_proto_state new_state;
                [|fee; exit_validity_s; Zk_rollup.to_scalar zk_rollup|];
                Zk_rollup.Operation.to_scalar_array op;
              ]
          in
          let private_inputs = Solver.solve op_solver public_inputs in
          ( new_state,
            insert
              "op"
              Plonk.Main_protocol.
                {witness = private_inputs; input_commitments = []}
              rev_inputs,
            ("op", pi_to_send) :: rev_pending_pis ))
        (s, rev_inputs, [])
        pending
        exit_validities
    in
    let pending_pis = List.rev rev_pending_pis in

    let _circ, _pi_size, batch_solver = SMap.find batch_name circuit_map in
    (* Process the private operation batches *)
    let s, rev_inputs, rev_private_pis =
      if private_ops = [] then (s, rev_inputs, [])
      else
        List.fold_left
          (fun (s, rev_inputs, rev_private_pis) batch ->
            let new_state =
              List.fold_left
                (fun s op ->
                  if s = of_proto_state Zk_rollup.Operation.(op.payload) then
                    not s
                  else s)
                s
                batch
            in
            let fees = Bls12_381.Fr.zero in
            let pi_to_send : Zk_rollup.Update.private_inner_pi =
              Zk_rollup.Update.{new_state = to_proto_state new_state; fees}
            in
            let public_inputs =
              Array.concat
                [
                  to_proto_state s;
                  to_proto_state new_state;
                  [|fees; Zk_rollup.to_scalar zk_rollup|];
                ]
            in
            let initial =
              Array.concat
                ([public_inputs]
                @ List.map Zk_rollup.Operation.to_scalar_array batch)
            in
            let private_inputs = Solver.solve batch_solver initial in
            ( new_state,
              insert
                batch_name
                Plonk.Main_protocol.
                  {witness = private_inputs; input_commitments = []}
                rev_inputs,
              (batch_name, pi_to_send) :: rev_private_pis ))
          (s, rev_inputs, [])
          private_ops
    in
    let private_pis = List.rev rev_private_pis in
    (* Dummy fee circuit *)
    let _circ, _pi_size, fee_solver = SMap.find "fee" circuit_map in
    let rev_inputs, fee_pi =
      let fee_pi = Zk_rollup.Update.{new_state = to_proto_state s} in
      let fees = Bls12_381.Fr.zero in

      let public_inputs =
        Array.concat [to_proto_state s; to_proto_state s; [|fees|]]
      in
      let private_inputs = Solver.solve fee_solver public_inputs in
      ( insert
          "fee"
          Plonk.Main_protocol.{witness = private_inputs; input_commitments = []}
          rev_inputs,
        fee_pi )
    in
    let inputs = SMap.map List.rev rev_inputs in
    let proof = Plonk.Main_protocol.prove prover_pp ~inputs in
    let verifier_inputs =
      Plonk.Main_protocol.to_verifier_inputs prover_pp inputs
    in
    assert (
      Plonk.Main_protocol.verify public_parameters ~inputs:verifier_inputs proof) ;
    ( to_proto_state s,
      Zk_rollup.Update.{pending_pis; private_pis; fee_pi; proof} )

  let init_state = to_proto_state false

  module Internal_for_tests = struct
    let true_op =
      Zk_rollup.Operation.
        {
          op_code = 0;
          price =
            (let id =
               Data_encoding.Binary.of_bytes_exn
                 Ticket_hash.encoding
                 dummy_ticket_hash
             in
             {id; amount = Z.zero});
          l1_dst =
            Data_encoding.Binary.of_bytes_exn
              Signature.Public_key_hash.encoding
              dummy_l1_dst;
          rollup_id =
            Data_encoding.Binary.of_bytes_exn
              Zk_rollup.Address.encoding
              dummy_rollup_id;
          payload = [|Bls12_381.Fr.one|];
        }

    let false_op = {true_op with payload = [|Bls12_381.Fr.zero|]}

    let pending = [false_op; true_op; true_op]

    let n_batches = 10

    let private_ops =
      Stdlib.List.init n_batches @@ Fun.const
      @@ Stdlib.List.init Params.batch_size (fun i ->
             if i mod 2 = 0 then false_op else true_op)

    let lazy_update_data =
      lazy
        (snd
        @@ craft_update
             init_state
             ~zk_rollup:
               (Data_encoding.Binary.of_bytes_exn
                  Zk_rollup.Address.encoding
                  dummy_rollup_id)
             ~private_ops
             pending)
  end
end
