(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module V2_0_0 = struct
  (*
    This is the state hash of reference that both the prover of the
    node and the verifier of the protocol {!Protocol_implementation}
    have to agree on (if they do, it means they are using the same
    tree structure).

    We have to hard-code this value because the Wasm PVM uses Irmin as
    its Merkle proof verification backend, and the economic protocol
    cannot create an empty Irmin context. Such a context is required to
    create an empty tree, itself required to create the initial state of
    the Wasm PVM.

    Utlimately, the value of this constant is decided by the prover of
    reference (the only need is for it to be compatible with
    {!Protocol_implementation}.)

    Its value is the result of the following snippet

    {|
    let*! state = Prover.initial_state context in
    Prover.state_hash state
    |}
  *)
  let reference_initial_state_hash =
    Sc_rollup_repr.State_hash.of_b58check_exn
      "scs11pDQTn37TBnWgQAiCPdMAcQPiXARjg9ZZVmLx26sZwxeSxovE5"

  open Sc_rollup_repr
  module PS = Sc_rollup_PVM_sig

  module type TreeS =
    Context.TREE with type key = string list and type value = bytes

  module type Make_wasm = module type of Wasm_2_0_0.Make

  module type P = sig
    module Tree : TreeS

    type tree = Tree.tree

    type proof

    val proof_encoding : proof Data_encoding.t

    val proof_before : proof -> State_hash.t

    val proof_after : proof -> State_hash.t

    val verify_proof :
      proof -> (tree -> (tree * 'a) Lwt.t) -> (tree * 'a) option Lwt.t

    val produce_proof :
      Tree.t -> tree -> (tree -> (tree * 'a) Lwt.t) -> (proof * 'a) option Lwt.t
  end

  module type S = sig
    include Sc_rollup_PVM_sig.S

    val name : string

    val parse_boot_sector : string -> string option

    val pp_boot_sector : Format.formatter -> string -> unit

    (** [get_tick state] gets the total tick counter for the given PVM state. *)
    val get_tick : state -> Sc_rollup_tick_repr.t Lwt.t

    (** PVM status *)
    type status = Computing | Waiting_for_input_message

    (** [get_status state] gives you the current execution status for the PVM. *)
    val get_status : state -> status Lwt.t

    val get_outbox : state -> Sc_rollup_PVM_sig.output list Lwt.t
  end

  (* [Make (Make_backend) (Context)] creates a PVM.

     The Make_backend is a functor that creates the backend of the PVM.
     The Conext provides the tree and the proof types.
  *)
  module Make (Make_backend : Make_wasm) (Context : P) :
    S
      with type context = Context.Tree.t
       and type state = Context.tree
       and type proof = Context.proof = struct
    module Tree = Context.Tree

    type context = Context.Tree.t

    type hash = State_hash.t

    type proof = Context.proof

    let proof_encoding = Context.proof_encoding

    let proof_start_state proof = Context.proof_before proof

    let proof_stop_state proof = Context.proof_after proof

    let name = "wasm_2_0_0"

    let parse_boot_sector s = Hex.to_string @@ `Hex s

    let pp_boot_sector fmt s = Format.fprintf fmt "%s" s

    type tree = Tree.tree

    type status = Computing | Waiting_for_input_message

    module State = struct
      type state = tree

      module Monad : sig
        type 'a t

        val run : 'a t -> state -> (state * 'a) Lwt.t

        val return : 'a -> 'a t

        module Syntax : sig
          val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
        end

        val get : tree t

        val set : tree -> unit t

        val lift : 'a Lwt.t -> 'a t
      end = struct
        type 'a t = state -> (state * 'a) Lwt.t

        let return x state = Lwt.return (state, x)

        let bind m f state =
          let open Lwt_syntax in
          let* state, res = m state in
          f res state

        module Syntax = struct
          let ( let* ) = bind
        end

        let run m state = m state

        let get s = Lwt.return (s, s)

        let set s _ = Lwt.return (s, ())

        let lift m s = Lwt.map (fun r -> (s, r)) m
      end
    end

    type state = State.state

    module WASM_machine = Make_backend (Tree)
    open State

    let pp _state =
      Lwt.return @@ fun fmt () -> Format.pp_print_string fmt "<wasm-state>"

    open Monad

    let initial_state ctxt =
      let open Lwt_syntax in
      let state = Tree.empty ctxt in
      let* state = Tree.add state ["wasm-version"] (Bytes.of_string "2.0.0") in
      Lwt.return state

    let install_boot_sector state boot_sector =
      Tree.add
        state
        ["boot-sector"]
        Data_encoding.(Binary.to_bytes_exn string boot_sector)

    let state_hash state =
      let context_hash = Tree.hash state in
      Lwt.return @@ State_hash.context_hash_to_state_hash context_hash

    let result_of m state =
      let open Lwt_syntax in
      let* _, v = run m state in
      return v

    let state_of m state =
      let open Lwt_syntax in
      let* s, _ = run m state in
      return s

    let get_tick : Sc_rollup_tick_repr.t Monad.t =
      let open Monad.Syntax in
      let* s = get in
      let* info = lift (WASM_machine.get_info s) in
      return @@ Sc_rollup_tick_repr.of_z info.current_tick

    let get_tick : state -> Sc_rollup_tick_repr.t Lwt.t = result_of get_tick

    let get_status : status Monad.t =
      let open Monad.Syntax in
      let* s = get in
      let* info = lift (WASM_machine.get_info s) in
      return
      @@
      match info.input_request with
      | No_input_required -> Computing
      | Input_required -> Waiting_for_input_message

    let get_last_message_read : _ Monad.t =
      let open Monad.Syntax in
      let* s = get in
      let* info = lift (WASM_machine.get_info s) in
      return
      @@
      match info.last_input_read with
      | Some {inbox_level; message_counter} ->
          let inbox_level = Raw_level_repr.of_int32_non_negative inbox_level in
          Some (inbox_level, message_counter)
      | _ -> None

    let is_input_state =
      let open Monad.Syntax in
      let* status = get_status in
      match status with
      | Waiting_for_input_message -> (
          let* last_read = get_last_message_read in
          match last_read with
          | Some (level, n) -> return (PS.First_after (level, n))
          | None -> return PS.Initial)
      | Computing -> return PS.No_input_required

    let is_input_state = result_of is_input_state

    let get_status : state -> status Lwt.t = result_of get_status

    let get_outbox _state =
      (* FIXME: https://gitlab.com/tezos/tezos/-/issues/3790 *)
      let open Lwt_syntax in
      return []

    let set_input_state input =
      match input with
      | PS.Inbox_message input ->
          let open PS in
          let open Monad.Syntax in
          let {inbox_level; message_counter; payload} = input in
          let* s = get in
          let* s =
            lift
              (WASM_machine.set_input_step
                 {
                   inbox_level = Raw_level_repr.to_int32_non_negative inbox_level;
                   message_counter;
                 }
                 (payload :> string)
                 s)
          in
          set s
      | PS.Reveal _ ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/3754

             The WASM PVM does not produce [Needs_reveal] input
             requests.  Thus, no [set_input_state] should transmit a
             [Reveal_revelation].
          *)
          assert false

    let set_input input = state_of @@ set_input_state input

    let eval_step =
      let open Monad.Syntax in
      let* s = get in
      let* s = lift (WASM_machine.compute_step s) in
      set s

    let eval state = state_of eval_step state

    let step_transition input_given state =
      let open Lwt_syntax in
      let* request = is_input_state state in
      let* state =
        match request with
        | PS.No_input_required -> eval state
        | _ -> (
            match input_given with
            | Some input -> set_input input state
            | None -> return state)
      in
      return (state, request)

    type error += WASM_proof_verification_failed

    let verify_proof input_given proof =
      let open Lwt_tzresult_syntax in
      let*! result = Context.verify_proof proof (step_transition input_given) in
      match result with
      | None -> fail WASM_proof_verification_failed
      | Some (_state, request) -> return request

    type error += WASM_proof_production_failed

    let produce_proof context input_given state =
      let open Lwt_tzresult_syntax in
      let*! result =
        Context.produce_proof context state (step_transition input_given)
      in
      match result with
      | Some (tree_proof, _requested) -> return tree_proof
      | None -> fail WASM_proof_production_failed

    let verify_origination_proof proof boot_sector =
      let open Lwt_syntax in
      let before = Context.proof_before proof in
      if State_hash.(before <> reference_initial_state_hash) then return false
      else
        let* result =
          Context.verify_proof proof (fun state ->
              let* state = install_boot_sector state boot_sector in
              return (state, ()))
        in
        match result with None -> return false | Some (_, ()) -> return true

    let produce_origination_proof context boot_sector =
      let open Lwt_tzresult_syntax in
      let*! state = initial_state context in
      let*! result =
        Context.produce_proof context state (fun state ->
            let open Lwt_syntax in
            let* state = install_boot_sector state boot_sector in
            return (state, ()))
      in
      match result with
      | Some (tree_proof, ()) -> return tree_proof
      | None -> fail WASM_proof_production_failed

    type output_proof = {
      output_proof : Context.proof;
      output_proof_state : hash;
      output_proof_output : PS.output;
    }

    let output_proof_encoding =
      let open Data_encoding in
      conv
        (fun {output_proof; output_proof_state; output_proof_output} ->
          (output_proof, output_proof_state, output_proof_output))
        (fun (output_proof, output_proof_state, output_proof_output) ->
          {output_proof; output_proof_state; output_proof_output})
        (obj3
           (req "output_proof" Context.proof_encoding)
           (req "output_proof_state" State_hash.encoding)
           (req "output_proof_output" PS.output_encoding))

    let output_of_output_proof s = s.output_proof_output

    let state_of_output_proof s = s.output_proof_state

    let has_output : PS.output -> bool Monad.t = function
      | {outbox_level; message_index; message} ->
          let open Monad.Syntax in
          let* s = get in
          let* result =
            lift
              (WASM_machine.get_output
                 {
                   outbox_level =
                     Raw_level_repr.to_int32_non_negative outbox_level;
                   message_index;
                 }
                 s)
          in
          let message_encoded =
            Data_encoding.Binary.to_string_exn
              Sc_rollup_outbox_message_repr.encoding
              message
          in
          return @@ Compare.String.(result = message_encoded)

    let verify_output_proof p =
      let open Lwt_syntax in
      let transition = run @@ has_output p.output_proof_output in
      let* result = Context.verify_proof p.output_proof transition in
      match result with None -> return false | Some _ -> return true

    type error += Wasm_output_proof_production_failed

    type error += Wasm_invalid_claim_about_outbox

    let produce_output_proof context state output_proof_output =
      let open Lwt_result_syntax in
      let*! output_proof_state = state_hash state in
      let*! result =
        Context.produce_proof context state
        @@ run
        @@ has_output output_proof_output
      in
      match result with
      | Some (output_proof, true) ->
          return {output_proof; output_proof_state; output_proof_output}
      | Some (_, false) -> fail Wasm_invalid_claim_about_outbox
      | None -> fail Wasm_output_proof_production_failed

    module Internal_for_tests = struct
      let insert_failure state =
        let add n = Tree.add state ["failures"; string_of_int n] Bytes.empty in
        let open Lwt_syntax in
        let* n = Tree.length state ["failures"] in
        add n
    end
  end

  module Protocol_implementation =
    Make
      (Wasm_2_0_0.Make)
      (struct
        module Tree = struct
          include Context.Tree

          type tree = Context.tree

          type t = Context.t

          type key = string list

          type value = bytes
        end

        type tree = Context.tree

        type proof = Context.Proof.tree Context.Proof.t

        let verify_proof p f =
          Lwt.map Result.to_option (Context.verify_tree_proof p f)

        let produce_proof _context _state _f =
          (* Can't produce proof without full context*)
          Lwt.return None

        let kinded_hash_to_state_hash = function
          | `Value hash | `Node hash ->
              State_hash.context_hash_to_state_hash hash

        let proof_before proof =
          kinded_hash_to_state_hash proof.Context.Proof.before

        let proof_after proof =
          kinded_hash_to_state_hash proof.Context.Proof.after

        let proof_encoding =
          Context.Proof_encoding.V2.Tree32.tree_proof_encoding
      end)
end
