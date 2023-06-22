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

type error += WASM_proof_verification_failed

type error += WASM_proof_production_failed

type error += WASM_output_proof_production_failed

type error += WASM_invalid_claim_about_outbox

type error += WASM_invalid_dissection_distribution

let () =
  let open Data_encoding in
  let msg = "Invalid claim about outbox" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_wasm_invalid_claim_about_outbox"
    ~title:msg
    ~pp:(fun fmt () -> Format.pp_print_string fmt msg)
    ~description:msg
    unit
    (function WASM_invalid_claim_about_outbox -> Some () | _ -> None)
    (fun () -> WASM_invalid_claim_about_outbox) ;
  let msg = "Output proof production failed" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_wasm_output_proof_production_failed"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function WASM_output_proof_production_failed -> Some () | _ -> None)
    (fun () -> WASM_output_proof_production_failed) ;
  let msg = "Proof production failed" in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_wasm_proof_production_failed"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function WASM_proof_production_failed -> Some () | _ -> None)
    (fun () -> WASM_proof_production_failed) ;
  let msg =
    "Invalid dissection distribution: not all ticks are a multiplier of the \
     maximum number of ticks of a snapshot"
  in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_wasm_invalid_dissection_distribution"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function WASM_invalid_dissection_distribution -> Some () | _ -> None)
    (fun () -> WASM_invalid_dissection_distribution)

module V2_0_0 = struct
  let current_version = Wasm_2_0_0.v2

  let ticks_per_snapshot = Z.of_int64 11_000_000_000L

  let outbox_validity_period = Int32.of_int 80_640

  let outbox_message_limit = Z.of_int 100

  let well_known_reveal_preimage =
    Sc_rollup_reveal_hash.well_known_reveal_preimage

  let well_known_reveal_hash = Sc_rollup_reveal_hash.well_known_reveal_hash

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
      "srs11qkRe5cbDBixB2fuumn4tfkvQcxUSuFXa94Lv5c6kdzzfpM9UF"

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

    val parse_boot_sector : string -> string option

    val pp_boot_sector : Format.formatter -> string -> unit

    (** [get_tick state] gets the total tick counter for the given PVM state. *)
    val get_tick : state -> Sc_rollup_tick_repr.t Lwt.t

    (** PVM status *)
    type status =
      | Computing
      | Waiting_for_input_message
      | Waiting_for_reveal of Sc_rollup_PVM_sig.reveal

    (** [get_status ~is_reveal_enabled state] gives you the current execution status for the PVM. *)
    val get_status :
      is_reveal_enabled:Sc_rollup_PVM_sig.is_reveal_enabled ->
      state ->
      status Lwt.t

    val get_outbox :
      Raw_level_repr.t -> state -> Sc_rollup_PVM_sig.output list Lwt.t
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

    let parse_boot_sector s = Hex.to_string @@ `Hex s

    let pp_boot_sector fmt s = Format.fprintf fmt "%s" s

    type tree = Tree.tree

    type status =
      | Computing
      | Waiting_for_input_message
      | Waiting_for_reveal of Sc_rollup_PVM_sig.reveal

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

    let initial_state ~empty = WASM_machine.initial_state current_version empty

    let install_boot_sector state boot_sector =
      WASM_machine.install_boot_sector
        ~ticks_per_snapshot
        ~outbox_validity_period
        ~outbox_message_limit
        boot_sector
        state

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

    let get_status ~is_reveal_enabled =
      let open Monad.Syntax in
      let open Sc_rollup_PVM_sig in
      let* s = get in
      let* info = lift (WASM_machine.get_info s) in
      let* last_read = get_last_message_read in
      (* We do not put the machine in a stuck condition if a kind of reveal
         happens to not be supported. This is a sensible thing to do, as if
         there is an off-by-one error in the WASM kernel one can do an
         incorrect reveal, which can put the PVM in a stuck state with no way
         to upgrade the kernel to fix the off-by-one. *)
      let try_return_reveal candidate =
        match last_read with
        | Some (current_level, _) ->
            let is_enabled = is_reveal_enabled current_level candidate in
            if is_enabled then Waiting_for_reveal candidate
            else Waiting_for_reveal (Reveal_raw_data well_known_reveal_hash)
        | None -> Waiting_for_reveal (Reveal_raw_data well_known_reveal_hash)
      in
      return
      @@
      match info.input_request with
      | No_input_required -> Computing
      | Input_required -> Waiting_for_input_message
      | Reveal_required (Wasm_2_0_0.Reveal_raw_data hash) -> (
          match
            Data_encoding.Binary.of_string_opt
              Sc_rollup_reveal_hash.encoding
              hash
          with
          | Some hash -> try_return_reveal (Reveal_raw_data hash)
          | None ->
              (* We do not put the machine in a stuck condition if a kind of reveal
                 happens to not be supported. Insteadn we wait for the well known
                 preimage. *)
              Waiting_for_reveal (Reveal_raw_data well_known_reveal_hash))
      | Reveal_required Wasm_2_0_0.Reveal_metadata ->
          try_return_reveal Reveal_metadata

    let is_input_state ~is_reveal_enabled =
      let open Monad.Syntax in
      let* status = get_status ~is_reveal_enabled in
      match status with
      | Waiting_for_input_message -> (
          let* last_read = get_last_message_read in
          match last_read with
          | Some (level, n) -> return (PS.First_after (level, n))
          | None -> return PS.Initial)
      | Computing -> return PS.No_input_required
      | Waiting_for_reveal reveal -> return (PS.Needs_reveal reveal)

    let is_input_state ~is_reveal_enabled =
      result_of (is_input_state ~is_reveal_enabled)

    let get_status ~is_reveal_enabled : state -> status Lwt.t =
      result_of (get_status ~is_reveal_enabled)

    let get_outbox outbox_level state =
      let outbox_level_int32 =
        Raw_level_repr.to_int32_non_negative outbox_level
      in
      let open Lwt_syntax in
      let rec aux outbox message_index =
        let output =
          Wasm_2_0_0.{outbox_level = outbox_level_int32; message_index}
        in
        let* res = WASM_machine.get_output output state in
        match res with
        | None -> return (List.rev outbox)
        | Some msg -> (
            let serialized =
              Sc_rollup_outbox_message_repr.unsafe_of_string msg
            in
            match Sc_rollup_outbox_message_repr.deserialize serialized with
            | Error _ ->
                (* The [write_output] host function does not guarantee that the contents
                   of the returned output is a valid encoding of an outbox message.
                   We choose to ignore such messages. An alternative choice would be to
                   craft an output with a payload witnessing the illformedness of the
                   output produced by the kernel. *)
                (aux [@ocaml.tailcall]) outbox (Z.succ message_index)
            | Ok message ->
                let output = PS.{outbox_level; message_index; message} in
                (aux [@ocaml.tailcall])
                  (output :: outbox)
                  (Z.succ message_index))
      in
      aux [] Z.zero

    let set_input_state input =
      let open Monad.Syntax in
      match input with
      | PS.Inbox_message input ->
          let open PS in
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
      | PS.Reveal (PS.Raw_data data) ->
          let* s = get in
          let* s = lift (WASM_machine.reveal_step (Bytes.of_string data) s) in
          set s
      | PS.Reveal (PS.Metadata metadata) ->
          let metadata_bytes =
            Data_encoding.Binary.to_bytes_exn
              Sc_rollup_metadata_repr.encoding
              metadata
          in
          let* s = get in
          let* s = lift (WASM_machine.reveal_step metadata_bytes s) in
          set s
      | PS.Reveal (PS.Dal_page _content_opt) ->
          (* FIXME/DAL: https://gitlab.com/tezos/tezos/-/issues/3927.
             Handle DAL pages in wasm PVM. *)
          assert false

    let set_input input = state_of @@ set_input_state input

    let eval_step =
      let open Monad.Syntax in
      let* s = get in
      let* s = lift (WASM_machine.compute_step s) in
      set s

    let eval state = state_of eval_step state

    let step_transition ~is_reveal_enabled input_given state =
      let open Lwt_syntax in
      let* request = is_input_state ~is_reveal_enabled state in
      let* state =
        match request with
        | PS.No_input_required -> eval state
        | _ -> (
            match input_given with
            | Some input -> set_input input state
            | None -> return state)
      in
      return (state, request)

    let verify_proof ~is_reveal_enabled input_given proof =
      let open Lwt_result_syntax in
      let*! result =
        Context.verify_proof
          proof
          (step_transition ~is_reveal_enabled input_given)
      in
      match result with
      | None -> tzfail WASM_proof_verification_failed
      | Some (_state, request) -> return request

    let produce_proof context ~is_reveal_enabled input_given state =
      let open Lwt_result_syntax in
      let*! result =
        Context.produce_proof
          context
          state
          (step_transition ~is_reveal_enabled input_given)
      in
      match result with
      | Some (tree_proof, _requested) -> return tree_proof
      | None -> tzfail WASM_proof_production_failed

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
      | {outbox_level; message_index; message} -> (
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
          return
          @@
          match result with
          | Some result -> Compare.String.(result = message_encoded)
          | None -> false)

    let verify_output_proof p =
      let open Lwt_syntax in
      let transition = run @@ has_output p.output_proof_output in
      let* result = Context.verify_proof p.output_proof transition in
      match result with None -> return false | Some _ -> return true

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
      | Some (_, false) -> fail WASM_invalid_claim_about_outbox
      | None -> fail WASM_output_proof_production_failed

    let check_sections_number ~default_number_of_sections ~number_of_sections
        ~dist =
      let open Sc_rollup_dissection_chunk_repr in
      let is_stop_chunk_aligned =
        Compare.Z.(Z.rem dist ticks_per_snapshot = Z.zero)
      in
      let max_number_of_sections = Z.(div dist ticks_per_snapshot) in
      let expected =
        Compare.Z.min
          (Z.of_int default_number_of_sections)
          (if is_stop_chunk_aligned then max_number_of_sections
          else Z.succ max_number_of_sections)
      in
      let given = Z.of_int number_of_sections in
      error_unless
        Compare.Z.(given = expected)
        (Dissection_number_of_sections_mismatch {given; expected})

    let check_dissection ~default_number_of_sections ~start_chunk ~stop_chunk
        dissection =
      let open Result_syntax in
      let open Sc_rollup_dissection_chunk_repr in
      let dist =
        Sc_rollup_tick_repr.distance start_chunk.tick stop_chunk.tick
      in
      (*
        We fall back to the default dissection check when the
        [kernel_run] culprit has been found and is being dissected.

        This condition will also be met if the PVM is stuck (because
        it is unlikely that [ticks_per_snapshot] messages can be
        posted in a commitment period), which is OKay because the Fast
        Execution cannot be leveraged in that case, which means the
        ad-hoc dissection predicate would not provide any speed up.
      *)
      if Compare.Z.(dist <= ticks_per_snapshot) then
        default_check
          ~section_maximum_size:Z.(div dist (Z.of_int 2))
          ~check_sections_number:default_check_sections_number
          ~default_number_of_sections
          ~start_chunk
          ~stop_chunk
          dissection
      else
        (*
           There are enough ticks to consider that at least one call
           to [kernel_run] is involved.

           We now need to consider two cases: either [stop_chunk] is a
           multiple of [ticks_per_snapshot] (the PVM is not stuck), or
           it is not (the PVM has been stuck during the processing
           of one of the ticks of the dissection).

           For the latter case, we want to validate a dissection if

             1. Every complete [kernel_run] invocations are dissected
                as normal in the n-1 first chunks, and
             2. The final section contains all the ticks of the
                interrupted [kernel_run].
        *)
        let is_stop_chunk_aligned =
          Compare.Z.(Z.rem dist ticks_per_snapshot = Z.zero)
        in
        (*
           We keep the same dissection predicate as the default
           dissection that a given section cannot be more than half of
           the “full distance”, but we only consider the complete
           calls to [kernel_run] in the “full distance”. The remainder
           ticks will be put in the very last section.
        *)
        let considered_dist =
          if is_stop_chunk_aligned then dist
          else
            let last_valid_stop_tick =
              Sc_rollup_tick_repr.of_z
                Z.(
                  mul
                    (div
                       (Sc_rollup_tick_repr.to_z stop_chunk.tick)
                       ticks_per_snapshot)
                    ticks_per_snapshot)
            in
            Sc_rollup_tick_repr.(distance start_chunk.tick last_valid_stop_tick)
        in
        (*
           There is one last corner case to consider: if the stuck
           state happens in the second [kernel_run] of the period.

           In this case, the considered distance is equal to the
           snapshot size, and divided this value by two means the
           maximum size of a section becomes 0.

           So we keep that a section length is at least
           [ticks_per_snapshot].
        *)
        let section_maximum_size =
          Z.max ticks_per_snapshot (Z.div considered_dist (Z.of_int 2))
        in
        let* () =
          default_check
            ~section_maximum_size
            ~check_sections_number
            ~default_number_of_sections
            ~start_chunk
            ~stop_chunk
            dissection
        in
        error_unless
          (List.for_all
             (fun chunk ->
               let open Sc_rollup_tick_repr in
               Z.(
                 equal (rem (to_z chunk.tick) ticks_per_snapshot) zero
                 || Sc_rollup_tick_repr.equal start_chunk.tick chunk.tick
                 || Sc_rollup_tick_repr.equal stop_chunk.tick chunk.tick))
             dissection)
          WASM_invalid_dissection_distribution

    let get_current_level state =
      let open Lwt_syntax in
      let+ res = result_of get_last_message_read state in
      Option.map fst res

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
          let open Lwt_option_syntax in
          let*? () =
            Result.to_option (Context_binary_proof.check_is_binary p)
          in
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

        let proof_encoding = Context.Proof_encoding.V2.Tree2.tree_proof_encoding
      end)
end
