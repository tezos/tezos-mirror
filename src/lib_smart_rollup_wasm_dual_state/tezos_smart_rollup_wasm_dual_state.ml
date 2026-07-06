(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025-2026 Nomadic Labs <contact@nomadic-labs.com> *)
(*                                                                           *)
(*****************************************************************************)

module type NDS_BACKEND = sig
  module Proof : Octez_riscv_nds_common.Intf.PROOF

  val empty_registry_hash : bytes

  type prove_session

  val open_prove_session :
    Octez_riscv_nds_common.Nds.t -> prove_session * Octez_riscv_nds_common.Nds.t

  val produce_proof : prove_session -> Proof.t

  val open_verify_session : Proof.t -> Octez_riscv_nds_common.Nds.t

  val make_empty_nds : unit -> Octez_riscv_nds_common.Nds.t
end

module Make
    (Irmin : sig
      include Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF

      val proof_compact_encoding : proof Data_encoding.Compact.t

      val find_value : state -> string list -> bytes option Lwt.t

      val set_value : state -> string list -> bytes -> state Lwt.t
    end)
    (Backend : NDS_BACKEND) =
struct
  module Dual_state = struct
    type nds_state = Inactive | Active of Octez_riscv_nds_common.Nds.t

    type state = Irmin.state * nds_state

    type context = Irmin.context

    type dual_proof = {irmin_proof : Irmin.proof; nds_proof : Backend.Proof.t}

    type proof = Irmin_only of Irmin.proof | Dual of dual_proof

    let dual_proof_encoding =
      let open Data_encoding in
      (* [conv_with_guard]'s decoder returns [(_, string) result], so
         a malformed NDS proof surfaces as a clean decoder error
         instead of an exception thrown out of the deserialisation
         pipeline.

         Both fields carry an explicit [dynamic_size] (Uint30) length
         prefix so off-chain tooling inspecting a [submit_proof] L1
         operation can delineate the Irmin and NDS halves without parsing
         the Irmin proof grammar. This only affects the [Dual] wire form
         (emitted post-activation); [Irmin_only] is unframed and stays
         byte-identical to a bare [Irmin.proof_encoding]. *)
      conv_with_guard
        (fun {irmin_proof; nds_proof} ->
          (irmin_proof, Backend.Proof.serialise nds_proof))
        (fun (irmin_proof, nds_bytes) ->
          match Backend.Proof.deserialise nds_bytes with
          | Ok nds_proof -> Ok {irmin_proof; nds_proof}
          | Error _ ->
              Error
                "Dual_state: malformed NDS proof in composite proof_encoding")
        (obj2
           (req "irmin" (dynamic_size Irmin.proof_encoding))
           (req "nds" (dynamic_size bytes)))

    let proof_encoding =
      let open Data_encoding in
      (* A [Compact.union] joining the tag space of the bare Irmin proof
         compact.  A compact union writes [case_index << inner_tag_bits
         lor inner_tag] as its tag byte, so [Irmin_only] (case 0) keeps
         the bare proof's tag values — and hence its bytes — verbatim,
         and [Dual] claims a tag byte no bare proof uses.  Byte-identity
         keeps pre-activation proofs interchangeable with a legacy
         node's (refutations, output proofs); the disjoint tag makes a
         legacy decoder reject [Dual] proofs outright.  Unlike a
         [Variable]-classified scheme, the union is [`Dynamic] like the
         bare encoding, so it survives embedding in [obj]s with
         following fields (the protocol's [output_proof_encoding] in
         [Sc_rollup_wasm.Make_pvm]). *)
      Compact.(
        make ~tag_size:`Uint8
        @@ union
             ~union_tag_bits:1
             ~cases_tag_bits:2
             [
               case
                 ~title:"Irmin_only"
                 Irmin.proof_compact_encoding
                 (function Irmin_only p -> Some p | Dual _ -> None)
                 (fun p -> Irmin_only p);
               case
                 ~title:"Dual"
                 (payload dual_proof_encoding)
                 (function Dual d -> Some d | Irmin_only _ -> None)
                 (fun d -> Dual d);
             ])

    let nds_hash_path = ["pvm"; "nds_hash"]

    let read_nds_hash_marker irmin_state =
      Irmin.find_value irmin_state nds_hash_path

    let write_nds_hash_marker irmin_state hash =
      Irmin.set_value irmin_state nds_hash_path hash

    let empty_state () = (Irmin.empty_state (), Inactive)

    let state_hash (irmin, _) = Irmin.state_hash irmin

    let proof_start_state = function
      | Irmin_only p -> Irmin.proof_start_state p
      | Dual {irmin_proof; _} -> Irmin.proof_start_state irmin_proof

    let proof_stop_state = function
      | Irmin_only p -> Irmin.proof_stop_state p
      | Dual {irmin_proof; _} -> Irmin.proof_stop_state irmin_proof

    let cast_read_only = function
      | Irmin_only p -> Irmin_only (Irmin.cast_read_only p)
      | Dual d -> Dual {d with irmin_proof = Irmin.cast_read_only d.irmin_proof}

    module Encoding_runner = struct
      (** Rebuild [pvm_storage] from [durable], the NDS-hash [marker], and
          the [nds_state] tag.  Rejects two disagreements: presence (marker
          present iff [Active]) and value (marker bytes must equal
          {!Octez_riscv_nds_common.Nds.registry_hash} of the [Active]
          handle).  The value check is a no-op on a coherently-built pair;
          it guards on-disk, where marker (Irmin repo) and handle (separate
          NDS store) come from two stores that can drift apart. *)
      let reconstruct_pvm_storage_of_marker durable marker nds_state =
        match (marker, nds_state) with
        | None, Inactive ->
            (* Pre-activation state: no marker, tag agrees. *)
            Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.Durable_only
              {durable}
        | Some marker, Active nds ->
            (* Post-activation state: marker present and the tag carries a
               live [Nds.t] handle.  Reattach it only if the handle's
               registry hash matches the marker — otherwise the marker and
               the handle were sourced from stores that have drifted. *)
            if Bytes.equal marker (Octez_riscv_nds_common.Nds.registry_hash nds)
            then
              Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.Dual {durable; nds}
            else
              invalid_arg
                "Dual_state.Encoding_runner.reconstruct_pvm_storage_of_marker: \
                 the nds_hash marker does not match the registry hash of the \
                 live Nds.t handle — the marker and the handle were sourced \
                 from stores that have drifted apart"
        | None, Active _ ->
            invalid_arg
              "Dual_state.Encoding_runner.reconstruct_pvm_storage_of_marker: \
               nds_state is Active but the state carries no nds_hash marker — \
               the producer broke the marker / tag invariant"
        | Some _, Inactive ->
            invalid_arg
              "Dual_state.Encoding_runner.reconstruct_pvm_storage_of_marker: \
               the state carries an nds_hash marker but nds_state is Inactive \
               — no Nds.t handle available to reattach"

      (* Stamp the active registry's hash into the state via
         {!write_nds_hash_marker}, after the base encoder has written the
         durable and PVM metadata.  Pre-activation states write no
         marker, so their state hash stays Irmin-only. *)
      let stamp_marker irmin_state = function
        | Inactive -> Lwt.return (irmin_state, Inactive)
        | Active nds ->
            let open Lwt.Syntax in
            let+ irmin_state =
              write_nds_hash_marker
                irmin_state
                (Octez_riscv_nds_common.Nds.registry_hash nds)
            in
            (irmin_state, Active nds)

      (* Derive the output [nds_state] from the encoded [storage]: a
         coherent pair passes through unchanged; the activation tick
         ([Dual] storage with an [Inactive] tag) flips to [Active] so the
         next decode reattaches the handle; [Durable_only] storage with
         an [Active] tag is a producer error — deactivation is not part
         of the kernel contract. *)
      let nds_state_of_storage storage nds_state =
        match (storage, nds_state) with
        | ( Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.Durable_only _,
            Inactive )
        | Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.Dual _, Active _
        (* storage and nds_state are coherent. *) ->
            nds_state
        | ( Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.Durable_only _,
            Active _ ) ->
            invalid_arg
              "Dual_state.encode: pvm storage is Durable_only but nds_state is \
               Active — deactivation is not part of the kernel contract"
        | Dual {nds; _}, Inactive -> Active nds

      let encode pvm_state (irmin_state, nds_state) =
        let open Lwt.Syntax in
        let nds_state' =
          nds_state_of_storage
            Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.(pvm_state.storage)
            nds_state
        in
        let* irmin_state = Irmin.Encoding_runner.encode pvm_state irmin_state in
        stamp_marker irmin_state nds_state'

      let decode (irmin_state, nds_state) =
        let open Lwt.Syntax in
        let* pvm_state = Irmin.Encoding_runner.decode irmin_state in
        let durable =
          Tezos_scoru_wasm.Wasm_pvm_state.Internal_state.durable_of
            pvm_state.storage
        in
        let+ marker = read_nds_hash_marker irmin_state in
        {
          pvm_state with
          storage = reconstruct_pvm_storage_of_marker durable marker nds_state;
        }

      let decode_durable_storage (irmin_state, _) =
        Irmin.Encoding_runner.decode_durable_storage irmin_state

      let encode_storage storage (irmin_state, nds_state) =
        let open Lwt.Syntax in
        let nds_state' = nds_state_of_storage storage nds_state in
        let* irmin' =
          Irmin.Encoding_runner.encode_storage storage irmin_state
        in
        stamp_marker irmin' nds_state'

      let decode_storage (irmin_state, nds_state) =
        let open Lwt.Syntax in
        let* durable =
          Irmin.Encoding_runner.decode_durable_storage irmin_state
        in
        let+ marker = read_nds_hash_marker irmin_state in
        reconstruct_pvm_storage_of_marker durable marker nds_state

      let decode_buffers (irmin_state, _) =
        Irmin.Encoding_runner.decode_buffers irmin_state
    end

    (** Soundness gate shared by {!produce_dual_proof} and
        {!verify_dual_proof}: the NDS proof's start/stop hashes must
        equal the [/pvm/nds_hash] marker before/after the step. *)
    let nds_proof_endpoints_match ~expected_pre ~expected_post nds_proof =
      Bytes.equal (Backend.Proof.start_state nds_proof) expected_pre
      && Bytes.equal (Backend.Proof.stop_state nds_proof) expected_post

    module Internal_for_tests = struct
      let insert_failure (irmin_state, nds_state) =
        let open Lwt.Syntax in
        let+ irmin' = Irmin.Internal_for_tests.insert_failure irmin_state in
        (irmin', nds_state)

      let make_dual_proof ~irmin_proof ~nds_proof =
        Dual {irmin_proof; nds_proof}

      let write_nds_hash = write_nds_hash_marker

      let read_nds_hash = read_nds_hash_marker
    end

    (** Produce the proof for a post-activation step ([Active]
        pre-state): open a [Prove]-mode session over the handle
        ({!NDS_BACKEND.open_prove_session}), replay [step] against the
        [Prove]-mode handle so its NDS operations are recorded, pair the
        minted NDS proof with the Irmin proof, and emit [Dual] only if the
        proof's endpoints match the durable marker
        ({!nds_proof_endpoints_match}); otherwise [None]. *)
    let produce_dual_proof context (irmin_state, nds) step =
      let open Lwt.Syntax in
      let prove_session, prove_nds = Backend.open_prove_session nds in
      let* pre_nds_hash = read_nds_hash_marker irmin_state in
      let irmin_step irmin_state =
        (* Step's returned [nds_state] is discarded: deactivation is
           forbidden by the kernel contract and rejected one layer
           up in {!Encoding_runner.encode}, so a step reaching here
           can only return [Active _]. *)
        let* (irmin', nds'), a = step (irmin_state, Active prove_nds) in
        let+ post_nds_hash = read_nds_hash_marker irmin' in
        (irmin', (nds', post_nds_hash, a))
      in
      let* irmin_step_res =
        Irmin.produce_proof context irmin_state irmin_step
      in
      match irmin_step_res with
      | None -> Lwt.return_none
      | Some (irmin_proof, (nds', post_nds_hash, a)) -> (
          (* Pre-state's [nds_state] is [Active], so the encoder must
             have written [/pvm/nds_hash] both before and after the
             step.  A missing hash on either side is a broken encoder
             invariant — refuse to emit rather than paper over it. *)
          match (nds', pre_nds_hash, post_nds_hash) with
          | Inactive, _, _ | _, None, _ | _, _, None -> Lwt.return_none
          | Active _, Some pre, Some post ->
              let nds_proof = Backend.produce_proof prove_session in
              if
                nds_proof_endpoints_match
                  ~expected_pre:pre
                  ~expected_post:post
                  nds_proof
              then Lwt.return_some (Dual {irmin_proof; nds_proof}, a)
              else Lwt.return_none)

    (** Produce the proof for an [Inactive] pre-state.  Both legal
        transitions emit [Irmin_only] — [Inactive -> Inactive] and the
        host-function-free activation tick [Inactive -> Active(fresh)],
        whose only NDS effect is the [empty_registry_hash] marker the
        Irmin proof already attests to.  The per-arm cases below reject
        the inconsistent tag/marker combinations. *)
    let produce_irmin_only_proof context irmin_state step =
      let open Lwt.Syntax in
      (* Thread the post-step [nds_state] and [/pvm/nds_hash]
         through ['a] so the dispatch below can branch on them
         without ref cells. *)
      let irmin_step irmin_state =
        let* (irmin', nds'), a = step (irmin_state, Inactive) in
        let+ post_nds_hash = read_nds_hash_marker irmin' in
        (irmin', (nds', post_nds_hash, a))
      in
      let* irmin_step_res =
        Irmin.produce_proof context irmin_state irmin_step
      in
      match irmin_step_res with
      | None -> Lwt.return_none
      | Some (irmin, (Inactive, None, a)) ->
          (* Stayed pre-activation: Irmin proof alone is canonical. *)
          Lwt.return_some (Irmin_only irmin, a)
      | Some (_, (Inactive, Some _, _)) ->
          (* [Inactive] tag with a present [/pvm/nds_hash] is a
             broken encoder invariant — reject. *)
          Lwt.return_none
      | Some (_, (Active _, None, _)) ->
          (* [Active] post-tag without [/pvm/nds_hash] in the
             durable is a broken encoder invariant — reject. *)
          Lwt.return_none
      | Some (irmin, (Active _, Some post_nds_hash, a)) ->
          (* Activation tick: the kernel performs no host-function
             calls during [Padding -> Snapshot], so the post-NDS
             must be a fresh empty registry — its hash equals the
             canonical [empty_registry_hash].  A mismatch means the
             kernel mutated the registry during the activation tick
             (or the encoder wrote a stale hash) — refuse to emit. *)
          if Bytes.equal post_nds_hash Backend.empty_registry_hash then
            Lwt.return_some (Irmin_only irmin, a)
          else Lwt.return_none

    (** Proof-production entry point ({!STATE_PROOF.produce_proof}):
        dispatch on the pre-state's tag — [Active] to
        {!produce_dual_proof}, [Inactive] to {!produce_irmin_only_proof}. *)
    let produce_proof context (irmin_state, nds_state) step =
      match nds_state with
      | Active nds -> produce_dual_proof context (irmin_state, nds) step
      | Inactive -> produce_irmin_only_proof context irmin_state step

    (** Verify a [Dual] proof, mirroring {!produce_dual_proof}: open a
        [Verify]-mode session against the NDS proof
        ({!NDS_BACKEND.open_verify_session}), replay [step] against the
        [Verify]-mode handle, verify the Irmin half, then run the
        {!nds_proof_endpoints_match} cross-check.  Operation-level
        mismatches surface as {!Nds_errors.Verification_failed}, caught at
        the {!Nds.with_verification} boundary. *)
    let verify_dual_proof {irmin_proof; nds_proof} step =
      let open Lwt.Syntax in
      let verify_nds = Backend.open_verify_session nds_proof in
      (* Snapshot the [/pvm/nds_hash] marker before and after the
         kernel's step and thread the readings through ['a] so the
         cross-check below sees them. *)
      let irmin_step verify_nds irmin_state =
        let* pre = read_nds_hash_marker irmin_state in
        (* Step's returned [nds_state] is discarded: deactivation is
           forbidden by the execution, so a step reaching here can only
           return [Active _]. *)
        let* (irmin', _), a = step (irmin_state, Active verify_nds) in
        let+ post = read_nds_hash_marker irmin' in
        (irmin', (pre, post, a))
      in
      let* result =
        Octez_riscv_nds_common.Nds.with_verification verify_nds
        @@ fun verify_nds ->
        Irmin.verify_proof irmin_proof (irmin_step verify_nds)
      in
      match result with
      | Error _ | Ok None -> Lwt.return_none
      | Ok (Some (irmin', (pre_nds_hash, post_nds_hash, a))) -> (
          (* Same cross-check as {!produce_dual_proof}: the proof
             asserts NDS-active before and after, so the durable must
             carry [/pvm/nds_hash] on both ends, and the proof's
             start/stop must match those readings. *)
          match (pre_nds_hash, post_nds_hash) with
          | None, _ | _, None -> Lwt.return_none
          | Some pre, Some post ->
              if
                nds_proof_endpoints_match
                  ~expected_pre:pre
                  ~expected_post:post
                  nds_proof
              then Lwt.return_some ((irmin', Active verify_nds), a)
              else Lwt.return_none)

    (** Verify an [Irmin_only] proof: replay [step] from [Inactive] and
        verify the Irmin half.  The [/pvm/nds_hash] marker
        (committed by the Irmin proof) is the soundness gate that pins
        down the post-state's NDS handle; the per-arm cases below accept
        the two canonical transitions and reject the rest. *)
    let verify_irmin_only_proof irmin_p step =
      let open Lwt.Syntax in
      (* Read [/pvm/nds_hash] inside [irmin_step] so the verifier
         traces the same path as the prover's
         [produce_irmin_only_proof]. *)
      let irmin_step irmin_state =
        let* (irmin', nds_state'), v = step (irmin_state, Inactive) in
        let+ post_nds_hash = read_nds_hash_marker irmin' in
        (irmin', (nds_state', post_nds_hash, v))
      in
      let* result = Irmin.verify_proof irmin_p irmin_step in
      match result with
      | None -> Lwt.return_none
      | Some (irmin', (Inactive, None, v)) ->
          (* Pre-activation tick that stayed pre-activation: step's
             tag and the durable agree on [Inactive].  Accept. *)
          Lwt.return_some ((irmin', Inactive), v)
      | Some (irmin', ((Active _ as nds_state'), Some post_nds_hash, v))
        when Bytes.equal post_nds_hash Backend.empty_registry_hash ->
          (* Activation tick: step minted a fresh [Active] handle via
             the verifier-configured factory and the encoder wrote
             the canonical empty-registry hash to [/pvm/nds_hash].
             Reuse the step's handle. *)
          Lwt.return_some ((irmin', nds_state'), v)
      | Some (_, (Inactive, Some _, _)) ->
          (* Step says [Inactive] but the durable carries
             [/pvm/nds_hash] — the encoder wrote a marker without a
             handle to back it.  Broken invariant. *)
          Lwt.return_none
      | Some (_, (Active _, None, _)) ->
          (* Step says [Active] but the tree carries no marker —
             a step cannot unilaterally promote to [Active] without
             the encoder committing the cryptographic marker. *)
          Lwt.return_none
      | Some (_, (Active _, Some _, _)) ->
          (* Step's tag and marker agree on [Active] but the marker
             is not the canonical [empty_registry_hash] — the kernel
             mutated the registry within this tick (impossible for a
             single activation tick) or the proof is non-canonical
             and a [Dual] proof would be required. *)
          Lwt.return_none

    (** Proof-verification entry point, mirror of {!produce_proof}:
        dispatch on the proof variant — [Irmin_only] to
        {!verify_irmin_only_proof}, [Dual] to {!verify_dual_proof}. *)
    let verify_proof p step =
      match p with
      | Irmin_only irmin_p -> verify_irmin_only_proof irmin_p step
      | Dual dual -> verify_dual_proof dual step
  end

  let wasm_pvm_machine_dual ~config =
    let module Vm = Tezos_scoru_wasm.Wasm_vm.Make_vm (struct
      let config = config

      let make_empty_nds = Some Backend.make_empty_nds
    end) in
    (module Tezos_scoru_wasm.Wasm_pvm.Make_pvm (Vm) (Dual_state)
    : Tezos_scoru_wasm.Wasm_pvm_sig.S
      with type context = Dual_state.context
       and type state = Dual_state.state
       and type proof = Dual_state.proof)
end
