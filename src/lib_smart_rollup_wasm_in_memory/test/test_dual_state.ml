(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Lib_smart_rollup_wasm_in_memory dual state
    Invocation:   dune exec src/lib_smart_rollup_wasm_in_memory/test/main.exe \
                    -- --file test_dual_state.ml
    Subject:      Tests for Dual_state and wasm_pvm_machine_dual: the
                  [/pvm/nds_hash] hash schema, encode/decode round-trips,
                  and produce_proof / verify_proof over the [Irmin_only]
                  and [Dual] proof variants. *)

open Tezos_scoru_wasm
open Tztest

(** Dual-state test suite, generic over the Irmin durable [Irmin] and
    the NDS backend [Backend].  [Setup] supplies the few
    instantiation-specific helpers; the in-memory binding is at the
    bottom of this file, so a disk variant can reuse the whole suite by
    supplying its own [Setup]. *)
module Make_tests
    (Irmin : sig
      include Tezos_scoru_wasm.Wasm_pvm_sig.STATE_PROOF

      val proof_compact_encoding : proof Data_encoding.Compact.t

      val find_value : state -> string list -> bytes option Lwt.t

      val set_value : state -> string list -> bytes -> state Lwt.t
    end)
    (Backend : Tezos_smart_rollup_wasm_dual_state.NDS_BACKEND)
    (Setup : sig
      (** A fresh empty proof context for [Irmin]. *)
      val make_empty_context : unit -> Irmin.context

      (** The Irmin-only (tree-only) PVM machine, for the empty-state
          equivalence check. *)
      val tree_only_machine :
        config:Wasm_pvm_config.t -> (module Wasm_pvm_sig.S)

      (** A fresh empty [Normal]-mode NDS handle — the canonical handle
          the activation boundary mints. *)
      val make_empty_nds : unit -> Octez_riscv_nds_common.Nds.t
    end) =
struct
  module Dual_lib = Tezos_smart_rollup_wasm_dual_state.Make (Irmin) (Backend)
  module Dual = Dual_lib.Dual_state

  let wasm_pvm_machine_dual ~config =
    Dual_lib.wasm_pvm_machine_dual ~config ~make_empty_nds:Setup.make_empty_nds

  open Setup

  (* -------------------------------------------------------------------- *)
  (* Helpers                                                              *)
  (* -------------------------------------------------------------------- *)

  (** A {!Check} typ for smart-rollup state hashes, so hash (in)equality
      assertions report a readable [%L]/[%R] diff. *)
  let state_hash_typ =
    Check.equalable
      Tezos_crypto.Hashed.Smart_rollup_state_hash.pp
      Tezos_crypto.Hashed.Smart_rollup_state_hash.equal

  (** A {!Check} typ for raw [bytes], compared with {!Bytes.equal} and
      rendered as hex in [%L]/[%R] diffs since [Check] has no native
      [bytes] typ. *)
  let bytes_typ =
    Check.equalable (fun fmt b -> Hex.pp fmt (Hex.of_bytes b)) Bytes.equal

  (** Shorthand {!Dual.state} constructor, keeping call sites concise
      now that the dual state is a record. *)
  let dual irmin nds : Dual.state = {irmin; nds}

  (** Persist a bare [durable] into the state.  [encode_storage] on a
      [Durable_only] variant is the exact equivalent — both encode the
      same [durable_storage_encoding]. *)
  let encode_durable_storage durable state =
    Irmin.Encoding_runner.encode_storage
      (Wasm_pvm_state.Internal_state.Durable_only {durable})
      state

  (** Stamp [nds]'s registry hash as the NDS marker on [irmin] (at the
      kernel-invisible [/pvm/nds_hash] path), as {!Dual.Encoding_runner}
      does for a [Dual] storage. *)
  let stamp_nds_marker irmin nds =
    Dual.Internal_for_tests.write_nds_hash
      irmin
      (Octez_riscv_nds_common.Nds.registry_hash nds)

  (** Assert that the [/pvm/nds_hash] marker stamped on [irmin] is
      present and equals the live registry hash of [nds] — the invariant
      the encoder maintains so the Irmin tree's hash commits to the NDS
      state.  Hashes are compared as hex strings (and the marker as an
      [option]) for a readable [%L]/[%R] diff, so an absent marker fails
      with [None] rather than a confusing default. *)
  let check_nds_hash_marker irmin nds =
    let open Lwt_syntax in
    let+ marker_hash = Dual.Internal_for_tests.read_nds_hash irmin in
    let registry_hash = Octez_riscv_nds_common.Nds.registry_hash nds in
    Check.(
      (marker_hash = Some registry_hash)
        (option bytes_typ)
        ~__LOC__
        ~error_msg:"/pvm/nds_hash marker = %L, but live NDS registry hash = %R")

  (* -------------------------------------------------------------------- *)
  (* Unit tests: hash schema                                              *)
  (* -------------------------------------------------------------------- *)

  (** Pre-activation [state_hash] equals {!Irmin.state_hash}: with the
      NDS gate closed, the dual hash delegates to the Irmin tree. *)
  let test_inactive_hash_is_irmin_only () =
    let open Lwt_result_syntax in
    let irmin = Irmin.empty_state () in
    let*! irmin_hash = Irmin.state_hash irmin in
    let*! dual_hash = Dual.state_hash (dual irmin Dual.Inactive) in
    Check.(
      (irmin_hash = dual_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"pre-activation dual hash should equal Irmin.state_hash %R") ;
    return_unit

  (** Activation changes [state_hash] on the same tree: the
      [/pvm/nds_hash] marker makes the tree's hash commit to the NDS
      state. *)
  let test_activated_hash_differs_from_inactive () =
    let open Lwt_result_syntax in
    let irmin_state_inactive = Irmin.empty_state () in
    let*! inactive_hash =
      Dual.state_hash (dual irmin_state_inactive Dual.Inactive)
    in
    let nds = make_empty_nds () in
    let*! irmin_state_active = stamp_nds_marker irmin_state_inactive nds in
    let*! activated_hash =
      Dual.state_hash (dual irmin_state_active (Dual.Active nds))
    in
    Check.(
      (inactive_hash <> activated_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"activation should change the state hash (both = %L)") ;
    return_unit

  (* -------------------------------------------------------------------- *)
  (* E2E test: encode/decode round-trip through the durable path          *)
  (* -------------------------------------------------------------------- *)

  (** End-to-end encode/decode, mirroring {!Wasm_vm.maybe_activate_nds}
      at the [Padding -> Snapshot] boundary: encoding [Dual] storage
      writes the handle's hash to [/pvm/nds_hash] (so the state hash
      changes), and decoding reads it back and reattaches the handle to
      reproduce [Dual] storage. *)
  let test_e2e_encode_writes_path_and_decode_restores_dual () =
    let open Lwt_result_syntax in
    let irmin = Irmin.empty_state () in
    let state = dual irmin Dual.Inactive in
    let*! durable = Irmin.Encoding_runner.decode_durable_storage irmin in
    let dual_storage =
      Wasm_pvm_state.Internal_state.Dual {durable; nds = make_empty_nds ()}
    in
    let*! state' =
      Dual.Encoding_runner.encode_storage
        dual_storage
        (dual irmin (Dual.Active (make_empty_nds ())))
    in
    (* The encoded state's hash now reflects the NDS hash via the
     [/pvm/nds_hash] path. *)
    let*! before_hash = Dual.state_hash state in
    let*! after_hash = Dual.state_hash state' in
    Check.(
      (before_hash <> after_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:
          "encoding Dual storage should change the state hash (both = %L)") ;
    (* The decoder reads the path back and constructs [Dual] storage. *)
    let*! storage = Dual.Encoding_runner.decode_storage state' in
    (match storage with
    | Wasm_pvm_state.Internal_state.Dual _ -> ()
    | Wasm_pvm_state.Internal_state.Durable_only _ ->
        Test.fail ~__LOC__ "Expected Dual storage after re-decode") ;
    return_unit

  (** A fresh dual machine starts pre-activation: its [empty_state] is
      {!Irmin.empty_state}, so it hashes identically to the tree-only
      machine's. *)
  let test_dual_machine_empty_state_is_inactive () =
    let open Lwt_result_syntax in
    let (module M_dual) = wasm_pvm_machine_dual ~config:Wasm_pvm_config.empty in
    let (module M_tree) = tree_only_machine ~config:Wasm_pvm_config.empty in
    let dual_state = M_dual.empty_state () in
    let irmin_only_state = M_tree.empty_state () in
    let*! dual_hash = M_dual.state_hash dual_state in
    let*! tree_hash = M_tree.state_hash irmin_only_state in
    Check.(
      (dual_hash = tree_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:
          "fresh dual machine should hash like the tree-only machine %R") ;
    return_unit

  (* -------------------------------------------------------------------- *)
  (* Property test: hash schema invariants                                *)
  (* -------------------------------------------------------------------- *)

  let fresh_nds_of_size n =
    let nds = make_empty_nds () in
    let rec aux i nds =
      if Compare.Int64.(i > n) then nds
      else
        match Octez_riscv_nds_common.Nds.resize nds i with
        | Ok () -> aux (Int64.succ i) nds
        | Error _ -> Test.fail ~__LOC__ "failed to resize"
    in
    aux 0L nds

  (** Across a range of NDS registry sizes, each activated [Dual] hash
      is distinct from the pre-activation hash and from every other —
      the accumulator seeds with [Inactive] so both kinds of collision
      share one rejection path. *)
  let test_property_activation_changes_hash () =
    let open Lwt_result_syntax in
    let inactive = Irmin.empty_state () in
    let*! inactive_hash = Dual.state_hash (dual inactive Dual.Inactive) in
    let sizes = [0L; 1L; 2L; 3L; 5L; 8L] in
    let*! _seen =
      Lwt_list.fold_left_s
        (fun seen n ->
          let nds = fresh_nds_of_size n in
          let open Lwt.Syntax in
          let* durable =
            Irmin.Encoding_runner.decode_durable_storage inactive
          in
          let dual_storage =
            Wasm_pvm_state.Internal_state.Dual {durable; nds}
          in
          let* activated =
            Dual.Encoding_runner.encode_storage
              dual_storage
              (dual inactive (Dual.Active nds))
          in
          let+ activated_hash = Dual.state_hash activated in
          Check.list_not_mem
            state_hash_typ
            ~__LOC__
            activated_hash
            (Tezos_crypto.Hashed.Smart_rollup_state_hash.Set.elements seen)
            ~error_msg:"activated hash %L collided with a previously seen hash" ;
          Tezos_crypto.Hashed.Smart_rollup_state_hash.Set.add
            activated_hash
            seen)
        (Tezos_crypto.Hashed.Smart_rollup_state_hash.Set.singleton
           inactive_hash)
        sizes
    in
    return_unit

  (* -------------------------------------------------------------------- *)
  (* Proof tests: produce_proof / verify_proof                             *)
  (* -------------------------------------------------------------------- *)

  (** A non-empty Irmin tree (one seed value) so [produce_proof]'s
      Context commit doesn't trip irmin-pack's empty-node check. *)
  let make_seeded_inactive_state () =
    let open Lwt.Syntax in
    let irmin = Irmin.empty_state () in
    let* durable = Irmin.Encoding_runner.decode_durable_storage irmin in
    let* durable =
      Durable.set_value_exn durable (Durable.key_of_string_exn "/test/seed") "1"
    in
    encode_durable_storage durable irmin

  (** Pre-activation no-op: a real {!Dual.Encoding_runner}
      decode/encode round-trip over [Inactive] (the no-kernel analog of
      a [compute_step]) writes no [/pvm/nds_hash], so [produce_proof]
      emits [Irmin_only] and [verify_proof] accepts it. *)
  let test_proof_irmin_only_noop_roundtrip () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let state = dual irmin Dual.Inactive in
    let step (st : Dual.state) =
      let open Lwt.Syntax in
      let* storage = Dual.Encoding_runner.decode_storage st in
      let* st' = Dual.Encoding_runner.encode_storage storage st in
      Lwt.return (st', ())
    in
    let*! proof_opt = Dual.produce_proof context state step in
    match proof_opt with
    | None ->
        Test.fail
          ~__LOC__
          "produce_proof returned None for honest Irmin_only no-op"
    | Some (Dual.Dual _, ()) ->
        Test.fail
          ~__LOC__
          "produce_proof emitted Dual variant for a pre-activation no-op step; \
           expected Irmin_only"
    | Some ((Dual.Irmin_only _ as proof), ()) -> (
        let*! verify_opt = Dual.verify_proof proof step in
        match verify_opt with
        | None ->
            Test.fail
              ~__LOC__
              "verify_proof rejected honest Irmin_only no-op proof"
        | Some _ -> return_unit)

  (** Wire format: an [Irmin_only] proof must encode byte-identically to
    the bare [Irmin.proof_encoding] — until NDS activates, a node emits
    only [Irmin_only] proofs and their bytes must match a legacy node's,
    or the two diverge in a refutation game — and decode back to
    [Irmin_only], not mis-decode as [Dual]. *)
  let test_proof_encoding_irmin_only_untagged () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let state = dual irmin Dual.Inactive in
    let step (st : Dual.state) =
      let open Lwt.Syntax in
      let* storage = Dual.Encoding_runner.decode_storage st in
      let* st' = Dual.Encoding_runner.encode_storage storage st in
      Lwt.return (st', ())
    in
    let*! proof_opt = Dual.produce_proof context state step in
    match proof_opt with
    | None ->
        Test.fail
          ~__LOC__
          "produce_proof returned None for honest Irmin_only no-op"
    | Some (Dual.Dual _, ()) ->
        Test.fail
          ~__LOC__
          "produce_proof emitted Dual variant for a pre-activation no-op step"
    | Some ((Dual.Irmin_only irmin_proof as proof), ()) -> (
        let dual_machine_bytes =
          Data_encoding.Binary.to_bytes_exn Dual.proof_encoding proof
        in
        let bare_irmin_bytes =
          Data_encoding.Binary.to_bytes_exn Irmin.proof_encoding irmin_proof
        in
        Check.(
          (dual_machine_bytes = bare_irmin_bytes)
            bytes_typ
            ~__LOC__
            ~error_msg:
              "Irmin_only proof encoding is not byte-identical to the bare \
               Irmin proof encoding — a pre-activation proof would diverge \
               from a legacy node's (%L <> %R)") ;
        match
          Data_encoding.Binary.of_bytes_opt
            Dual.proof_encoding
            dual_machine_bytes
        with
        | Some (Dual.Irmin_only _) -> return_unit
        | Some (Dual.Dual _) ->
            Test.fail
              ~__LOC__
              "Irmin_only proof bytes mis-decoded as Dual — the tag spaces are \
               not disjoint"
        | None ->
            Test.fail ~__LOC__ "Irmin_only proof bytes failed to decode back")

  (** Produce a real [Irmin_only] proof (no-op step on a seeded
      pre-activation state) with its underlying Irmin proof. *)
  let make_irmin_only_proof () =
    let open Lwt_syntax in
    let context = make_empty_context () in
    let* irmin = make_seeded_inactive_state () in
    let no_op_step state = Lwt.return (state, ()) in
    let* proof_opt =
      Dual.produce_proof context (dual irmin Dual.Inactive) no_op_step
    in
    match proof_opt with
    | Some ((Dual.Irmin_only irmin_proof as proof), ()) ->
        return (proof, irmin_proof)
    | Some (Dual.Dual _, ()) ->
        Test.fail ~__LOC__ "expected an Irmin_only proof from Inactive"
    | None -> Test.fail ~__LOC__ "produce_proof returned None"

  (** Build a real [Dual] proof from a no-op Irmin proof and an NDS
      proof minted from an empty prove session. *)
  let make_dual_proof_for_encoding () =
    let open Lwt_syntax in
    let+ _proof, irmin_proof = make_irmin_only_proof () in
    let session, _prove_nds = Backend.open_prove_session (make_empty_nds ()) in
    let nds_proof = Backend.produce_proof session in
    Dual.Internal_for_tests.make_dual_proof ~irmin_proof ~nds_proof

  (** [proof_encoding] must classify [`Dynamic]: the frozen protocol's
      [output_proof_encoding] places the proof as a {e non-final} [obj2]
      field, and data-encoding raises at encoding construction — i.e. at
      module initialisation of any binary linking a protocol machine over
      the dual state — if the field is [`Variable]. *)
  let test_proof_encoding_classifies_dynamic () =
    let open Lwt_result_syntax in
    (match Data_encoding.classify Dual.proof_encoding with
    | `Dynamic -> ()
    | `Fixed _ ->
        Test.fail ~__LOC__ "proof_encoding classifies `Fixed; expected `Dynamic"
    | `Variable ->
        Test.fail
          ~__LOC__
          "proof_encoding classifies `Variable — composing it in the \
           protocol's output_proof_encoding would raise at module \
           initialisation") ;
    return_unit

  (** Simulate the frozen protocol's [output_proof_encoding] composition
      ([obj2] with the proof {e first}): construction must not raise, and
      a composed value must roundtrip — the proof field has to
      self-delimit mid-object. *)
  let test_proof_encoding_composes_in_obj2 () =
    let open Lwt_result_syntax in
    let composed =
      let open Data_encoding in
      obj2
        (req "output_proof" Dual.proof_encoding)
        (req "output_proof_output" (dynamic_size Variable.bytes))
    in
    let*! proof, _irmin_proof = make_irmin_only_proof () in
    let output = Bytes.of_string "output-payload" in
    let bytes = Data_encoding.Binary.to_bytes_exn composed (proof, output) in
    let proof', output' = Data_encoding.Binary.of_bytes_exn composed bytes in
    Check.(
      (output' = output)
        bytes_typ
        ~__LOC__
        ~error_msg:
          "second obj2 field corrupted by the proof field: %L, expected %R — \
           the proof encoding does not self-delimit mid-object") ;
    Check.(
      (Data_encoding.Binary.to_bytes_exn Dual.proof_encoding proof'
      = Data_encoding.Binary.to_bytes_exn Dual.proof_encoding proof)
        bytes_typ
        ~__LOC__
        ~error_msg:"proof field corrupted by obj2 composition: %L <> %R") ;
    return_unit

  (** [Dual] proofs occupy the compact tag byte 4 — outside the legacy
      tag space [{0..3}].  Pins the union case layout: if the compact
      union ever reallocates its tag bits, this catches the wire-format
      move that a "legacy rejects Dual" check alone would miss. *)
  let test_dual_proof_tag_byte () =
    let open Lwt_result_syntax in
    let*! dual_proof = make_dual_proof_for_encoding () in
    let bytes =
      Data_encoding.Binary.to_bytes_exn Dual.proof_encoding dual_proof
    in
    Check.(
      (Bytes.get bytes 0 = '\x04')
        char
        ~__LOC__
        ~error_msg:
          "Dual proof tag byte is %L, expected %R — the compact union case \
           layout moved; legacy/dual wire disjointness relies on it") ;
    return_unit

  (** [Irmin_only] JSON is unchanged: the compact union's JSON side is
      wrapper-less with the legacy case first, so schema-sensitive tooling
      keeps seeing the flat legacy proof object. *)
  let test_irmin_only_json_unchanged () =
    let open Lwt_result_syntax in
    let*! proof, irmin_proof = make_irmin_only_proof () in
    let dual_json = Data_encoding.Json.construct Dual.proof_encoding proof in
    let legacy_json =
      Data_encoding.Json.construct Irmin.proof_encoding irmin_proof
    in
    if not (dual_json = legacy_json) then
      Test.fail
        ~__LOC__
        "Irmin_only JSON differs from the legacy proof JSON:\n%s\nvs\n%s"
        (Data_encoding.Json.to_string dual_json)
        (Data_encoding.Json.to_string legacy_json) ;
    return_unit

  (** Active no-op: a canonical [Active nds] state and a real
      decode/encode round-trip — the Irmin half attests the
      [/pvm/nds_hash] write, the NDS half an empty transition.
      [produce_proof] emits a composite [Dual] (live since TZX-113).
  *)
  let test_proof_dual_noop_roundtrip () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    (* Start from a size-1 registry (one database) rather than an empty
       one, so the NDS proof carries real registry-tree content for the
       verifier's hash fold. *)
    let nds = fresh_nds_of_size 1L in
    (* Write an entry into the database so the registry tree carries a
       real value node. *)
    let*? () =
      Octez_riscv_nds_common.Nds.set
        nds
        ~db_index:0L
        ~key:(Bytes.of_string "key")
        ~value:(Bytes.of_string "value")
      |> Result.map_error (fun e ->
             [Octez_riscv_nds_common.Nds_errors.Invalid_argument e])
    in
    let*! activated_irmin = stamp_nds_marker (Irmin.empty_state ()) nds in
    (* The stamped marker must agree with the live handle's registry
       hash before we feed the state to [produce_proof]. *)
    let*! () = check_nds_hash_marker activated_irmin nds in
    let state = dual activated_irmin (Dual.Active nds) in
    (* Real storage decode/encode round-trip: [decode_storage]
     reattaches the (prove-mode) NDS handle from the [Active] tag and
     [encode_storage] writes its registry hash back to
     [/pvm/nds_hash], exactly as the production [compute_step]
     pipeline does each tick. *)
    let step (st : Dual.state) =
      (* Read the entry back through the (prove/verify-mode) NDS handle so
         the database index is recorded as accessed during the proof. *)
      let*! value =
        match st with
        | {Dual.nds = Dual.Active nds; _} -> (
            match
              Octez_riscv_nds_common.Nds.read
                nds
                ~db_index:0L
                ~key:(Bytes.of_string "key")
                ~offset:0L
                ~len:5L
            with
            | Ok value -> Lwt.return value
            | _ -> Test.fail ~__LOC__ "missing value in nds")
        | {Dual.nds = Dual.Inactive; _} -> Test.fail ~__LOC__ "Nds is inactive"
      in
      Check.(
        (value = Bytes.of_string "value")
          bytes_typ
          ~__LOC__
          ~error_msg:"nds read returned %L, expected %R") ;
      let*! storage = Dual.Encoding_runner.decode_storage st in
      match storage with
      | Durable_only _ -> Test.fail ~__LOC__ "Nds is inactive"
      | Dual _ ->
          let*! st' = Dual.Encoding_runner.encode_storage storage st in
          Lwt.return (st', ())
    in
    let*! proof_opt = Dual.produce_proof context state step in
    match proof_opt with
    | None ->
        Test.fail
          ~__LOC__
          "produce_proof returned None for honest Active no-op state"
    | Some ((Dual.Dual _ as p), ()) -> (
        (* A [Dual] proof claims a tag byte no bare Irmin proof uses: a
           legacy (single-state) decoder must reject it outright rather
           than mis-parse it. *)
        let wire = Data_encoding.Binary.to_bytes_exn Dual.proof_encoding p in
        (match Data_encoding.Binary.of_bytes_opt Irmin.proof_encoding wire with
        | None -> ()
        | Some _ ->
            Test.fail
              ~__LOC__
              "a Dual proof's bytes decoded as a bare Irmin proof — the tag \
               spaces are not disjoint") ;
        let*! verify_opt = Dual.verify_proof p step in
        match verify_opt with
        | None ->
            Test.fail ~__LOC__ "verify_proof rejected honest Dual no-op proof"
        | Some _ -> return_unit)
    | Some (Dual.Irmin_only _, ()) ->
        Test.fail
          ~__LOC__
          "produce_proof on Active state emitted Irmin_only; expected Dual"

  (** [produce_proof] rejects an [Active] state whose [Active nds]
      handle disagrees with the [/pvm/nds_hash] marker — here a
      fabricated non-empty marker paired with a fresh empty handle. *)
  let test_proof_dual_rejects_inconsistent_state () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    (* A fabricated 32-byte hash value that cannot equal an empty
     Normal registry's hash.  Pinned to a specific byte pattern so
     the mismatch is unambiguous and the test fails noisily if the
     cross-check is removed. *)
    let bogus_hash = Bytes.make 32 '\xff' in
    let*! activated_irmin =
      Dual.Internal_for_tests.write_nds_hash (Irmin.empty_state ()) bogus_hash
    in
    let state = dual activated_irmin (Dual.Active (make_empty_nds ())) in
    let step state = Lwt.return (state, ()) in
    let*! proof_opt = Dual.produce_proof context state step in
    match proof_opt with
    | None -> return_unit
    | Some _ ->
        Test.fail
          ~__LOC__
          "produce_proof accepted Active state whose nds disagrees with \
           durable's /pvm/nds_hash"

  (** [verify_proof] rejects a replayed step that diverges from the
      proof: a no-op proof verified against a durable-mutating step
      fails because the replayed post-tree no longer hashes to the
      proof's stop_state. *)
  let test_proof_verify_rejects_divergent_step () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let state = dual irmin Dual.Inactive in
    let no_op_step state = Lwt.return (state, ()) in
    let*! proof_opt = Dual.produce_proof context state no_op_step in
    let proof =
      match proof_opt with
      | Some (p, ()) -> p
      | None -> Test.fail ~__LOC__ "produce_proof returned None"
    in
    let mutating_step ({irmin; nds = nds_state} : Dual.state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/marker")
          "x"
      in
      let+ irmin' = encode_durable_storage durable irmin in
      (dual irmin' nds_state, ())
    in
    let*! verify_opt = Dual.verify_proof proof mutating_step in
    match verify_opt with
    | None -> return_unit
    | Some _ ->
        Test.fail
          ~__LOC__
          "verify_proof accepted a divergent step that should have made the \
           Irmin replay disagree with the proof's stop_state"

  (* -------------------------------------------------------------------- *)
  (* Cross-machine proofs: single-state (tree-only) <-> dual-state        *)
  (* -------------------------------------------------------------------- *)

  (** One durable mutation in the single-state machine's step shape; the
      dual variant below lifts it over the [irmin] half, so both replay
      to the same post-tree. *)
  let irmin_cross_step irmin_state =
    let open Lwt.Syntax in
    let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
    let* durable =
      Durable.set_value_exn
        durable
        (Durable.key_of_string_exn "/test/cross")
        "1"
    in
    let+ irmin' = encode_durable_storage durable irmin_state in
    (irmin', ())

  let dual_cross_step ({irmin = irmin_state; nds = nds_state} : Dual.state) =
    let open Lwt.Syntax in
    let+ irmin', () = irmin_cross_step irmin_state in
    (dual irmin' nds_state, ())

  (** Pre-activation interop, legacy -> dual: a single-state proof's
      bare bytes decode as-is with [Dual.proof_encoding] and pass the
      dual machine's [verify_proof] — a dual node ingesting a legacy
      node's proof. *)
  let test_cross_single_state_proof_verified_by_dual () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let*! proof_opt = Irmin.produce_proof context irmin irmin_cross_step in
    match proof_opt with
    | None ->
        Test.fail
          ~__LOC__
          "single-state produce_proof returned None for an honest step"
    | Some (irmin_proof, ()) -> (
        let bare =
          Data_encoding.Binary.to_bytes_exn Irmin.proof_encoding irmin_proof
        in
        let proof =
          match Data_encoding.Binary.of_bytes_opt Dual.proof_encoding bare with
          | Some (Dual.Irmin_only _ as p) -> p
          | Some (Dual.Dual _) ->
              Test.fail
                ~__LOC__
                "bare single-state proof bytes mis-decoded as Dual"
          | None ->
              Test.fail
                ~__LOC__
                "bare single-state proof bytes failed to decode as a dual \
                 machine proof"
        in
        let*! verify_opt = Dual.verify_proof proof dual_cross_step in
        match verify_opt with
        | Some ({Dual.nds = Dual.Inactive; _}, ()) -> return_unit
        | Some ({Dual.nds = Dual.Active _; _}, ()) ->
            Test.fail
              ~__LOC__
              "dual verify_proof promoted a pre-activation single-state proof \
               to Active"
        | None ->
            Test.fail
              ~__LOC__
              "dual verify_proof rejected an honest proof produced by the \
               single-state machine")

  (** Pre-activation interop, dual -> legacy: a dual [Irmin_only]
      proof's wire bytes decode as a bare Irmin proof and pass the
      single-state machine's [verify_proof] — a legacy node ingesting a
      dual node's proof. *)
  let test_cross_dual_proof_verified_by_single_state () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let state = dual irmin Dual.Inactive in
    let*! proof_opt = Dual.produce_proof context state dual_cross_step in
    match proof_opt with
    | None ->
        Test.fail
          ~__LOC__
          "dual produce_proof returned None for an honest pre-activation step"
    | Some (Dual.Dual _, ()) ->
        Test.fail
          ~__LOC__
          "dual produce_proof emitted Dual for a pre-activation step"
    | Some ((Dual.Irmin_only _ as proof), ()) -> (
        let wire =
          Data_encoding.Binary.to_bytes_exn Dual.proof_encoding proof
        in
        let irmin_proof =
          match Data_encoding.Binary.of_bytes_opt Irmin.proof_encoding wire with
          | Some p -> p
          | None ->
              Test.fail
                ~__LOC__
                "the dual machine's Irmin_only proof bytes failed to decode as \
                 a bare Irmin proof"
        in
        let*! verify_opt = Irmin.verify_proof irmin_proof irmin_cross_step in
        match verify_opt with
        | Some _ -> return_unit
        | None ->
            Test.fail
              ~__LOC__
              "single-state verify_proof rejected an honest Irmin_only proof \
               produced by the dual machine")

  (* -------------------------------------------------------------------- *)
  (* Plumbing tests: composite proof wiring                               *)
  (* -------------------------------------------------------------------- *)

  (** A seeded Irmin tree whose [/pvm/nds_hash] marker matches a fresh
      empty handle — a consistent non-empty [Active] starting point. *)
  let make_seeded_active_state () =
    let open Lwt.Syntax in
    let irmin = Irmin.empty_state () in
    let* durable = Irmin.Encoding_runner.decode_durable_storage irmin in
    let* durable =
      Durable.set_value_exn durable (Durable.key_of_string_exn "/test/seed") "1"
    in
    let* irmin = encode_durable_storage durable irmin in
    stamp_nds_marker irmin (make_empty_nds ())

  (** The kernel's [Padding -> Snapshot] activation step: write
      [/pvm/nds_hash] with a fresh empty NDS's hash and flip the tag to
      [Active fresh_nds] (the input [nds_state] is ignored). *)
  let activation_step ({irmin = irmin_state; _} : Dual.state) =
    let open Lwt.Syntax in
    let fresh_nds = make_empty_nds () in
    let+ irmin' = stamp_nds_marker irmin_state fresh_nds in
    (dual irmin' (Dual.Active fresh_nds), ())

  (** Activation tick produce + verify: from [Inactive], the step
      writes the canonical empty-registry hash and flips to [Active].
      [produce_proof] emits [Irmin_only] (the tree captures the whole
      transition); [verify_proof] reads the marker, sees the empty
      value, and mints its own fresh [Normal] registry. *)
  let test_proof_activation_tick_roundtrip () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let state = dual irmin Dual.Inactive in
    let*! proof_opt = Dual.produce_proof context state activation_step in
    match proof_opt with
    | None ->
        Test.fail
          ~__LOC__
          "produce_proof returned None for honest Inactive -> Active \
           activation tick"
    | Some (Dual.Dual _, ()) ->
        Test.fail
          ~__LOC__
          "produce_proof emitted Dual for an activation step; the post-NDS is \
           a fresh empty registry, so the Irmin proof alone is sufficient — \
           expected Irmin_only"
    | Some ((Dual.Irmin_only _ as proof), ()) -> (
        let*! verify_opt = Dual.verify_proof proof activation_step in
        match verify_opt with
        | None ->
            Test.fail
              ~__LOC__
              "verify_proof rejected an honest activation proof produced by \
               the same step"
        | Some ({Dual.nds = Dual.Active _; _}, ()) -> return_unit
        | Some ({Dual.nds = Dual.Inactive; _}, ()) ->
            Test.fail
              ~__LOC__
              "verify_proof returned Inactive for an activation tick — the \
               post-tree's /pvm/nds_hash should have promoted the post-state \
               to Active(fresh_nds)")

  (** [produce_proof] rejects an activation tick whose post-tree
      [/pvm/nds_hash] is non-canonical: the tick is op-free, so the only
      legal post-NDS is a fresh empty registry ([empty_registry_hash]) —
      any other marker breaks the kernel contract. *)
  let test_proof_activation_tick_rejects_non_canonical_hash () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let state = dual irmin Dual.Inactive in
    let bogus_step ({irmin = irmin_state; _} : Dual.state) =
      let open Lwt.Syntax in
      let fresh_nds = make_empty_nds () in
      let+ irmin' =
        Dual.Internal_for_tests.write_nds_hash
          irmin_state
          (Bytes.make 32 '\xff')
      in
      (dual irmin' (Dual.Active fresh_nds), ())
    in
    let*! proof_opt = Dual.produce_proof context state bogus_step in
    match proof_opt with
    | None -> return_unit
    | Some _ ->
        Test.fail
          ~__LOC__
          "produce_proof accepted an activation tick whose post-tree carries a \
           non-canonical /pvm/nds_hash"

  (** A [Dual] proof with a mismatched NDS half is rejected: splice the
      empty-NDS proof's Irmin half onto the size-1 proof's NDS half —
      the Irmin replay passes, then [nds_proof_endpoints_match] sees the
      NDS endpoints disagree with the empty durable's marker. *)
  let test_dual_proof_with_swapped_nds_half_rejected () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let nds_empty = make_empty_nds () in
    let nds_size_1 = fresh_nds_of_size 1L in
    let make_state_with_nds nds = stamp_nds_marker (Irmin.empty_state ()) nds in
    let*! irmin_empty = make_state_with_nds nds_empty in
    let*! irmin_size_1 = make_state_with_nds nds_size_1 in
    (* don't touch NDS at all during the proof *)
    let step st = Lwt.return (st, ()) in
    let*! proof_empty_opt =
      Dual.produce_proof context (dual irmin_empty (Dual.Active nds_empty)) step
    in
    let dual_empty =
      match proof_empty_opt with
      | Some (Dual.Dual d, ()) -> d
      | _ ->
          Test.fail
            ~__LOC__
            "produce_proof for empty-NDS state did not emit a Dual proof"
    in
    let*! proof_size_1_opt =
      Dual.produce_proof
        context
        (dual irmin_size_1 (Dual.Active nds_size_1))
        step
    in
    let dual_size_1 =
      match proof_size_1_opt with
      | Some (Dual.Dual d, ()) -> d
      | _ ->
          Test.fail
            ~__LOC__
            "produce_proof for size-1-NDS state did not emit a Dual proof"
    in
    let swapped =
      Dual.Internal_for_tests.make_dual_proof
        ~irmin_proof:dual_empty.irmin_proof
        ~nds_proof:dual_size_1.nds_proof
    in
    let*! verify_opt = Dual.verify_proof swapped step in
    match verify_opt with
    | None -> return_unit
    | Some _ ->
        Test.fail
          ~__LOC__
          "verify_proof accepted a Dual proof whose NDS half attests to a \
           different registry hash than the durable's /pvm/nds_hash"

  (** A [Dual] proof with a mismatched Irmin half is rejected: swap the
      Irmin halves of a no-op and a mutating proof — the swapped half
      attests a different post-tree than the step produces, so the Irmin
      replay's stop_state diverges. *)
  let test_dual_proof_with_swapped_irmin_half_rejected () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_active_state () in
    let state = dual irmin (Dual.Active (make_empty_nds ())) in
    let step_noop state = Lwt.return (state, ()) in
    let step_mutate ({irmin = irmin_state; nds = nds_state} : Dual.state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/foo")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      (dual irmin' nds_state, ())
    in
    let*! noop_opt = Dual.produce_proof context state step_noop in
    let dual_noop =
      match noop_opt with
      | Some (Dual.Dual d, ()) -> d
      | _ ->
          Test.fail
            ~__LOC__
            "produce_proof for no-op step did not emit a Dual proof"
    in
    let*! mutate_opt = Dual.produce_proof context state step_mutate in
    let dual_mutate =
      match mutate_opt with
      | Some (Dual.Dual d, ()) -> d
      | _ ->
          Test.fail
            ~__LOC__
            "produce_proof for mutating step did not emit a Dual proof"
    in
    let swapped =
      Dual.Internal_for_tests.make_dual_proof
        ~irmin_proof:dual_mutate.irmin_proof
        ~nds_proof:dual_noop.nds_proof
    in
    let*! verify_opt = Dual.verify_proof swapped step_noop in
    match verify_opt with
    | None -> return_unit
    | Some _ ->
        Test.fail
          ~__LOC__
          "verify_proof accepted a Dual proof whose Irmin half attests to a \
           different transition than the step replays"

  (* -------------------------------------------------------------------- *)
  (* Public dispatcher surface                                            *)
  (* -------------------------------------------------------------------- *)

  (** [proof_start_state] / [proof_stop_state] agree with [state_hash]
      on [Irmin_only]: a mutating step pins start to the pre-hash and
      stop to the post-hash, catching a dispatcher that confuses them. *)
  let test_proof_endpoints_match_state_hash_irmin_only () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let pre_state = dual irmin Dual.Inactive in
    let*! pre_hash = Dual.state_hash pre_state in
    (* Mutating step so start_state <> stop_state — otherwise the
     stop_state assertion would be satisfied vacuously by [pre_hash]. *)
    let step ({irmin = irmin_state; nds = nds_state} : Dual.state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/endpoints")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      (dual irmin' nds_state, ())
    in
    let*! post_state, () = step pre_state in
    let*! post_hash = Dual.state_hash post_state in
    Check.(
      (pre_hash <> post_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"mutating step should change the state hash (both = %L)") ;
    let*! proof_opt = Dual.produce_proof context pre_state step in
    let proof =
      match proof_opt with
      | Some ((Dual.Irmin_only _ as p), ()) -> p
      | _ -> Test.fail ~__LOC__ "expected Irmin_only proof"
    in
    Check.(
      (Dual.proof_start_state proof = pre_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"proof_start_state %L should equal the pre-state hash %R") ;
    Check.(
      (Dual.proof_stop_state proof = post_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"proof_stop_state %L should equal the post-state hash %R") ;
    return_unit

  (** Same endpoint/[state_hash] agreement on [Dual]: the dispatchers
      must reach into [dual_proof.irmin_proof]. *)
  let test_proof_endpoints_match_state_hash_dual () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_active_state () in
    let pre_state = dual irmin (Dual.Active (make_empty_nds ())) in
    let*! pre_hash = Dual.state_hash pre_state in
    (* Mutating step so start_state <> stop_state — otherwise the
     stop_state assertion would be satisfied vacuously by [pre_hash]. *)
    let step ({irmin = irmin_state; nds = nds_state} : Dual.state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/endpoints")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      (dual irmin' nds_state, ())
    in
    let*! post_state, () = step pre_state in
    let*! post_hash = Dual.state_hash post_state in
    Check.(
      (pre_hash <> post_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"mutating step should change the state hash (both = %L)") ;
    let*! proof_opt = Dual.produce_proof context pre_state step in
    let proof =
      match proof_opt with
      | Some ((Dual.Dual _ as p), ()) -> p
      | _ -> Test.fail ~__LOC__ "expected Dual proof"
    in
    Check.(
      (Dual.proof_start_state proof = pre_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"proof_start_state %L should equal the pre-state hash %R") ;
    Check.(
      (Dual.proof_stop_state proof = post_hash)
        state_hash_typ
        ~__LOC__
        ~error_msg:"proof_stop_state %L should equal the post-state hash %R") ;
    return_unit

  (** [cast_read_only] on [Irmin_only] preserves the variant and
      start_state, collapsing stop_state to start_state. *)
  let test_cast_read_only_irmin_only () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let pre_state = dual irmin Dual.Inactive in
    (* Mutating step so the honest proof's stop_state differs from its
     start_state — otherwise [cast_read_only]'s stop->start collapse
     would be satisfied vacuously by a no-op proof. *)
    let step ({irmin = irmin_state; nds = nds_state} : Dual.state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/cast")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      (dual irmin' nds_state, ())
    in
    let*! proof_opt = Dual.produce_proof context pre_state step in
    let proof =
      match proof_opt with
      | Some (p, ()) -> p
      | None -> Test.fail ~__LOC__ "produce_proof returned None"
    in
    (* The step moved the durable, so the honest proof has stop <> start. *)
    Check.(
      (Dual.proof_stop_state proof <> Dual.proof_start_state proof)
        state_hash_typ
        ~__LOC__
        ~error_msg:
          "honest proof should have stop_state <> start_state (both = %L)") ;
    let p_ro = Dual.cast_read_only proof in
    (match (proof, p_ro) with
    | Dual.Irmin_only _, Dual.Irmin_only _ -> ()
    | _ -> Test.fail ~__LOC__ "cast_read_only changed variant on Irmin_only") ;
    Check.(
      (Dual.proof_start_state p_ro = Dual.proof_start_state proof)
        state_hash_typ
        ~__LOC__
        ~error_msg:"cast_read_only should preserve start_state (%L <> %R)") ;
    Check.(
      (Dual.proof_stop_state p_ro = Dual.proof_start_state proof)
        state_hash_typ
        ~__LOC__
        ~error_msg:
          "cast_read_only should collapse stop_state to start_state (%L <> %R)") ;
    return_unit

  (** Same [cast_read_only] contract on [Dual]: reach into
      [dual_proof.irmin_proof] and collapse its stop to its start. *)
  let test_cast_read_only_dual () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_active_state () in
    let pre_state = dual irmin (Dual.Active (make_empty_nds ())) in
    (* Mutating step so the honest proof's stop_state differs from its
     start_state — otherwise [cast_read_only]'s stop->start collapse
     would be satisfied vacuously by a no-op proof. *)
    let step ({irmin = irmin_state; nds = nds_state} : Dual.state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/cast")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      (dual irmin' nds_state, ())
    in
    let*! proof_opt = Dual.produce_proof context pre_state step in
    let proof =
      match proof_opt with
      | Some (p, ()) -> p
      | None -> Test.fail ~__LOC__ "produce_proof returned None"
    in
    (* The step moved the durable, so the honest proof has stop <> start. *)
    Check.(
      (Dual.proof_stop_state proof <> Dual.proof_start_state proof)
        state_hash_typ
        ~__LOC__
        ~error_msg:
          "honest proof should have stop_state <> start_state (both = %L)") ;
    let p_ro = Dual.cast_read_only proof in
    (match (proof, p_ro) with
    | Dual.Dual _, Dual.Dual _ -> ()
    | _ -> Test.fail ~__LOC__ "cast_read_only changed variant on Dual") ;
    Check.(
      (Dual.proof_start_state p_ro = Dual.proof_start_state proof)
        state_hash_typ
        ~__LOC__
        ~error_msg:"cast_read_only should preserve start_state (%L <> %R)") ;
    Check.(
      (Dual.proof_stop_state p_ro = Dual.proof_start_state proof)
        state_hash_typ
        ~__LOC__
        ~error_msg:
          "cast_read_only should collapse stop_state to start_state (%L <> %R)") ;
    return_unit

  (** [verify_proof] on [Irmin_only] rejects a step whose returned
      [nds_state] disagrees with the [/pvm/nds_hash] marker: a step that
      claims [Active] without writing the marker leaves the post-tree
      bare, so the verifier sees [(Active, None)] and rejects — a step
      can't promote to [Active] without the cryptographic marker. *)
  let test_verify_irmin_only_rejects_step_tag_marker_disagreement () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let pre_state = dual irmin Dual.Inactive in
    let noop_step state = Lwt.return (state, ()) in
    let*! proof_opt = Dual.produce_proof context pre_state noop_step in
    let proof =
      match proof_opt with
      | Some ((Dual.Irmin_only _ as p), ()) -> p
      | _ -> Test.fail ~__LOC__ "expected Irmin_only proof for a no-op step"
    in
    (* Step claims [Active] in its return tag but does not touch
     [/pvm/nds_hash].  Marker is absent in the post-tree — the
     verifier must reject. *)
    let activating_hint_step ({irmin = irmin_state; _} : Dual.state) =
      let fresh_nds = make_empty_nds () in
      Lwt.return (dual irmin_state (Dual.Active fresh_nds), ())
    in
    let*! verify_opt = Dual.verify_proof proof activating_hint_step in
    match verify_opt with
    | None -> return_unit
    | Some ({Dual.nds = Dual.Inactive; _}, ()) ->
        Test.fail
          ~__LOC__
          "verify_proof on Irmin_only silently demoted to Inactive — the \
           contract now requires outright rejection of step tag / marker \
           disagreement"
    | Some ({Dual.nds = Dual.Active _; _}, ()) ->
        Test.fail
          ~__LOC__
          "verify_proof on Irmin_only accepted Active without the durable's \
           /pvm/nds_hash marker"

  (* -------------------------------------------------------------------- *)
  (* Encoder/decoder contract surface                                      *)
  (* -------------------------------------------------------------------- *)

  (** [Encoding_runner.encode] raises [Invalid_argument] on the
      deactivation pattern ([Durable_only] storage + [Active] tag): once
      the NDS gate opens, the storage stays [Dual]. *)
  let test_encode_raises_on_deactivation () =
    let open Lwt_result_syntax in
    let open Tezos_scoru_wasm.Wasm_pvm_state.Internal_state in
    let irmin = Irmin.empty_state () in
    let*! durable = Irmin.Encoding_runner.decode_durable_storage irmin in
    let pvm_state =
      {
        last_input_info = None;
        current_tick = Z.zero;
        reboot_counter = Z.zero;
        storage = Durable_only {durable};
        buffers = default_buffers 0l Z.zero ();
        tick_state = Collect;
        last_top_level_call = Z.zero;
        max_nb_ticks = Z.zero;
        maximum_reboots_per_input = Z.zero;
        output_buffer_parameters =
          {validity_period = 0l; message_limit = Z.zero};
      }
    in
    let storage_state = dual irmin (Dual.Active (make_empty_nds ())) in
    let*! raised =
      Lwt.catch
        (fun () ->
          let*! _ = Dual.Encoding_runner.encode pvm_state storage_state in
          Lwt.return_false)
        (function
          | Invalid_argument _ -> Lwt.return_true | exn -> Lwt.reraise exn)
    in
    if raised then return_unit
    else
      Test.fail
        ~__LOC__
        "Encoding_runner.encode accepted Durable_only + Active — deactivation \
         contract not enforced"

  (* -------------------------------------------------------------------- *)
  (* Mutable-state conversions: snapshot independence                      *)
  (* -------------------------------------------------------------------- *)

  let nds_handle_exn (s : Dual.state) =
    match s with
    | {nds = Dual.Active nds; _} -> nds
    | {nds = Dual.Inactive; _} -> Test.fail ~__LOC__ "expected an Active state"

  let resize_exn nds n =
    match Octez_riscv_nds_common.Nds.resize nds n with
    | Ok () -> ()
    | Error _ -> Test.fail ~__LOC__ "failed to resize"

  (** [to_imm] severs the NDS handle: mutating the live handle through
      the mut_state after taking a snapshot must not change the
      snapshot's registry hash.  This is the dissection-cache scenario —
      an aliasing [to_imm] would let later evaluation corrupt cached
      refutation-game states. *)
  let test_to_imm_snapshot_independent () =
    let open Lwt_result_syntax in
    let irmin = Irmin.empty_state () in
    let m = Dual.from_imm (dual irmin (Dual.Active (make_empty_nds ()))) in
    let snapshot = Dual.to_imm m in
    let h0 =
      Octez_riscv_nds_common.Nds.registry_hash (nds_handle_exn snapshot)
    in
    (* Mutate the live handle through the mut_state, as kernel eval
       does; [Dual.read] aliases, so this is the same handle [m]
       carries. *)
    let live = nds_handle_exn (Dual.read m) in
    resize_exn live 1L ;
    let h_live = Octez_riscv_nds_common.Nds.registry_hash live in
    Check.(
      (h_live <> h0)
        bytes_typ
        ~__LOC__
        ~error_msg:"sanity: resize should have changed the live hash (= %L)") ;
    let h_snapshot =
      Octez_riscv_nds_common.Nds.registry_hash (nds_handle_exn snapshot)
    in
    Check.(
      (h_snapshot = h0)
        bytes_typ
        ~__LOC__
        ~error_msg:
          "mutating the live handle changed the to_imm snapshot: %L, expected \
           %R") ;
    return_unit

  (** [from_imm] severs symmetrically: evaluating through the returned
      mut_state must leave the source state unchanged. *)
  let test_from_imm_independent () =
    let open Lwt_result_syntax in
    let irmin = Irmin.empty_state () in
    let source = dual irmin (Dual.Active (make_empty_nds ())) in
    let h0 = Octez_riscv_nds_common.Nds.registry_hash (nds_handle_exn source) in
    let m = Dual.from_imm source in
    resize_exn (nds_handle_exn (Dual.read m)) 1L ;
    let h_source =
      Octez_riscv_nds_common.Nds.registry_hash (nds_handle_exn source)
    in
    Check.(
      (h_source = h0)
        bytes_typ
        ~__LOC__
        ~error_msg:
          "mutating through from_imm's mut_state changed the source: %L, \
           expected %R") ;
    return_unit

  (** The [Inactive -> Active] activation flip propagates through the
      read / step / write cycle the PVM's mutable-state wrapper uses —
      it cannot propagate through handle aliasing since [Inactive]
      carries no handle. *)
  let test_write_propagates_activation_flip () =
    let open Lwt_result_syntax in
    let*! irmin = make_seeded_inactive_state () in
    let m = Dual.from_imm (dual irmin Dual.Inactive) in
    let*! post, () = activation_step (Dual.read m) in
    Dual.write m post ;
    match Dual.read m with
    | {nds = Dual.Active _; _} -> return_unit
    | {nds = Dual.Inactive; _} ->
        Test.fail
          ~__LOC__
          "activation flip written through Dual.write is not visible through \
           Dual.read"

  (** [read] aliases: a mutation through the handle [read] exposes is
      the same mutation kernel evaluation performs — a later [to_imm]
      must observe it (evaluation effects are never lost), and the
      snapshot must equal the live hash at capture time. *)
  let test_read_aliases_live_handle () =
    let open Lwt_result_syntax in
    let irmin = Irmin.empty_state () in
    let m = Dual.from_imm (dual irmin (Dual.Active (make_empty_nds ()))) in
    let h0 =
      Octez_riscv_nds_common.Nds.registry_hash (nds_handle_exn (Dual.read m))
    in
    resize_exn (nds_handle_exn (Dual.read m)) 1L ;
    let snapshot = Dual.to_imm m in
    let h_snapshot =
      Octez_riscv_nds_common.Nds.registry_hash (nds_handle_exn snapshot)
    in
    let h_live =
      Octez_riscv_nds_common.Nds.registry_hash (nds_handle_exn (Dual.read m))
    in
    Check.(
      (h_snapshot <> h0)
        bytes_typ
        ~__LOC__
        ~error_msg:
          "mutation through [read]'s handle was lost — to_imm still snapshots \
           the pre-mutation content (%L)") ;
    Check.(
      (h_snapshot = h_live)
        bytes_typ
        ~__LOC__
        ~error_msg:"to_imm snapshot (%L) should equal the live hash (%R)") ;
    return_unit

  (** [write] installs the state verbatim: [read] after [write] returns
      the same Irmin tree and the {e same physical} NDS handle — write
      is alias-preserving by design (the copy boundary is
      [to_imm]/[from_imm], not [write]). *)
  let test_write_installs_verbatim () =
    let open Lwt_result_syntax in
    let*! irmin' = make_seeded_inactive_state () in
    let m = Dual.from_imm (dual (Irmin.empty_state ()) Dual.Inactive) in
    let nds = make_empty_nds () in
    let s = dual irmin' (Dual.Active nds) in
    Dual.write m s ;
    let r = Dual.read m in
    (match r with
    | {nds = Dual.Active h; _} ->
        if not (h == nds) then
          Test.fail
            ~__LOC__
            "write should install the NDS handle without copying (physical \
             equality expected)"
    | {nds = Dual.Inactive; _} ->
        Test.fail ~__LOC__ "write dropped the Active tag") ;
    let*! h_r = Irmin.state_hash (match r with {irmin; _} -> irmin) in
    let*! h_s = Irmin.state_hash irmin' in
    Check.(
      (h_r = h_s)
        state_hash_typ
        ~__LOC__
        ~error_msg:"write should install the Irmin tree verbatim (%L <> %R)") ;
    return_unit

  (** Pre-activation states pass through the conversions untouched:
      [to_imm]/[from_imm] on [Inactive] never mint or copy a handle. *)
  let test_inactive_passthrough () =
    let open Lwt_result_syntax in
    let s = dual (Irmin.empty_state ()) Dual.Inactive in
    let m = Dual.from_imm s in
    (match Dual.read m with
    | {nds = Dual.Inactive; _} -> ()
    | {nds = Dual.Active _; _} ->
        Test.fail ~__LOC__ "from_imm minted a handle on an Inactive state") ;
    (match Dual.to_imm m with
    | {nds = Dual.Inactive; _} -> ()
    | {nds = Dual.Active _; _} ->
        Test.fail ~__LOC__ "to_imm minted a handle on an Inactive state") ;
    return_unit

  (** Conversions preserve marker coherence: on a state whose
      [/pvm/nds_hash] marker matches its handle, [from_imm] then
      [to_imm] still decode — the copy preserves the registry hash, so
      {!Dual.Encoding_runner.decode_storage}'s cross-check passes. *)
  let test_conversions_preserve_marker_coherence () =
    let open Lwt_result_syntax in
    let nds = make_empty_nds () in
    let*! irmin = stamp_nds_marker (Irmin.empty_state ()) nds in
    let m = Dual.from_imm (dual irmin (Dual.Active nds)) in
    let snapshot = Dual.to_imm m in
    let*! storage = Dual.Encoding_runner.decode_storage snapshot in
    (match storage with
    | Wasm_pvm_state.Internal_state.Dual _ -> ()
    | Wasm_pvm_state.Internal_state.Durable_only _ ->
        Test.fail ~__LOC__ "expected Dual storage after conversions") ;
    return_unit

  (** Regression for the aliasing bug this design fixes: snapshot a
      mut_state ([to_imm], as the interpreter's dissection cache does),
      keep evaluating through the live handle, then decode the
      snapshot.  Under the old [to_imm = (!)] the snapshot shared the
      live handle, whose registry hash no longer matched the
      snapshot's [/pvm/nds_hash] marker —
      [reconstruct_pvm_storage_of_marker] raised [Invalid_argument]
      and the refutation game lost its cached state. *)
  let test_snapshot_decodes_after_live_mutation () =
    let open Lwt_result_syntax in
    let nds = make_empty_nds () in
    let*! irmin = stamp_nds_marker (Irmin.empty_state ()) nds in
    let m = Dual.from_imm (dual irmin (Dual.Active nds)) in
    let snapshot = Dual.to_imm m in
    (* Kernel evaluation on the live state, after the snapshot. *)
    resize_exn (nds_handle_exn (Dual.read m)) 1L ;
    let*! storage =
      Lwt.catch
        (fun () ->
          let*! s = Dual.Encoding_runner.decode_storage snapshot in
          Lwt.return_some s)
        (function Invalid_argument _ -> Lwt.return_none | e -> Lwt.reraise e)
    in
    match storage with
    | Some (Wasm_pvm_state.Internal_state.Dual _) -> return_unit
    | Some (Wasm_pvm_state.Internal_state.Durable_only _) ->
        Test.fail ~__LOC__ "expected Dual storage from the snapshot"
    | None ->
        Test.fail
          ~__LOC__
          "decoding the snapshot raised Invalid_argument — the live handle's \
           mutation reached the snapshot (the dissection-cache aliasing bug)"

  (** {!NDS_BACKEND.copy} is [Normal]-mode only: a transient
      [Prove]-mode session handle cannot be copied — a snapshot of a
      recording session would be meaningless. *)
  let test_copy_rejects_prove_handle () =
    let open Lwt_result_syntax in
    let _session, prove_nds = Backend.open_prove_session (make_empty_nds ()) in
    match Backend.copy prove_nds with
    | exception Invalid_argument _ -> return_unit
    | _ ->
        Test.fail
          ~__LOC__
          "Backend.copy accepted a Prove-mode handle; it should raise \
           Invalid_argument"

  (* -------------------------------------------------------------------- *)
  (* Suite                                                                *)
  (* -------------------------------------------------------------------- *)

  let tests =
    [
      tztest
        "unit: pre-activation state hash equals Irmin.state_hash"
        `Quick
        test_inactive_hash_is_irmin_only;
      tztest
        "unit: post-activation state hash differs from pre-activation hash"
        `Quick
        test_activated_hash_differs_from_inactive;
      tztest
        "e2e: encode of Dual storage writes /pvm/nds_hash and decode restores \
         Dual"
        `Quick
        test_e2e_encode_writes_path_and_decode_restores_dual;
      tztest
        "e2e: fresh dual machine consumes empty_state without crashing"
        `Quick
        test_dual_machine_empty_state_is_inactive;
      tztest
        "property: activation changes the state hash across NDS sizes"
        `Quick
        test_property_activation_changes_hash;
      tztest
        "proof: Irmin_only no-op produce + verify roundtrip"
        `Quick
        test_proof_irmin_only_noop_roundtrip;
      tztest
        "proof: Irmin_only proof encodes untagged, byte-identical to bare \
         Irmin proof"
        `Quick
        test_proof_encoding_irmin_only_untagged;
      tztest
        "proof: proof_encoding classifies `Dynamic (composes in obj2)"
        `Quick
        test_proof_encoding_classifies_dynamic;
      tztest
        "proof: proof_encoding composes as a non-final obj2 field"
        `Quick
        test_proof_encoding_composes_in_obj2;
      tztest
        "proof: Dual proof occupies compact tag byte 4"
        `Quick
        test_dual_proof_tag_byte;
      tztest
        "proof: Irmin_only JSON matches the bare Irmin proof JSON"
        `Quick
        test_irmin_only_json_unchanged;
      tztest
        "proof: Dual no-op produce + verify roundtrip"
        `Quick
        test_proof_dual_noop_roundtrip;
      tztest
        "proof: produce_proof on Active state rejects when nds disagrees with \
         durable's /pvm/nds_hash"
        `Quick
        test_proof_dual_rejects_inconsistent_state;
      tztest
        "proof: verify_proof rejects a divergent step that disagrees with the \
         proof's stop_state"
        `Quick
        test_proof_verify_rejects_divergent_step;
      (* Cross-machine proofs *)
      tztest
        "cross: single-state machine proof verified by the dual machine"
        `Quick
        test_cross_single_state_proof_verified_by_dual;
      tztest
        "cross: dual machine Irmin_only proof verified by the single-state \
         machine"
        `Quick
        test_cross_dual_proof_verified_by_single_state;
      (* Composite proof plumbing *)
      tztest
        "plumbing: activation tick Inactive -> Active produce + verify \
         roundtrip"
        `Quick
        test_proof_activation_tick_roundtrip;
      tztest
        "plumbing: produce_proof rejects activation tick with non-canonical \
         /pvm/nds_hash"
        `Quick
        test_proof_activation_tick_rejects_non_canonical_hash;
      tztest
        "plumbing: Dual proof with swapped Irmin half rejected by verify_proof"
        `Quick
        test_dual_proof_with_swapped_irmin_half_rejected;
      (* Public dispatcher surface *)
      tztest
        "dispatcher: proof_start/stop_state match state_hash on Irmin_only"
        `Quick
        test_proof_endpoints_match_state_hash_irmin_only;
      tztest
        "dispatcher: proof_start/stop_state match state_hash on Dual"
        `Quick
        test_proof_endpoints_match_state_hash_dual;
      tztest
        "dispatcher: cast_read_only preserves Irmin_only and collapses stop"
        `Quick
        test_cast_read_only_irmin_only;
      tztest
        "dispatcher: cast_read_only preserves Dual and collapses stop"
        `Quick
        test_cast_read_only_dual;
      tztest
        "dispatcher: verify_proof on Irmin_only rejects step tag / marker \
         disagreement"
        `Quick
        test_verify_irmin_only_rejects_step_tag_marker_disagreement;
      (* Encoder/decoder contracts *)
      tztest
        "encoder: Encoding_runner.encode raises Invalid_argument on \
         Durable_only + Active (deactivation)"
        `Quick
        test_encode_raises_on_deactivation;
      tztest
        "plumbing: Dual proof with swapped NDS half rejected by verify_proof"
        `Quick
        test_dual_proof_with_swapped_nds_half_rejected;
      (* Mutable-state conversions *)
      tztest
        "mut_state: to_imm snapshot unaffected by later live-handle mutation"
        `Quick
        test_to_imm_snapshot_independent;
      tztest
        "mut_state: from_imm evaluation leaves the source state unchanged"
        `Quick
        test_from_imm_independent;
      tztest
        "mut_state: activation flip propagates through read/write"
        `Quick
        test_write_propagates_activation_flip;
      tztest
        "mut_state: read aliases the live handle (eval effects reach to_imm)"
        `Quick
        test_read_aliases_live_handle;
      tztest
        "mut_state: write installs the state verbatim (no copy)"
        `Quick
        test_write_installs_verbatim;
      tztest
        "mut_state: Inactive passes through the conversions untouched"
        `Quick
        test_inactive_passthrough;
      tztest
        "mut_state: conversions preserve marker coherence (decode_storage)"
        `Quick
        test_conversions_preserve_marker_coherence;
      tztest
        "regression: snapshot still decodes after live-handle mutation \
         (dissection-cache aliasing bug)"
        `Quick
        test_snapshot_decodes_after_live_mutation;
      tztest
        "backend: copy rejects a Prove-mode handle"
        `Quick
        test_copy_rejects_prove_handle;
    ]
end

(* In-memory instantiation: the dual-state PVM over the in-memory Irmin
   durable ([State_in_memory]) and the in-memory NDS backend. *)
module In_memory_tests =
  Make_tests
    (Tezos_smart_rollup_wasm_in_memory_dual.Irmin)
    (Tezos_smart_rollup_wasm_in_memory_dual.In_memory_backend)
    (struct
      let make_empty_context () =
        Tezos_context_memory.Context_binary.make_empty_context ()

      let tree_only_machine ~config =
        (module (val Tezos_smart_rollup_wasm_in_memory.wasm_pvm_machine ~config)
        : Tezos_scoru_wasm.Wasm_pvm_sig.S)

      let make_empty_nds () = Octez_riscv_nds_memory.make_empty_normal_nds ()
    end)

(* Regression guard for the hardcoded
   [Tezos_smart_rollup_wasm_dual_state.empty_registry_hash].  That
   constant is a literal so the library need not link the in-memory NDS
   FFI, but it must stay equal to the hash the backend computes for a
   fresh empty registry — the value a verifier cross-checks the
   activation tick against.  A mismatch means the NDS hashing scheme or
   the empty registry changed: update the literal deliberately. *)
let test_empty_registry_hash_is_canonical () =
  let open Lwt_result_syntax in
  let live =
    Octez_riscv_nds_memory.Normal.Registry.hash
      (Octez_riscv_nds_memory.Normal.Registry.create ())
  in
  let canonical = Tezos_smart_rollup_wasm_dual_state.empty_registry_hash in
  if not (Bytes.equal canonical live) then
    Test.fail
      ~__LOC__
      "empty_registry_hash drifted from the in-memory backend: hardcoded %s, \
       computed %s"
      (Hex.show (Hex.of_bytes canonical))
      (Hex.show (Hex.of_bytes live)) ;
  return_unit

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib smart rollup wasm in memory"
    [
      ("Dual state and PVM", In_memory_tests.tests);
      ( "empty_registry_hash",
        [
          tztest
            "regression: empty_registry_hash matches the in-memory backend"
            `Quick
            test_empty_registry_hash_is_canonical;
        ] );
    ]
  |> Lwt_main.run
