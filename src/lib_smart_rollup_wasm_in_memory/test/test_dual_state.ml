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

  let wasm_pvm_machine_dual = Dual_lib.wasm_pvm_machine_dual

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
    let*! dual_hash = Dual.state_hash (irmin, Dual.Inactive) in
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
      Dual.state_hash (irmin_state_inactive, Dual.Inactive)
    in
    let nds = make_empty_nds () in
    let*! irmin_state_active = stamp_nds_marker irmin_state_inactive nds in
    let*! activated_hash =
      Dual.state_hash (irmin_state_active, Dual.Active nds)
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
    let state = (irmin, Dual.Inactive) in
    let*! durable = Irmin.Encoding_runner.decode_durable_storage irmin in
    let dual_storage =
      Wasm_pvm_state.Internal_state.Dual {durable; nds = make_empty_nds ()}
    in
    let*! state' =
      Dual.Encoding_runner.encode_storage
        dual_storage
        (irmin, Dual.Active (make_empty_nds ()))
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
    let*! inactive_hash = Dual.state_hash (inactive, Dual.Inactive) in
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
              (inactive, Dual.Active nds)
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
    let state = (irmin, Dual.Inactive) in
    let step st =
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
    let state = (irmin, Dual.Inactive) in
    let step st =
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
              "Irmin_only proof bytes mis-decoded as Dual — the composite \
               fallback is not disjoint"
        | None ->
            Test.fail ~__LOC__ "Irmin_only proof bytes failed to decode back")

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
    let state = (activated_irmin, Dual.Active nds) in
    (* Real storage decode/encode round-trip: [decode_storage]
     reattaches the (prove-mode) NDS handle from the [Active] tag and
     [encode_storage] writes its registry hash back to
     [/pvm/nds_hash], exactly as the production [compute_step]
     pipeline does each tick. *)
    let step st =
      (* Read the entry back through the (prove/verify-mode) NDS handle so
         the database index is recorded as accessed during the proof. *)
      let*! value =
        match st with
        | _, Dual.Active nds -> (
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
        | _, Dual.Inactive -> Test.fail ~__LOC__ "Nds is inactive"
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
    let state = (activated_irmin, Dual.Active (make_empty_nds ())) in
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
    let state = (irmin, Dual.Inactive) in
    let no_op_step state = Lwt.return (state, ()) in
    let*! proof_opt = Dual.produce_proof context state no_op_step in
    let proof =
      match proof_opt with
      | Some (p, ()) -> p
      | None -> Test.fail ~__LOC__ "produce_proof returned None"
    in
    let mutating_step (irmin, nds_state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/marker")
          "x"
      in
      let+ irmin' = encode_durable_storage durable irmin in
      ((irmin', nds_state), ())
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
  let activation_step (irmin_state, _) =
    let open Lwt.Syntax in
    let fresh_nds = make_empty_nds () in
    let+ irmin' = stamp_nds_marker irmin_state fresh_nds in
    ((irmin', Dual.Active fresh_nds), ())

  (** Activation tick produce + verify: from [Inactive], the step
      writes the canonical empty-registry hash and flips to [Active].
      [produce_proof] emits [Irmin_only] (the tree captures the whole
      transition); [verify_proof] reads the marker, sees the empty
      value, and mints its own fresh [Normal] registry. *)
  let test_proof_activation_tick_roundtrip () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_inactive_state () in
    let state = (irmin, Dual.Inactive) in
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
        | Some ((_, Dual.Active _), ()) -> return_unit
        | Some ((_, Dual.Inactive), ()) ->
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
    let state = (irmin, Dual.Inactive) in
    let bogus_step (irmin_state, _) =
      let open Lwt.Syntax in
      let fresh_nds = make_empty_nds () in
      let+ irmin' =
        Dual.Internal_for_tests.write_nds_hash
          irmin_state
          (Bytes.make 32 '\xff')
      in
      ((irmin', Dual.Active fresh_nds), ())
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
    (* Give the size-1 registry's database an entry so [step] can read it
       back: the read records the database-index access that makes the
       produced NDS proof well-formed (its registry length node present),
       so [verify_proof] can reach the endpoint cross-check rather than
       failing to reconstruct the proof. *)
    let*? () =
      Octez_riscv_nds_common.Nds.set
        nds_size_1
        ~db_index:0L
        ~key:(Bytes.of_string "key")
        ~value:(Bytes.of_string "value")
      |> Result.map_error (fun e ->
             [Octez_riscv_nds_common.Nds_errors.Invalid_argument e])
    in
    let make_state_with_nds nds = stamp_nds_marker (Irmin.empty_state ()) nds in
    let*! irmin_empty = make_state_with_nds nds_empty in
    let*! irmin_size_1 = make_state_with_nds nds_size_1 in
    let step st =
      (* Read db 0 to record the database-index access (a no-op for the
         empty registry, whose db 0 does not exist). *)
      (match st with
      | _, Dual.Active nds ->
          ignore
            (Octez_riscv_nds_common.Nds.read
               nds
               ~db_index:0L
               ~key:(Bytes.of_string "key")
               ~offset:0L
               ~len:5L)
      | _, Dual.Inactive -> ()) ;
      Lwt.return (st, ())
    in
    let*! proof_empty_opt =
      Dual.produce_proof context (irmin_empty, Dual.Active nds_empty) step
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
      Dual.produce_proof context (irmin_size_1, Dual.Active nds_size_1) step
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
      replay's stop_state diverges.

      Not blocked on TZX-114: the Irmin half is checked before the NDS
      verify path. *)
  let test_dual_proof_with_swapped_irmin_half_rejected () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_active_state () in
    let state = (irmin, Dual.Active (make_empty_nds ())) in
    let step_noop state = Lwt.return (state, ()) in
    let step_mutate (irmin_state, nds_state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/foo")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      ((irmin', nds_state), ())
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
    let pre_state = (irmin, Dual.Inactive) in
    let*! pre_hash = Dual.state_hash pre_state in
    (* Mutating step so start_state <> stop_state — otherwise the
     stop_state assertion would be satisfied vacuously by [pre_hash]. *)
    let step (irmin_state, nds_state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/endpoints")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      ((irmin', nds_state), ())
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
      must reach into [dual_proof.irmin_proof].

      Not blocked on TZX-114: no [verify_proof] call is made. *)
  let test_proof_endpoints_match_state_hash_dual () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_active_state () in
    let pre_state = (irmin, Dual.Active (make_empty_nds ())) in
    let*! pre_hash = Dual.state_hash pre_state in
    (* Mutating step so start_state <> stop_state — otherwise the
     stop_state assertion would be satisfied vacuously by [pre_hash]. *)
    let step (irmin_state, nds_state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/endpoints")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      ((irmin', nds_state), ())
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
    let pre_state = (irmin, Dual.Inactive) in
    (* Mutating step so the honest proof's stop_state differs from its
     start_state — otherwise [cast_read_only]'s stop->start collapse
     would be satisfied vacuously by a no-op proof. *)
    let step (irmin_state, nds_state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/cast")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      ((irmin', nds_state), ())
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
      [dual_proof.irmin_proof] and collapse its stop to its start.

      Not blocked on TZX-114: no [verify_proof] call is made. *)
  let test_cast_read_only_dual () =
    let open Lwt_result_syntax in
    let context = make_empty_context () in
    let*! irmin = make_seeded_active_state () in
    let pre_state = (irmin, Dual.Active (make_empty_nds ())) in
    (* Mutating step so the honest proof's stop_state differs from its
     start_state — otherwise [cast_read_only]'s stop->start collapse
     would be satisfied vacuously by a no-op proof. *)
    let step (irmin_state, nds_state) =
      let open Lwt.Syntax in
      let* durable = Irmin.Encoding_runner.decode_durable_storage irmin_state in
      let* durable =
        Durable.set_value_exn
          durable
          (Durable.key_of_string_exn "/test/cast")
          "1"
      in
      let+ irmin' = encode_durable_storage durable irmin_state in
      ((irmin', nds_state), ())
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
    let pre_state = (irmin, Dual.Inactive) in
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
    let activating_hint_step (irmin_state, _) =
      let fresh_nds = make_empty_nds () in
      Lwt.return ((irmin_state, Dual.Active fresh_nds), ())
    in
    let*! verify_opt = Dual.verify_proof proof activating_hint_step in
    match verify_opt with
    | None -> return_unit
    | Some ((_, Dual.Inactive), ()) ->
        Test.fail
          ~__LOC__
          "verify_proof on Irmin_only silently demoted to Inactive — the \
           contract now requires outright rejection of step tag / marker \
           disagreement"
    | Some ((_, Dual.Active _), ()) ->
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
    let storage_state = (irmin, Dual.Active (make_empty_nds ())) in
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

let () =
  Alcotest_lwt.run
    ~__FILE__
    "test lib smart rollup wasm in memory"
    [("Dual state and PVM", In_memory_tests.tests)]
  |> Lwt_main.run
