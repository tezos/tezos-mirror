(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>          *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(* SPDX-FileCopyrightText: 2026 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(** Dual-storage instantiation of the WASM PVM: the shared Irmin durable
    {!Wasm_2_0_0_irmin_state} paired with the on-disk NDS backend, through the
    generic dual-state functor.  Reusing {!Wasm_2_0_0_irmin_state} as the Irmin
    half means there is no second Irmin backend to keep aligned with the
    protocol's proof format.

    The node-facing [state] is the dual record
    [{irmin : Irmin_context.tree; nds : Dual_state.nds_state}]; the context backend
    {!Dual_context} wraps {!Irmin_context} (which does all the storage work) and
    reconstructs the NDS half from the [/pvm/nds_hash] marker on checkout.  The
    rocksdb repository holding the [Normal]-mode registries is part of the
    context backend's [repo] handle, opened by [Dual_context.load] alongside the
    Irmin index.

    This module is protocol-agnostic; the protocol plugin packages {!Machine}
    into a [Pvm_sig.S] through [Sc_rollup.Wasm_2_0_0PVM.Make_pvm]. *)

(** The shared Irmin durable, extended with the [/pvm/nds_hash] marker
    accessors the dual-state functor needs. *)
module Irmin = struct
  include Wasm_2_0_0_irmin_state

  let find_value = Irmin_context.Tree.find

  let set_value = Irmin_context.Tree.add
end

include
  Tezos_smart_rollup_wasm_dual_state.Make
    (Irmin)
    (Tezos_smart_rollup_nds_on_disk)

(* Repo lifecycle and [Normal]-handle recovery for the on-disk NDS half.
   Kept here rather than in [Tezos_smart_rollup_nds_on_disk] — which stays a
   pure {!Tezos_smart_rollup_wasm_dual_state.NDS_BACKEND} — because opening the
   rocksdb repository and recovering a live handle from a marker are node
   concerns: the backend the dual-state functor consumes never sees a repo. *)
type nds_repo = Octez_riscv_nds_disk.Repo.t

let create_nds_repo path = Octez_riscv_nds_disk.Repo.create path

(* [Normal.Registry.checkout] raises when [commit_id] is absent from the
   rocksdb repo.  Callers reach this from [Dual_context.checkout] et al.,
   which the {!Context_sigs.S} signature promises will return [None] for
   missing states; a raw rocksdb exception at that boundary is opaque to
   operators.  Catch it here and re-raise a [Failure] naming the marker
   the checkout was for, so the daemon's log points straight at the
   registry that the Irmin marker refers to but rocksdb has lost — a
   drift the two stores enter after a snapshot import that carried only
   the Irmin half (see the L2-1427 TODO on GC/snapshot coverage) or a
   crash between [persist_nds] and the Irmin commit. *)
let checkout_nds nds_repo commit_id =
  match
    Octez_riscv_nds_disk.Normal.Registry.checkout nds_repo commit_id
    (* Not [Lwt.catch]: [Normal.Registry.checkout] is synchronous. *)
  with
  | registry ->
      Octez_riscv_nds_common.Nds.wrap
        Octez_riscv_nds_disk.Normal_tag
        (module Octez_riscv_nds_disk.Normal)
        registry
  | exception exn ->
      (* [Stdlib.failwith] rather than the [Tezos_base.TzPervasives.failwith]
         in scope, which returns [_ tzresult Lwt.t]; this function is
         synchronous and must raise. *)
      Format.kasprintf
        Stdlib.failwith
        "Wasm_2_0_0_dual_state.checkout_nds: rocksdb registry checkout failed \
         for commit id %s (marker read from /pvm/nds_hash): %s.  The Irmin \
         marker and the rocksdb registry stores have drifted apart."
        (Hex.show (Hex.of_bytes commit_id))
        (Printexc.to_string exn)

(* Persist the [Normal]-mode registry carried by [nds_state] to the rocksdb
   repo.  [Normal.Registry.commit] writes the registry and returns its commit
   id, which equals the registry's content hash — the exact bytes the
   dual-state encoder already stamped into the [/pvm/nds_hash] marker.  We
   re-check that equality (the same drift guard the encoder applies on decode)
   so a divergence between the NDS hashing and commit schemes fails loudly
   here, rather than as a silent checkout miss after a restart.

   No-op for pre-activation states ([Inactive]) and for non-[Normal] handles
   (transient [Prove]/[Verify] states are never reached through a context
   commit).

   MUST run before the Irmin commit: the durable [/pvm/nds_hash] marker must
   never become durable ahead of the registry it points at, or a restart would
   check out a commit id that is absent from rocksdb. *)
let persist_nds = function
  | Dual_state.Inactive -> ()
  | Dual_state.Active nds -> (
      match Octez_riscv_nds_disk.unwrap_normal nds with
      | None -> ()
      | Some registry ->
          let commit_id =
            Octez_riscv_nds_disk.Normal.Registry.commit registry
          in
          if
            not
              (Bytes.equal
                 commit_id
                 (Octez_riscv_nds_common.Nds.registry_hash nds))
          then
            invalid_arg
              "Wasm_2_0_0_dual_state.persist_nds: the rocksdb commit id \
               differs from the registry content hash stamped at /pvm/nds_hash \
               — the NDS hashing and commit schemes have diverged, so a \
               restart would fail to check the registry out")

type repo = {irmin : Irmin_context.repo; nds : nds_repo}

(** Context backend over [Irmin + NDS].  Delegates all storage work to
      {!Irmin_context} on the Irmin half of the state — the state hash and
      checkouts are exactly the Irmin ones, since the [/pvm/nds_hash] marker
      (the registry's content hash, stamped by the dual-state encoder) makes
      the Irmin tree commit to the NDS half.  Commit additionally persists the
      [Normal]-mode registry to the rocksdb repo (see {!persist_nds}), so the
      marker doubles as the key from which the NDS handle is reconstructed on
      every checkout. *)
module Dual_context :
  Context_sigs.S
    with type repo = repo
     and type state = Dual_state.state
     and type mut_state = Dual_state.mut_state
     and type hash = Irmin_context.hash = struct
  type nonrec repo = repo

  type state = Dual_state.state

  type mut_state = Dual_state.mut_state

  type hash = Irmin_context.hash

  type nonrec 'a index = ('a, repo) Context_sigs.index

  type nonrec 'a t = ('a, repo, mut_state) Context_sigs.t

  let impl_name = "Irmin+NDS"

  let equality_witness : (repo, state, mut_state) Context_sigs.equality_witness
      =
    ( Context_sigs.Equality_witness.make (),
      Context_sigs.Equality_witness.make (),
      Context_sigs.Equality_witness.make () )

  (* [Dual_state.to_imm] / [from_imm] sever the NDS handle (CoW), so the
     generic [PVMState.copy] / [imm_copy] / [mut_copy] built on them
     yield genuinely independent snapshots — the dissection cache must
     not alias the live handle mutated by later evaluation. *)
  let to_imm = Dual_state.to_imm

  let from_imm = Dual_state.from_imm

  (** Project the Irmin half of an index, preserving the access mode. *)
  let irmin_index (i : 'a index) : ('a, Irmin_context.repo) Context_sigs.index =
    {Context_sigs.path = i.path; repo = i.repo.irmin}

  (* The path the rollup node's context stores the PVM state under —
       same as [Irmin_context.PVMState.key]. *)
  let pvm_state_key = ["pvm_state"]

  (** Reconstruct the {!Dual_state.nds_state} half from the
        [/pvm/nds_hash] marker found under [prefix] in [tree]: absent
        marker is a pre-activation state; a present marker is the
        rocksdb commit id {!persist_nds} wrote at commit time, checked
        out from [nds_repo]. *)
  let restore_nds ?(prefix = []) nds_repo tree =
    let open Lwt.Syntax in
    let+ marker =
      Irmin_context.Tree.find tree (prefix @ Dual_state.nds_hash_path)
    in
    match marker with
    | None -> Dual_state.Inactive
    | Some commit_id -> Dual_state.Active (checkout_nds nds_repo commit_id)

  (* TODO: https://linear.app/tezos/issue/L2-1427
       [create_nds_repo] ignores [mode]:
       [Octez_riscv_nds_disk.Repo.create] has no read-only path, so a
       read-only rollup consumer would open the rocksdb repo for
       writing and race the operator's writer lock.  Harmless while
       [Impl_two_state] is unwired; needs a read-only entry in the
       disk API before the NDS gate is opened. *)
  let load ~cache_size mode path =
    let open Lwt_result_syntax in
    let* irmin_index = Irmin_context.load ~cache_size mode path in
    Lwt.catch
      (fun () ->
        let nds_path = Filename.concat path "nds" in
        (* [Repo.create] does not create the directory (the underlying
           rocksdb [DirectoryManager] opens an existing one), so a fresh
           data dir needs it created here first. *)
        let*! () = Lwt_utils_unix.create_dir nds_path in
        let nds = create_nds_repo nds_path in
        return {Context_sigs.path; repo = {irmin = irmin_index.repo; nds}})
      (fun exn ->
        let*! () = Irmin_context.close irmin_index in
        Lwt.reraise exn)

  let index (ctxt : 'a t) = ctxt.index

  (* The NDS repo exposes no close; closing the Irmin index is the
       only teardown there is. *)
  let close i = Irmin_context.close (irmin_index i)

  let readonly (i : [> `Read] index) = (i :> [`Read] index)

  (* [checkout] / [empty] build the mut record directly rather than
     through [from_imm]: the handle from [restore_nds] is a fresh
     rocksdb checkout, already independent, so a copy would be
     redundant. *)
  let checkout (i : 'a index) hash =
    let open Lwt.Syntax in
    let* irmin_ctxt = Irmin_context.checkout (irmin_index i) hash in
    match irmin_ctxt with
    | None -> Lwt.return_none
    | Some irmin_ctxt ->
        let context_tree = Irmin_context.to_imm irmin_ctxt.state in
        let+ nds = restore_nds ~prefix:pvm_state_key i.repo.nds context_tree in
        Some
          {
            Context_sigs.index = i;
            state = {Dual_state.irmin = context_tree; nds};
          }

  let empty (i : 'a index) : 'a t =
    let irmin_ctxt = Irmin_context.empty (irmin_index i) in
    let context_tree = Irmin_context.to_imm irmin_ctxt.state in
    {
      Context_sigs.index = i;
      state = {Dual_state.irmin = context_tree; nds = Dual_state.Inactive};
    }

  (** Persist the NDS half then commit the Irmin half.  The encoder has
        already stamped the registry's content hash into [/pvm/nds_hash];
        {!persist_nds} writes the registry to the rocksdb repo under that same
        id, and must run before the Irmin commit so the marker never becomes
        durable ahead of the registry it points at. *)
  let commit ?message (ctxt : [> `Write] t) =
    let ({irmin = context_tree; nds} : Dual_state.state) =
      Dual_state.read ctxt.state
    in
    persist_nds nds ;
    Irmin_context.commit
      ?message
      {
        Context_sigs.index = irmin_index ctxt.index;
        state = Irmin_context.from_imm context_tree;
      }

  (* TODO: https://linear.app/tezos/issue/L2-1427
       GC, split and snapshot export only cover the Irmin half; the
       rocksdb NDS repo grows unboundedly until it gets its own GC.
    *)
  let is_gc_finished i = Irmin_context.is_gc_finished (irmin_index i)

  let cancel_gc i = Irmin_context.cancel_gc (irmin_index i)

  let split i = Irmin_context.split (irmin_index i)

  let gc i ?callback hash = Irmin_context.gc (irmin_index i) ?callback hash

  let wait_gc_completion i = Irmin_context.wait_gc_completion (irmin_index i)

  let export_snapshot i hash ~path =
    Irmin_context.export_snapshot (irmin_index i) hash ~path

  let context_hash_of_hash = Irmin_context.context_hash_of_hash

  let hash_of_context_hash = Irmin_context.hash_of_context_hash

  module PVMState = struct
    type value = mut_state

    let empty () : value =
      {
        Dual_state.irmin = Irmin_context.to_imm (Irmin_context.PVMState.empty ());
        nds = Dual_state.Inactive;
      }

    let find (ctxt : _ t) =
      let open Lwt.Syntax in
      let ({irmin = context_tree; _} : Dual_state.state) =
        Dual_state.read ctxt.state
      in
      let* pvm_tree = Irmin_context.Tree.find_tree context_tree pvm_state_key in
      match pvm_tree with
      | None -> Lwt.return_none
      | Some pvm_tree ->
          let+ nds = restore_nds ctxt.index.repo.nds pvm_tree in
          Some {Dual_state.irmin = pvm_tree; nds}

    let lookup (state : value) path =
      let ({irmin = pvm_tree; _} : Dual_state.state) = Dual_state.read state in
      Irmin_context.Tree.find pvm_tree path

    (* Splice the (mutated) PVM subtree back into the enclosing context
       tree, and adopt the state's NDS handle as the context's: kernel
       eval mutated that handle in place, and it also carries any
       [Inactive -> Active] activation flip, which cannot propagate
       through handle aliasing. *)
    let set (ctxt : _ t) (state : value) =
      let open Lwt.Syntax in
      let ({irmin = context_tree; _} : Dual_state.state) =
        Dual_state.read ctxt.state
      in
      let ({irmin = pvm_tree; nds} : Dual_state.state) =
        Dual_state.read state
      in
      let+ context_tree =
        Irmin_context.Tree.add_tree context_tree pvm_state_key pvm_tree
      in
      Dual_state.write ctxt.state {Dual_state.irmin = context_tree; nds}

    let cache_preference = Context_sigs.In_memory

    (* Persist the NDS half before committing the PVM-state tree, for the same
       ordering reason as {!Dual_context.commit}. *)
    let commit (i : [> `Write] index) (state : value) =
      let ({irmin = pvm_tree; nds} : Dual_state.state) =
        Dual_state.read state
      in
      persist_nds nds ;
      Irmin_context.PVMState.commit
        (irmin_index i)
        (Irmin_context.from_imm pvm_tree)

    let checkout (i : _ index) hash =
      let open Lwt.Syntax in
      let* pvm_tree = Irmin_context.PVMState.checkout (irmin_index i) hash in
      match pvm_tree with
      | None -> Lwt.return_none
      | Some pvm_tree ->
          let pvm_tree = Irmin_context.to_imm pvm_tree in
          let+ nds = restore_nds i.repo.nds pvm_tree in
          Some {Dual_state.irmin = pvm_tree; nds}
  end

  module Internal_for_tests = struct
    let get_a_tree key : state Lwt.t =
      let open Lwt.Syntax in
      let+ tree = Irmin_context.Internal_for_tests.get_a_tree key in
      ({irmin = tree; nds = Dual_state.Inactive} : Dual_state.state)
  end
end

(* Documented in the .mli: the activation-boundary factory, closed over
   the index's rocksdb repository. *)
let make_empty_nds (i : _ Dual_context.index) () =
  Octez_riscv_nds_common.Nds.wrap
    Octez_riscv_nds_disk.Normal_tag
    (module Octez_riscv_nds_disk.Normal)
    (Octez_riscv_nds_disk.Normal.Registry.create i.repo.nds)

module Fast_pvm_params = Tezos_scoru_wasm_fast.Pvm.Default_params

module Wasm_fast_dual_machine :
  Tezos_scoru_wasm.Wasm_pvm_sig.S
    with type context = Wasm_2_0_0_irmin_state.context
     and type state = Dual_state.state
     and type proof = Dual_state.proof =
  Tezos_scoru_wasm_fast.Pvm.Make_pvm (Fast_pvm_params) (Dual_state)

(** The fast dual machine adapted to the node-side [Make_pvm] context: its
    proof context is the Irmin half of the dual context backend's [repo]
    handle (the NDS prove session needs no repository — it operates on the
    live handle carried by the state).  As with the single-state PVM, the
    protocol [Make_pvm] consumer specialises [compute_step] to the WASM
    entrypoint, so the machine stays a plain {!Tezos_scoru_wasm.Wasm_pvm_sig.S}
    here. *)
module Machine :
  Tezos_scoru_wasm.Wasm_pvm_sig.S
    with type state = Dual_state.state
     and type proof = Dual_state.proof
     and type context = ([`Read | `Write], repo) Context_sigs.raw_index = struct
  include (
    Wasm_fast_dual_machine :
      Tezos_scoru_wasm.Wasm_pvm_sig.S
        with type state = Dual_state.state
         and type proof = Dual_state.proof
         and type context := Wasm_2_0_0_irmin_state.context)

  type context = ([`Read | `Write], repo) Context_sigs.raw_index

  let produce_proof (ctxt : context) state step =
    Wasm_fast_dual_machine.produce_proof
      {Context_sigs.path = ctxt.path; repo = ctxt.repo.irmin}
      state
      step
end
