(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Store_sigs
open Context_sigs
open Octez_riscv_pvm

type repo = Storage.Repo.t

type tree = Storage.State.t

type mut_state = tree ref

type 'a raw_index = ('a, repo) Context_sigs.raw_index

type 'a index = ('a, repo) Context_sigs.index

type rw_index = [`Read | `Write] index

let impl_name = "RISC-V"

let from_imm imm_state = ref imm_state

let to_imm mut_state = !mut_state

let equality_witness : (repo, tree) Context_sigs.equality_witness =
  (Context_sigs.Equality_witness.make (), Context_sigs.Equality_witness.make ())

type nonrec 'a t = ('a, repo, tree) t

type hash = Storage.Id.t

let context_hash_of_hash h =
  Storage.Id.to_raw_string h |> Smart_rollup_context_hash.of_string_exn

let hash_of_context_hash h =
  Smart_rollup_context_hash.to_string h |> Storage.Id.unsafe_of_raw_string

let load : type a. cache_size:int -> a mode -> string -> a raw_index Lwt.t =
 fun ~cache_size mode path ->
  let open Lwt_syntax in
  let readonly = match mode with Read_only -> true | Read_write -> false in
  let+ repo = Storage.load ~cache_size ~readonly path in
  {path; repo}

let load ~cache_size mode path =
  let open Lwt_result_syntax in
  let*! index = load ~cache_size mode path in
  return index

let index ctxt = ctxt.index

let close index = Storage.close index.repo

let readonly (index : [> `Read] index) = (index :> [`Read] index)

let checkout index hash =
  let open Lwt_syntax in
  let* tree = Storage.checkout index.repo hash in
  Lwt.return (Option.bind tree (fun tree -> Some {index; tree}))

let empty index = {index; tree = Storage.empty ()}

let commit ?message ctxt = Storage.commit ?message ctxt.index.repo ctxt.tree

let is_gc_finished index = Storage.is_gc_finished index.repo

let cancel_gc index = Storage.cancel_gc index.repo

let split index = Storage.split index.repo

let gc index ?(callback : unit -> unit Lwt.t = fun () -> Lwt.return ())
    (hash : hash) =
  Storage.gc index.repo ~callback hash

let wait_gc_completion index = Storage.wait_gc_completion index.repo

let export_snapshot {path = _; repo} hash ~path =
  Storage.export_snapshot repo hash path

module PVMState = struct
  type value = tree

  let empty () = Storage.empty ()

  let find ctxt = Storage.find ctxt.tree Storage.pvm_state_key

  let lookup tree path = Storage.lookup tree path

  let set ctxt state =
    let open Lwt_syntax in
    let+ tree = Storage.set ctxt.tree Storage.pvm_state_key state in
    {ctxt with tree}
end

module Internal_for_tests = struct
  let get_a_tree key =
    let tree = Storage.empty () in
    Storage.add tree [key] Bytes.empty
end
