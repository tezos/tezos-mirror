(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Shared helpers for NDS property-based tests: generators, backend
    abstraction, and assertion utilities. *)

open Intf

(** {1 Backend abstraction} *)

(** Module type satisfied by both memory and disk backends for Normal mode,
    extended with a creation helper. Both backends share the common
    {!Nds_errors.invalid_argument_error} type in their results. *)
module type BACKEND = sig
  val name : string

  module Registry :
    REGISTRY
      with type invalid_argument_error := Nds_errors.invalid_argument_error

  module Database :
    DATABASE
      with type registry := Registry.t
       and type invalid_argument_error := Nds_errors.invalid_argument_error

  val create_registry_with_dbs : int -> Registry.t
end

let grow_registry resize r n =
  let rec loop i =
    if i >= n then ()
    else (
      (match resize r (Int64.of_int (i + 1)) with
      | Ok () -> ()
      | Error _ -> assert false) ;
      loop (i + 1))
  in
  loop 0

module Memory_backend = struct
  let name = "memory"

  include Octez_riscv_nds_memory.Normal

  let create_registry_with_dbs n =
    let r = Registry.create () in
    grow_registry Registry.resize r n ;
    r
end

(** Create a fresh disk repo in a Tezt-managed temporary directory. *)
let get_disk_repo () =
  let path = Tezt.Temp.dir "nds_disk_test" in
  Octez_riscv_nds_disk.Repo.create path

module Disk_backend = struct
  let name = "disk"

  include Octez_riscv_nds_disk.Normal

  let create_registry_with_dbs n =
    let repo = get_disk_repo () in
    let r = Registry.create repo in
    grow_registry Registry.resize r n ;
    r
end

(** Force finalization of Rust-side RocksDB handles at the end of the
    test, before Tezt removes the temporary directory; otherwise the
    cleanup emits "Failed to remove" warnings.  Scoped per-test rather
    than via a global [Test.declare_clean_up_function] so other test
    suites linked into the same binary (e.g. [tezt/tests/main.exe])
    don't pay the [Gc.full_major] cost on every test. *)
let with_disk_gc body () =
  Lwt.finalize body (fun () ->
      Gc.full_major () ;
      Lwt.return_unit)

(** {1 Error matching helpers} *)

let pp_invalid_argument_error fmt = function
  | Key_not_found -> Format.pp_print_string fmt "Key_not_found"
  | Key_too_long -> Format.pp_print_string fmt "Key_too_long"
  | Offset_too_large -> Format.pp_print_string fmt "Offset_too_large"
  | Database_index_out_of_bounds ->
      Format.pp_print_string fmt "Database_index_out_of_bounds"
  | Registry_resize_too_large ->
      Format.pp_print_string fmt "Registry_resize_too_large"

(** Check that a result is an error equal to the expected invalid-argument
    error. *)
let check_error ~msg expected = function
  | Ok _ ->
      Test.fail
        "%s: succeeded but expected error %a"
        msg
        pp_invalid_argument_error
        expected
  | Error e when e = expected -> ()
  | Error e ->
      Test.fail
        "%s: expected error %a but got %a"
        msg
        pp_invalid_argument_error
        expected
        pp_invalid_argument_error
        e

(** Assert a result is Ok and return the value, failing the Tezt test
    with the error on Error. *)
let get_ok = function
  | Ok x -> x
  | Error e ->
      Test.fail "Expected Ok, got Error: %a" pp_invalid_argument_error e

(** Binding operator that unwraps a result or fails the Tezt test. *)
let ( let^? ) r f = f (get_ok r)
