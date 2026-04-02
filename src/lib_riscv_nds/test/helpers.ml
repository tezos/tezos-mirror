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

(** Like [Qcheck_tezt.register] but wraps the test body with
    [with_disk_gc].  The GC runs once at the end of the Tezt test, not
    after each QCheck iteration. *)
let register_pbt_with_disk_gc ~__FILE__ ?title ~tags ?long ?seed
    (t : QCheck2.Test.t) =
  let (QCheck2.Test.Test cell) = t in
  let title =
    match title with Some x -> x | None -> QCheck2.Test.get_name cell
  in
  let tags = "qcheck" :: tags in
  Test.register ~__FILE__ ~title ~tags ?seed
  @@ with_disk_gc (fun () ->
         let rand = Random.get_state () in
         QCheck2.Test.check_cell_exn ?long ~rand cell ;
         unit)

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

(** {1 QCheck2 generators} *)

(** Generate a valid key: 0 to 256 bytes of random content. *)
let gen_valid_key =
  let open QCheck2.Gen in
  let* len =
    frequency [(1, pure 0); (5, 1 -- 10); (3, 11 -- 100); (1, pure 256)]
  in
  map Bytes.of_string (string_size (pure len))

(** Generate a small value (for faster tests). *)
let gen_small_value =
  let open QCheck2.Gen in
  let* len = frequency [(1, pure 0); (5, 1 -- 32); (3, 33 -- 256)] in
  map Bytes.of_string (string_size (pure len))

(** Generate a non-empty small value. *)
let gen_nonempty_value =
  let open QCheck2.Gen in
  let* len = frequency [(5, 1 -- 32); (3, 33 -- 256)] in
  map Bytes.of_string (string_size (pure len))

(** Generate a valid database index for a registry of given size. *)
let gen_valid_db_index size =
  let open QCheck2.Gen in
  if size <= 0 then pure 0L else map Int64.of_int (0 -- (size - 1))

(** Pretty-printers for QCheck2 output. *)

let pp_bytes fmt b =
  if Bytes.length b <= 20 then Format.fprintf fmt "%S" (Bytes.to_string b)
  else Format.fprintf fmt "<bytes len=%d>" (Bytes.length b)

(** {1 Model-based testing} *)

(** Abstract operations on an NDS registry, indexed by their return type.
    Using a GADT lets the model and the NDS share a single polymorphic
    [apply] function whose return type is dictated by the constructor. *)
type _ op =
  | Set : {db : int64; key : bytes; value : bytes} -> unit op
  | Write : {db : int64; key : bytes; offset : int64; value : bytes} -> int64 op
  | Read : {db : int64; key : bytes; offset : int64; len : int64} -> bytes op
  | Delete : {db : int64; key : bytes} -> unit op
  | Exists : {db : int64; key : bytes} -> bool op
  | Value_length : {db : int64; key : bytes} -> int64 op
  | Copy_database : {src : int64; dst : int64} -> unit op
  | Move_database : {src : int64; dst : int64} -> unit op
  | Clear : int64 -> unit op
  | Resize : int64 -> unit op

(** Existential wrapper for heterogeneous lists of operations. *)
type any_op = Any : _ op -> any_op

let pp_op fmt (Any op) =
  match op with
  | Set {db; key; value} ->
      Format.fprintf
        fmt
        "Set(db=%Ld, key=%a, val=%a)"
        db
        pp_bytes
        key
        pp_bytes
        value
  | Write {db; key; offset; value} ->
      Format.fprintf
        fmt
        "Write(db=%Ld, key=%a, off=%Ld, val=%a)"
        db
        pp_bytes
        key
        offset
        pp_bytes
        value
  | Read {db; key; offset; len} ->
      Format.fprintf
        fmt
        "Read(db=%Ld, key=%a, off=%Ld, len=%Ld)"
        db
        pp_bytes
        key
        offset
        len
  | Delete {db; key} ->
      Format.fprintf fmt "Delete(db=%Ld, key=%a)" db pp_bytes key
  | Exists {db; key} ->
      Format.fprintf fmt "Exists(db=%Ld, key=%a)" db pp_bytes key
  | Value_length {db; key} ->
      Format.fprintf fmt "Value_length(db=%Ld, key=%a)" db pp_bytes key
  | Copy_database {src; dst} ->
      Format.fprintf fmt "Copy_database(src=%Ld, dst=%Ld)" src dst
  | Move_database {src; dst} ->
      Format.fprintf fmt "Move_database(src=%Ld, dst=%Ld)" src dst
  | Clear db -> Format.fprintf fmt "Clear(%Ld)" db
  | Resize n -> Format.fprintf fmt "Resize(%Ld)" n

let pp_ops fmt ops =
  Format.fprintf
    fmt
    "@[<v>%a@]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
       (fun fmt (i, op) -> Format.fprintf fmt "%d: %a" i pp_op op))
    (List.mapi (fun i op -> (i, op)) ops)

let print_ops ops = Format.asprintf "%a" pp_ops ops

let apply_op (type r) (type a) (module B : BACKEND with type Registry.t = r)
    (r : r) (op : a op) : a tzresult =
  match op with
  | Set {db; key; value} -> B.Database.set r ~db_index:db ~key ~value
  | Write {db; key; offset; value} ->
      B.Database.write r ~db_index:db ~key ~offset ~value
  | Read {db; key; offset; len} ->
      B.Database.read r ~db_index:db ~key ~offset ~len
  | Delete {db; key} -> B.Database.delete r ~db_index:db ~key
  | Exists {db; key} -> B.Database.exists r ~db_index:db ~key
  | Value_length {db; key} -> B.Database.value_length r ~db_index:db ~key
  | Copy_database {src; dst} -> B.Registry.copy_database r ~src ~dst
  | Move_database {src; dst} -> B.Registry.move_database r ~src ~dst
  | Clear db -> B.Registry.clear r db
  | Resize n -> B.Registry.resize r n

(** Apply all operations on the NDS, ignoring results and errors.
    Useful for setting up state in determinism and snapshot tests. *)
let apply_ops (type r) (module B : BACKEND with type Registry.t = r) (r : r) ops
    =
  List.iter (fun (Any op) -> ignore (apply_op (module B) r op)) ops

(** {2 Operation generators for model-based tests} *)

(** Generate a single operation biased toward valid operations for a registry
    with [num_dbs] databases. Keys are drawn from a small pool [keys] so that
    operations frequently interact. *)
let gen_op ~num_dbs ~keys =
  let open QCheck2.Gen in
  let gen_db =
    if num_dbs <= 0 then pure 0L else map Int64.of_int (0 -- (num_dbs - 1))
  in
  let gen_k = oneofl keys in
  frequency
    [
      ( 10,
        let* db = gen_db in
        let* key = gen_k in
        let* value = gen_small_value in
        return (Any (Set {db; key; value})) );
      ( 10,
        let* db = gen_db in
        let* key = gen_k in
        let* offset =
          map Int64.of_int (frequency [(5, pure 0); (3, 0 -- 64)])
        in
        let* value = gen_small_value in
        return (Any (Write {db; key; offset; value})) );
      ( 10,
        let* db = gen_db in
        let* key = gen_k in
        let* offset =
          map Int64.of_int (frequency [(5, pure 0); (3, 0 -- 64)])
        in
        let* len = map Int64.of_int (1 -- 256) in
        return (Any (Read {db; key; offset; len})) );
      ( 10,
        let* db = gen_db in
        let* key = gen_k in
        return (Any (Delete {db; key})) );
      ( 10,
        let* db = gen_db in
        let* key = gen_k in
        return (Any (Exists {db; key})) );
      ( 10,
        let* db = gen_db in
        let* key = gen_k in
        return (Any (Value_length {db; key})) );
      ( 5,
        let* src = gen_db in
        let* dst = gen_db in
        return (Any (Copy_database {src; dst})) );
      ( 5,
        let* src = gen_db in
        let* dst = gen_db in
        return (Any (Move_database {src; dst})) );
      (3, map (fun db -> Any (Clear db)) gen_db);
      (* Resize: target is current size +/- 1 or 2, clamped to [0, num_dbs+2].
         This exercises grow, shrink, no-op (same size), and the
         Registry_resize_too_large error path. *)
      ( 1,
        let* delta = frequencyl [(1, -2); (3, -1); (2, 0); (3, 1); (1, 2)] in
        let target = max 0 (num_dbs + delta) in
        return (Any (Resize (Int64.of_int target))) );
    ]

(** Generate a small strictly positive integer that shrinks toward 1, capped
    at [max]. *)
let gen_small_pos ~max =
  let open QCheck2.Gen in
  map (fun n -> (n mod max) + 1) small_nat

(** Generate a scenario: the number of databases and a sequence of operations.
    All parameters (number of databases, keys, operations) are generated and
    shrink toward 1, producing minimal counterexamples on failure. *)
let gen_scenario ~max_dbs ~max_keys ~max_ops =
  let open QCheck2.Gen in
  let* num_dbs = gen_small_pos ~max:max_dbs in
  let* num_keys = gen_small_pos ~max:max_keys in
  let* keys =
    list_repeat
      num_keys
      (map Bytes.of_string (string_size (frequency [(3, 1 -- 8); (1, pure 0)])))
  in
  (* Use [small_nat] for the ops count so QCheck2 shrinks the list
     length toward 0, giving minimal counterexamples. *)
  let* ops = list_size (gen_small_pos ~max:max_ops) (gen_op ~num_dbs ~keys) in
  return (num_dbs, ops)

let print_scenario (num_dbs, ops) =
  Format.asprintf "num_dbs=%d\n%a" num_dbs pp_ops ops
