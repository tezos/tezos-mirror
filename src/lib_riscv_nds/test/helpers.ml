(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Shared helpers for NDS property-based tests: generators, backend
    abstraction, and assertion utilities. *)

open Nds_errors
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

let pp_op_result : type a. a op -> Format.formatter -> a -> unit = function
  | Set _ -> Format.pp_print_nothing
  | Write _ -> fun fmt -> Format.fprintf fmt "%Ld"
  | Read _ -> fun fmt b -> Format.fprintf fmt "%S" (String.of_bytes b)
  | Delete _ -> Format.pp_print_nothing
  | Exists _ -> Format.pp_print_bool
  | Value_length _ -> fun fmt -> Format.fprintf fmt "%Ld"
  | Copy_database _ -> Format.pp_print_nothing
  | Move_database _ -> Format.pp_print_nothing
  | Clear _ -> Format.pp_print_nothing
  | Resize _ -> Format.pp_print_nothing

(** Build an [nds_ops] handle from a backend module and registry. *)
let apply_op (type r) (type a) (module B : BACKEND with type Registry.t = r)
    (r : r) (op : a op) : (a, Nds_errors.invalid_argument_error) result =
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

(** Pretty-print a [result] for bisimulation violation messages. *)
let pp_result (type a) (op : a op)
    (r : (a, Nds_errors.invalid_argument_error) result) =
  match r with
  | Ok r -> Format.asprintf "Ok(%a)" (pp_op_result op) r
  | Error e -> Format.asprintf "Error(%a)" pp_invalid_argument_error e

(** Build a [check_state] callback that verifies the NDS contains every
    key-value pair present in the model, with matching value lengths and
    contents.  Also checks that the number of databases is the same.
    Note: this does not detect extra keys in the NDS that are absent from
    the model, because the DATABASE interface has no key enumeration. *)
let check_state_full (type r) (model_reg : Model.registry)
    (module Nds : BACKEND with type Registry.t = r) (nds : r) _idx _op =
  let^? nds_size = Nds.Registry.size nds in
  let^? model_size = Model.Registry.size model_reg in
  if not (Int64.equal nds_size model_size) then
    Test.fail
      "State check: registry size mismatch: model=%Ld nds=%Ld"
      model_size
      nds_size ;
  Model.iter_all_keys model_reg (fun ~db_index ~key ~value:expected_value ->
      let^? exists = Nds.Database.exists nds ~db_index ~key in
      if not exists then
        Test.fail "State check: key %a missing in db %Ld" pp_bytes key db_index ;
      let expected_len = Int64.of_int (Bytes.length expected_value) in
      let^? nds_len = Nds.Database.value_length nds ~db_index ~key in
      if not (Int64.equal nds_len expected_len) then
        Test.fail
          "State check: value_length mismatch for key %a in db %Ld: model=%Ld \
           nds=%Ld"
          pp_bytes
          key
          db_index
          expected_len
          nds_len ;
      let^? nds_value =
        Nds.Database.read nds ~db_index ~key ~offset:0L ~len:expected_len
      in
      if not (Bytes.equal nds_value expected_value) then
        Test.fail
          "State check: value mismatch for key %a in db %Ld"
          pp_bytes
          key
          db_index) ;
  true

(** Build a [check_state] callback that verifies two NDS handles agree on
    all per-database hashes at each step. *)
let check_state_hash (type r1) (type r2)
    (module Nds1 : BACKEND with type Registry.t = r1) (nds1 : r1)
    (module Nds2 : BACKEND with type Registry.t = r2) (nds2 : r2) idx op =
  let^? size1 = Nds1.Registry.size nds1 in
  let^? size2 = Nds2.Registry.size nds2 in
  if not (Int64.equal size1 size2) then
    Test.fail
      "Hash state check: registry size mismatch at op %d: %a (%Ld vs %Ld)"
      idx
      pp_op
      op
      size1
      size2 ;
  let n = Int64.to_int size1 in
  for i = 0 to n - 1 do
    let db_index = Int64.of_int i in
    let h1 = Nds1.Database.hash nds1 ~db_index in
    let h2 = Nds2.Database.hash nds2 ~db_index in
    match (h1, h2) with
    | Ok h1, Ok h2 ->
        if not (Bytes.equal h1 h2) then
          Test.fail
            "Hash state check: db %d hash mismatch at op %d: %a"
            i
            idx
            pp_op
            op
    | Error _, Error _ -> ()
    | _ ->
        Test.fail
          "Hash state check: db %d hash error mismatch at op %d: %a"
          i
          idx
          pp_op
          op
  done ;
  true

(** Check the bisimulation relation at one step: run the operation on both
    handles, verify that they agree on the result, then call [check_state]
    to verify observable state agreement. Fails the Tezt test on any
    mismatch. *)
let check_op ~check_state ~label (type r1) (type r2)
    (module Nds1 : BACKEND with type Registry.t = r1) (r1 : r1)
    (module Nds2 : BACKEND with type Registry.t = r2) (r2 : r2) idx (Any op) =
  let res1 = apply_op (module Nds1) r1 op in
  let res2 = apply_op (module Nds2) r2 op in
  let ok =
    match (res1, res2) with
    | Ok v1, Ok v2 -> v1 = v2
    | Error e1, Error e2 -> e1 = e2
    | _ -> false
  in
  if not ok then
    Test.fail
      "%s violation at op %d: %a\n  %s: %s\n  %s: %s"
      label
      idx
      pp_op
      (Any op)
      Nds1.name
      (pp_result op res1)
      Nds2.name
      (pp_result op res2) ;
  check_state idx (Any op)

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
