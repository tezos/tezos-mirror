(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech  <contact@trili.tech>                        *)
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

open Tezos_test_helpers
open Tezos_scoru_wasm_helpers.Encodings_util
module Context_binary_tree = Tezos_context_memory.Context_binary.Tree

(* This is a generalised projection of
   durable_snapshot/durable.mli methods,
   which might be tested within snapshotting tests.
*)
module type Testable_durable_sig = sig
  type t

  type key

  val encoding : t Tezos_tree_encoding.t

  val max_key_length : int

  val key_of_string_exn : string -> key

  val key_of_string_opt : string -> key option

  val find_value : t -> key -> bytes option Lwt.t

  val find_value_exn : t -> key -> bytes Lwt.t

  val copy_tree_exn : t -> ?edit_readonly:bool -> key -> key -> t Lwt.t

  val move_tree_exn : t -> key -> key -> t Lwt.t

  val list : t -> key -> string list Lwt.t

  val count_subtrees : t -> key -> int Lwt.t

  val subtree_name_at : t -> key -> int -> string Lwt.t

  val delete : ?edit_readonly:bool -> t -> key -> t Lwt.t

  val hash : t -> key -> Context_hash.t option Lwt.t

  val hash_exn : t -> key -> Context_hash.t Lwt.t

  val set_value_exn : t -> ?edit_readonly:bool -> key -> string -> t Lwt.t

  val write_value_exn :
    t -> ?edit_readonly:bool -> key -> int64 -> string -> t Lwt.t

  val read_value_exn : t -> key -> int64 -> int64 -> string Lwt.t
end

module CBV = Tezos_lazy_containers.Chunked_byte_vector

(* Adapter of snapshotted durable interface
   with additional cbv type, which it doesn't have *)
module Snapshot : Testable_durable_sig = struct
  include Tezos_scoru_wasm_durable_snapshot.Durable

  let find_value tree key =
    let open Lwt_syntax in
    let* cbv = find_value tree key in
    match cbv with
    | None -> Lwt.return None
    | Some cbv -> Lwt.map Option.some (CBV.to_bytes cbv)

  let find_value_exn tree key =
    let open Lwt_syntax in
    let* cbv = find_value_exn tree key in
    CBV.to_bytes cbv
end

(* Adapter of current durable interface,
   tweaking find_value/find_value_exn signatures *)
module Current : Testable_durable_sig = struct
  include Tezos_scoru_wasm.Durable

  let find_value tree key =
    let open Lwt_syntax in
    let* cbv = find_value tree key in
    match cbv with
    | None -> Lwt.return None
    | Some cbv -> Lwt.map Option.some (CBV.to_bytes cbv)

  let find_value_exn tree key =
    let open Lwt_syntax in
    let* cbv = find_value_exn tree key in
    CBV.to_bytes cbv
end

(* Returns Ok () or Error (string * string) with diverged hashes *)
let compare_durable_storages (ts : Snapshot.t) (tc : Current.t) :
    (unit, string * string) result Lwt.t =
  let open Lwt_syntax in
  let hash t encoding =
    let* tree = empty_tree () in
    let+ tree = Tree_encoding_runner.encode encoding t tree in
    Context_binary_tree.hash tree
  in
  let* hash_snapshot = hash ts Snapshot.encoding in
  let* hash_current = hash tc Current.encoding in
  if Context_hash.equal hash_snapshot hash_current then Lwt.return_ok ()
  else
    Lwt.return_error
      ( Format.asprintf "%a" Context_hash.pp hash_snapshot,
        Format.asprintf "%a" Context_hash.pp hash_current )

(* This module implements a durable testable interface
   for a current implementation (Current module) against
   the reference implementation (Snapshot module) .
   All the methods are performed on both durables and
   returned values and resulting durables tested on equality.
*)
module Make_paired_durable (Config : sig
  val compare_durables_after_each_operation : bool
end) : Testable_durable_sig with type t = Snapshot.t * Current.t = struct
  type t = Snapshot.t * Current.t

  type key = Snapshot.key * Current.key

  (* Helper functions *)
  let guard (f : unit -> 'a Lwt.t) =
    Lwt.try_bind
      f
      (fun res -> Lwt.return (Ok res))
      (fun exn -> Lwt.return (Error exn))

  let assert_trees_equality (t_s, t_c) =
    let open Lwt_syntax in
    let* res = compare_durable_storages t_s t_c in
    match res with
    | Error (snapshot_str, current_str) ->
        Assert.fail_msg
          "Tree states diverged: snapshot = %s vs current = %s"
          snapshot_str
          current_str
    | _ -> Lwt.return_unit

  (* Motivation behind this function that
     we would like to be able to test exceptions
     thrown from Snapshot durable and Current on equality.

     Without this function there are two different sets of
     exceptions:
       Tezos_scoru_wasm_durable_snapshot.Durable.Value_not_found
       Tezos_scoru_wasm.Durable.Value_not_found
     even though essentially it's the same exception.
  *)
  let convert_to_snapshot_durable_exception (e : exn) =
    Tezos_scoru_wasm_durable_snapshot.Durable.(
      match e with
      | Tezos_scoru_wasm.Durable.Invalid_key k -> Invalid_key k
      | Tezos_scoru_wasm.Durable.Index_too_large i -> Index_too_large i
      | Tezos_scoru_wasm.Durable.Value_not_found -> Value_not_found
      | Tezos_scoru_wasm.Durable.Tree_not_found -> Tree_not_found
      | Tezos_scoru_wasm.Durable.Out_of_bounds b -> Out_of_bounds b
      | Tezos_scoru_wasm.Durable.Durable_empty -> Durable_empty
      | Tezos_scoru_wasm.Durable.Readonly_value -> Readonly_value
      | Tezos_scoru_wasm.Durable.IO_too_large -> IO_too_large
      | e -> e)

  let ensure_same_outcome (type a) ~(pp : Format.formatter -> a -> unit)
      ~(eq : a -> a -> bool) (f_s : unit -> (a * Snapshot.t) Lwt.t)
      (f_c : unit -> (a * Current.t) Lwt.t) :
      (a * (Snapshot.t * Current.t)) Lwt.t =
    let open Lwt_syntax in
    let* outcome_snapshot = guard f_s in
    let* outcome_current = guard f_c in
    match (outcome_snapshot, outcome_current) with
    | Error error_snapshot, Error error_current ->
        Assert.equal
          ~loc:__LOC__
          ~msg:
            (Format.asprintf
               "Tree methods failed with different exceptions: %s vs %s"
               (Printexc.to_string error_snapshot)
               (Printexc.to_string error_current))
          error_snapshot
          (convert_to_snapshot_durable_exception error_current) ;
        raise error_snapshot
    | Ok (val_snapshot, tree_snapshot), Ok (val_current, tree_current) ->
        Assert.equal
          ~loc:__LOC__
          ~msg:"Returned values from durables are not equal"
          ~pp
          ~eq
          val_snapshot
          val_current ;
        let+ () =
          if Config.compare_durables_after_each_operation then
            assert_trees_equality (tree_snapshot, tree_current)
          else Lwt.return_unit
        in
        (val_current, (tree_snapshot, tree_current))
    | Ok (val_snapshot, _), Error error_current ->
        Assert.fail_msg
          "Expected returned value %a but failed with error %s"
          pp
          val_snapshot
          (Printexc.to_string error_current)
    | Error error_snapshot, Ok (val_current, _) ->
        Assert.fail_msg
          "Expected to fail with error %s but value returned %a"
          (Printexc.to_string error_snapshot)
          pp
          val_current

  let same_trees (f_s : unit -> Snapshot.t Lwt.t)
      (f_c : unit -> Current.t Lwt.t) : (Snapshot.t * Current.t) Lwt.t =
    let open Lwt_syntax in
    let add_unit r = ((), r) in
    let+ _, trees =
      ensure_same_outcome
        ~pp:(fun fmt _ -> Format.fprintf fmt "unit")
        ~eq:(fun _ _ -> true)
        (fun () -> Lwt.map add_unit @@ f_s ())
        (fun () -> Lwt.map add_unit @@ f_c ())
    in
    trees

  let same_values (type a) ~pp ~eq (f_s : unit -> (a * Snapshot.t) Lwt.t)
      (f_c : unit -> (a * Current.t) Lwt.t) : a Lwt.t =
    Lwt.map fst @@ ensure_same_outcome ~pp ~eq f_s f_c

  let add_tree tree f = Lwt.map (fun r -> (r, tree)) f

  (* Actual methods implementation starts here *)

  let encoding =
    let open Tezos_tree_encoding in
    let paired = tup2 ~flatten:true Snapshot.encoding Current.encoding in
    (* Make sure that trees are the same when decoded.
       Check for encoding can be omitted as we anyway support equality invariant.
    *)
    conv_lwt
      (fun t -> Lwt.map (Fun.const t) @@ assert_trees_equality t)
      Lwt.return
      paired

  let max_key_length =
    Assert.Int.equal
      ~loc:__LOC__
      ~msg:"max_key_length different for Snapshot and Current"
      Snapshot.max_key_length
      Current.max_key_length ;
    Current.max_key_length

  let key_of_string_exn key =
    let res1 = try Ok (Snapshot.key_of_string_exn key) with e -> Error e in
    let res2 = try Ok (Current.key_of_string_exn key) with e -> Error e in
    match (res1, res2) with
    | Error e1, Error e2 ->
        Assert.equal
          ~loc:__LOC__
          ~msg:
            (Format.asprintf
               "key_of_string_exn failed with different exceptions: %s vs %s"
               (Printexc.to_string e1)
               (Printexc.to_string e2))
          e1
          e2 ;
        raise e2
    | Ok k1, Ok k2 -> (k1, k2)
    | Ok _k1, Error e2 ->
        Assert.fail_msg
          "Result of key_of_string_exn diverged: Snapshot returned a value, \
           Current failed with error %s for key %s"
          (Printexc.to_string e2)
          key
    | Error e1, Ok _k2 ->
        Assert.fail_msg
          "Result of key_of_string_exn diverged: Snapshot failed with error \
           %s, Current returned a value for key %s"
          (Printexc.to_string e1)
          key

  let key_of_string_opt key =
    match (Snapshot.key_of_string_opt key, Current.key_of_string_opt key) with
    | None, None -> None
    | Some k1, Some k2 -> Some (k1, k2)
    | Some _k1, None ->
        Assert.fail_msg
          "Result of key_of_string_opt diverged: Snapshot returned Some, \
           current returned None for key %s"
          key
    | None, Some _k2 ->
        Assert.fail_msg
          "Result of key_of_string_opt diverged: Snapshot returned None, \
           current returned Some for key %s"
          key

  let find_value (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:(fun fmt b -> (Fmt.option Hex.pp) fmt (Option.map Hex.of_bytes b))
      ~eq:(Option.equal Bytes.equal)
      (fun () -> add_tree tree_s @@ Snapshot.find_value tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.find_value tree_c key_c)

  let find_value_exn (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:(fun fmt b -> Hex.pp fmt (Hex.of_bytes b))
      ~eq:Bytes.equal
      (fun () -> add_tree tree_s @@ Snapshot.find_value_exn tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.find_value_exn tree_c key_c)

  let copy_tree_exn (tree_s, tree_c) ?edit_readonly (from_key_s, from_key_c)
      (to_key_s, to_key_c) =
    same_trees
      (fun () ->
        Snapshot.copy_tree_exn tree_s ?edit_readonly from_key_s to_key_s)
      (fun () ->
        Current.copy_tree_exn tree_c ?edit_readonly from_key_c to_key_c)

  let move_tree_exn (tree_s, tree_c) (from_key_s, from_key_c)
      (to_key_s, to_key_c) =
    same_trees
      (fun () -> Snapshot.move_tree_exn tree_s from_key_s to_key_s)
      (fun () -> Current.move_tree_exn tree_c from_key_c to_key_c)

  let list (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:(Fmt.list ~sep:Fmt.semi Fmt.string)
      ~eq:(List.equal String.equal)
      (fun () -> add_tree tree_s @@ Snapshot.list tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.list tree_c key_c)

  let count_subtrees (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:Fmt.int
      ~eq:Int.equal
      (fun () -> add_tree tree_s @@ Snapshot.count_subtrees tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.count_subtrees tree_c key_c)

  let subtree_name_at (tree_s, tree_c) (key_s, key_c) n =
    same_values
      ~pp:Fmt.string
      ~eq:String.equal
      (fun () -> add_tree tree_s @@ Snapshot.subtree_name_at tree_s key_s n)
      (fun () -> add_tree tree_c @@ Current.subtree_name_at tree_c key_c n)

  let delete ?edit_readonly (tree_s, tree_c) (key_s, key_c) =
    same_trees
      (fun () -> Snapshot.delete ?edit_readonly tree_s key_s)
      (fun () -> Current.delete ?edit_readonly tree_c key_c)

  let hash (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:(Fmt.option Context_hash.pp)
      ~eq:(Option.equal Context_hash.equal)
      (fun () -> add_tree tree_s @@ Snapshot.hash tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.hash tree_c key_c)

  let hash_exn (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:Context_hash.pp
      ~eq:Context_hash.equal
      (fun () -> add_tree tree_s @@ Snapshot.hash_exn tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.hash_exn tree_c key_c)

  let set_value_exn (tree_s, tree_c) ?edit_readonly (key_s, key_c) bytes =
    same_trees
      (fun () -> Snapshot.set_value_exn tree_s ?edit_readonly key_s bytes)
      (fun () -> Current.set_value_exn tree_c ?edit_readonly key_c bytes)

  let write_value_exn (tree_s, tree_c) ?edit_readonly (key_s, key_c) offset
      bytes =
    same_trees
      (fun () ->
        Snapshot.write_value_exn tree_s ?edit_readonly key_s offset bytes)
      (fun () ->
        Current.write_value_exn tree_c ?edit_readonly key_c offset bytes)

  let read_value_exn (tree_s, tree_c) (key_s, key_c) offset len =
    same_values
      ~pp:Fmt.string
      ~eq:String.equal
      (fun () ->
        add_tree tree_s @@ Snapshot.read_value_exn tree_s key_s offset len)
      (fun () ->
        add_tree tree_c @@ Current.read_value_exn tree_c key_c offset len)
end

module Paired_durable = Make_paired_durable (struct
  let compare_durables_after_each_operation = true
end)
