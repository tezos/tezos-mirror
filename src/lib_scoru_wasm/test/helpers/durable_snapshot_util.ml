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

  val hash : kind:[`Subtree | `Value] -> t -> key -> Context_hash.t option Lwt.t

  val hash_exn : kind:[`Subtree | `Value] -> t -> key -> Context_hash.t Lwt.t

  val set_value_exn : t -> ?edit_readonly:bool -> key -> string -> t Lwt.t

  val write_value_exn :
    t -> ?edit_readonly:bool -> key -> int64 -> string -> t Lwt.t

  val read_value_exn : t -> key -> int64 -> int64 -> string Lwt.t

  module Internal_for_tests : sig
    val key_to_list : key -> string list
  end
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

  let coerce_kind = function `Value -> Value | `Subtree -> Directory

  let hash ~kind tree key = hash ~kind:(coerce_kind kind) tree key

  let hash_exn ~kind tree key = hash_exn ~kind:(coerce_kind kind) tree key

  let delete = delete ~kind:Directory

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
module Make_verifiable_current_durable (Config : sig
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

  let hash ~kind (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:(Fmt.option Context_hash.pp)
      ~eq:(Option.equal Context_hash.equal)
      (fun () -> add_tree tree_s @@ Snapshot.hash ~kind tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.hash ~kind tree_c key_c)

  let hash_exn ~kind (tree_s, tree_c) (key_s, key_c) =
    same_values
      ~pp:Context_hash.pp
      ~eq:Context_hash.equal
      (fun () -> add_tree tree_s @@ Snapshot.hash_exn ~kind tree_s key_s)
      (fun () -> add_tree tree_c @@ Current.hash_exn ~kind tree_c key_c)

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

  module Internal_for_tests = struct
    let key_to_list (k1, k2) =
      let s1 = Snapshot.Internal_for_tests.key_to_list k1 in
      let s2 = Current.Internal_for_tests.key_to_list k2 in
      Assert.equal
        ~eq:(List.equal String.equal)
        ~loc:__LOC__
        ~msg:
          "String key representation are different for Snapshot and Current \
           storages"
        s1
        s2 ;
      s2
  end
end

(* Wrapper around tested durable which keeps track some
   statistic, also might be used for debug tracing.
*)
module Traceable_durable = struct
  module type S = sig
    include Testable_durable_sig

    val print_collected_statistic : unit -> unit
  end

  module Default_traceable_config = struct
    let print_operations : Durable_operation.Set.t =
      Durable_operation.Set.of_list Durable_operation.all_operation_tags

    let count_methods_invocations = true
  end

  module type Traceable_config = module type of Default_traceable_config

  module Make (Config : Traceable_config) (D : Testable_durable_sig) :
    S with type t = D.t and type key = D.key = struct
    open Durable_operation

    type st = {succ : int; fails : int}

    let method_invocations : st Map.t ref = ref Map.empty

    let tot_method_invocations : int ref = ref 0

    type t = D.t

    type key = D.key

    let is_op_printable (kind : _ operation_kind) =
      Set.mem (Operation_tag kind) Config.print_operations

    let inspect_op (type input) (op : input operation_kind) (inp : input)
        (is_succ : 'a -> bool) (operation : unit -> 'a Lwt.t) : 'a Lwt.t =
      let inc f =
        if Config.count_methods_invocations then
          method_invocations :=
            Map.update
              (Operation_tag op)
              (Option.fold
                 ~none:(Some (f {succ = 0; fails = 0}))
                 ~some:(fun x -> Some (f x)))
              !method_invocations
      in
      let inc_succ () = inc (fun t -> {t with succ = t.succ + 1}) in
      let inc_fails () = inc (fun t -> {t with fails = t.fails + 1}) in
      tot_method_invocations := !tot_method_invocations + 1 ;
      Lwt.try_bind
        operation
        (fun a ->
          if is_succ a then inc_succ () else inc_fails () ;
          if is_op_printable op then
            Format.printf
              "%4d: %a completed normally\n\n"
              !tot_method_invocations
              Durable_operation.pp
              (Operation (op, inp)) ;
          Lwt.return a)
        (fun exn ->
          inc_fails () ;
          if is_op_printable op then
            Format.printf
              "%4d: %a completed with an exception: %s\n\n"
              !tot_method_invocations
              Durable_operation.pp
              (Operation (op, inp))
              (Printexc.to_string exn) ;
          raise exn)

    let encoding = D.encoding

    let max_key_length = D.max_key_length

    let key_of_string_exn = D.key_of_string_exn

    let key_of_string_opt = D.key_of_string_opt

    let find_value dur key =
      inspect_op
        Find_value
        (D.Internal_for_tests.key_to_list key)
        Option.is_some
      @@ fun () -> D.find_value dur key

    let find_value_exn dur key =
      inspect_op
        Find_value_exn
        (D.Internal_for_tests.key_to_list key)
        (Fun.const true)
      @@ fun () -> D.find_value_exn dur key

    let copy_tree_exn dur ?(edit_readonly = false) key_from key_to =
      inspect_op
        Copy_tree_exn
        ( edit_readonly,
          D.Internal_for_tests.key_to_list key_from,
          D.Internal_for_tests.key_to_list key_to )
        (Fun.const true)
      @@ fun () -> D.copy_tree_exn dur ~edit_readonly key_from key_to

    let move_tree_exn dur key_from key_to =
      inspect_op
        Move_tree_exn
        ( D.Internal_for_tests.key_to_list key_from,
          D.Internal_for_tests.key_to_list key_to )
        (Fun.const true)
      @@ fun () -> D.move_tree_exn dur key_from key_to

    let list dur key =
      inspect_op List (D.Internal_for_tests.key_to_list key) (fun l ->
          not (List.is_empty l))
      @@ fun () -> D.list dur key

    let count_subtrees dur key =
      inspect_op Count_subtrees (D.Internal_for_tests.key_to_list key) (fun l ->
          l > 0)
      @@ fun () -> D.count_subtrees dur key

    let subtree_name_at dur key sibling_id =
      inspect_op
        Substree_name_at
        (D.Internal_for_tests.key_to_list key, sibling_id)
        (Fun.const true)
      @@ fun () -> D.subtree_name_at dur key sibling_id

    let delete ?(edit_readonly = false) dur key =
      inspect_op
        Delete
        (edit_readonly, D.Internal_for_tests.key_to_list key)
        (Fun.const true)
      @@ fun () -> D.delete ~edit_readonly dur key

    let hash ~kind dur key =
      inspect_op
        Hash
        (D.Internal_for_tests.key_to_list key, kind)
        Option.is_some
      @@ fun () -> D.hash ~kind dur key

    let hash_exn ~kind dur key =
      inspect_op
        Hash_exn
        (D.Internal_for_tests.key_to_list key, kind)
        (Fun.const true)
      @@ fun () -> D.hash_exn ~kind dur key

    let set_value_exn dur ?(edit_readonly = false) key value =
      inspect_op
        Set_value_exn
        (edit_readonly, D.Internal_for_tests.key_to_list key, value)
        (Fun.const true)
      @@ fun () -> D.set_value_exn dur ~edit_readonly key value

    let write_value_exn dur ?(edit_readonly = false) key offset value =
      inspect_op
        Write_value_exn
        (edit_readonly, D.Internal_for_tests.key_to_list key, offset, value)
        (Fun.const true)
      @@ fun () -> D.write_value_exn dur ~edit_readonly key offset value

    let read_value_exn dur key offset len =
      inspect_op
        Read_value_exn
        (D.Internal_for_tests.key_to_list key, offset, len)
        (Fun.const true)
      @@ fun () -> D.read_value_exn dur key offset len

    module Internal_for_tests = D.Internal_for_tests

    let print_collected_statistic () =
      let collected = Map.bindings !method_invocations in
      let sm = !tot_method_invocations in
      let to_perc (x : int) (tot : int) =
        Float.(div (of_int (Int.mul 100 x)) (of_int tot))
      in
      Format.printf "Methods invocation statistic, total invocations: %d\n" sm ;
      List.iter
        (fun (op, st) ->
          Format.printf
            "%4s%a: %.1f%% of all ops\n%8sSuccessful: %.1f%% / Fails: %.1f%%\n"
            ""
            Durable_operation.pp_operation_tag
            op
            (to_perc (st.succ + st.fails) sm)
            ""
            (to_perc st.succ (st.succ + st.fails))
            (to_perc st.fails (st.succ + st.fails)))
        collected
  end
end

module Verifiable_current_durable = Make_verifiable_current_durable (struct
  let compare_durables_after_each_operation = false
end)
