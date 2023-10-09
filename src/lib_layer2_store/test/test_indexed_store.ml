(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* Testing
   -------
   Component:  Layer-2 indexed store
   Invocation: dune exec src/lib_layer2_store/test/main.exe \
                  -- --file test_indexed_store.ml
   Subject:    Test the indexed store
*)

(** The properties checked are:
    - no errors
    - read value is the same as the last written one
    - resulting store agrees with equivalent hash table
    - reloading values from disk agrees with version with cache
*)

open Error_monad
open Store_sigs
open Indexed_store

let default_index_buffer_size = 10_000

(** Signature for type equipped with a generator and a pretty printing
    function. *)
module type GENERATABLE = sig
  type t

  val gen : t QCheck2.Gen.t

  val pp : Format.formatter -> t -> unit
end

module type GENERATABLE_DISTINCT = sig
  include GENERATABLE

  val gen_distinct : int -> t list QCheck2.Gen.t
end

(** Keys as strings of 32 characters used for tests *)
module SKey = struct
  let size = 32

  let name = "key_string_32"

  include Index.Key.String_fixed (struct
    let length = size
  end)

  let gen =
    let open QCheck2.Gen in
    let size_gen = pure size in
    let c =
      let+ code = 97 -- 122 in
      Char.chr code
    in
    let gen = string_size ~gen:c size_gen in
    graft_corners gen [String.init size (fun _ -> '\000')] () |> no_shrink

  let gen_distinct distinct_keys = QCheck2.Gen.list_repeat distinct_keys gen

  let pp fmt s = Format.fprintf fmt "%S" s

  let encoding = Data_encoding.Fixed.string size
end

module Int32Key = struct
  type t = Int32.t

  let fixed_size = 4

  let name = "key_int32"

  let gen = QCheck2.Gen.int32

  let gen_distinct distinct_keys = QCheck2.Gen.list_repeat distinct_keys gen

  let pp fmt i = Format.fprintf fmt "%ld" i

  let encoding = Data_encoding.int32

  let equal = Int32.equal
end

(** Used for singleton store tests which do not need keys. *)
module NoKey = struct
  type t = unit

  let _name = "no_key"

  let gen = QCheck2.Gen.pure ()

  let gen_distinct = function
    | 0 -> QCheck2.Gen.pure []
    | 1 -> QCheck2.Gen.pure [()]
    | _ -> assert false

  let pp _ () = ()
end

(** Module to generate values for the stores, as unbounded byte sequences. *)
module Value = struct
  type t = bytes

  let name = "bytes_value"

  let gen = QCheck2.Gen.(bytes |> no_shrink)

  let pp fmt b = Hex.of_bytes b |> Hex.show |> Format.fprintf fmt "%S"

  let encoding = Data_encoding.bytes
end

(** Module to generate fixed size values for the stores, byte sequences (of size
    500 here). *)
module FixedValue = struct
  type t = bytes

  let size = 500

  let name = "fixed_bytes_value_500"

  let gen =
    let open QCheck2.Gen in
    let size_gen = pure size in
    bytes_size size_gen |> no_shrink

  let pp = Value.pp

  let encoding = Data_encoding.Fixed.bytes size
end

module Action (Key : GENERATABLE_DISTINCT) (Value : GENERATABLE) = struct
  (** Actions for a key-value store whose keys are [Key.t] and values are
      [Value.t]. *)
  type t =
    | Write of Key.t * Value.t
    | Read of Key.t
    | Delete of Key.t
    | GC of Key.t list

  (** Generator for actions. The parameter [no_delete] indicates if the
      generator should generate delete actions or not, because some append-only
      stores do not support delete. *)
  let gen ?(no_delete = false) ?(gc = false) k_gen =
    let open QCheck2.Gen in
    let* k = k_gen in
    let write =
      let+ v = Value.gen in
      Write (k, v)
    in
    let read = pure (Read k) in
    let delete = pure (Delete k) in
    let gc_act =
      let s = int_range 2 18 in
      let+ retain = list_size s k_gen in
      GC retain
    in
    let l = [read; write] in
    let l = if no_delete then l else l @ [delete] in
    let l = if gc then l @ [gc_act] else l in
    oneof l

  let _gen_for_key k = gen (QCheck2.Gen.pure k)

  let _gen_simple = gen Key.gen

  let pp fmt = function
    | Write (k, _v) -> Format.fprintf fmt "+ %a" Key.pp k
    | Read k -> Format.fprintf fmt "%a ?" Key.pp k
    | Delete k -> Format.fprintf fmt "- %a" Key.pp k
    | GC l -> Format.fprintf fmt "GC [%a]" (Format.pp_print_list Key.pp) l

  let keys = function Write (k, _) | Read k | Delete k -> [k] | GC ks -> ks

  let parallelizable_with action parallel_actions =
    match action with
    | Read k ->
        (* Can be in parallel with other reads, and writes on other keys than k *)
        List.for_all
          (function
            | Read _ -> true
            | Write (k', _) | Delete k' -> k <> k'
            | GC ks -> Stdlib.List.mem k ks)
          parallel_actions
    | Write (k, _) | Delete k ->
        (* Can be in parallel with actions on other keys *)
        List.for_all
          (function
            | GC ks -> Stdlib.List.mem k ks
            | a -> not (Stdlib.List.mem k (keys a)))
          parallel_actions
    | GC _ -> false
end

(** A scenario is a parallelizable list of sequence of actions. Sequential tests
    have only a single list. *)
module Scenario (Key : GENERATABLE_DISTINCT) (Value : GENERATABLE) = struct
  module Action = Action (Key) (Value)

  module KeyMap = Map.Make (struct
    type t = Key.t

    let compare = Stdlib.compare
  end)

  let fill_table keys =
    let open QCheck2.Gen in
    List.rev_map
      (fun k ->
        let+ v = Value.gen in
        Action.Write (k, v))
      keys
    |> List.rev |> flatten_l

  let gen_sequence ?no_delete ?gc ?(fill = false) keys =
    let open QCheck2.Gen in
    let key_gen = oneofl keys in
    let size = frequency [(95, small_nat); (5, nat)] in
    let* seq = list_size size (Action.gen ?no_delete ?gc key_gen) in
    if fill then
      let+ fill_seq = fill_table keys |> no_shrink in
      fill_seq @ seq
    else return seq

  let gen_sequential ?no_delete ?gc ?fill keys =
    let open QCheck2.Gen in
    let+ sequence = gen_sequence ?no_delete ?gc ?fill keys in
    List.rev_map (fun a -> [a]) sequence |> List.rev

  let parallelize sequence =
    let l =
      List.fold_left
        (fun acc action ->
          match acc with
          | parallel_actions :: previous
            when Action.parallelizable_with action parallel_actions ->
              (action :: parallel_actions) :: previous
          | previous -> [action] :: previous)
        []
        sequence
    in
    List.rev_map List.rev l

  let gen_parallel ?no_delete ?gc ?fill keys =
    let open QCheck2.Gen in
    let+ sequence = gen_sequence ?no_delete ?gc ?fill keys in
    parallelize sequence

  let gen ?no_delete ?gc ?fill keys kind =
    match kind with
    | `Sequential -> gen_sequential ?no_delete ?gc ?fill keys
    | `Parallel -> gen_parallel ?no_delete ?gc ?fill keys

  let pp_parallel fmt =
    Format.fprintf fmt "[@[<hov 2> %a@ @]]"
    @@ Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ||@ ")
         Action.pp

  let pp fmt =
    Format.fprintf fmt "[@[<hov 2> %a@ @]]"
    @@ Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ;@ ")
         pp_parallel

  let print = Format.asprintf "%a" pp
end

(* Because a scenario creates files onto the disk, we need a way to
   generate unique names. For debugging purpose, and because of the
   shrinking of QCheck2, it is easier to track tries with a simple
   counter. *)
let uid = ref 0

(** This functor produces a [check_run] functions that plays a scenario and runs
    checks on it. *)
module Runner
    (Key : GENERATABLE_DISTINCT)
    (Value : GENERATABLE) (Store : sig
      type t

      val load : path:string -> t tzresult Lwt.t

      val read : t -> Key.t -> Value.t option tzresult Lwt.t

      val write : t -> Key.t -> Value.t -> unit tzresult Lwt.t

      val delete : t -> Key.t -> unit tzresult Lwt.t

      val close : t -> unit tzresult Lwt.t

      val gc : t -> retain:Key.t list -> unit tzresult Lwt.t
    end) =
struct
  module Scenario = Scenario (Key) (Value)

  module KeySet = Set.Make (struct
    type t = Key.t

    let compare = Stdlib.compare
  end)

  (** Retrieve the last written value from a list of executed actions (from
      newest to oldest).  *)
  let rec last_written_value key = function
    | [] -> None
    | Scenario.Action.Delete k :: _ when k = key -> None
    | Write (k, v) :: _ when k = key -> Some v
    | GC ks :: _ when not (Stdlib.List.mem key ks) -> None
    | _ :: executed -> last_written_value key executed

  let pp_opt_value =
    Format.pp_print_option
      ~none:(fun fmt () -> Format.pp_print_string fmt "[None]")
      Value.pp

  let check_read_value_last_write ~message key res executed =
    let open Lwt_result_syntax in
    let last = last_written_value key executed in
    if res <> last then
      failwith
        "Read %a for key %a, but last wrote %a %s@."
        pp_opt_value
        res
        Key.pp
        key
        pp_opt_value
        last
        message
    else return_unit

  (** Checks that the value associated to [key] in the store is the last written
      value is the list of executed actions.  *)
  let check_read_last_write ~message store executed key =
    let open Lwt_result_syntax in
    let* res = Store.read store key in
    check_read_value_last_write ~message key res executed

  (** Checks that the store and the witness agree on the value associated to
      [key] .  *)
  let check_store_agree_witness ~message store witness key =
    let open Lwt_result_syntax in
    let* store_res = Store.read store key in
    let witness_res = Stdlib.Hashtbl.find_opt witness key in
    if store_res <> witness_res then
      failwith
        "Read %a from store for key %a, but hash table witness contains wrote \
         %a %s@."
        pp_opt_value
        store_res
        Key.pp
        key
        pp_opt_value
        witness_res
        message
    else return_unit

  let check_store_agree_witness ~message store witness keys =
    KeySet.iter_es (check_store_agree_witness ~message store witness) keys

  (** Always close loaded stores to avoid leak when some tests fail. *)
  let with_store path f =
    let open Lwt_result_syntax in
    let* store = Store.load ~path in
    Lwt.finalize (fun () -> f store) @@ fun () ->
    let open Lwt_syntax in
    let* _ = Store.close store in
    return_unit

  let run_in_dir scenario dir =
    let open Lwt_result_syntax in
    let path = Filename.concat dir "store" in
    (* Use use a hash table as a witness for the result of our scenario. Each
       action is performed both on the witness (in memory) and the real
       store. *)
    (* Actions on the hash table witness. *)
    let witness = Stdlib.Hashtbl.create 9 in
    let last_witness_read = ref None in
    let run_witness_action = function
      | Scenario.Action.Write (k, v) -> Stdlib.Hashtbl.replace witness k v
      | Read k ->
          let res = Stdlib.Hashtbl.find_opt witness k in
          last_witness_read := res
      | Delete k -> Stdlib.Hashtbl.remove witness k
      | GC retain ->
          Stdlib.Hashtbl.filter_map_inplace
            (fun k v -> if Stdlib.List.mem k retain then Some v else None)
            witness
    in
    (* Actions on the real store. *)
    let* keys =
      with_store path @@ fun store ->
      let last_store_read = ref None in
      let run_store_action executed = function
        | Scenario.Action.Write (k, v) -> Store.write store k v
        | Read k ->
            let* res = Store.read store k in
            last_store_read := res ;
            check_read_value_last_write ~message:"after read" k res executed
        | Delete k -> Store.delete store k
        | GC retain -> Store.gc store ~retain
      in
      (* Inner loop to run actions. It returns the keys of the scenario and the
         executed actions. *)
      let rec run_actions keys executed = function
        | [] -> return (keys, executed)
        | parallel_actions :: rest ->
            List.iter run_witness_action parallel_actions ;
            let* () =
              List.iter_ep (run_store_action executed) parallel_actions
            in
            let keys =
              KeySet.add_seq
                (List.to_seq parallel_actions
                |> Seq.flat_map (fun a -> Scenario.Action.keys a |> List.to_seq)
                )
                keys
            in
            run_actions keys (parallel_actions @ executed) rest
      in
      let* keys, executed =
        protect @@ fun () -> run_actions KeySet.empty [] scenario
      in
      (* Check that the read value is the last write for all keys at the end. *)
      let* () =
        KeySet.iter_es
          (check_read_last_write ~message:"after scenario" store executed)
          keys
      in
      (* Check that the store and witness agree at the end. *)
      let* () =
        check_store_agree_witness ~message:"after scenario" store witness keys
      in
      return keys
    in
    (* Reload the store to clear the caches (of the stores, etc.). We
       then check that the version on disk still agrees with the witness. *)
    let* () =
      with_store path @@ fun store ->
      check_store_agree_witness ~message:"after reload" store witness keys
    in
    return_unit

  let run scenario =
    Tezos_stdlib_unix.Lwt_utils_unix.with_tempdir
      "tezos-layer2-indexed-store-test-"
      (run_in_dir scenario)

  let check_run scenario =
    let promise =
      let open Lwt_result_syntax in
      let* _ = run scenario in
      return_true
    in
    match Lwt_main.run promise with
    | Ok _ -> true
    | Error err ->
        QCheck2.Test.fail_reportf "%a@." Error_monad.pp_print_trace err
end

let tests = ref []

(** Small imperative helper to create test and register it, so we don't forget
    one. *)
let register_test ?if_assumptions_fail ?count ?long_factor ?max_gen ?max_fail
    ?retries ?name ?print ?collect ?stats get prop =
  let t =
    QCheck2.Test.make
      ?if_assumptions_fail
      ?count
      ?long_factor
      ?max_gen
      ?max_fail
      ?retries
      ?name
      ?print
      ?collect
      ?stats
      get
      prop
  in
  tests := t :: !tests

let () =
  let module Singleton_for_test = struct
    module S = Make_singleton (Value)

    type t = rw S.t

    let load = S.load Read_write

    let read s () = S.read s

    let write s () v = S.write s v

    let delete s () = S.delete s

    let close _ = Lwt_result_syntax.return_unit

    let gc _ ~retain:_ = Lwt_result_syntax.return_unit
  end in
  let module R = Runner (NoKey) (Value) (Singleton_for_test) in
  let test_gen = R.Scenario.gen [()] in
  register_test
    ~print:R.Scenario.print
    ~name:"singleton store (sequential)"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    (test_gen `Sequential)
    R.check_run

module Indexable_for_test = struct
  module S =
    Make_indexable
      (struct
        let name = "indexable_test"
      end)
      (SKey)
      (Make_index_value (Make_fixed_encodable (FixedValue)))

  type t = rw S.t

  let load = S.load ~index_buffer_size:default_index_buffer_size Read_write

  let read s k = S.find s k

  let write s k v = S.add s k v

  let delete _ _ = assert false

  let close = S.close

  let gc s ~retain = S.gc ~async:false s (Retain retain)
end

let () =
  let module R = Runner (SKey) (FixedValue) (Indexable_for_test) in
  let test_gen kind =
    let open QCheck2.Gen in
    let* n = int_range 2 10 in
    let* keys = SKey.gen_distinct n in
    R.Scenario.gen ~no_delete:true keys kind
  in
  register_test
    ~print:R.Scenario.print
    ~name:"indexable store (sequential)"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    (test_gen `Sequential)
    R.check_run ;
  register_test
    ~print:R.Scenario.print
    ~name:"indexable store (parallel)"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    (test_gen `Parallel)
    R.check_run

module Indexable_removable_for_test = struct
  module S =
    Make_indexable_removable
      (struct
        let name = "indexable_removable_test"
      end)
      (SKey)
      (Make_index_value (Make_fixed_encodable (FixedValue)))

  type t = rw S.t

  let load = S.load ~index_buffer_size:default_index_buffer_size Read_write

  let read s k = S.find s k

  let write s k v = S.add s k v

  let delete s k = S.remove s k

  let close = S.close

  let gc s ~retain = S.gc ~async:false s (Retain retain)
end

let () =
  let module R = Runner (SKey) (FixedValue) (Indexable_removable_for_test) in
  let test_gen kind =
    let open QCheck2.Gen in
    let* n = int_range 2 10 in
    let* keys = SKey.gen_distinct n in
    R.Scenario.gen keys kind
  in
  register_test
    ~print:R.Scenario.print
    ~name:"indexable removable store (sequential)"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    (test_gen `Sequential)
    R.check_run ;
  register_test
    ~print:R.Scenario.print
    ~name:"indexable removable store (parallel)"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    (test_gen `Parallel)
    R.check_run

module Value_with_size_header = struct
  include Value

  module Header = struct
    type t = int32

    let name = "sum_chars"

    let encoding = Data_encoding.int32

    let fixed_size = 4
  end

  (* Header contains sum of byte codes as an example *)
  let header b =
    Bytes.fold_left (fun n c -> n + Char.code c) 0 b |> Int32.of_int
end

module Indexed_file_for_test = struct
  module S =
    Make_simple_indexed_file
      (struct
        let name = "indexed_file"
      end)
      (Make_index_key (struct
        include Make_fixed_encodable (SKey)

        let equal = String.equal
      end))
      (Value_with_size_header)

  type t = rw S.t

  let load ~path =
    S.load
      ~index_buffer_size:default_index_buffer_size
      ~path
      ~cache_size:10
      Read_write

  open Lwt_result_syntax

  let read s k =
    let+ v = S.read s k in
    Option.map fst v

  let write s k v = S.append s ~key:k ~value:v

  let delete _ _ = assert false

  let close = S.close

  let gc s ~retain = S.gc ~async:false s (Retain retain)
end

module Indexed_file_integers = struct
  module S =
    Make_simple_indexed_file
      (struct
        let name = "indexed_file_ints"
      end)
      (Make_index_key (Int32Key))
      (Value_with_size_header)

  type t = rw S.t

  let load ~path =
    S.load
      ~index_buffer_size:default_index_buffer_size
      ~path
      ~cache_size:10
      Read_write

  open Lwt_result_syntax

  let read s k =
    let+ v = S.read s k in
    Option.map fst v

  let write s k v = S.append s ~key:k ~value:v

  let delete _ _ = assert false

  let close = S.close

  let gc s ~largest ~smallest =
    S.gc
      ~async:false
      s
      (Iterator
         {
           first = largest;
           next =
             (fun i _ ->
               if i <= smallest then Lwt.return_none
               else Lwt.return_some (Int32.pred i));
         })
end

let () =
  let module R = Runner (SKey) (Value) (Indexed_file_for_test) in
  let test_gen kind =
    let open QCheck2.Gen in
    let* n = int_range 2 10 in
    let* keys = SKey.gen_distinct n in
    R.Scenario.gen ~no_delete:true keys kind
  in
  register_test
    ~print:R.Scenario.print
    ~name:"indexed file store (sequential)"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    (test_gen `Sequential)
    R.check_run ;
  register_test
    ~print:R.Scenario.print
    ~name:"indexed file store (parallel)"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    (test_gen `Parallel)
    R.check_run

(* GC tests *)

let () =
  let module R = Runner (SKey) (FixedValue) (Indexable_for_test) in
  let test_gen kind =
    let open QCheck2.Gen in
    let* keys = SKey.gen_distinct 20 in
    R.Scenario.gen ~no_delete:true ~gc:true ~fill:true keys kind
  in
  register_test
    ~print:R.Scenario.print
    ~name:"gc in indexable store (sequential)"
    ~count:20
    (test_gen `Sequential)
    R.check_run ;
  register_test
    ~print:R.Scenario.print
    ~name:"gc in indexable store (parallel)"
    ~count:20
    (test_gen `Parallel)
    R.check_run

let () =
  let module R = Runner (SKey) (Value) (Indexed_file_for_test) in
  let test_gen kind =
    let open QCheck2.Gen in
    let* keys = SKey.gen_distinct 20 in
    R.Scenario.gen ~no_delete:true ~gc:true ~fill:true keys kind
  in
  register_test
    ~print:R.Scenario.print
    ~name:"gc in indexed file store (sequential)"
    ~count:20
    (test_gen `Sequential)
    R.check_run ;
  register_test
    ~print:R.Scenario.print
    ~name:"gc in indexed file store (parallel)"
    ~count:20
    (test_gen `Parallel)
    R.check_run

(* Unit tests *)

let test_read_gc_index () =
  let module R = Runner (SKey) (FixedValue) (Indexable_for_test) in
  let scenario =
    let k1 = String.init 32 (fun _ -> 'a') in
    let k2 = String.init 32 (fun _ -> 'z') in
    let v = Bytes.init 500 (fun _ -> ' ') in
    [
      [R.Scenario.Action.Write (k1, v); Write (k2, v)];
      [Read k1];
      [GC [k1; k1]];
      [GC [k2; k2]];
    ]
  in
  R.run scenario

let test_read_gc_store () =
  let module R = Runner (SKey) (Value) (Indexed_file_for_test) in
  let scenario =
    let k1 = String.init 32 (fun _ -> 'a') in
    let k2 = String.init 32 (fun _ -> 'z') in
    let v = Bytes.of_string "b" in
    [
      [R.Scenario.Action.Write (k1, v); Write (k2, v)];
      [Read k1];
      [GC [k1; k1]];
      [GC [k2; k2]];
    ]
  in
  R.run scenario

let test_load () =
  let path = Tezt.Temp.dir "tezos-layer2-indexed-store-test-load" in
  let open Lwt_result_syntax in
  let* store =
    Indexed_file_for_test.S.load
      ~index_buffer_size:default_index_buffer_size
      ~path
      ~cache_size:1
      Read_only
  in
  let* () = Indexed_file_for_test.S.close store in
  let* store =
    Indexed_file_for_test.S.load
      ~index_buffer_size:default_index_buffer_size
      ~path
      ~cache_size:1
      Read_write
  in
  Indexed_file_for_test.S.close store

module type LOADABLE_STORE = sig
  type t

  val load : path:string -> t tzresult Lwt.t

  val close : t -> unit tzresult Lwt.t
end

let with_load_store ~load ~close ~path f =
  let open Lwt_result_syntax in
  let* store = load ~path in
  Lwt.finalize (fun () -> f store) @@ fun () ->
  let open Lwt_syntax in
  let* _ = close store in
  return_unit

let test_gc_iterator () =
  let path = Tezt.Temp.dir "indexed-store-test-gc-iterator" in
  let open Lwt_result_syntax in
  with_load_store
    ~load:Indexed_file_integers.load
    ~close:Indexed_file_integers.close
    ~path
  @@ fun store ->
  let bound = 1000l in
  let rec fill i =
    unless (i > bound) @@ fun () ->
    let* () =
      Indexed_file_integers.write store i (Int32.to_string i |> Bytes.of_string)
    in
    fill (Int32.succ i)
  in
  let* () = fill 0l in
  let* () = Indexed_file_integers.gc store ~largest:998l ~smallest:500l in
  let rec check i =
    unless (i > bound) @@ fun () ->
    let* found = Indexed_file_integers.read store i in
    let* () =
      if 500l <= i && i <= 998l then
        match found with
        | None -> failwith "Key %ld not found after GC" i
        | Some _ -> return_unit
      else
        match found with
        | Some _ -> failwith "Key %ld was not collected by the GC" i
        | None -> return_unit
    in
    check (Int32.succ i)
  in
  check 0l

let unit_tests =
  let wrap (name, t) =
    let f () =
      match Lwt_main.run (t ()) with
      | Ok () -> ()
      | Error e -> Alcotest.failf "%a" pp_print_trace e
    in
    (name, `Quick, f)
  in
  List.map
    wrap
    [
      ("load", test_load);
      ("read_gc_index", test_read_gc_index);
      ("read_gc_store", test_read_gc_store);
      ("gc_iterator", test_gc_iterator);
    ]

let () =
  Alcotest.run
    ~__FILE__
    "tezos-layer2-store"
    [
      ( "indexed-store-pbt",
        List.map QCheck_alcotest.to_alcotest (List.rev !tests) );
      ("indexed-store-unit", unit_tests);
    ]
