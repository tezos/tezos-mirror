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
   Invocation: dune exec src/lib_layer2_store/test/test_indexed_store.exe -- -v
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

(** Signature for type equipped with a generator and a pretty printing
    function. *)
module type GENERATABLE = sig
  type t

  val gen : t QCheck2.Gen.t

  val pp : Format.formatter -> t -> unit
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
    let gen = string_size ~gen:printable size_gen in
    graft_corners gen [String.init size (fun _ -> '\000')] ()

  let gen_distinct distinct_keys = QCheck2.Gen.list_repeat distinct_keys gen

  let pp fmt s = Format.fprintf fmt "%S" s

  let encoding = Data_encoding.Fixed.string size
end

(** Used for singleton store tests which do not need keys. *)
module NoKey = struct
  type t = unit

  let _name = "no_key"

  let gen = QCheck2.Gen.pure ()

  let pp _ () = ()
end

(** Module to generate values for the stores, as unbounded byte sequences. *)
module Value = struct
  type t = bytes

  let name = "bytes_value"

  let gen = QCheck2.Gen.bytes

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
    bytes_size size_gen

  let pp = Value.pp

  let encoding = Data_encoding.Fixed.bytes size
end

module Action (Key : GENERATABLE) (Value : GENERATABLE) = struct
  (** Actions for a key-value store whose keys are [Key.t] and values are
      [Value.t]. *)
  type t = Write of Key.t * Value.t | Read of Key.t | Delete of Key.t

  (** Generator for actions. The parameter [no_delete] indicates if the
      generator should generate delete actions or not, because some append-only
      stores do not support delete. *)
  let gen ?(no_delete = false) k_gen =
    let open QCheck2.Gen in
    let* k = k_gen in
    let write =
      let+ v = Value.gen in
      Write (k, v)
    in
    let read = pure (Read k) in
    let delete = pure (Delete k) in
    let l = if no_delete then [read; write] else [read; write; delete] in
    oneof l

  let _gen_for_key k = gen (QCheck2.Gen.pure k)

  let _gen_simple = gen Key.gen

  let pp fmt = function
    | Write (k, v) -> Format.fprintf fmt "+ %a -> %a" Key.pp k Value.pp v
    | Read k -> Format.fprintf fmt "%a ?" Key.pp k
    | Delete k -> Format.fprintf fmt "- %a" Key.pp k
end

(** A scenario is a list of actions. *)
module Scenario (Key : GENERATABLE) (Value : GENERATABLE) = struct
  module Action = Action (Key) (Value)

  let gen ?no_delete keys =
    let open QCheck2.Gen in
    let key_gen = oneofl keys in
    let size = frequency [(95, small_nat); (5, nat)] in
    list_size size (Action.gen ?no_delete key_gen)

  let pp fmt =
    Format.fprintf fmt "[@[<hov 2> %a@ @]]"
    @@ Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " ;@ ")
         Action.pp

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
    (Key : GENERATABLE)
    (Value : GENERATABLE) (Store : sig
      type t

      val load : path:string -> t tzresult Lwt.t

      val read : t -> Key.t -> Value.t option tzresult Lwt.t

      val write : t -> Key.t -> Value.t -> unit tzresult Lwt.t

      val delete : t -> Key.t -> unit tzresult Lwt.t

      val close : t -> unit tzresult Lwt.t
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
    | _ :: executed -> last_written_value key executed

  let pp_opt_value =
    Format.pp_print_option
      ~none:(fun fmt () -> Format.pp_print_string fmt "[None]")
      Value.pp

  let check_read_value_last_write key res executed =
    let open Lwt_result_syntax in
    let last = last_written_value key executed in
    if res <> last then
      failwith
        "Read %a for key %a, but last wrote %a@."
        pp_opt_value
        res
        Key.pp
        key
        pp_opt_value
        last
    else return_unit

  (** Checks that the value associated to [key] in the store is the last written
      value is the list of executed actions.  *)
  let check_read_last_write store executed key =
    let open Lwt_result_syntax in
    let* res = Store.read store key in
    check_read_value_last_write key res executed

  (** Checks that the store and the witness agree on the value associated to
      [key] .  *)
  let check_store_agree_witness store witness key =
    let open Lwt_result_syntax in
    let* store_res = Store.read store key in
    let witness_res = Stdlib.Hashtbl.find_opt witness key in
    if store_res <> witness_res then
      failwith
        "Read %a from store for key %a, but hash table witness contains wrote \
         %a@."
        pp_opt_value
        store_res
        Key.pp
        key
        pp_opt_value
        witness_res
    else return_unit

  let check_store_agree_witness store witness keys =
    KeySet.iter_es (check_store_agree_witness store witness) keys

  let run scenario =
    let open Lwt_result_syntax in
    incr uid ;
    (* To avoid any conflict with previous runs of this test. *)
    let pid = Unix.getpid () in
    let path =
      Filename.(concat @@ get_temp_dir_name ())
        (Format.sprintf "tezos-layer2-indexed-store-test-%d-%d" pid !uid)
    in
    (* Use use a hash table as a witness for the result of our scenario. Each
       action is performed both on the witness (in memory) and the real
       store. *)
    (* Actions on the real witness. *)
    let witness = Stdlib.Hashtbl.create 9 in
    let last_witness_read = ref None in
    let run_witness_action = function
      | Scenario.Action.Write (k, v) -> Stdlib.Hashtbl.replace witness k v
      | Read k ->
          let res = Stdlib.Hashtbl.find_opt witness k in
          last_witness_read := res
      | Delete k -> Stdlib.Hashtbl.remove witness k
    in
    (* Actions on the real store. *)
    let* store = Store.load ~path in
    let last_store_read = ref None in
    let run_store_action executed = function
      | Scenario.Action.Write (k, v) -> Store.write store k v
      | Read k ->
          let* res = Store.read store k in
          last_store_read := res ;
          check_read_value_last_write k res executed
      | Delete k -> Store.delete store k
    in
    (* Inner loop to run actions. It returns the keys of the scenario and the
       executed actions. *)
    let rec run_actions keys executed = function
      | [] -> return (keys, executed)
      | a :: rest ->
          run_witness_action a ;
          let* () = run_store_action executed a in
          let (Write (k, _) | Read k | Delete k) = a in
          let keys = KeySet.add k keys in
          run_actions keys (a :: executed) rest
    in
    let* keys, executed = run_actions KeySet.empty [] scenario in
    (* Check that the read value is the last write for all keys at the end. *)
    let* () = KeySet.iter_es (check_read_last_write store executed) keys in
    (* Check that the store and witness agree at the end. *)
    let* () = check_store_agree_witness store witness keys in
    let* () = Store.close store in
    (* To clear the caches (of the stores, etc.), we close and reopen it. We
       then check that the version on disk still agrees with the witness. *)
    let* store = Store.load ~path in
    let* () = check_store_agree_witness store witness keys in
    let* () = Store.close store in
    return keys

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

let test_singleton =
  let module Singleton_for_test = struct
    module S = Make_singleton (Value)

    type t = rw S.t

    let load = S.load Read_write

    let read s () = S.read s

    let write s () v = S.write s v

    let delete s () = S.delete s

    let close _ = Lwt_result_syntax.return_unit
  end in
  let module R = Runner (NoKey) (Value) (Singleton_for_test) in
  let open QCheck2 in
  let test_gen = R.Scenario.gen [()] in
  Test.make
    ~print:R.Scenario.print
    ~name:"singleton store"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    test_gen
    R.check_run

let test_indexable =
  let module Indexable_for_test = struct
    module S =
      Make_indexable
        (struct
          let name = "indexable_test"
        end)
        (SKey)
        (Make_index_value (Make_fixed_encodable (FixedValue)))

    type t = rw S.t

    let load = S.load Read_write

    let read s k = S.find s k

    let write s k v = S.add s k v

    let delete _ _ = assert false

    let close = S.close
  end in
  let module R = Runner (SKey) (FixedValue) (Indexable_for_test) in
  let open QCheck2 in
  let test_gen =
    let open Gen in
    let* n = int_range 2 10 in
    let* keys = SKey.gen_distinct n in
    R.Scenario.gen ~no_delete:true keys
  in
  Test.make
    ~print:R.Scenario.print
    ~name:"indexable store"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    test_gen
    R.check_run

let test_indexable_removable =
  let module Indexable_for_test = struct
    module S =
      Make_indexable_removable
        (struct
          let name = "indexable_removable_test"
        end)
        (SKey)
        (Make_index_value (Make_fixed_encodable (FixedValue)))

    type t = rw S.t

    let load = S.load Read_write

    let read s k = S.find s k

    let write s k v = S.add s k v

    let delete s k = S.remove s k

    let close = S.close
  end in
  let module R = Runner (SKey) (FixedValue) (Indexable_for_test) in
  let open QCheck2 in
  let test_gen =
    let open Gen in
    let* n = int_range 2 10 in
    let* keys = SKey.gen_distinct n in
    R.Scenario.gen keys
  in
  Test.make
    ~print:R.Scenario.print
    ~name:"indexable removable store"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    test_gen
    R.check_run

let test_indexed_file =
  let module Indexed_file_for_test = struct
    module S =
      Make_simple_indexed_file
        (struct
          let name = "indexed_file"
        end)
        (Make_index_key (struct
          include Make_fixed_encodable (SKey)

          let equal = String.equal
        end))
        (struct
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
        end)

    type t = rw S.t

    let load ~path = S.load ~path ~cache_size:10 Read_write

    open Lwt_result_syntax

    let read s k =
      let+ v = S.read s k in
      Option.map fst v

    let write s k v = S.append s ~key:k ~value:v

    let delete _ _ = assert false

    let close = S.close
  end in
  let module R = Runner (SKey) (Value) (Indexed_file_for_test) in
  let open QCheck2 in
  let test_gen =
    let open Gen in
    let* n = int_range 2 10 in
    let* keys = SKey.gen_distinct n in
    R.Scenario.gen ~no_delete:true keys
  in
  Test.make
    ~print:R.Scenario.print
    ~name:"indexed file store"
    ~count:2_000
    ~max_fail:1_000 (*to stop shrinking after [max_fail] failures. *)
    test_gen
    R.check_run

let () =
  QCheck_base_runner.run_tests_main
    [
      test_singleton; test_indexable; test_indexable_removable; test_indexed_file;
    ]
