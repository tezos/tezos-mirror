(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** Testing
    -------
    Component:  Protocol (tx rollup l2)
    Invocation: dune exec src/proto_015_PtLimaPt/lib_protocol/test/unit/main.exe
    Subject:    test the layer-2 implementation of transaction rollup
*)

open Tztest
open Tx_rollup_l2_helpers
open Protocol
open Tx_rollup_l2_context_sig

(** {1. Storage and context tests. } *)

let wrap_test t () =
  t () >|= function
  | Ok x -> Ok x
  | Error err -> Error [Environment.Ecoproto_error err]

let wrap_tztest_tests =
  List.map (fun (name, test) -> tztest name `Quick @@ wrap_test test)

(** {2. Storage tests. } *)

type Environment.Error_monad.error += Test

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2362
   Use the Irmin store provided by [lib_context] for layer-2
   solutions, once available.
   As of now, we define a ad-hoc [STORAGE] implementation to run our
   tests, but eventually we need to actually make use of the same
   implementation as the transaction rollup node and the protocol. *)

(** [test_irmin_storage] checks that the implementation of [STORAGE]
    has the expected properties. *)
let test_irmin_storage () =
  let open Irmin_storage.Syntax in
  let store = empty_storage in

  let k1 = Bytes.of_string "k1" in
  let k2 = Bytes.of_string "k2" in
  let v1 = Bytes.of_string "v1" in
  let v2 = Bytes.of_string "v2" in

  (* 1. get (set store k1 v1) k1 == Some v1 *)
  let* store = Irmin_storage.set store k1 v1 in
  let* v1' = Irmin_storage.get store k1 in
  assert (v1' = Some v1) ;

  (* 2. k1 != k2 -> get (set store k2 v2) k1 = get store k1*)
  let* store = Irmin_storage.set store k2 v2 in
  let* v1'' = Irmin_storage.get store k1 in
  assert (v1' = v1'') ;

  (* 3. catch (fail e) f return == e *)
  let* e = catch (fail Test) (fun _ -> assert false) return in
  assert (e = Test) ;

  (* 4. get (remove store k1) k1 = None *)
  let* store = Irmin_storage.remove store k1 in
  let* v = Irmin_storage.get store k1 in
  assert (v = None) ;

  return_unit

(** {2. Context tests. } *)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2461
   A lot of l2-context properties can be property-based tested. *)

(** {3. Utils } *)

let context_with_one_addr =
  let open Context_l2 in
  let open Syntax in
  let ctxt = empty_context in
  let _, _, addr1 = gen_l2_address () in
  let+ ctxt, _, idx1 = Address_index.get_or_associate_index ctxt addr1 in
  (ctxt, idx1)

let ((_, pk, addr) as l2_addr1) = gen_l2_address ()

(** {3. Test Address_metadata.} *)

module Test_Address_medata = struct
  open Context_l2
  open Address_metadata
  open Syntax

  (** Test that an initilized metadata has a counter of zero and is correctly
      incremented. *)
  let test_init_and_incr () =
    let* ctxt, idx = context_with_one_addr in

    let* metadata = get ctxt idx in
    assert (metadata = None) ;

    let* ctxt = init_with_public_key ctxt idx pk in
    let* metadata = get ctxt idx in
    assert (metadata = Some {counter = 0L; public_key = pk}) ;

    let* ctxt = incr_counter ctxt idx in
    let* metadata = get ctxt idx in
    assert (metadata = Some {counter = 1L; public_key = pk}) ;

    return_unit

  (** Test that initializing an index to a public key fails if the index
      has already been initialized. *)
  let test_init_twice_fails () =
    let* ctxt, idx = context_with_one_addr in

    let* ctxt = init_with_public_key ctxt idx pk in

    let* () =
      expect_error
        (init_with_public_key ctxt idx pk)
        (Metadata_already_initialized (Indexable.index_exn 0l))
    in

    return_unit

  (** Test that incrementing the counter of an unknown index fails. *)
  let test_incr_unknown_index () =
    let ctxt = empty_context in

    let idx = Indexable.index_exn 0l in

    let* () =
      expect_error
        (incr_counter ctxt idx)
        (Unknown_address_index (Indexable.index_exn 0l))
    in

    return_unit

  (** Test that crediting more than {!Int64.max_int} causes an overflow. *)
  let test_counter_overflow () =
    let* ctxt, idx = context_with_one_addr in
    let* ctxt = init_with_public_key ctxt idx pk in

    let* ctxt =
      Internal_for_tests.set ctxt idx {counter = Int64.max_int; public_key = pk}
    in

    let* () = expect_error (incr_counter ctxt idx) Counter_overflow in

    return_unit

  let tests =
    wrap_tztest_tests
      [
        ("test init and increments", test_init_and_incr);
        ("test init twice fails", test_init_twice_fails);
        ("test incr unknown index", test_incr_unknown_index);
        ("test overflow counter", test_counter_overflow);
      ]
end

(** {3. Test indexes. } *)

module type S = sig
  open Context_l2

  type value

  type index = value Indexable.index

  val name : string

  val init_context_n : int -> (t * value list) m

  val count : t -> int32 m

  val set_count : t -> int32 -> t m

  val get_or_associate_index :
    t -> value -> (t * [`Created | `Existed] * index) m

  val get : t -> value -> index option m

  val too_many : Environment.Error_monad.error
end

module Test_index (Index : S) = struct
  let init_context_1 () =
    let open Context_l2.Syntax in
    let* ctxt, values = Index.init_context_n 1 in
    let value = nth_exn values 0 in
    return (ctxt, value)

  (** Test that first associating a value creates an index and getting the index
      from the value gives the same index. *)
  let test_set_and_get () =
    let open Context_l2.Syntax in
    let* ctxt, value = init_context_1 () in

    let* ctxt, created, idx1 = Index.get_or_associate_index ctxt value in
    assert (created = `Created) ;
    let* idx2 = Index.get ctxt value in

    assert (Some idx1 = idx2) ;

    return_unit

  (** Test that the empty context has no address indexes and associating a new
    address increments the count. *)
  let test_associate_fresh_index () =
    let open Context_l2.Syntax in
    let* ctxt, value = init_context_1 () in

    let* count = Index.count ctxt in
    assert (count = 0l) ;

    let* idx = Index.get ctxt value in
    assert (idx = None) ;

    let* ctxt, created, idx = Index.get_or_associate_index ctxt value in
    assert (created = `Created) ;
    let* count = Index.count ctxt in

    assert (count = 1l) ;
    assert (idx = Indexable.index_exn 0l) ;

    return_unit

  (** Test that associating twice the same value give the same index. *)
  let test_associate_value_twice () =
    let open Context_l2.Syntax in
    let* ctxt, value = init_context_1 () in

    let expected = Indexable.index_exn 0l in

    let* ctxt, created, idx = Index.get_or_associate_index ctxt value in
    assert (created = `Created) ;
    assert (idx = expected) ;

    let* idx = Index.get ctxt value in
    assert (idx = Some (Indexable.index_exn 0l)) ;

    let* ctxt, existed, idx = Index.get_or_associate_index ctxt value in
    assert (existed = `Existed) ;
    assert (idx = expected) ;

    let* count = Index.count ctxt in
    assert (count = 1l) ;

    return_unit

  let test_reach_too_many_l2 () =
    let open Context_l2.Syntax in
    let* ctxt, value = init_context_1 () in
    let* ctxt = Index.set_count ctxt Int32.max_int in

    let* () =
      expect_error (Index.get_or_associate_index ctxt value) Index.too_many
    in

    return_unit

  let tests =
    wrap_tztest_tests
      [
        (Index.name ^ ", test set and get", test_set_and_get);
        (Index.name ^ ", test associate fresh index", test_associate_fresh_index);
        ( Index.name ^ ", test associate same value twice",
          test_associate_value_twice );
        (Index.name ^ ", test the limit of indexes", test_reach_too_many_l2);
      ]
end

module Test_Address_index = Test_index (struct
  include Context_l2.Address_index

  let name = "Address"

  type value = Tx_rollup_l2_address.t

  type index = value Indexable.index

  let init_context_n n =
    let open Context_l2.Syntax in
    let ctxt = empty_context in
    let addresses = gen_n_address n in
    let addresses = List.map (fun (_, _, x) -> x) addresses in
    return (ctxt, addresses)

  let set_count = Internal_for_tests.set_count

  let too_many = Too_many_l2_addresses
end)

(** [gen_n_ticket_hash n] generates [n]  {!Alpha_context.Ticket_hash.t} based on
    {!gen_n_address} and {!make_unit_ticket_key}.

    TODO: Is there a more convenient way to forge such hashes? Are dumb hashes
    enough?
*)
let gen_n_ticket_hash n =
  let x =
    Lwt_main.run
      ( Context.init_n n () >>=? fun (_b, contracts) ->
        let addressess = gen_n_address n in
        let tickets =
          List.map2
            ~when_different_lengths:[]
            (fun contract (_, _, address) ->
              Tx_rollup_l2_helpers.make_unit_ticket_key contract address)
            contracts
            addressess
        in
        match tickets with Ok x -> return x | Error _ -> assert false )
  in

  match x with Ok x -> x | Error _ -> assert false

module Test_Ticket_index = Test_index (struct
  include Context_l2.Ticket_index

  let name = "Ticket"

  type value = Alpha_context.Ticket_hash.t

  type index = value Indexable.index

  let init_context_n n =
    let open Context_l2.Syntax in
    let ctxt = empty_context in
    let tickets = gen_n_ticket_hash n in
    return (ctxt, tickets)

  let set_count = Internal_for_tests.set_count

  let too_many = Too_many_l2_tickets
end)

module Test_Ticket_ledger = struct
  open Context_l2
  open Ticket_ledger
  open Syntax

  let ticket_idx1 = Indexable.index_exn 0l

  (** Test that crediting a ticket index to an index behaves correctly. *)
  let test_credit () =
    let* ctxt, idx1 = context_with_one_addr in

    let* amount = get ctxt ticket_idx1 idx1 in
    assert (Tx_rollup_l2_qty.(amount = zero)) ;

    let one = Tx_rollup_l2_qty.of_int64_exn 1L in
    let* ctxt = credit ctxt ticket_idx1 idx1 one in
    let* amount = get ctxt ticket_idx1 idx1 in
    assert (Tx_rollup_l2_qty.(amount = one)) ;

    return_unit

  (** Test that crediting more than {!Int64.max_int} causes an overflow. *)
  let test_credit_too_much () =
    let* ctxt, idx1 = context_with_one_addr in

    let* ctxt =
      credit ctxt ticket_idx1 idx1 (Tx_rollup_l2_qty.of_int64_exn Int64.max_int)
    in

    let* () =
      expect_error
        (credit ctxt ticket_idx1 idx1 (Tx_rollup_l2_qty.of_int64_exn Int64.one))
        Balance_overflow
    in

    return_unit

  (** Test that an index can be credited ticket indexes even if its not associated
      to an address. *)
  let test_credit_unknown_index () =
    let ctxt = empty_context in

    let* _ctxt =
      credit
        ctxt
        ticket_idx1
        (Indexable.index_exn 0l)
        (Tx_rollup_l2_qty.of_int64_exn 1L)
    in

    return_unit

  (** Test that spending a ticket from an index to another one behaves correctly *)
  let test_spend_valid () =
    let* ctxt, idx1 = context_with_one_addr in

    let* ctxt =
      credit ctxt ticket_idx1 idx1 (Tx_rollup_l2_qty.of_int64_exn 10L)
    in

    let* amount = get ctxt ticket_idx1 idx1 in
    assert (Tx_rollup_l2_qty.(amount = of_int64_exn 10L)) ;

    let* ctxt =
      spend ctxt ticket_idx1 idx1 (Tx_rollup_l2_qty.of_int64_exn 5L)
    in

    let* amount = get ctxt ticket_idx1 idx1 in
    assert (Tx_rollup_l2_qty.(amount = of_int64_exn 5L)) ;

    return_unit

  (** Test that spending a ticket without the required balance fails. *)
  let test_spend_without_balance () =
    let* ctxt, idx1 = context_with_one_addr in

    let* () =
      expect_error
        (spend ctxt ticket_idx1 idx1 (Tx_rollup_l2_qty.of_int64_exn 1L))
        Balance_too_low
    in

    return_unit

  let test_remove_empty_balance () =
    let* ctxt, idx1 = context_with_one_addr in

    let* ctxt = credit ctxt ticket_idx1 idx1 Tx_rollup_l2_qty.one in
    let* qty = Internal_for_tests.get_opt ctxt ticket_idx1 idx1 in
    assert (qty = Some Tx_rollup_l2_qty.one) ;

    let* ctxt = spend ctxt ticket_idx1 idx1 Tx_rollup_l2_qty.one in
    let* qty = Internal_for_tests.get_opt ctxt ticket_idx1 idx1 in
    assert (qty = None) ;

    let* qty = get ctxt ticket_idx1 idx1 in
    assert (qty = Tx_rollup_l2_qty.zero) ;

    return_unit

  let tests =
    wrap_tztest_tests
      [
        ("test credit", test_credit);
        ("test credit too much", test_credit_too_much);
        ("test credit unknown index", test_credit_unknown_index);
        ("test spend", test_spend_valid);
        ("test spend without required balance", test_spend_without_balance);
        ("test remove empty balance", test_remove_empty_balance);
      ]
end

(* ------ L2 Batch encodings ------------------------------------------------ *)

module Test_batch_encodings = struct
  open Lwt_result_syntax
  open Protocol.Tx_rollup_l2_batch.V1
  open Data_encoding

  (* Encoding from compact encoding *)
  let operation_content_encoding =
    Compact.make ~tag_size:`Uint8 compact_operation_content

  let operation_encoding = Compact.make ~tag_size:`Uint8 compact_operation

  let transaction_encoding = Compact.make ~tag_size:`Uint8 compact_transaction

  (* Helper functions to encode and decode *)
  let encode_content op = Binary.to_bytes_exn operation_content_encoding op

  let decode_content buffer =
    Data_encoding.Binary.of_bytes_exn operation_content_encoding buffer

  let encode_operation op = Binary.to_bytes_exn operation_encoding op

  let decode_operation buffer = Binary.of_bytes_exn operation_encoding buffer

  let encode_transaction t = Binary.to_bytes_exn transaction_encoding t

  let decode_transaction buffer =
    Binary.of_bytes_exn transaction_encoding buffer

  let operation_content_pp fmt = function
    | Transfer {destination; ticket_hash; qty} ->
        Format.fprintf
          fmt
          "@[<hov 2>Transfer:@ destination=%a,@ ticket_hash=%a,@ qty:%a@]"
          Tx_rollup_l2_address.Indexable.pp
          destination
          Tx_rollup_l2_context_sig.Ticket_indexable.pp
          ticket_hash
          Tx_rollup_l2_qty.pp
          qty
    | Withdraw {destination; ticket_hash; qty} ->
        Format.fprintf
          fmt
          "@[<hov 2>Withdraw:@ destination=%a,@ ticket_hash=%a,@ qty:%a@]"
          Tezos_crypto.Signature.V0.Public_key_hash.pp
          destination
          Alpha_context.Ticket_hash.pp
          ticket_hash
          Tx_rollup_l2_qty.pp
          qty

  let test_l2_transaction_size () =
    (* Assert the smallest operation_content size is 5 *)
    let opc =
      Transfer
        {
          destination = Indexable.from_index_exn 0l;
          ticket_hash = Indexable.from_index_exn 1l;
          qty = Tx_rollup_l2_qty.of_int64_exn 12L;
        }
    in
    let buffer = encode_content opc in
    let opc' = decode_content buffer in

    Alcotest.(check int "smallest operation content" 4 (Bytes.length buffer)) ;
    assert (opc = opc') ;

    (* Assert the smallest operation size is 7 *)
    let op =
      {signer = Indexable.from_index_exn 2l; counter = 0L; contents = [opc]}
    in
    let buffer = encode_operation op in
    let op' = decode_operation buffer in

    Alcotest.(check int "smallest operation" 7 (Bytes.length buffer)) ;
    assert (op = op') ;

    (* Assert the smallest transaction size is 8 *)
    let t = [op] in
    let buffer = encode_transaction t in
    let t' = decode_transaction buffer in

    Alcotest.(check int "smallest transaction" 8 (Bytes.length buffer)) ;
    assert (t = t') ;

    return_unit

  let tests =
    [
      tztest
        "test layer-2 transaction encoding size"
        `Quick
        test_l2_transaction_size;
    ]
end

let tests =
  [tztest "test irmin storage" `Quick @@ wrap_test test_irmin_storage]
  @ Test_Address_index.tests @ Test_Ticket_index.tests
  @ Test_Address_medata.tests @ Test_Ticket_ledger.tests
  @ Test_batch_encodings.tests
