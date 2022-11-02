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

open Alpha_context
open Tx_rollup_l2_context_sig
open Tx_rollup_l2_batch

type error +=
  | Counter_mismatch of {
      account : Tx_rollup_l2_address.t;
      expected : int64;
      provided : int64;
    }
  | Incorrect_aggregated_signature
  | Unallocated_metadata of int32
  | Multiple_operations_for_signer of Bls.Public_key.t
  | Invalid_transaction_encoding
  | Invalid_batch_encoding
  | Unexpectedly_indexed_ticket
  | Missing_ticket of Ticket_hash.t
  | Unknown_address of Tx_rollup_l2_address.t
  | Invalid_self_transfer
  | Invalid_zero_transfer
  | Maximum_withdraws_per_message_exceeded of {current : int; maximum : int}

let () =
  let open Data_encoding in
  (* Counter mismatch *)
  register_error_kind
    `Branch
    ~id:"tx_rollup_operation_counter_mismatch"
    ~title:"Operation counter mismatch"
    ~description:
      "A transaction rollup operation has been submitted with an incorrect \
       counter"
    (obj3
       (req "account" Tx_rollup_l2_address.encoding)
       (req "expected" int64)
       (req "provided" int64))
    (function
      | Counter_mismatch {account; expected; provided} ->
          Some (account, expected, provided)
      | _ -> None)
    (fun (account, expected, provided) ->
      Counter_mismatch {account; expected; provided}) ;
  (* Incorrect aggregated signature *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_incorrect_aggregated_signature"
    ~title:"Incorrect aggregated signature"
    ~description:"The aggregated signature is incorrect"
    empty
    (function Incorrect_aggregated_signature -> Some () | _ -> None)
    (function () -> Incorrect_aggregated_signature) ;
  (* Unallocated metadata *)
  register_error_kind
    `Branch
    ~id:"tx_rollup_unknown_metadata"
    ~title:"Unknown metadata"
    ~description:
      "A public key index was provided but the account information for this \
       index is not present in the context."
    (obj1 (req "idx" int32))
    (function Unallocated_metadata i -> Some i | _ -> None)
    (function i -> Unallocated_metadata i) ;
  (* Invalid transaction *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_invalid_transaction"
    ~title:"Invalid transaction"
    ~description:
      "The signer signed multiple operations in the same transaction. He must \
       gather all the contents in a single operation"
    (obj1 (req "pk" Bls.Public_key.encoding))
    (function Multiple_operations_for_signer idx -> Some idx | _ -> None)
    (function idx -> Multiple_operations_for_signer idx) ;
  (* Invalid transaction encoding *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_invalid_transaction_encoding"
    ~title:"Invalid transaction encoding"
    ~description:"The transaction could not be decoded from bytes"
    empty
    (function Invalid_transaction_encoding -> Some () | _ -> None)
    (function () -> Invalid_transaction_encoding) ;
  (* Invalid batch encoding *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_invalid_batch_encoding"
    ~title:"Invalid batch encoding"
    ~description:"The batch could not be decoded from bytes"
    empty
    (function Invalid_batch_encoding -> Some () | _ -> None)
    (function () -> Invalid_batch_encoding) ;
  (* Unexpectedly indexed ticket *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_unexpectedly_indexed_ticket"
    ~title:"Unexpected indexed ticket in deposit or transfer"
    ~description:
      "Tickets in layer2-to-layer1 transfers must be referenced by value."
    empty
    (function Unexpectedly_indexed_ticket -> Some () | _ -> None)
    (function () -> Unexpectedly_indexed_ticket) ;
  (* Missing ticket *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_missing_ticket"
    ~title:"Attempted to withdraw from a ticket missing in the rollup"
    ~description:
      "A withdrawal must reference a ticket that already exists in the rollup."
    (obj1 (req "ticket_hash" Ticket_hash.encoding))
    (function Missing_ticket ticket_hash -> Some ticket_hash | _ -> None)
    (function ticket_hash -> Missing_ticket ticket_hash) ;
  (* Unknown address *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_unknown_address"
    ~title:"Attempted to sign a transfer with an unknown address"
    ~description:
      "The address must exist in the context when signing a transfer with it."
    (obj1 (req "address" Tx_rollup_l2_address.encoding))
    (function Unknown_address addr -> Some addr | _ -> None)
    (function addr -> Unknown_address addr) ;
  (* Invalid self transfer *)
  register_error_kind
    `Temporary
    ~id:"tx_rollup_invalid_self_transfer"
    ~title:"Attempted to transfer ticket to self"
    ~description:"The index for the destination is the same as the sender"
    empty
    (function Invalid_self_transfer -> Some () | _ -> None)
    (function () -> Invalid_self_transfer) ;
  (* Invalid zero transfer *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_invalid_zero_transfer"
    ~title:"Attempted to transfer zero ticket"
    ~description:"A transfer's amount must be greater than zero."
    empty
    (function Invalid_zero_transfer -> Some () | _ -> None)
    (function () -> Invalid_zero_transfer) ;
  (* Maximum_withdraws_per_message_exceeded *)
  register_error_kind
    `Permanent
    ~id:"tx_rollup_maximum_withdraws_per_message_exceeded"
    ~title:"Maximum tx-rollup withdraws per message exceeded"
    ~description:
      "The maximum number of withdraws allowed per tx-rollup message exceeded"
    (obj2 (req "current" int31) (req "limit" int31))
    (function
      | Maximum_withdraws_per_message_exceeded {current; maximum} ->
          Some (current, maximum)
      | _ -> None)
    (fun (current, maximum) ->
      Maximum_withdraws_per_message_exceeded {current; maximum})

type indexes = {
  address_indexes :
    (Tx_rollup_l2_address.t * Tx_rollup_l2_address.Indexable.index) list;
  ticket_indexes : (Ticket_hash.t * Ticket_indexable.index) list;
}

let encoding_indexes : indexes Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {address_indexes; ticket_indexes} -> (address_indexes, ticket_indexes))
    (fun (address_indexes, ticket_indexes) -> {address_indexes; ticket_indexes})
  @@ obj2
       (req
          "address_indexes"
          (list
             (tup2
                Tx_rollup_l2_address.encoding
                Tx_rollup_l2_address.Indexable.index_encoding)))
       (req
          "ticket_indexes"
          (list (tup2 Ticket_hash.encoding Ticket_indexable.index_encoding)))

module Message_result = struct
  type transaction_result =
    | Transaction_success
    | Transaction_failure of {index : int; reason : error}

  type deposit_result = Deposit_success of indexes | Deposit_failure of error

  let encoding_transaction_result =
    let open Data_encoding in
    union
      [
        (let kind = "transaction_success" in
         case
           ~title:kind
           (Tag 0)
           (constant kind)
           (function Transaction_success -> Some () | _ -> None)
           (fun () -> Transaction_success));
        (let kind = "transaction_failure" in
         case
           ~title:kind
           (Tag 1)
           (obj1
              (req
                 kind
                 (obj2
                    (req "transaction_index" Data_encoding.int31)
                    (req "reason" Error_monad.error_encoding))))
           (function
             | Transaction_failure {index; reason} -> Some (index, reason)
             | _ -> None)
           (fun (index, reason) -> Transaction_failure {index; reason}));
      ]

  let encoding_deposit_result =
    let open Data_encoding in
    union
      [
        (let kind = "deposit_success" in
         case
           ~title:kind
           (Tag 0)
           (obj1 (req kind encoding_indexes))
           (function Deposit_success indexes -> Some indexes | _ -> None)
           (fun indexes -> Deposit_success indexes));
        (let kind = "deposit_failure" in
         case
           ~title:kind
           (Tag 1)
           (obj1 (req kind (obj1 (req "reason" Error_monad.error_encoding))))
           (function Deposit_failure reason -> Some reason | _ -> None)
           (fun reason -> Deposit_failure reason));
      ]

  module Batch_V1 = struct
    type t =
      | Batch_result of {
          results :
            ((Indexable.index_only, Indexable.unknown) V1.transaction
            * transaction_result)
            list;
          indexes : indexes;
        }

    let encoding =
      let open Data_encoding in
      conv
        (fun (Batch_result {results; indexes}) -> (results, indexes))
        (fun (results, indexes) -> Batch_result {results; indexes})
        (obj2
           (req "results"
           @@ list
                (Data_encoding.tup2
                   (Compact.make
                      ~tag_size:`Uint8
                      V1.compact_transaction_signer_index)
                   encoding_transaction_result))
           (req "allocated_indexes" encoding_indexes))
  end

  type message_result =
    | Deposit_result of deposit_result
    | Batch_V1_result of Batch_V1.t

  let message_result_encoding =
    let open Data_encoding in
    union
      [
        (let kind = "deposit_result" in
         case
           ~title:kind
           (Tag 0)
           (obj1 (req kind encoding_deposit_result))
           (function Deposit_result result -> Some result | _ -> None)
           (fun result -> Deposit_result result));
        (let kind = "batch_v1_result" in
         case
           ~title:kind
           (Tag 1)
           (obj1 (req kind Batch_V1.encoding))
           (function Batch_V1_result result -> Some result | _ -> None)
           (fun result -> Batch_V1_result result));
      ]

  type t = message_result * Tx_rollup_withdraw.t list

  let encoding =
    Data_encoding.(
      tup2 message_result_encoding (list Tx_rollup_withdraw.encoding))
end

type parameters = {
  (* Maximum number of allowed L2-to-L1 withdraws per batch *)
  tx_rollup_max_withdrawals_per_batch : int;
}

module Make (Context : CONTEXT) = struct
  open Context
  open Syntax
  open Message_result

  type ctxt = Context.t

  (** {3. Indexes. } *)

  (** The application of a message can (and is supposed to) use and
      create several indexes during the application of a {Tx_rollup_message.t}.
  *)

  let index get_or_associate_index add_index ctxt indexes indexable =
    let open Indexable in
    match destruct indexable with
    | Right v -> (
        let+ ctxt, created, idx = get_or_associate_index ctxt v in
        match created with
        | `Existed -> (ctxt, indexes, idx)
        | `Created -> (ctxt, add_index indexes (v, idx), idx))
    | Left i -> return (ctxt, indexes, i)

  let address_index ctxt indexes indexable =
    let get_or_associate_index = Address_index.get_or_associate_index in
    let add_index indexes x =
      {indexes with address_indexes = x :: indexes.address_indexes}
    in
    index get_or_associate_index add_index ctxt indexes indexable

  let ticket_index ctxt indexes indexable =
    let get_or_associate_index = Ticket_index.get_or_associate_index in
    let add_index indexes x =
      {indexes with ticket_indexes = x :: indexes.ticket_indexes}
    in
    index get_or_associate_index add_index ctxt indexes indexable

  let address_of_signer_index :
      Signer_indexable.index -> Tx_rollup_l2_address.Indexable.index =
   fun idx -> Indexable.(index_exn (to_int32 idx))

  let signer_of_address_index :
      Tx_rollup_l2_address.Indexable.index -> Signer_indexable.index =
   fun idx -> Indexable.(index_exn (to_int32 idx))

  let empty_indexes = {address_indexes = []; ticket_indexes = []}

  let assert_non_zero_quantity qty =
    fail_when Tx_rollup_l2_qty.(qty = zero) Invalid_zero_transfer

  (** {2. Counter } *)

  (** [get_metadata ctxt idx] returns the metadata associated to [idx] in
      [ctxt]. It must have an associated metadata in the context, otherwise,
      something went wrong in {!check_signature}. *)
  let get_metadata : ctxt -> address_index -> metadata m =
   fun ctxt idx ->
    let open Address_metadata in
    let* metadata = get ctxt idx in
    match metadata with
    | None -> fail (Unallocated_metadata (Indexable.to_int32 idx))
    | Some metadata -> return metadata

  (** [get_metadata_signer] gets the metadata for a signer using {!get_metadata}.
      It transforms a signer index to an address one. *)
  let get_metadata_signer : ctxt -> Signer_indexable.index -> metadata m =
   fun ctxt signer_idx -> get_metadata ctxt (address_of_signer_index signer_idx)

  (** [transfers ctxt source_idx destination_idx tidx amount] transfers [amount]
      from [source_idx] to [destination_idx] of [tidx]. *)
  let transfer ctxt source_idx destination_idx tidx amount =
    let* () =
      fail_unless
        Compare.Int.(Indexable.compare_indexes source_idx destination_idx <> 0)
        Invalid_self_transfer
    in
    let* () = assert_non_zero_quantity amount in
    let* ctxt = Ticket_ledger.spend ctxt tidx source_idx amount in
    Ticket_ledger.credit ctxt tidx destination_idx amount

  (** [deposit ctxt aidx tidx amount] credits [amount] of [tidx] to [aidx].
      They are deposited from the layer1 and created in the layer2 context, but,
      we only handle the creation part (i.e. in the layer2) in this module. *)
  let deposit ctxt aidx tidx amount = Ticket_ledger.credit ctxt tidx aidx amount

  module Batch_V1 = struct
    open Tx_rollup_l2_batch.V1

    (** [operation_with_signer_index ctxt indexes op] takes an operation
        and performs multiple get/sets on the context to return an operation
        where the signer is replaced by its index.

        It performs on the [ctxt]:
        {ul {li If the signer is an index, we read the public key from the
                [ctxt].}
            {li If the signer is a public key, we associate a new index to
                it in the [ctxt]. The public key is also added to the metadata
                if not already present.}}

        {b Note:} If the context already contains all the required information,
        we only read from it. *)
    let operation_with_signer_index :
        ctxt ->
        indexes ->
        ('signer, 'content) operation ->
        (ctxt
        * indexes
        * (Indexable.index_only, 'content) operation
        * Bls.Public_key.t)
        m =
     fun ctxt indexes op ->
      let* ctxt, indexes, pk, idx =
        match Indexable.destruct op.signer with
        | Left signer_index ->
            (* Get the public key from the index. *)
            let address_index = address_of_signer_index signer_index in
            let* metadata = get_metadata ctxt address_index in
            let pk = metadata.public_key in
            return (ctxt, indexes, pk, address_index)
        | Right (Bls_pk signer_pk) -> (
            (* Initialize the ctxt with public_key if it's necessary. *)
            let addr = Bls.Public_key.hash signer_pk in
            let* ctxt, created, idx =
              Address_index.get_or_associate_index ctxt addr
            in

            (* If the address is created, we add it to [indexes]. *)
            match created with
            | `Existed ->
                (* If the public key existed in the context, it should not
                   be added in [indexes]. However, the metadata might not
                   have been initialized for the public key. Especially during
                   a deposit, the deposit destination is a layer2 address and
                   it contains no information about the public key.
                *)
                let* ctxt =
                  let* metadata = Address_metadata.get ctxt idx in
                  match metadata with
                  | Some _ ->
                      (* If the metadata exists, then the public key necessarily
                         exists, we do not need to change the context. *)
                      return ctxt
                  | None ->
                      Address_metadata.init_with_public_key ctxt idx signer_pk
                in
                return (ctxt, indexes, signer_pk, idx)
            | `Created ->
                (* If the index is created, we need to add to indexes and
                   initialize the metadata. *)
                let indexes =
                  {
                    indexes with
                    address_indexes = (addr, idx) :: indexes.address_indexes;
                  }
                in
                let* ctxt =
                  Address_metadata.init_with_public_key ctxt idx signer_pk
                in
                return (ctxt, indexes, signer_pk, idx))
        | Right (L2_addr signer_addr) -> (
            (* In order to get the public key associated to [signer_addr], there
               needs to be both an index associated to it, and a metadata for this
               index. *)
            let* idx = Address_index.get ctxt signer_addr in
            match idx with
            | None -> fail (Unknown_address signer_addr)
            | Some idx ->
                let* metadata = get_metadata ctxt idx in
                return (ctxt, indexes, metadata.public_key, idx))
      in
      let op : (Indexable.index_only, 'content) operation =
        {op with signer = signer_of_address_index idx}
      in
      return (ctxt, indexes, op, pk)

    (** [check_transaction ctxt indexes transmitted transaction] performs an
        *active* check of an operation.
        We consider this as an *active* check because the function is likely to
        write in the [ctxt], since it replaces the signer's public key
        (if provided) by its index in {!operation_with_signer_index}.

        Outside of the active preprocessing, we check that a signer signs
        at most one operation in the [transaction].

        It also associates the signer to the bytes representation of a
        transaction in [transmitted], which is used to check the aggregated
        signature.
    *)
    let check_transaction ctxt indexes transmitted transaction =
      let* buf =
        match
          Data_encoding.Binary.to_bytes_opt
            (Data_encoding.Compact.make ~tag_size:`Uint8 compact_transaction)
            transaction
        with
        | Some buf -> return buf
        | None -> fail Invalid_transaction_encoding
      in
      let* ctxt, indexes, transmitted, _, rev_ops =
        list_fold_left_m
          (fun (ctxt, indexes, transmitted, signers, ops) op ->
            let* ctxt, indexes, op, pk =
              operation_with_signer_index ctxt indexes op
            in
            if List.mem ~equal:Bls.Public_key.equal pk signers then
              fail (Multiple_operations_for_signer pk)
            else
              return
                ( ctxt,
                  indexes,
                  (pk, buf) :: transmitted,
                  pk :: signers,
                  op :: ops ))
          (ctxt, indexes, transmitted, [], [])
          transaction
      in
      return (ctxt, indexes, transmitted, List.rev rev_ops)

    let check_signature :
        ctxt ->
        ('signer, 'content) t ->
        (ctxt * indexes * (Indexable.index_only, 'content) t) m =
     fun ctxt ({contents = transactions; aggregated_signature} as batch) ->
      let* ctxt, indexes, transmitted, rev_new_transactions =
        list_fold_left_m
          (fun (ctxt, indexes, transmitted, new_transactions) transaction ->
            (* To check the signature, we need the list of [buf] each signer
               signed. That is, the [buf] is the binary encoding of the
               [transaction]. *)
            let* ctxt, indexes, transmitted, transaction =
              check_transaction ctxt indexes transmitted transaction
            in
            return (ctxt, indexes, transmitted, transaction :: new_transactions))
          (ctxt, empty_indexes, [], [])
          transactions
      in
      (* Once we collected the public keys for each signer and the buffers
         they signed, we can check the signature. *)
      let* b = bls_verify transmitted aggregated_signature in
      let* () = fail_unless b Incorrect_aggregated_signature in
      let batch = {batch with contents = List.rev rev_new_transactions} in
      return (ctxt, indexes, batch)

    (** {2. Apply } *)

    (** [apply_operation_content ctxt source content] performs the transfer
        on the [ctxt]. The validity of the transfer is checked in
        the context itself, e.g. for an invalid balance.

        It returns the potential created indexes:

        {ul {li The destination address index.}
            {li The ticket exchanged index.}}
    *)
    let apply_operation_content :
        ctxt ->
        indexes ->
        Signer_indexable.index ->
        'content operation_content ->
        (ctxt * indexes * Tx_rollup_withdraw.t option) m =
     fun ctxt indexes source_idx op_content ->
      match op_content with
      | Withdraw {destination = claimer; ticket_hash; qty = amount} ->
          (* To withdraw, the ticket must already exist in the
             rollup and be indexed (the ticket must have already been
             assigned an index in the content: otherwise the ticket has
             not been seen before and we can't withdraw from it). *)
          let* tidx_opt = Ticket_index.get ctxt ticket_hash in
          let*? tidx =
            Option.value_e ~error:(Missing_ticket ticket_hash) tidx_opt
          in
          let source_idx = address_of_signer_index source_idx in

          (* spend the ticket -- this is responsible for checking that
             the source has the required balance *)
          let* () = assert_non_zero_quantity amount in
          let* ctxt = Ticket_ledger.spend ctxt tidx source_idx amount in
          let withdrawal = Tx_rollup_withdraw.{claimer; ticket_hash; amount} in
          return (ctxt, indexes, Some withdrawal)
      | Transfer {destination; ticket_hash; qty} ->
          let* ctxt, indexes, dest_idx =
            address_index ctxt indexes destination
          in
          let* ctxt, indexes, tidx = ticket_index ctxt indexes ticket_hash in
          let source_idx = address_of_signer_index source_idx in
          let* ctxt = transfer ctxt source_idx dest_idx tidx qty in
          return (ctxt, indexes, None)

    (** [check_counter ctxt signer counter] asserts that the provided [counter] is the
        successor of the one associated to the [signer] in the [ctxt]. *)
    let check_counter :
        ctxt -> Indexable.index_only Signer_indexable.t -> int64 -> unit m =
     fun ctxt signer counter ->
      let* metadata = get_metadata_signer ctxt signer in
      fail_unless
        Compare.Int64.(counter = Int64.succ metadata.counter)
        (Counter_mismatch
           {
             account = Bls.Public_key.hash metadata.public_key;
             expected = Int64.succ metadata.counter;
             provided = counter;
           })

    (** [apply_operation ctxt indexes op] checks the counter validity for the [op.signer] with
        {!check_counter}, and then calls {!apply_operation_content} for each content in [op]. *)
    let apply_operation :
        ctxt ->
        indexes ->
        (Indexable.index_only, Indexable.unknown) operation ->
        (ctxt * indexes * Tx_rollup_withdraw.t list) m =
     fun ctxt indexes {signer; counter; contents} ->
      (* Before applying any operation, we check the counter *)
      let* () = check_counter ctxt signer counter in
      let* ctxt, indexes, rev_withdrawals =
        list_fold_left_m
          (fun (ctxt, indexes, withdrawals) content ->
            let* ctxt, indexes, withdrawal_opt =
              apply_operation_content ctxt indexes signer content
            in
            return (ctxt, indexes, Option.to_list withdrawal_opt @ withdrawals))
          (ctxt, indexes, [])
          contents
      in
      return (ctxt, indexes, rev_withdrawals |> List.rev)

    (** [apply_transaction ctxt indexes transaction] applies each operation in
        the [transaction]. It returns a {!transaction_result}, i.e. either
        every operation in the [transaction] succedeed and the [ctxt] is
        modified, or the [transaction] is a failure and the context
        is left untouched.
    *)
    let apply_transaction :
        ctxt ->
        indexes ->
        (Indexable.index_only, Indexable.unknown) transaction ->
        (ctxt * indexes * transaction_result * Tx_rollup_withdraw.t list) m =
     fun initial_ctxt initial_indexes transaction ->
      let rec fold (ctxt, prev_indexes, withdrawals) index ops =
        match ops with
        | [] -> return (ctxt, prev_indexes, Transaction_success, withdrawals)
        | op :: rst ->
            let* ctxt, indexes, status, withdrawals =
              catch
                (apply_operation ctxt prev_indexes op)
                (fun (ctxt, indexes, op_withdrawals) ->
                  fold
                    (ctxt, indexes, withdrawals @ op_withdrawals)
                    (index + 1)
                    rst)
                (fun reason ->
                  return
                    ( initial_ctxt,
                      initial_indexes,
                      Transaction_failure {index; reason},
                      [] ))
            in
            return (ctxt, indexes, status, withdrawals)
      in
      fold (initial_ctxt, initial_indexes, []) 0 transaction

    (** [update_counters ctxt status transaction] updates the counters for
        the signers of operations in [transaction]. If the [transaction]
        failed because of a [Counter_mismatch] the counters are left
        untouched.
    *)
    let update_counters ctxt status transaction =
      match status with
      | Transaction_failure {reason = Counter_mismatch _; _} -> return ctxt
      | Transaction_failure _ | Transaction_success ->
          list_fold_left_m
            (fun ctxt (op : (Indexable.index_only, _) operation) ->
              Address_metadata.incr_counter ctxt
              @@ address_of_signer_index op.signer)
            ctxt
            transaction

    let apply_batch :
        ctxt ->
        parameters ->
        (Indexable.unknown, Indexable.unknown) t ->
        (ctxt * Message_result.Batch_V1.t * Tx_rollup_withdraw.t list) m =
     fun ctxt parameters batch ->
      let* ctxt, indexes, batch = check_signature ctxt batch in
      let {contents; _} = batch in
      let* ctxt, indexes, rev_results, withdrawals =
        list_fold_left_m
          (fun (prev_ctxt, prev_indexes, results, withdrawals) transaction ->
            let* new_ctxt, new_indexes, status, transaction_withdrawals =
              apply_transaction prev_ctxt prev_indexes transaction
            in
            let* new_ctxt = update_counters new_ctxt status transaction in
            return
              ( new_ctxt,
                new_indexes,
                (transaction, status) :: results,
                withdrawals @ transaction_withdrawals ))
          (ctxt, indexes, [], [])
          contents
      in
      let limit = parameters.tx_rollup_max_withdrawals_per_batch in
      if Compare.List_length_with.(withdrawals > limit) then
        fail
          (Maximum_withdraws_per_message_exceeded
             {current = List.length withdrawals; maximum = limit})
      else
        let results = List.rev rev_results in
        return
          ( ctxt,
            Message_result.Batch_V1.Batch_result {results; indexes},
            withdrawals )
  end

  let apply_deposit :
      ctxt ->
      Tx_rollup_message.deposit ->
      (ctxt * deposit_result * Tx_rollup_withdraw.t option) m =
   fun initial_ctxt Tx_rollup_message.{sender; destination; ticket_hash; amount} ->
    let apply_deposit () =
      let* ctxt, indexes, aidx =
        address_index initial_ctxt empty_indexes destination
      in
      let* ctxt, indexes, tidx =
        ticket_index ctxt indexes Indexable.(value ticket_hash)
      in
      let* ctxt = deposit ctxt aidx tidx amount in
      return (ctxt, indexes)
    in
    catch
      (apply_deposit ())
      (fun (ctxt, indexes) -> return (ctxt, Deposit_success indexes, None))
      (fun reason ->
        (* Should there be an error during the deposit, then return
           the full [amount] to [sender] in the form of a
           withdrawal. *)
        let withdrawal =
          Tx_rollup_withdraw.{claimer = sender; ticket_hash; amount}
        in
        return (initial_ctxt, Deposit_failure reason, Some withdrawal))

  let apply_message :
      ctxt -> parameters -> Tx_rollup_message.t -> (ctxt * Message_result.t) m =
   fun ctxt parameters msg ->
    let open Tx_rollup_message in
    match msg with
    | Deposit deposit ->
        let* ctxt, result, withdrawl_opt = apply_deposit ctxt deposit in
        return (ctxt, (Deposit_result result, Option.to_list withdrawl_opt))
    | Batch str -> (
        let batch =
          Data_encoding.Binary.of_string_opt Tx_rollup_l2_batch.encoding str
        in
        match batch with
        | Some (V1 batch) ->
            let* ctxt, result, withdrawals =
              Batch_V1.apply_batch ctxt parameters batch
            in
            return (ctxt, (Batch_V1_result result, withdrawals))
        | None -> fail Invalid_batch_encoding)
end
