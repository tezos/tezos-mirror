(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* The L2 context for the rollup node has the form of a binary Merkle tree. We
   reuse the implementation provided by [Tezos_context] as it is stable. Proofs
   produced by the node will be verifiable by the protocol. *)
module Raw = Tezos_context.Context_binary
include Raw

module Keys = struct
  let l2_context = ["l2_context"]

  let tickets = ["tickets"]
end

module Irmin_storage :
  Protocol.Tx_rollup_l2_storage_sig.STORAGE
    with type t = context
     and type 'a m = 'a tzresult Lwt.t = struct
  type t = context

  type 'a m = 'a tzresult Lwt.t

  let path k = Keys.l2_context @ [Bytes.to_string k]

  let get (ctxt : context) key : bytes option m =
    let open Lwt_result_syntax in
    let*! res = Raw.find ctxt (path key) in
    return res

  let set ctxt key value =
    let open Lwt_result_syntax in
    let*! ctxt = Raw.add ctxt (path key) value in
    return ctxt

  let remove ctxt key =
    let open Lwt_result_syntax in
    let*! ctxt = Raw.remove ctxt (path key) in
    return ctxt

  module Syntax = struct
    include Lwt_result_syntax

    let catch m k h =
      let open Lwt_syntax in
      let* res = m in
      match res with
      | Ok x -> k x
      | Error (Environment.Ecoproto_error e :: _) -> h e
      | Error err ->
          (* TODO/TORU: replace error either in STORAGE or here *)
          (* Should not happen *)
          let* () = Debug_events.(emit should_not_happen) __LOC__ in
          fail err

    let fail e =
      let e = Environment.wrap_tzerror e in
      Lwt.return (Error [e])

    let list_fold_left_m = List.fold_left_es
  end
end

include Protocol.Tx_rollup_l2_context.Make (Irmin_storage)

let context_hash_to_l2 hash =
  Context_hash.to_bytes hash |> Protocol.Tx_rollup_l2_context_hash.of_bytes_exn

let l2_to_context_hash hash =
  Protocol.Tx_rollup_l2_context_hash.to_bytes hash |> Context_hash.of_bytes_exn

let exists index hash = Raw.exists index (l2_to_context_hash hash)

(* The context hashes are not dependant on the time, we use EPOCH (i.e. 0) to
   commit (and hash). *)

let hash ?(message = "") context =
  Raw.hash ~time:Time.Protocol.epoch ~message context |> context_hash_to_l2

let commit ?(message = "") context =
  let open Lwt_syntax in
  if Raw.is_empty context then
    (* We cannot commit empty contexts with Irmin 3 *)
    return (hash ~message context)
  else
    let+ hash = Raw.commit ~time:Time.Protocol.epoch ~message context in
    context_hash_to_l2 hash

let checkout_opt index context_hash =
  let open Lwt_syntax in
  let+ context = Raw.checkout index (l2_to_context_hash context_hash) in
  match context with
  | Some context -> Some context
  | None ->
      let empty = Raw.empty index in
      let hash_empty = hash empty in
      if Protocol.Tx_rollup_l2_context_hash.(context_hash = hash_empty) then
        Some empty
      else None

let checkout_exn index hash =
  let open Lwt_syntax in
  let+ context = checkout_opt index hash in
  match context with None -> raise Not_found | Some context -> context

let checkout index hash =
  let open Lwt_syntax in
  let+ context = checkout_opt index hash in
  Option.to_result ~none:[Error.Tx_rollup_cannot_checkout_context hash] context

(** {2 Prover context} *)

exception Error of Environment.Error_monad.error

module Prover_storage :
  Protocol.Tx_rollup_l2_storage_sig.STORAGE
    with type t = tree
     and type 'a m = 'a Lwt.t = struct
  type t = tree

  type 'a m = 'a Lwt.t

  module Syntax = struct
    include Lwt.Syntax

    let return = Lwt.return

    let fail e = Lwt.fail (Error e)

    let catch (m : 'a m) k h =
      Lwt.catch
        (fun () -> m >>= k)
        (function Error e -> h e | e -> Lwt.fail e)

    let list_fold_left_m = Lwt_list.fold_left_s
  end

  let path k = [Bytes.to_string k]

  let get store key = Raw.Tree.find store (path key)

  let set store key value = Raw.Tree.add store (path key) value

  let remove store key = Raw.Tree.remove store (path key)
end

module Prover_context = Protocol.Tx_rollup_l2_context.Make (Prover_storage)

type 'a produce_proof_result = {tree : tree; result : 'a}

let get_tree ctxt =
  let open Lwt_result_syntax in
  let*! tree_opt = Raw.find_tree ctxt Keys.l2_context in
  match tree_opt with
  | Some tree -> return tree
  | None -> fail [Error.Tx_rollup_tree_not_found]

let produce_proof ctxt f =
  let open Lwt_result_syntax in
  let index = Raw.index ctxt in
  let* tree = get_tree ctxt in
  let* kinded_key =
    match Raw.Tree.kinded_key tree with
    | Some kinded_key -> return kinded_key
    | None -> fail [Error.Tx_rollup_tree_kinded_key_not_found]
  in
  let*! proof, result =
    Raw.produce_stream_proof index kinded_key (fun tree ->
        let*! res = f tree in
        Lwt.return (res.tree, res))
  in
  return (proof, result)

let hash_tree = Raw.Tree.hash

let tree_hash_of_context ctxt =
  let open Lwt_result_syntax in
  let+ tree = get_tree ctxt in
  hash_tree tree

let add_tree ctxt tree =
  let open Lwt_syntax in
  let* ctxt = Raw.add_tree ctxt Keys.l2_context tree in
  (* Irmin requires that we commit the context before generating the proof. *)
  let* ctxt_hash = commit ctxt in
  return (ctxt, ctxt_hash)

(** The initial context must be constructed using the internal empty tree.
    This tree however, *needs* to be non-empty. Otherwise, its hash will
    be inconsistent.
    See {!Protocol.Tx_rollup_commitment_repr.empty_l2_context_hash} for more
    context.
*)
let init_context index =
  let open Prover_context.Syntax in
  let ctxt = Raw.empty index in
  let tree = Raw.Tree.empty ctxt in
  let* tree = Prover_context.Address_index.init_counter tree in
  let* tree = Prover_context.Ticket_index.init_counter tree in
  let tree_hash = hash_tree tree in
  assert (
    Context_hash.(
      tree_hash = Protocol.Tx_rollup_message_result_repr.empty_l2_context_hash)) ;
  let* ctxt, _ = add_tree ctxt tree in
  return ctxt

(** {2 Sub-context for tickets } *)

module Ticket_indexable =
  Protocol.Indexable.Make (Protocol.Alpha_context.Ticket_hash)

let register_ticket ctxt
    (ticket_index : Protocol.Tx_rollup_l2_context_sig.ticket_index) ticket =
  let index_int32 = Protocol.Indexable.to_int32 ticket_index in
  let key = Keys.tickets @ [Int32.to_string index_int32] in
  let value = Data_encoding.Binary.to_bytes_exn Ticket.encoding ticket in
  Raw.add ctxt key value

let get_ticket ctxt
    (ticket_index : Protocol.Tx_rollup_l2_context_sig.ticket_index) =
  let open Lwt_syntax in
  let index_int32 = Protocol.Indexable.to_int32 ticket_index in
  let key = Keys.tickets @ [Int32.to_string index_int32] in
  let* value = Raw.find ctxt key in
  match value with
  | None -> return_none
  | Some value ->
      return_some (Data_encoding.Binary.of_bytes_exn Ticket.encoding value)
