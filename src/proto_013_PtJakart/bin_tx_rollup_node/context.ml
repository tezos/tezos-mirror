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
include Tezos_context.Context_binary

module Irmin_storage :
  Protocol.Tx_rollup_l2_storage_sig.STORAGE
    with type t = context
     and type 'a m = 'a tzresult Lwt.t = struct
  type t = context

  type 'a m = 'a tzresult Lwt.t

  let path k = [Bytes.to_string k]

  let get (ctxt : context) key : bytes option m =
    let open Lwt_result_syntax in
    let*! res = find ctxt (path key) in
    return res

  let set ctxt key value =
    let open Lwt_result_syntax in
    let*! ctxt = add ctxt (path key) value in
    return ctxt

  let remove ctxt key =
    let open Lwt_result_syntax in
    let*! ctxt = remove ctxt (path key) in
    return ctxt

  module Syntax = struct
    include Lwt_result_syntax

    let catch m k h =
      m >>= function
      | Ok x -> k x
      | Error (Environment.Ecoproto_error e :: _) -> h e
      | Error err ->
          (* TODO/TORU: replace error either in STORAGE or here *)
          (* Should not happen *)
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

let exists index hash = exists index (l2_to_context_hash hash)

(* The context hashes are not dependant on the time, we use EPOCH (i.e. 0) to
   commit (and hash). *)

let hash ?(message = "") context =
  hash ~time:Time.Protocol.epoch ~message context |> context_hash_to_l2

let commit ?(message = "") context =
  let open Lwt_syntax in
  if is_empty context then
    (* We cannot commit empty contexts with Irmin 3 *)
    return (hash ~message context)
  else
    let+ hash = commit ~time:Time.Protocol.epoch ~message context in
    context_hash_to_l2 hash

let checkout index context_hash =
  let open Lwt_syntax in
  let+ context = checkout index (l2_to_context_hash context_hash) in
  match context with
  | Some context -> Some context
  | None ->
      let empty = empty index in
      let hash_empty = hash empty in
      if Protocol.Tx_rollup_l2_context_hash.(context_hash = hash_empty) then
        Some empty
      else None

let checkout_exn index hash =
  let open Lwt_syntax in
  let+ context = checkout index hash in
  match context with None -> raise Not_found | Some context -> context
