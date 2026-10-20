(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context
module Payloads = Sc_rollup.Inbox_merkelized_payload_hashes
module Hash = Payloads.Hash

type t = Payloads.t list

type outcome = Valid | Inconsistent_indices | Unconvertible_cells

(* [basis] must be the one the protocol builds its shared-inbox skip lists
   with, since the index checks read the number of back pointers a cell holds
   off it. Nothing links the two: the protocol declares it in
   [Sc_rollup_inbox_repr] and [Sc_rollup_inbox_merkelized_payload_hashes_repr],
   neither reachable from here, and [Skip_list.S] takes it as a functor
   argument without exporting it. One instance serves both layers because those
   two declarations agree. A disagreement makes the checks reject the proofs
   the protocol itself produces, which [test_sc_rollup_inbox.ml] would catch.

   Protocol cells are abstract, so the index checks cannot read them. They are
   read back here through their own encoding: a cell encodes as an index, a
   content and a list of back pointers, and survives the round trip unchanged.
   Cells stay authenticated by the protocol's hash functions, which this module
   never reimplements. *)
module Skip_list_parameters = struct
  let basis = 4
end

module Skip_list = Skip_list.Make (Skip_list_parameters)

let payloads_cell_encoding =
  Skip_list.encoding Hash.encoding Sc_rollup.Inbox_message.Hash.encoding

let convert_payloads_cell cell =
  let open Option_syntax in
  let* bytes = Data_encoding.Binary.to_bytes_opt Payloads.encoding cell in
  Data_encoding.Binary.of_bytes_opt payloads_cell_encoding bytes

(* Two cells that share a pointer without being equal are rejected here. Only
   one of them would be checked, and the protocol reads whichever the proof
   ships, so the verdict must not turn on which one [deref] answers with. *)
let deref_of_cells ~hash ~equal_ptr ~equal_cell ~convert cells =
  let rec collect acc = function
    | [] -> Ok acc
    | cell :: cells -> (
        match convert cell with
        | None -> Error Unconvertible_cells
        | Some converted -> (
            let ptr = hash cell in
            match List.assoc ~equal:equal_ptr ptr acc with
            | Some (known, _) when not (equal_cell known cell) ->
                Error Inconsistent_indices
            | Some _ -> collect acc cells
            | None -> collect ((ptr, (cell, converted)) :: acc) cells))
  in
  Result.map
    (fun assoc ptr -> Option.map snd (List.assoc ~equal:equal_ptr ptr assoc))
    (collect [] cells)

let verify_payloads_indices ~inclusion ~index_proof =
  (* [verify_index_proof] rejects an empty [index_proof] on its own. *)
  match inclusion with
  | [] -> Inconsistent_indices
  | head :: _ -> (
      match
        deref_of_cells
          ~hash:Payloads.hash
          ~equal_ptr:Hash.equal
          ~equal_cell:Payloads.equal
          ~convert:convert_payloads_cell
          (inclusion @ index_proof)
      with
      | Error outcome -> outcome
      | Ok deref ->
          if
            Skip_list.verify_index_proof
              ~equal_ptr:Hash.equal
              ~deref
              ~cell_ptr:(Payloads.hash head)
              ~inclusion_proof:(List.map Payloads.hash inclusion)
              (List.map Payloads.hash index_proof)
          then Valid
          else Inconsistent_indices)

let produce history ~head =
  let open Option_syntax in
  let cell_of_hash ptr =
    let* {Payloads.merkelized; _} = Payloads.History.find ptr history in
    return merkelized
  in
  let deref ptr =
    let* cell = cell_of_hash ptr in
    convert_payloads_cell cell
  in
  let* path =
    Skip_list.produce_index_proof
      ~equal_ptr:Hash.equal
      ~deref
      ~cell_ptr:(Payloads.hash head)
  in
  let* index_proof =
    List.map_e (fun ptr -> Option.to_result ~none:() (cell_of_hash ptr)) path
    |> Result.to_option
  in
  match verify_payloads_indices ~inclusion:[head] ~index_proof with
  | Valid -> return index_proof
  | Inconsistent_indices | Unconvertible_cells -> None

(* The tag tells an index proof from any other message. It does not start with
   the frame byte a kernel reads, so framed kernels drop these messages. *)
let message_tag = "\xf1SIP\x01"

let message_header_size = String.length message_tag + 4

(* An external message carries at most [sc_rollup_message_size_limit] bytes,
   minus the one byte tagging it as external. *)
let max_chunk_size =
  Constants.sc_rollup_message_size_limit - 1 - message_header_size

let index_proof_encoding = Data_encoding.list Payloads.encoding

let to_messages ?(chunk_size = max_chunk_size) index_proof =
  (* A message holds at most [max_chunk_size] bytes; tests pass a smaller
     [chunk_size] to spread an index proof over several messages. *)
  let chunk_size = Compare.Int.(max 1 (min chunk_size max_chunk_size)) in
  let body =
    Data_encoding.Binary.to_string_exn index_proof_encoding index_proof
  in
  let length = String.length body in
  let chunk_count = max 1 ((length + chunk_size - 1) / chunk_size) in
  Stdlib.List.init chunk_count (fun chunk_index ->
      let offset = chunk_index * chunk_size in
      let buffer = Buffer.create (message_header_size + chunk_size) in
      Buffer.add_string buffer message_tag ;
      Buffer.add_uint16_be buffer chunk_index ;
      Buffer.add_uint16_be buffer chunk_count ;
      Buffer.add_substring buffer body offset (min chunk_size (length - offset)) ;
      Buffer.contents buffer)

let parse_chunk message =
  let tag_size = String.length message_tag in
  if
    String.length message >= message_header_size
    && String.starts_with ~prefix:message_tag message
  then
    let chunk_index = String.get_uint16_be message tag_size in
    let chunk_count = String.get_uint16_be message (tag_size + 2) in
    let chunk =
      String.sub
        message
        message_header_size
        (String.length message - message_header_size)
    in
    Some (chunk_index, chunk_count, chunk)
  else None

let of_messages messages =
  let flush index_proofs chunks =
    match
      Data_encoding.Binary.of_string_opt
        index_proof_encoding
        (String.concat "" (List.rev chunks))
    with
    | None | Some [] -> index_proofs
    | Some index_proof -> index_proof :: index_proofs
  in
  (* A chunk that does not continue the index proof [collect] is reading drops
     it and goes back to [skip], so a cut sequence does not swallow the one
     after it. *)
  let rec skip index_proofs = function
    | [] -> List.rev index_proofs
    | message :: rest -> (
        match parse_chunk message with
        | Some (0, count, chunk) -> collect index_proofs count 1 [chunk] rest
        | None | Some _ -> skip index_proofs rest)
  and collect index_proofs count read chunks messages =
    if Compare.Int.(read = count) then skip (flush index_proofs chunks) messages
    else
      match messages with
      | [] -> List.rev index_proofs
      | message :: rest -> (
          match parse_chunk message with
          | None -> collect index_proofs count read chunks rest
          | Some (index, count', chunk)
            when Compare.Int.(index = read && count' = count) ->
              collect index_proofs count (read + 1) (chunk :: chunks) rest
          | Some _ -> skip index_proofs messages)
  in
  skip [] messages
