(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Commitment_hash = Blake2B.Make_minimal_with_data (struct
  let name = "dal_slot_commitment"

  let title = "dal slot_commitment hash, used for Opentelemetry traces"

  (* Enforces the hash to have 16 bytes, to be compatible with Opentelemetry
     trace ids. *)
  let size = Some 16
end)

let commitment_to_trace_id slot_commitment =
  [
    Data_encoding.Binary.to_bytes_exn
      Cryptobox.Commitment.encoding
      slot_commitment;
  ]
  |> Commitment_hash.hash_bytes |> Commitment_hash.to_bytes
  |> Opentelemetry.Trace_id.of_bytes

let slot_to_trace_id slot =
  [slot] |> Commitment_hash.hash_bytes |> Commitment_hash.to_bytes
  |> Opentelemetry.Trace_id.of_bytes

let attr_of_index slot_index = ("slot_index", `Int slot_index)

let attrs_of_slot Types.Slot_id.{slot_level; slot_index} =
  [("level", `Int (Int32.to_int slot_level)); attr_of_index slot_index]

let attr_of_commitment commitment =
  [("commitment", `String (Cryptobox.Commitment.to_b58check commitment))]

let trace_slot ?(attrs = []) ~name ?slot_id ~slot_commitment f =
  let trace_id = commitment_to_trace_id slot_commitment in
  let slot_id_attrs =
    match slot_id with None -> [] | Some slot_id -> attrs_of_slot slot_id
  in
  let commitment_attr = attr_of_commitment slot_commitment in
  let attrs = slot_id_attrs @ commitment_attr @ attrs in
  Opentelemetry_profiler.trace ~attrs ~trace_id name f

let trace_slot_no_commitment ?(attrs = []) ~name ~slot f =
  let trace_id = slot_to_trace_id (Bytes.unsafe_of_string slot) in
  let parent_scope = Opentelemetry.Scope.get_ambient_scope () in
  let links =
    match parent_scope with
    | Some scope -> [Opentelemetry.Scope.to_span_link scope]
    | None -> []
  in
  Opentelemetry_profiler.trace
    ?scope:parent_scope
    ~links
    ~attrs
    ~trace_id
    name
    f

let error_span ~error_pp error ~attrs ~start_time ~end_time ~name =
  fst
  @@ Opentelemetry.Span.create
       ~trace_id:(Opentelemetry.Trace_id.create ())
       ~attrs:
         (attrs @ [("error", `String (Format.asprintf "%a" error_pp error))])
       ~start_time
       ~end_time
       name

(** [trace_slot_after_es ~attrs ~name ?slot_index ?slot_id ~error_pp
    ?attrs_of_result ~commitment_of_result f] allows to trace the application of
    f before the trace_id is known. The trace_id (commitment) is extracted from
    f result, as well as extra attributes by using the provided
    [commitment_of_result] and [attrs_of_result] functions.

    WARNING: The span emitted within the function will NOT have this span as
    parent, but the current ambient scope.
*)
let trace_slot_after_es ?(attrs = []) ~name ?slot_index ?slot_id ~error_pp
    ?attrs_of_result ~commitment_of_result f =
  let open Lwt_syntax in
  let parent_scope = Opentelemetry.Scope.get_ambient_scope () in
  let start_time = Opentelemetry.Timestamp_ns.now_unix_ns () in
  let* result = f () in
  (* f returns both payload and commitment *)
  let end_time = Opentelemetry.Timestamp_ns.now_unix_ns () in
  let span =
    match result with
    | Error error ->
        error_span ~error_pp error ~attrs ~start_time ~end_time ~name
    | Ok result ->
        let commitment = commitment_of_result result in
        let trace_id = commitment_to_trace_id commitment in
        let slot_id_attrs =
          match (slot_id, slot_index) with
          | None, None -> []
          | Some slot_id, _ -> attrs_of_slot slot_id
          | _, Some slot_index -> [attr_of_index slot_index]
        in
        let commitment_attr = attr_of_commitment commitment in
        let result_attrs =
          match attrs_of_result with None -> [] | Some f -> f result
        in
        let links =
          match parent_scope with
          | None -> []
          | Some scope -> [Opentelemetry.Scope.to_span_link scope]
        in
        let span, _ =
          Opentelemetry.Span.create
            ~trace_id
            ~links
            ~attrs:(slot_id_attrs @ commitment_attr @ result_attrs @ attrs)
            ~start_time
            ~end_time
            name
        in
        span
  in
  Opentelemetry.Trace.emit [span] ;
  return result
