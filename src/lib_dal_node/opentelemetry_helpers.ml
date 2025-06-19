(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Slot_id_hash = Blake2B.Make_minimal_with_data (struct
  let name = "dal_slot_id"

  let title = "dal slot_id hash, used for Opentelemetry traces"

  (* Enforces the hash to have 16 bytes, to be compatible with Opentelemetry
     trace ids. *)
  let size = Some 16
end)

(* The first string value serves as salt for the hash, it avoids any value
   that has the same encoding to have the same trace_id as the one produced
   for slot_id (and produce inconsistent traces). *)
let slot_encoding =
  Data_encoding.(
    conv
      (fun Types.Slot_id.{slot_level; slot_index} ->
        (Slot_id_hash.name, slot_level, slot_index))
      (fun (_, slot_level, slot_index) -> {slot_level; slot_index})
      (tup3 string int32 int31))

let trace_slot ?(attrs = []) ~name slot_id f =
  let trace_id =
    [Data_encoding.Binary.to_bytes_exn slot_encoding slot_id]
    |> Slot_id_hash.hash_bytes |> Slot_id_hash.to_bytes
    |> Opentelemetry.Trace_id.of_bytes
  in
  let attrs =
    [
      ("level", `Int (Int32.to_int slot_id.slot_level));
      ("slot_index", `Int slot_id.slot_index);
    ]
    @ attrs
  in
  Opentelemetry_profiler.trace ~attrs ~trace_id name f
