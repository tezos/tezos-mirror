(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

open Protocol
open Alpha_context
open Protocol_client_context

type error +=
  | Slot_size_is_too_big of {actual_size : int; max_size : int}
  | Wrong_slot_frame_version of {expected : int; provided : int}
  | Could_not_deserialize_slot

let () =
  register_error_kind
    `Permanent
    ~id:"slot_size_is_too_big"
    ~title:"Slot size is too big"
    ~description:"Slot cannot fit in maximum size"
    ~pp:(fun ppf (actual_size, max_size) ->
      Format.fprintf
        ppf
        "Actual size: %d, Maximum size: %d"
        actual_size
        max_size)
    Data_encoding.(obj2 (req "actual_size" int31) (req "maximum_size" int31))
    (function
      | Slot_size_is_too_big {actual_size; max_size} ->
          Some (actual_size, max_size)
      | _ -> None)
    (fun (actual_size, max_size) ->
      Slot_size_is_too_big {actual_size; max_size}) ;
  register_error_kind
    `Permanent
    ~id:"wrong_slot_frame_version"
    ~title:"Wrong slot frame version"
    ~description:"Wrong slot frame version"
    ~pp:(fun ppf (expected, provided) ->
      Format.fprintf
        ppf
        "Version expected: %d, Version provided: %d"
        expected
        provided)
    Data_encoding.(
      obj2 (req "version_expected" uint8) (req "version_provided" uint8))
    (function
      | Wrong_slot_frame_version {expected; provided} ->
          Some (expected, provided)
      | _ -> None)
    (fun (expected, provided) -> Wrong_slot_frame_version {expected; provided}) ;
  register_error_kind
    `Permanent
    ~id:"could_not_deserialize_slot"
    ~title:"Slot could not be deserialized"
    ~description:"Error when recovering slot contents from binary"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Error when recovering slot contents from binary")
    Data_encoding.(unit)
    (function Could_not_deserialize_slot -> Some () | _ -> None)
    (fun () -> Could_not_deserialize_slot)

type version = int

type message = string

module Rollups_map = Map.Make (Sc_rollup.Address)

type t = message list Rollups_map.t

module type Slot_version = sig
  val version_prefix : version

  val expected_slot_size : t -> int

  val serialize : max_size:int -> t -> string tzresult Lwt.t

  val deserialize : max_size:int -> string -> t tzresult Lwt.t
end

let version_encoding = Data_encoding.uint8

module V0 = struct
  let version_prefix = 0

  (* Binary representation of string uses 4 bytes as a header,
     containing the string length. This conforms to the specification
     given for messages. *)
  let message_encoding = Data_encoding.string

  (* Binary representation of lists uses 4 bytes as a header, containing
     the length of the list in bytes. This conforms to the
     specification given for messages frames. *)
  let messages_frame_encoding = Data_encoding.(list message_encoding)

  (* Binary representation of lists uses 4 bytes as a header, containing
     the size of the encoded list contents in bytes.
     `all_messages_frame_encoding` encodes not only the encoded messages
     frames, but also the 4 bytes containing the length of all messages
     frames that separate the rollups frame from the messages frame.
  *)
  let all_messages_frames_encoding =
    Data_encoding.(list messages_frame_encoding)

  (* Binary representation of a [Sc_rollup.Address.t] uses 20 bytes,
     while the encoding of a [int32] uses 4 bytes. These are
     concatenated together by `rollup_offset_entry_encoding`. *)
  let rollup_offset_entry_encoding =
    Data_encoding.(tup2 Sc_rollup.Address.encoding int32)

  (* Binary representation of lists uses 4 bytes as a header, containing
     the size of the encoded list contents in bytes.
     the size in bytes of the rollups frame. containing
     the length of the list in bytes. Thus the rollups_frame_encoding
     conforms to the specification given*)
  let rollups_frame_encoding = Data_encoding.(list rollup_offset_entry_encoding)

  let slot_encoding ~max_size =
    Data_encoding.(
      check_size max_size
      @@ tup3
           version_encoding
           rollups_frame_encoding
           all_messages_frames_encoding)

  module Internal = struct
    let size_of_opt size = match size with None -> assert false | Some n -> n

    let message_size = Data_encoding.Binary.length message_encoding

    let messages_frame_size =
      Data_encoding.Binary.length messages_frame_encoding

    let all_messages_frames_size =
      Data_encoding.Binary.length all_messages_frames_encoding

    let frame_prefix_size =
      size_of_opt Data_encoding.(Binary.fixed_length int32)

    let frame_version_size =
      size_of_opt Data_encoding.(Binary.fixed_length uint8)

    let rollup_entry_size =
      size_of_opt
      @@ Data_encoding.Binary.fixed_length rollup_offset_entry_encoding

    let rollups_frame_size number_of_rollups =
      (rollup_entry_size * number_of_rollups) + frame_prefix_size
  end

  let expected_slot_size all_rollups_messages =
    let bindings = Rollups_map.bindings all_rollups_messages in
    let number_of_rollups = bindings |> List.length in
    let messages = List.map snd bindings in
    Internal.(
      frame_version_size
      + rollups_frame_size number_of_rollups
      + all_messages_frames_size messages)

  let serialize ~max_size all_rollups_messages =
    let open Lwt_result_syntax in
    let first_messages_frame_offset =
      Internal.(
        frame_version_size
        + rollups_frame_size
            (Rollups_map.bindings all_rollups_messages |> List.length)
        + frame_prefix_size)
    in
    let rev_rollups_frame, rev_messages_frames, expected_slot_size =
      Rollups_map.fold
        (fun rollup messages (rollups_frame, messages_frames, next_offset) ->
          let rollups_frame =
            (rollup, Int32.of_int next_offset) :: rollups_frame
          in
          let messages_frames = messages :: messages_frames in
          let next_offset =
            next_offset + Internal.messages_frame_size messages
          in
          (rollups_frame, messages_frames, next_offset))
        all_rollups_messages
        ([], [], first_messages_frame_offset)
    in
    let* () =
      fail_unless
        (expected_slot_size <= max_size)
        (Slot_size_is_too_big {actual_size = expected_slot_size; max_size})
    in
    let rollups_frame = List.rev rev_rollups_frame in
    let messages_frames = List.rev rev_messages_frames in
    let*? result =
      Data_encoding.Binary.to_string
        (slot_encoding ~max_size)
        (version_prefix, rollups_frame, messages_frames)
      |> Result.map_error (fun _ -> assert false)
    in
    return result

  (* Deserialization of slot contents will be done by WASM PVM kernels, as
     per #3374. However, we should still provide a function to
     deserialize the contents of a slot, as other components other than the
     PVM kernel may need to inspect it. *)
  let deserialize ~max_size serialized =
    let open Lwt_result_syntax in
    let actual_size = String.length serialized in
    let* () =
      fail_unless
        (actual_size <= max_size)
        (Slot_size_is_too_big {actual_size; max_size})
    in
    let version_prefix, rollups_frame, messages_frames =
      Data_encoding.Binary.of_string_exn (slot_encoding ~max_size) serialized
    in
    let* () =
      fail_when
        (version_prefix <> 0)
        (Wrong_slot_frame_version {expected = 0; provided = version_prefix})
    in
    let* () =
      fail_when
        (List.compare_length_with messages_frames @@ List.length rollups_frame
        <> 0)
        Could_not_deserialize_slot
    in
    let*? deserialized =
      List.map2
        ~when_different_lengths:[]
        (fun (rollup, _offset) messages -> (rollup, messages))
        rollups_frame
        messages_frames
      |> Result.map_error (fun _ -> [Could_not_deserialize_slot])
      |> Result.map (fun rollups_with_messages ->
             Rollups_map.add_seq
               (List.to_seq rollups_with_messages)
               Rollups_map.empty)
    in
    return deserialized
end
