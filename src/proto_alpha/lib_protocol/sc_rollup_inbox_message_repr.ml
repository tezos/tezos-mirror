(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

type error +=
  | (* `Permanent *) Error_encode_inbox_message
  | (* `Permanent *) Error_decode_inbox_message

let () =
  let open Data_encoding in
  let msg =
    "Failed to encode a rollup management protocol inbox message value"
  in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_inbox_message_encoding"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_encode_inbox_message -> Some () | _ -> None)
    (fun () -> Error_encode_inbox_message) ;
  let msg =
    "Failed to decode a smart rollup management protocol inbox message value"
  in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_inbox_message_decoding"
    ~title:msg
    ~pp:(fun fmt () -> Format.fprintf fmt "%s" msg)
    ~description:msg
    unit
    (function Error_decode_inbox_message -> Some () | _ -> None)
    (fun () -> Error_decode_inbox_message)

type internal_inbox_message =
  | Transfer of {
      payload : Script_repr.expr;
      sender : Contract_hash.t;
      source : Signature.public_key_hash;
      destination : Sc_rollup_repr.Address.t;
    }
  | Start_of_level
  | End_of_level
  | Info_per_level of {
      predecessor_timestamp : Time.t;
      predecessor : Block_hash.t;
    }
  | Protocol_migration of string

let internal_inbox_message_encoding =
  let open Data_encoding in
  let kind name = req "internal_inbox_message_kind" (constant name) in
  union
    [
      case
        (Tag 0)
        ~title:"Transfer"
        (obj5
           (kind "transfer")
           (req "payload" Script_repr.expr_encoding)
           (req "sender" Contract_hash.encoding)
           (req "source" Signature.Public_key_hash.encoding)
           (req "destination" Sc_rollup_repr.Address.encoding))
        (function
          | Transfer {payload; sender; source; destination} ->
              Some ((), payload, sender, source, destination)
          | _ -> None)
        (fun ((), payload, sender, source, destination) ->
          Transfer {payload; sender; source; destination});
      case
        (Tag 1)
        ~title:"Start_of_level"
        (obj1 (kind "start_of_level"))
        (function Start_of_level -> Some () | _ -> None)
        (fun () -> Start_of_level);
      case
        (Tag 2)
        ~title:"End_of_level"
        (obj1 (kind "end_of_level"))
        (function End_of_level -> Some () | _ -> None)
        (fun () -> End_of_level);
      case
        (Tag 3)
        ~title:"Info_per_level"
        (obj3
           (kind "info_per_level")
           (req "predecessor_timestamp" Time.encoding)
           (req "predecessor" Block_hash.encoding))
        (function
          | Info_per_level {predecessor_timestamp; predecessor} ->
              Some ((), predecessor_timestamp, predecessor)
          | _ -> None)
        (fun ((), predecessor_timestamp, predecessor) ->
          Info_per_level {predecessor_timestamp; predecessor});
      case
        (Tag 4)
        ~title:"Protocol_migration"
        (obj2 (kind "protocol_migration") (req "protocol" (string Hex)))
        (function Protocol_migration proto -> Some ((), proto) | _ -> None)
        (fun ((), proto) -> Protocol_migration proto);
    ]

type t = Internal of internal_inbox_message | External of string

let encoding =
  let open Data_encoding in
  check_size
    Constants_repr.sc_rollup_message_size_limit
    (union
       [
         case
           (Tag 0)
           ~title:"Internal"
           internal_inbox_message_encoding
           (function
             | Internal internal_message -> Some internal_message
             | External _ -> None)
           (fun internal_message -> Internal internal_message);
         case
           (Tag 1)
           ~title:"External"
           Variable.(string Hex)
           (function External msg -> Some msg | Internal _ -> None)
           (fun msg -> External msg);
       ])

type serialized = string

let serialize msg =
  let open Result_syntax in
  match Data_encoding.Binary.to_string_opt encoding msg with
  | None -> tzfail Error_encode_inbox_message
  | Some str -> return str

let deserialize s =
  let open Result_syntax in
  match Data_encoding.Binary.of_string_opt encoding s with
  | None -> tzfail Error_decode_inbox_message
  | Some msg -> return msg

let unsafe_of_string s = s

let unsafe_to_string s = s

(* 32 *)
let hash_prefix = "\003\255\138\145\170" (* srib3(55) *)

module Hash = struct
  let prefix = "srib3"

  let encoded_size = 55

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "Smart_rollup_serialized_message_hash"

        let title =
          "The hash of a serialized message of the smart rollup inbox."

        let b58check_prefix = hash_prefix

        (* defaults to 32 *)
        let size = None
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size
end

let hash_serialized_message (payload : serialized) =
  Hash.hash_string [(payload :> string)]

let start_of_level_serialized =
  (* If [Start_of_level] cannot be serialized, this will be detected at
     startup time as we are defining a top-level value. *)
  Data_encoding.Binary.to_string_exn encoding (Internal Start_of_level)

let end_of_level_serialized =
  (* If [End_of_level] cannot be serialized, this will be detected at
     startup time as we are defining a top-level value. *)
  Data_encoding.Binary.to_string_exn encoding (Internal End_of_level)

let info_per_level_serialized ~predecessor_timestamp ~predecessor =
  match
    serialize (Internal (Info_per_level {predecessor_timestamp; predecessor}))
  with
  | Error _ ->
      (* The info per level should always be serializable as the encoding
         functions for this case do not fail. *)
      assert false
  | Ok info -> info

let (_dummy_serialized_info_per_level_serialized : serialized) =
  (* This allows to detect an error, at startup, we might have introduced in the
     encoding of serialization of info per level messages . *)
  info_per_level_serialized
    ~predecessor_timestamp:(Time.of_seconds Int64.min_int)
    ~predecessor:Block_hash.zero
