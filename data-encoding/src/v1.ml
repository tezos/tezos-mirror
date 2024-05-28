(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Encoding = struct
  include Encoding

  type 'a matching_function = 'a -> match_result

  let splitted ~json ~binary = raw_splitted ~json:(Json.convert json) ~binary

  let uint_like_n ?max_value () =
    let max_value =
      match max_value with
      | None -> Binary_size.max_int `Uint30
      | Some max_value ->
          if Binary_size.max_int `Uint30 < max_value then
            invalid_arg "Data_encoding.uint_like_n" ;
          max_value
    in
    let binary = uint_like_n ~max_value in
    let json = ranged_int 0 max_value in
    splitted ~json ~binary

  let int_like_z ?min_value ?max_value () =
    let max_value =
      match max_value with
      | None -> Binary_size.max_int `Int31
      | Some max_value ->
          if Binary_size.max_int `Int31 < max_value then
            invalid_arg "Data_encoding.int_like_z" ;
          max_value
    in
    let min_value =
      match min_value with
      | None -> Binary_size.min_int `Int31
      | Some min_value ->
          if min_value < Binary_size.min_int `Int31 then
            invalid_arg "Data_encoding.int_like_z" ;
          min_value
    in
    let binary = int_like_z ~min_value ~max_value in
    let json = ranged_int min_value max_value in
    splitted ~json ~binary

  let assoc enc =
    let json = Json_encoding.assoc (Json.convert enc) in
    let binary = list (tup2 string enc) in
    raw_splitted ~json ~binary

  module Bounded = struct
    let string' ?length_kind json_repr length =
      let max_length =
        match length_kind with
        | None -> Binary_size.max_int `Uint30 (* biggest that's allowed *)
        | Some kind -> Binary_size.max_int kind
      in
      if length > max_length then
        raise
          (Invalid_argument
             "Data_encoding.Encoding.Bounded.string': length bound is greater \
              than maximum length allowed in size header.") ;
      if length <= 0 then
        raise
          (Invalid_argument
             "Data_encoding.Encoding.Bounded.string': negative length.") ;
      raw_splitted
        ~binary:
          (let kind =
             match length_kind with
             | None ->
                 (Binary_size.unsigned_range_to_size length
                   :> Binary_size.length)
             | Some kind -> kind
           in
           dynamic_size ~kind (check_size length Variable.string))
        ~json:
          (let open Json_encoding in
          conv
            (fun s ->
              if String.length s > length then invalid_arg "oversized string" ;
              s)
            (fun s ->
              if String.length s > length then
                raise
                  (Cannot_destruct ([], Invalid_argument "oversized string")) ;
              s)
            (Json.string json_repr))

    let string length = string' Plain length

    let bytes' ?length_kind json_repr length =
      let max_length =
        match length_kind with
        | None -> Binary_size.max_int `Uint30 (* biggest that's allowed *)
        | Some kind -> Binary_size.max_int kind
      in
      if length > max_length then
        raise
          (Invalid_argument
             "Data_encoding.Encoding.Bounded.string': length bound is greater \
              than maximum length allowed in size header.") ;
      if length <= 0 then
        raise
          (Invalid_argument
             "Data_encoding.Encoding.Bounded.string': negative length.") ;
      raw_splitted
        ~binary:
          (let kind =
             match length_kind with
             | None ->
                 (Binary_size.unsigned_range_to_size length
                   :> Binary_size.length)
             | Some kind -> kind
           in
           dynamic_size ~kind (check_size length Variable.bytes))
        ~json:
          (let open Json_encoding in
          conv
            (fun s ->
              if Bytes.length s > length then invalid_arg "oversized string" ;
              s)
            (fun s ->
              if Bytes.length s > length then
                raise
                  (Cannot_destruct ([], Invalid_argument "oversized string")) ;
              s)
            (Json.bytes json_repr))

    let bytes length = bytes' Hex length

    let bigstring ?length_kind ?(string_json_repr = Hex) length =
      let max_length =
        match length_kind with
        | None -> Binary_size.max_int `Uint30 (* biggest that's allowed *)
        | Some kind -> Binary_size.max_int kind
      in
      if length > max_length then
        raise
          (Invalid_argument
             "Data_encoding.Encoding.Bounded.bigstring': length bound is \
              greater than maximum length allowed in size header.") ;
      if length <= 0 then
        raise
          (Invalid_argument
             "Data_encoding.Encoding.Bounded.bigstring: negative length.") ;
      raw_splitted
        ~binary:
          (let kind =
             match length_kind with
             | None ->
                 (Binary_size.unsigned_range_to_size length
                   :> Binary_size.length)
             | Some kind -> kind
           in
           dynamic_size ~kind (check_size length (Variable.bigstring ())))
        ~json:
          (let open Json_encoding in
          conv
            (fun s ->
              if Bigstringaf.length s > length then
                invalid_arg "oversized string" ;
              Bigstringaf.to_string s)
            (fun s ->
              if String.length s > length then
                raise
                  (Cannot_destruct ([], Invalid_argument "oversized string")) ;
              Bigstringaf.of_string ~off:0 ~len:(String.length s) s)
            (Json.string string_json_repr))
  end

  type 'a lazy_state = Value of 'a | Bytes of Bytes.t | Both of Bytes.t * 'a

  type 'a lazy_t = {mutable state : 'a lazy_state; encoding : 'a t}

  let force_decode_internal le =
    match le.state with
    | Value value -> value
    | Both (_, value) -> value
    | Bytes bytes -> (
        match Binary_reader.of_bytes_opt le.encoding bytes with
        | Some expr ->
            le.state <- Both (bytes, expr) ;
            expr
        | None ->
            raise
              (Json_encoding
               .Decoding_exception_whilst_conversion_of_lazy_encoding
                 bytes))

  let force_decode le =
    match force_decode_internal le with
    | exception
        Json_encoding.Decoding_exception_whilst_conversion_of_lazy_encoding _ ->
        None
    | v -> Some v

  let force_bytes le =
    match le.state with
    | Bytes bytes -> bytes
    | Both (bytes, _) -> bytes
    | Value value ->
        let bytes = Binary_writer.to_bytes_exn le.encoding value in
        le.state <- Both (bytes, value) ;
        bytes

  let lazy_encoding encoding =
    let binary =
      Encoding.conv
        force_bytes
        (fun bytes -> {state = Bytes bytes; encoding})
        Encoding.bytes
    in
    let json =
      Encoding.union
        [
          case
            ~title:"invalid_lazy_bytes"
            Json_only
            (raw_splitted ~json:Json_encoding.invalid_lazy_bytes ~binary:bytes)
            (fun _ -> None)
            (fun bytes -> {state = Bytes bytes; encoding});
          case
            ~title:"valid_lazy_bytes"
            Json_only
            (Encoding.conv
               force_decode_internal
               (fun value -> {state = Value value; encoding})
               encoding)
            (fun x -> Some x)
            (fun x -> x);
        ]
    in
    splitted ~json ~binary

  let make_lazy encoding value = {encoding; state = Value value}

  let apply_lazy ~fun_value ~fun_bytes ~fun_combine le =
    match le.state with
    | Value value -> fun_value value
    | Bytes bytes -> fun_bytes bytes
    | Both (bytes, value) -> fun_combine (fun_value value) (fun_bytes bytes)

  module With_field_name_duplicate_checks = SaferEncoding
  module With_JSON_discriminant = SaferEncoding
  module Compact = Compact

  type 'a compact = 'a Compact.t
end

include Encoding
module With_version = With_version
module Registration = Registration

module Json = struct
  include Json
  include Json_stream
end

module Bson = Bson
module Binary_schema = Binary_schema
module Binary_stream = Binary_stream

module Binary = struct
  include Binary_error_types
  include Binary_error
  include Binary_length
  include Binary_writer
  include Binary_reader
  include Binary_stream_reader
  module Slicer = Binary_slicer

  let describe = Binary_description.describe
end

type json = Json.t

let json = Json.encoding

type json_schema = Json.schema

let json_schema = Json.schema_encoding

type bson = Bson.t
