(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Generators

(* Basic functions for executing tests on a given input *)
let roundtrip_json pp ding v =
  let json =
    try Data_encoding.Json.construct ding v
    with Invalid_argument m ->
      Crowbar.fail (Format.asprintf "Cannot construct: %a (%s)" pp v m)
  in
  let vv =
    try Data_encoding.Json.destruct ding json
    with Data_encoding.Json.Cannot_destruct (_, _) ->
      Crowbar.fail "Cannot destruct"
  in
  Crowbar.check_eq ~pp v vv

let pp_jsonm_lexeme fmt = function
  | `Null -> Format.pp_print_string fmt "(null)"
  | `Bool true -> Format.pp_print_string fmt "(true)"
  | `Bool false -> Format.pp_print_string fmt "(false)"
  | `String _ -> Format.pp_print_string fmt "(string)"
  | `Float f -> Format.fprintf fmt "(float:%f)" f
  | `Name _ -> Format.pp_print_string fmt "(name)"
  | `As -> Format.pp_print_char fmt '['
  | `Ae -> Format.pp_print_char fmt ']'
  | `Os -> Format.pp_print_char fmt '{'
  | `Oe -> Format.pp_print_char fmt '}'

let pp_jsonm_lexeme_seq fmt s = Seq.iter (pp_jsonm_lexeme fmt) s

let roundtrip_json_stream pp ding v =
  let json =
    try Data_encoding.Json.construct_seq ding v
    with Invalid_argument m ->
      Crowbar.fail (Format.asprintf "Cannot construct: %a (%s)" pp v m)
  in
  let str =
    Seq.fold_left ( ^ ) ""
    @@ Data_encoding.Json.string_seq_of_jsonm_lexeme_seq
         ~newline:false
         ~chunk_size_hint:128
         json
  in
  let ezjsonm =
    match Data_encoding.Json.from_string str with
    | Error msg -> Crowbar.failf "%s (%a) (%s)" msg pp_jsonm_lexeme_seq json str
    | Ok json -> json
  in
  let vv =
    try Data_encoding.Json.destruct ding ezjsonm
    with Data_encoding.Json.Cannot_destruct (_, _) ->
      Crowbar.fail "Cannot destruct"
  in
  if v = vv then Crowbar.check true
  else
    Crowbar.failf
      "value: %a@\njsonm_lexeme: %a@\nstring: %s@\nezjsonm: %a;@\nvalue: %a"
      pp
      v
      pp_jsonm_lexeme_seq
      json
      str
      Data_encoding.Json.pp
      ezjsonm
      pp
      vv

let double_trip_binary pp encode_ding decode_ding v =
  let length =
    try Data_encoding.Binary.length encode_ding v
    with Data_encoding.Binary.Write_error we ->
      Format.kasprintf
        Crowbar.fail
        "Cannot length: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_write_error
        we
  in
  let bin =
    try Data_encoding.Binary.to_bytes_exn encode_ding v
    with Data_encoding.Binary.Write_error we ->
      Format.kasprintf
        Crowbar.fail
        "Cannot construct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_write_error
        we
  in
  Crowbar.check_eq ~pp:Format.pp_print_int length (Bytes.length bin) ;
  let vv =
    try Data_encoding.Binary.of_bytes_exn decode_ding bin
    with Data_encoding.Binary.Read_error re ->
      Format.kasprintf
        Crowbar.fail
        "Cannot destruct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_read_error
        re
  in
  Crowbar.check_eq ~pp v vv

let roundtrip_binary_to_bytes pp ding v = double_trip_binary pp ding ding v

let roundtrip_binary_to_string pp ding v =
  let length =
    try Data_encoding.Binary.length ding v
    with Data_encoding.Binary.Write_error we ->
      Format.kasprintf
        Crowbar.fail
        "Cannot length: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_write_error
        we
  in
  let bin =
    try Data_encoding.Binary.to_string_exn ding v
    with Data_encoding.Binary.Write_error we ->
      Format.kasprintf
        Crowbar.fail
        "Cannot construct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_write_error
        we
  in
  Crowbar.check_eq ~pp:Format.pp_print_int length (String.length bin) ;
  let vv =
    try Data_encoding.Binary.of_string_exn ding bin
    with Data_encoding.Binary.Read_error re ->
      Format.kasprintf
        Crowbar.fail
        "Cannot destruct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_read_error
        re
  in
  Crowbar.check_eq ~pp v vv

let roundtrip_binary_write pp ding v slack =
  let size = Data_encoding.Binary.length ding v in
  let buffer = Bytes.create (size + slack) in
  let state =
    Option.get
    @@ Data_encoding.Binary.make_writer_state
         buffer
         ~offset:0
         ~allowed_bytes:size
  in
  let written =
    try Data_encoding.Binary.write_exn ding v state
    with Data_encoding.Binary.Write_error we ->
      Format.kasprintf
        Crowbar.fail
        "Cannot construct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_write_error
        we
  in
  Crowbar.check_eq written size ;
  let read, vv =
    try
      Data_encoding.Binary.read_exn ding (Bytes.unsafe_to_string buffer) 0 size
    with Data_encoding.Binary.Read_error re ->
      Format.kasprintf
        Crowbar.fail
        "Cannot destruct: %a (%a)"
        pp
        v
        Data_encoding.Binary.pp_read_error
        re
  in
  Crowbar.check_eq read size ;
  Crowbar.check_eq ~pp v vv

(* Setting up the actual tests *)
let test_full_and_v_json (full_and_v : full_and_v) =
  match full_and_v with
  | FullAndV (full, v) ->
      let module Full = (val full) in
      roundtrip_json Full.pp Full.encoding v

let test_full_and_v_json_stream (full_and_v : full_and_v) =
  match full_and_v with
  | FullAndV (full, v) ->
      let module Full = (val full) in
      roundtrip_json_stream Full.pp Full.encoding v

let test_full_and_v_binary_to_bytes (full_and_v : full_and_v) =
  match full_and_v with
  | FullAndV (full, v) ->
      let module Full = (val full) in
      roundtrip_binary_to_bytes Full.pp Full.encoding v

let test_full_and_v_binary_to_string (full_and_v : full_and_v) =
  match full_and_v with
  | FullAndV (full, v) ->
      let module Full = (val full) in
      roundtrip_binary_to_string Full.pp Full.encoding v

let test_full_and_v_binary_write (full_and_v : full_and_v) slack =
  match full_and_v with
  | FullAndV (full, v) ->
      let module Full = (val full) in
      roundtrip_binary_write Full.pp Full.encoding v slack

let () =
  let open Crowbar in
  add_test
    ~name:"binary roundtrips (write/read)"
    [gen; uint8]
    test_full_and_v_binary_write ;
  add_test
    ~name:"binary roundtrips (to_/of_bytes)"
    [gen]
    test_full_and_v_binary_to_bytes ;
  add_test
    ~name:"binary roundtrips (to_/of_string)"
    [gen]
    test_full_and_v_binary_to_string ;
  add_test
    ~name:"binary roundtrips (to_/of_string) (compacts)"
    [gen_with_compact]
    test_full_and_v_binary_to_string ;
  add_test
    ~name:"json roundtrips (construct/destruct)"
    [gen]
    test_full_and_v_json ;
  ignore test_full_and_v_json_stream ;
  (* TODO:
      - Don't use [=] on floats because we don't have that level of precision
      - Improve perfs (use blit-instructions with a large enough buffer)
      - Escape strings when utf8 is malformed during streaming
     add_test
       ~name:"json roundtrips (construct/destruct)"
       [gen]
       test_full_and_v_json_stream ;
  *)
  ()
