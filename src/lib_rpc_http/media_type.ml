(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

include Resto_cohttp.Media_type.Make (RPC_encoding)

(* emits chunks of size approx chunk_size_hint but occasionally a bit bigger *)
let chunk_size_hint = 4096

let json =
  {
    name = Cohttp.Accept.MediaType ("application", "json");
    q = Some 1000;
    pp =
      (fun _enc ppf raw ->
        match Data_encoding.Json.from_string raw with
        | Error err ->
            Format.fprintf
              ppf
              "@[Invalid JSON:@  - @[<v 2>Error:@ %s@] - @[<v 2>Raw data:@ \
               %s@]@]"
              err
              raw
        | Ok json -> Data_encoding.Json.pp ppf json);
    construct =
      (fun enc v ->
        Data_encoding.Json.to_string ~newline:true ~minify:true
        @@ Data_encoding.Json.construct enc v);
    construct_seq =
      (fun enc v ->
        let buffer = Bytes.create chunk_size_hint in
        Data_encoding.Json.blit_instructions_seq_of_jsonm_lexeme_seq
          ~newline:true
          ~buffer
        @@ Data_encoding.Json.construct_seq enc v);
    destruct =
      (fun enc body ->
        match Data_encoding.Json.from_string body with
        | Error _ as err -> err
        | Ok json -> (
            try Ok (Data_encoding.Json.destruct enc json) with
            | (Stack_overflow | Out_of_memory) as exc -> raise exc
            | Data_encoding.Json.Cannot_destruct (_, exc) ->
                Error
                  (Format.asprintf
                     "%a"
                     (fun fmt -> Data_encoding.Json.print_error fmt)
                     exc)
            | exc -> Error (Printexc.to_string exc)));
  }

let bson =
  let construct_bytes enc v =
    Json_repr_bson.bson_to_bytes @@ Data_encoding.Bson.construct enc v
  in
  {
    name = Cohttp.Accept.MediaType ("application", "bson");
    q = Some 100;
    pp =
      (fun _enc ppf raw ->
        match
          Json_repr_bson.bytes_to_bson
            ~laziness:false
            ~copy:false
            (Bytes.of_string raw)
        with
        | exception Json_repr_bson.Bson_decoding_error (msg, _, _) ->
            Format.fprintf ppf "@[Invalid BSON:@ %s@]" msg
        | bson ->
            let json =
              Json_repr.convert
                (module Json_repr_bson.Repr)
                (module Json_repr.Ezjsonm)
                bson
            in
            Data_encoding.Json.pp ppf json);
    construct = (fun env v -> Bytes.unsafe_to_string (construct_bytes env v));
    construct_seq =
      (fun enc v ->
        let b = construct_bytes enc v in
        Seq.return (b, 0, Bytes.length b));
    destruct =
      (fun enc body ->
        match
          Json_repr_bson.bytes_to_bson
            ~laziness:false
            ~copy:false
            (Bytes.of_string body)
        with
        | exception Json_repr_bson.Bson_decoding_error (msg, _, pos) ->
            Error (Format.asprintf "(at offset: %d) %s" pos msg)
        | bson -> (
            try Ok (Data_encoding.Bson.destruct enc bson) with
            | (Stack_overflow | Out_of_memory) as exc -> raise exc
            | Data_encoding.Json.Cannot_destruct (_, exc) ->
                Error
                  (Format.asprintf
                     "%a"
                     (fun fmt -> Data_encoding.Json.print_error fmt)
                     exc)
            | exc -> Error (Printexc.to_string exc)));
  }

let octet_stream =
  let dynsize enc =
    (* We add a size header to all the binary exchanges between server and
       client *)
    Data_encoding.dynamic_size enc
  in
  let construct_bytes enc v =
    Data_encoding.Binary.to_bytes_exn (dynsize enc) v
  in
  let construct_string enc v =
    Data_encoding.Binary.to_string_exn (dynsize enc) v
  in
  {
    name = Cohttp.Accept.MediaType ("application", "octet-stream");
    q = Some 200;
    pp =
      (fun enc ppf raw ->
        match Data_encoding.Binary.of_string (dynsize enc) raw with
        | Error Data_encoding.Binary.Not_enough_data ->
            Format.fprintf
              ppf
              "Partial read (%d bytes), waiting for more data (Not enough \
               data): %a"
              (String.length raw)
              Hex.pp
              (Hex.of_string raw)
        | Error re ->
            Format.fprintf
              ppf
              "Invalid binary data: %a."
              Data_encoding.Binary.pp_read_error
              re
        | Ok v ->
            Format.fprintf
              ppf
              ";; binary equivalent of the following json@.%a"
              Data_encoding.Json.pp
              (Data_encoding.Json.construct (dynsize enc) v));
    construct = construct_string;
    construct_seq =
      (fun enc v ->
        let b = construct_bytes enc v in
        Seq.return (b, 0, Bytes.length b));
    destruct =
      (fun enc s ->
        match Data_encoding.Binary.of_string (dynsize enc) s with
        | Error re ->
            Error
              (Format.asprintf
                 "Failed to parse binary data: %a."
                 Data_encoding.Binary.pp_read_error
                 re)
        | Ok data -> Ok data);
  }

let all_media_types = [json; bson; octet_stream]

let encoding : t RPC_encoding.t =
  Data_encoding.string_enum
    [
      ("application/json", json);
      ("application/bson", bson);
      ("application/octet-stream", octet_stream);
    ]

module Content_type = struct
  type t = string * string

  let json = ("application", "json")

  let bson = ("application", "bson")

  let octet_stream = ("application", "octet-stream")

  let pp fmt (l, r) = Format.fprintf fmt "%s/%s" l r
end

let of_content_type c =
  if c = Content_type.json then Some json
  else if c = Content_type.bson then Some bson
  else if c = Content_type.octet_stream then Some octet_stream
  else None
