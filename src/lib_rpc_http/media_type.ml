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

include Resto_cohttp.Media_type.Make (Tezos_rpc.Encoding)

(* emits chunks of size approx chunk_size_hint but occasionally a bit bigger *)
let chunk_size_hint = 4096

let json =
  let destruct enc body =
    try
      Data_encoding.Json.from_string body
      |> Result.map (Data_encoding.Json.destruct enc)
    with
    | (Stack_overflow | Out_of_memory) as exc -> raise exc
    | Data_encoding.Json.Cannot_destruct (_, exc) ->
        Error
          (Format.asprintf
             "%a"
             (fun fmt -> Data_encoding.Json.print_error fmt)
             exc)
    | exc -> Error (Printexc.to_string exc)
  in
  let destruct_many enc body =
    match String.split_on_char '\n' body |> List.rev with
    | [] -> ([], "")
    | last :: rfirsts ->
        let firsts =
          List.rev_filter_map
            (fun s ->
              match destruct enc s with
              | Error e ->
                  Format.eprintf
                    "Ignoring unparsable value in JSON stream: %s\nValue: %s@."
                    e
                    s ;
                  None
              | Ok v -> Some v)
            rfirsts
        in
        (firsts, last)
  in
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
        let json = Data_encoding.Json.construct enc v in
        Data_encoding.Json.to_string ~minify:true ~newline:true json);
    construct_seq =
      (fun enc v ->
        let buffer = Bytes.create chunk_size_hint in
        Data_encoding.Json.blit_instructions_seq_of_jsonm_lexeme_seq
          ~newline:true
          ~buffer
        @@ Data_encoding.Json.construct_seq enc v);
    destruct;
    destruct_many;
  }

let bson =
  let construct_bytes enc v =
    Json_repr_bson.bson_to_bytes @@ Data_encoding.Bson.construct enc v
  in
  let destruct enc body =
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
        | exc -> Error (Printexc.to_string exc))
  in
  let destruct_many enc body =
    let seq, rest =
      try
        let seq, offset_rest =
          Json_repr_bson.bytes_to_bson_list
            ~laziness:false
            ~copy:false
            (Bytes.of_string body)
        in
        ( seq,
          match offset_rest with
          | None -> ""
          | Some ofs -> String.sub body ofs (String.length body - ofs) )
      with exn ->
        Format.eprintf
          "Ignoring unparsable value in BSON stream: %s@."
          (Printexc.to_string exn) ;
        ([], "")
    in
    ( List.filter_map
        (fun bson ->
          try Some (Data_encoding.Bson.destruct enc bson) with
          | Data_encoding.Json.Cannot_destruct (_, exc) ->
              Format.eprintf
                "Ignoring undecodable value in BSON stream: %a@."
                (Data_encoding.Json.print_error ?print_unknown:None)
                exc ;
              None
          | exn ->
              Format.eprintf
                "Ignoring failed decoded value in BSON stream: %s@."
                (Printexc.to_string exn) ;
              None)
        seq,
      rest )
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
        | exception e ->
            Format.fprintf ppf "@[Invalid BSON:@ %s@]" (Printexc.to_string e)
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
    destruct;
    destruct_many;
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
    destruct_many =
      (fun enc s ->
        let enc = dynsize enc in
        let len = String.length s in
        let rec loop acc ofs =
          let remaining = len - ofs in
          if remaining = 0 then (List.rev acc, "")
          else
            match Data_encoding.Binary.read_opt enc s ofs remaining with
            | None -> (List.rev acc, String.sub s ofs remaining)
            | Some (ofs, elt) -> loop (elt :: acc) ofs
        in
        loop [] 0);
  }

let all_media_types = [json; bson; octet_stream]

let encoding : t Tezos_rpc.Encoding.t =
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

module Command_line = struct
  type t = Any | Json | Binary

  let parse_cli_parameter = function
    | "json" -> Some Json
    | "binary" -> Some Binary
    | "any" -> Some Any
    | _ -> None

  let of_command_line = function
    | Any -> all_media_types
    | Binary -> [octet_stream]
    | Json -> [json; bson]

  let pp_parameter ppf = function
    | Any -> Format.fprintf ppf "any"
    | Binary -> Format.fprintf ppf "binary"
    | Json -> Format.fprintf ppf "json"

  let encoding =
    Data_encoding.string_enum [("json", Json); ("binary", Binary); ("any", Any)]
end
