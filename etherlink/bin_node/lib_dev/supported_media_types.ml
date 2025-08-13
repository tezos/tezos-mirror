(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let m_json =
  Media_type.
    {
      json with
      construct =
        (fun enc v ->
          let json =
            Opentelemetry.Trace.with_
              ~kind:Span_kind_server
              ~service_name:"HTTP_server"
              "Data_encoding.Json.construct"
            @@ fun _ -> Data_encoding.Json.construct enc v
          in
          Opentelemetry.Trace.with_
            ~kind:Span_kind_server
            ~service_name:"HTTP_server"
            "Data_encoding.Json.to_string"
          @@ fun _ ->
          Data_encoding.Json.to_string ~minify:true ~newline:true json);
    }

let json_with_charset charset =
  Media_type.
    [
      {
        m_json with
        name =
          Cohttp.Accept.MediaType
            ("application", Format.sprintf "json; charset=%s" charset);
      };
      {
        m_json with
        name =
          Cohttp.Accept.MediaType
            ("application", Format.sprintf "json;charset=%s" charset);
      };
    ]

let all =
  [m_json] @ json_with_charset "utf-8" @ json_with_charset "UTF-8"
  (* We keep [bson] and [octet_stream] after [json] and its variants as the
     formers are less likely to be used. *)
  @ Media_type.[bson; octet_stream]
