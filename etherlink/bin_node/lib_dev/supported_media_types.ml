(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let json_with_charset charset =
  Media_type.
    [
      {
        json with
        name =
          Cohttp.Accept.MediaType
            ("application", Format.sprintf "json; charset=%s" charset);
      };
      {
        json with
        name =
          Cohttp.Accept.MediaType
            ("application", Format.sprintf "json;charset=%s" charset);
      };
    ]

let all =
  [Media_type.json] @ json_with_charset "utf-8" @ json_with_charset "UTF-8"
  (* We keep [bson] and [octet_stream] after [json] and its variants as the
     formers are less likely to be used. *)
  @ Media_type.[bson; octet_stream]
