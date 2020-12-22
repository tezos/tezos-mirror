(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type sources_config = {min_agreement : float; uris : Uri.t list}

let default_agreement = 1.0

let sources_encoding =
  let open Data_encoding in
  obj2 (opt "min_agreement" float) (req "uris" (list string))

let sources_config_encoding =
  let open Data_encoding in
  conv
    (fun srcs_config ->
      (Some srcs_config.min_agreement, List.map Uri.to_string srcs_config.uris))
    (fun (min_agreement, uris) ->
      {
        min_agreement = Option.value min_agreement ~default:default_agreement;
        uris = List.map Uri.of_string uris;
      })
    sources_encoding

let min_agreement_error sources_cfg =
  let f = sources_cfg.min_agreement in
  if 0.0 < f && f <= 1.0 then None
  else
    Some "min_agreement value must be within 0 (exclusive) and 1 (inclusive)"

let endpoints_error sources_cfg =
  match List.length sources_cfg.uris with
  | 0 ->
      Some
        "Endpoints list in file specified with --sources should not be empty. \
         It should be of size 2 or more."
  | 1 ->
      Some
        "Endpoints list in file specified with --sources should not be of \
         size 1. At least two endpoints are required to validate the data of \
         one endpoint against the data of another endpoint."
  | _ ->
      None

let destruct_sources_config json =
  let sources_cfg = Data_encoding.Json.destruct sources_config_encoding json in
  match (min_agreement_error sources_cfg, endpoints_error sources_cfg) with
  | (Some err, _) | (_, Some err) ->
      Error err
  | _ ->
      Ok sources_cfg

type sources = {
  min_agreement : float;
  endpoints : (Uri.t * RPC_context.simple) list;
}

let sources_config_to_sources sources_config rpc_context_builder =
  let endpoints =
    List.map (fun u -> (u, rpc_context_builder u)) sources_config.uris
  in
  {min_agreement = sources_config.min_agreement; endpoints}

let hash_of_block (block : Tezos_shell_services.Block_services.block) :
    Block_hash.t option =
  match block with
  | `Hash (h, 0) ->
      Some h
  | `Alias (_, _) | `Genesis | `Head _ | `Level _ | `Hash (_, _) ->
      None
