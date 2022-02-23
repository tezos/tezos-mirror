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

let sources_config_to_uris ({uris; _} : sources_config) = uris

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

let mk_sources_config ~min_agreement ~uris =
  if min_agreement <= 0.0 || 1.0 < min_agreement then
    Error
      Format.(
        sprintf
          "min_agreement value must be within 0 (exclusive) and 1 (inclusive) \
           but you provided %f"
          min_agreement)
  else if Compare.List_length_with.(uris < 2) then
    Error
      Format.(
        asprintf
          "A minimum of 2 endpoints is required in Light mode: one endpoint to \
           retrieve data and one to validate data. You only provided %a"
          (pp_print_list Uri.pp_hum)
          uris)
  else Ok {min_agreement; uris}

let example_sources =
  {|{"min_agreement": 1.0, "uris": ["http://localhost:8732", "https://localhost:8733"]}|}

let destruct_sources_config json =
  let {min_agreement; uris} =
    Data_encoding.Json.destruct sources_config_encoding json
  in
  mk_sources_config ~min_agreement ~uris

type sources = {
  min_agreement : float;
  endpoints : (Uri.t * RPC_context.simple) list;
}

let sources_config_to_sources rpc_context_builder {min_agreement; uris} =
  let endpoints = List.map (fun u -> (u, rpc_context_builder u)) uris in
  {min_agreement; endpoints}

let hash_of_block (block : Tezos_shell_services.Block_services.block) :
    Block_hash.t option =
  match block with
  | `Hash (h, 0) -> Some h
  | `Alias (_, _) | `Genesis | `Head _ | `Level _ | `Hash (_, _) -> None
