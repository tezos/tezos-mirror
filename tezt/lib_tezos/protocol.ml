(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Declaration order must respect the version order. *)
type t = Edo | Alpha

let name = function Alpha -> "Alpha" | Edo -> "Edo"

(* Test tags must be lowercase. *)
let tag protocol = String.lowercase_ascii (name protocol)

let hash = function
  | Alpha ->
      "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
  | Edo ->
      "PtEdo2ZkT9oKpimTah6x2embF25oss54njMuPzkJTEi5RqfdZFA"

let parameter_file = function
  | Alpha ->
      "src/proto_alpha/parameters/sandbox-parameters.json"
  | Edo ->
      "src/proto_008_PtEdo2Zk/parameters/sandbox-parameters.json"

let accuser = function
  | Alpha ->
      "./tezos-accuser-alpha"
  | Edo ->
      "./tezos-accuser-008-PtEdo2Zk"

let daemon_name = function Alpha -> "alpha" | Edo -> "008-PtEdo2Zk"

(* The encoding prefix is the part which is added at the beginning of all encoding names.
   It turns out this is equal to what the [daemon_name] function returns. *)
let encoding_prefix = daemon_name

(** Protocol parameters overrides are pairs of JSON paths and optional values
    that can be used to override or remove (when the value is [None]) the
    default parameters when activating protocol. *)
type parameter_overrides = (string list * string option) list

(** Write a file with protocol parameters, overriding the defaults with
    [parameter_overrides] *)
let write_parameter_file : protocol:t -> parameter_overrides -> string Lwt.t =
 fun ~protocol parameter_overrides ->
  (* make a copy of the parameters file and update the given constants *)
  let overriden_parameters = Temp.file "parameters.json" in
  let original_parameters =
    JSON.parse_file @@ parameter_file protocol |> JSON.unannotate
  in
  let parameters =
    List.fold_left
      (fun acc (path, value) ->
        let parsed_value = Option.map Ezjsonm.value_from_string value in
        Ezjsonm.update acc path parsed_value)
      original_parameters
      parameter_overrides
  in
  let* overriden_parameters_out =
    Lwt_io.open_file ~mode:Output overriden_parameters
  in
  let* () =
    Lwt_io.write overriden_parameters_out @@ JSON.encode_u parameters
  in
  Lwt.return overriden_parameters

let next_protocol = function Edo -> Some Alpha | Alpha -> None

let previous_protocol = function Alpha -> Some Edo | Edo -> None

let all = [Alpha; Edo]

let current_mainnet = Edo
