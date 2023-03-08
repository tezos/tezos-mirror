(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2023 Marigold, <contact@marigold.dev>                       *)
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
  | Reveal_data_path_not_a_directory of string
  | Cannot_create_reveal_data_dir of string

let () =
  register_error_kind
    `Permanent
    ~id:"dac.node.dac.reveal_data_path_not_a_dir"
    ~title:"Reveal data path is not a directory"
    ~description:"Reveal data path is not a directory"
    ~pp:(fun ppf reveal_data_path ->
      Format.fprintf
        ppf
        "Reveal data path %s is not a directory"
        reveal_data_path)
    Data_encoding.(obj1 (req "path" string))
    (function Reveal_data_path_not_a_directory path -> Some path | _ -> None)
    (fun path -> Reveal_data_path_not_a_directory path) ;
  register_error_kind
    `Permanent
    ~id:"dac.node.dac.cannot_create_directory"
    ~title:"Cannot create directory to store reveal data"
    ~description:"Cannot create directory to store reveal data"
    ~pp:(fun ppf reveal_data_path ->
      Format.fprintf
        ppf
        "Cannot create a directory \"%s\" to store reveal data"
        reveal_data_path)
    Data_encoding.(obj1 (req "path" string))
    (function Cannot_create_reveal_data_dir path -> Some path | _ -> None)
    (fun path -> Cannot_create_reveal_data_dir path)

module Storage = struct
  let ensure_reveal_data_dir_exists reveal_data_dir =
    let open Lwt_result_syntax in
    Lwt.catch
      (fun () ->
        let*! () = Lwt_utils_unix.create_dir ~perm:0o744 reveal_data_dir in
        return ())
      (function
        | Failure s ->
            if String.equal s "Not a directory" then
              tzfail @@ Reveal_data_path_not_a_directory reveal_data_dir
            else tzfail @@ Cannot_create_reveal_data_dir reveal_data_dir
        | _ -> tzfail @@ Cannot_create_reveal_data_dir reveal_data_dir)
end

let resolve_plugin
    (protocols : Tezos_shell_services.Chain_services.Blocks.protocols) =
  let open Lwt_syntax in
  let current_protocol = protocols.current_protocol in
  let next_protocol = protocols.next_protocol in
  let plugin_opt =
    Option.either
      (Dac_plugin.get current_protocol)
      (Dac_plugin.get next_protocol)
  in
  match plugin_opt with
  | None ->
      let+ () =
        Event.emit_protocol_plugin_not_resolved current_protocol next_protocol
      in
      None
  | Some dac_plugin ->
      let (module Dac_plugin : Dac_plugin.T) = dac_plugin in
      let+ () = Event.emit_protocol_plugin_resolved Dac_plugin.Proto.hash in
      Some dac_plugin
