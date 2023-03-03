(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

module Keys = struct
  type t = {
    public_key_hash : Tezos_crypto.Aggregate_signature.public_key_hash;
    public_key_opt : Tezos_crypto.Aggregate_signature.public_key option;
    aggregate_sk_uris : Client_keys.aggregate_sk_uri;
  }

  let get_public_key_hash t = t.public_key_hash

  let get_public_key_opt t = t.public_key_opt

  let get_aggregate_sk_uris t = t.aggregate_sk_uris

  let get_address_keys cctxt address =
    let open Lwt_result_syntax in
    let open Tezos_client_base.Client_keys in
    let* alias = Aggregate_alias.Public_key_hash.rev_find cctxt address in
    match alias with
    | None -> return_none
    | Some alias -> (
        let* keys_opt = alias_aggregate_keys cctxt alias in
        match keys_opt with
        | None ->
            (* DAC/TODO: https://gitlab.com/tezos/tezos/-/issues/4193
               Revisit this once the Dac committee will be spread across
               multiple dac nodes.*)
            let*! () = Event.(emit dac_account_not_available address) in
            return_none
        | Some (pkh, pk, sk_uri_opt) -> (
            match sk_uri_opt with
            | None ->
                let*! () = Event.(emit dac_account_cannot_sign address) in
                return_none
            | Some sk_uri ->
                return_some
                  {
                    public_key_hash = pkh;
                    public_key_opt = pk;
                    aggregate_sk_uris = sk_uri;
                  }))

  let get_public_key cctxt address =
    let open Lwt_result_syntax in
    let+ address_keys_opt = get_address_keys cctxt address in
    Option.bind address_keys_opt (fun t -> get_public_key_opt t)

  let get_keys ~addresses ~threshold cctxt =
    let open Lwt_result_syntax in
    let* keys = List.map_es (get_address_keys cctxt) addresses in
    let recovered_keys = List.length @@ List.filter Option.is_some keys in
    let*! () =
      (* We emit a warning if the threshold of dac accounts needed to sign a
         root page hash is not reached. We also emit a warning for each DAC
         account whose secret key URI was not recovered.
         We do not stop the dac node at this stage.
      *)
      if recovered_keys < threshold then
        Event.(emit dac_threshold_not_reached (recovered_keys, threshold))
      else Event.(emit dac_is_ready) ()
    in
    return keys
end

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
