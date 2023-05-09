(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
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

module Reveal_hash = Protocol.Sc_rollup_reveal_hash
module Dac_client = Dac_node_client

type error +=
  | Failed_to_initialize_dac_plugin
  | Failed_to_fetch_missing_page_from_observer of Reveal_hash.t
  | Failed_to_verify_raw_data_with_hash of (Reveal_hash.t * Protocol_hash.t)
  | Timeout of Z.t
  | Wrong_hash of {found : Reveal_hash.t; expected : Reveal_hash.t}

let () =
  register_error_kind
    ~id:"dac_observer_client.failed_to_initialize_dac_plugin"
    ~title:"Failed to initialize DAC plugin"
    ~description:"Failed to initialize DAC plugin in DAC Observer client."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to initialize DAC plugin in DAC Observer client.")
    `Permanent
    Data_encoding.unit
    (function Failed_to_initialize_dac_plugin -> Some () | _ -> None)
    (fun _ -> Failed_to_initialize_dac_plugin) ;
  register_error_kind
    ~id:"dac_observer_client_timeout"
    ~title:"Timeout while querying Dac Observer Node"
    ~description:"Timeout while querying Dac Observer Node."
    ~pp:(fun ppf seconds ->
      Format.fprintf
        ppf
        "Timeout after %as when querying Dac Observer Node"
        Z.pp_print
        seconds)
    `Permanent
    Data_encoding.(obj1 (req "seconds" z))
    (function Timeout seconds -> Some seconds | _ -> None)
    (fun seconds -> Timeout seconds) ;
  register_error_kind
    ~id:"dac_observer_client.failed_to_fetch_missing_page_from_observer"
    ~title:"Failed to fetch missing page from DAC Observer."
    ~description:"Failed to fetch missing page from DAC Observer."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Failed to fetch missing page from DAC Observer for hash: %s"
        (Reveal_hash.to_hex hash))
    `Permanent
    Data_encoding.(obj1 (req "hash" Reveal_hash.encoding))
    (function
      | Failed_to_fetch_missing_page_from_observer hash -> Some hash | _ -> None)
    (fun hash -> Failed_to_fetch_missing_page_from_observer hash) ;
  register_error_kind
    ~id:"dac_observer_client.failed_to_verify_raw_data"
    ~title:"Failed to verify raw data with hash."
    ~description:"Failed to verify raw data with hash."
    ~pp:(fun ppf (hash, protocol_hash) ->
      Format.fprintf
        ppf
        "Failed to verify raw data with preimage hash using expected current \
         protocol encoding. Preimage hash: %s. Current protocol: %s."
        (Reveal_hash.to_hex hash)
        (Protocol_hash.to_b58check protocol_hash))
    `Permanent
    Data_encoding.(
      obj2
        (req "preimage_hash" Reveal_hash.encoding)
        (req "protocol_hash" Protocol_hash.encoding))
    (function
      | Failed_to_verify_raw_data_with_hash (hash, protocol_hash) ->
          Some (hash, protocol_hash)
      | _ -> None)
    (fun (hash, protocol_hash) ->
      Failed_to_verify_raw_data_with_hash (hash, protocol_hash)) ;
  register_error_kind
    ~id:"dac_observer_client.wrong_hash_of_reveal_preimage"
    ~title:"Hash of reveal preimage is incorrect."
    ~description:"Hash of reveal preimage is  incorrect."
    ~pp:(fun ppf (found, expected) ->
      Format.fprintf
        ppf
        "Reveal preimage hash is '%a' while a value of '%a' is expected"
        Reveal_hash.pp
        found
        Reveal_hash.pp
        expected)
    `Permanent
    Data_encoding.(
      obj2
        (req "found" Reveal_hash.encoding)
        (req "expected" Reveal_hash.encoding))
    (function
      | Wrong_hash {found; expected} -> Some (found, expected) | _ -> None)
    (fun (found, expected) -> Wrong_hash {found; expected})

module Configuration = struct
  type t = {
    observer_endpoint : Uri.t;
    reveal_data_dir : string;
    timeout_seconds : Z.t option;
  }
end

type t = {
  reveal_data_dir : string;
  timeout_seconds : Z.t;
  observer_cctxt : Dac_client.unix_cctxt;
  dac_plugin : Dac_plugin.t;
  protocol : Protocol_hash.t;
}

module Client = struct
  let make_unix endpoint =
    let rpc_config =
      {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
    in
    new Dac_node_client.unix_cctxt ~rpc_config

  let proto_hash_to_dac_hash ((module P) : Dac_plugin.t) proto_reveal_hash =
    proto_reveal_hash
    |> Data_encoding.Binary.to_bytes_exn Protocol.Sc_rollup_reveal_hash.encoding
    |> Data_encoding.Binary.of_bytes_exn P.encoding

  let fetch_missing_page dac_plugin observer_cctxt hash =
    let open Lwt_result_syntax in
    let dac_hash =
      Dac_plugin.hash_to_raw @@ proto_hash_to_dac_hash dac_plugin hash
    in
    let+ preimage =
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5627
         Currently we have only one major DAC API version ([V0]). For this reason,
         clients can always default to [Dac_node_client.V0]. This should be
         revisited once we add another major version. *)
      Dac_node_client.V0.Observer.get_missing_page observer_cctxt dac_hash
    in
    String.of_bytes preimage
end

let timeout_default = Z.of_int 30

let init Configuration.{observer_endpoint; reveal_data_dir; timeout_seconds} =
  let open Lwt_result_syntax in
  let* dac_plugin =
    match Dac_plugin.get Protocol.hash with
    | Some dac_plugin -> return dac_plugin
    | None ->
        let () = Dac_plugin.register Dac_plugin_registration.make_plugin in
        Option.fold_f
          ~none:(fun () -> tzfail @@ Failed_to_initialize_dac_plugin)
          ~some:(fun p -> return p)
          (Dac_plugin.get Protocol.hash)
  in
  let observer_cctxt = Client.make_unix observer_endpoint in
  let timeout_seconds = Option.value ~default:timeout_default timeout_seconds in
  return
    {
      reveal_data_dir;
      timeout_seconds;
      observer_cctxt;
      dac_plugin;
      protocol = Protocol.hash;
    }

(** TODO: https://gitlab.com/tezos/tezos/-/issues/5519
    Remove duplicate code from reveals.ml 
  *)
let file_contents filename =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! contents = Lwt_utils_unix.read_file filename in
      return_some contents)
    (fun _ -> return_none)

let path data_dir hash =
  let hash = Reveal_hash.to_hex hash in
  Filename.(concat data_dir hash)

(** TODO: https://gitlab.com/tezos/tezos/-/issues/5521
    Return non fatal error kinds if any.
*)
let fetch_preimage dac_client hash =
  let open Lwt_result_syntax in
  let {reveal_data_dir; timeout_seconds; observer_cctxt; dac_plugin; protocol} =
    dac_client
  in
  let filename = path reveal_data_dir hash in
  let* contents_opt = file_contents filename in
  let* contents =
    match contents_opt with
    | Some preimage -> return preimage
    | None ->
        let run () =
          Lwt_unix.with_timeout (Z.to_float timeout_seconds) (fun () ->
              Client.fetch_missing_page dac_plugin observer_cctxt hash)
        in
        Lwt.catch run (function
            | Lwt_unix.Timeout -> tzfail @@ Timeout timeout_seconds
            | _ -> tzfail @@ Failed_to_fetch_missing_page_from_observer hash)
  in
  let*? () =
    let contents_hash =
      Reveal_hash.hash_string ~scheme:Reveal_hash.Blake2B [contents]
    in
    error_unless
      (Reveal_hash.equal contents_hash hash)
      (Wrong_hash {found = contents_hash; expected = hash})
  in
  let* _encoded =
    (* Check that the reveal input can be encoded within the bounds enforced by
       the protocol. *)
    trace (Failed_to_verify_raw_data_with_hash (hash, protocol))
    @@ protect
    @@ fun () ->
    Data_encoding.Binary.to_bytes_exn
      Protocol.Alpha_context.Sc_rollup.input_encoding
      (Reveal (Raw_data contents))
    |> return
  in
  return contents
