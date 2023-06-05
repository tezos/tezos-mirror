(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
(* Copyright (c) 2023 Marigold  <contact@marigold.dev>                       *)
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
  | DAC_node_not_ready of string
  | Cannot_construct_external_message
  | Cannot_deserialize_external_message

let () =
  register_error_kind
    `Permanent
    ~id:"dac_node_not_ready"
    ~title:"DAC Node is not ready"
    ~description:
      "RPC server of DAC node is not started and plugin is not resolved."
    ~pp:(fun ppf message ->
      Format.fprintf ppf "DAC Node is not ready, current status is: %s" message)
    Data_encoding.(obj1 (req "value" string))
    (function DAC_node_not_ready message -> Some message | _ -> None)
    (fun message -> DAC_node_not_ready message) ;
  register_error_kind
    `Permanent
    ~id:"dac_cannot_construct_external_message"
    ~title:"External rollup message could not be constructed"
    ~description:"External rollup message could not be constructed"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "External rollup message could not be constructed")
    Data_encoding.unit
    (function Cannot_construct_external_message -> Some () | _ -> None)
    (fun () -> Cannot_construct_external_message) ;
  register_error_kind
    `Permanent
    ~id:"dac_cannot_deserialize_rollup_external_message"
    ~title:"External rollup message could not be deserialized"
    ~description:"External rollup message could not be deserialized"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "External rollup message could not be deserialized")
    Data_encoding.unit
    (function Cannot_deserialize_external_message -> Some () | _ -> None)
    (fun () -> Cannot_deserialize_external_message)

let handle_get_health_live node_ctxt =
  match Node_context.get_status node_ctxt with
  | Ready _ | Starting -> Lwt_result_syntax.return true

let handle_get_health_ready node_ctxt =
  match Node_context.get_status node_ctxt with
  | Ready _ -> Lwt_result_syntax.return true
  | Starting -> Lwt_result_syntax.tzfail @@ DAC_node_not_ready "starting"

module Shared_by_V0_and_V1 = struct
  (** [handle_get_page] is a handler shared by both "GET v0/preimage" 
      and "GET v1/pages". It fetches a page that corresponds
      to a given [raw_hash]. *)
  let handle_get_page dac_plugin page_store raw_hash =
    let open Lwt_result_syntax in
    let*? hash = Dac_plugin.raw_to_hash dac_plugin raw_hash in
    Page_store.Filesystem.load dac_plugin page_store hash
end

module V0 = struct
  let handle_post_store_preimage dac_plugin cctxt dac_sk_uris page_store
      hash_streamer (data, pagination_scheme) =
    let open Lwt_result_syntax in
    let open Pages_encoding in
    let* root_hash =
      match pagination_scheme with
      | Pagination_scheme.Merkle_tree_V0 ->
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4897
             Once new "PUT /preimage" endpoint is implemented, pushing
             a new root hash to the data streamer should be moved there.
             Tezt for testing streaming of root hashes should also use
             the new endpoint. *)
          let* root_hash =
            Merkle_tree.V0.Filesystem.serialize_payload
              dac_plugin
              ~page_store
              data
          in
          let () =
            Data_streamer.publish
              hash_streamer
              (Dac_plugin.hash_to_raw root_hash)
          in
          let*! () =
            Event.emit_root_hash_pushed_to_data_streamer dac_plugin root_hash
          in
          return root_hash
      | Pagination_scheme.Hash_chain_V0 ->
          Hash_chain.V0.serialize_payload
            dac_plugin
            ~for_each_page:(fun (hash, content) ->
              Page_store.Filesystem.save dac_plugin page_store ~hash ~content)
            data
    in
    let* signature, witnesses =
      Signature_manager.Legacy.sign_root_hash
        dac_plugin
        cctxt
        dac_sk_uris
        root_hash
    in
    let raw_root_hash = Dac_plugin.hash_to_raw root_hash in
    let*! external_message =
      External_message.Default.make dac_plugin root_hash signature witnesses
    in
    match external_message with
    | Ok external_message -> return @@ (raw_root_hash, external_message)
    | Error _ -> tzfail @@ Cannot_construct_external_message

  let handle_get_verify_signature dac_plugin public_keys_opt encoded_l1_message
      =
    let open Lwt_result_syntax in
    let ((module Plugin) : Dac_plugin.t) = dac_plugin in
    let external_message =
      let open Option_syntax in
      let* encoded_l1_message in
      let* as_bytes = Hex.to_bytes @@ `Hex encoded_l1_message in
      External_message.Default.of_bytes Plugin.encoding as_bytes
    in
    match external_message with
    | None -> tzfail @@ Cannot_deserialize_external_message
    | Some {root_hash; signature; witnesses} ->
        Signature_manager.verify
          dac_plugin
          ~public_keys_opt
          (Dac_plugin.hash_to_raw root_hash)
          signature
          witnesses

  let handle_monitor_root_hashes hash_streamer =
    let open Lwt_syntax in
    let stream, stopper = Data_streamer.handle_subscribe hash_streamer in
    let shutdown () = Lwt_watcher.shutdown stopper in
    let next () = Lwt_stream.get stream in
    let* () = Event.(emit handle_new_subscription_to_hash_streamer ()) in
    Tezos_rpc.Answer.return_stream {next; shutdown}

  let handle_get_certificate dac_plugin node_store raw_root_hash =
    let open Lwt_result_syntax in
    let*? root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
    let+ value_opt = Store.Certificate_store.find node_store root_hash in
    Option.map
      (fun Store.{aggregate_signature; witnesses} ->
        Certificate_repr.(
          V0 (V0.make raw_root_hash aggregate_signature witnesses)))
      value_opt

  let handle_get_serialized_certificate dac_plugin node_store raw_root_hash =
    let open Lwt_result_syntax in
    let ((module Plugin) : Dac_plugin.t) = dac_plugin in
    let*? root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
    let* value_opt = Store.Certificate_store.find node_store root_hash in
    Option.map_es
      (fun Store.{aggregate_signature; witnesses} ->
        let serialized_certificate =
          Certificate_repr.V0.Protocol_dependant.serialize_certificate
            Plugin.encoding
            ~root_hash
            ~aggregate_signature
            ~witnesses
        in
        return @@ String.of_bytes serialized_certificate)
      value_opt

  module Coordinator = struct
    let handle_post_preimage dac_plugin page_store hash_streamer payload =
      let open Lwt_result_syntax in
      let* root_hash =
        Pages_encoding.Merkle_tree.V0.Filesystem.serialize_payload
          dac_plugin
          ~page_store
          payload
      in
      let () =
        Data_streamer.publish hash_streamer (Dac_plugin.hash_to_raw root_hash)
      in
      let*! () =
        Event.emit_root_hash_pushed_to_data_streamer dac_plugin root_hash
      in
      return @@ Dac_plugin.hash_to_raw root_hash

    let handle_monitor_certificate dac_plugin ro_node_store
        certificate_streamers raw_root_hash committee_members =
      let open Lwt_result_syntax in
      let*? stream, stopper =
        Certificate_streamers.handle_subscribe
          dac_plugin
          certificate_streamers
          raw_root_hash
      in
      let*? root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
      let*! () = Event.emit_new_subscription_to_certificate_updates root_hash in
      let shutdown () = Lwt_watcher.shutdown stopper in
      let next () = Lwt_stream.get stream in
      (* Add the current certificate to the streamer, if any, to ensure that
         a certificate is returned even in the case that no updates to the
         certificate happen for a long time. *)
      let*! current_certificate_store_value_res =
        Store.Certificate_store.find ro_node_store root_hash
      in
      match current_certificate_store_value_res with
      | Ok current_certificate_store_value ->
          let () =
            Option.iter
              (fun Store.{aggregate_signature; witnesses} ->
                let certificate =
                  Certificate_repr.(
                    V0 (V0.make raw_root_hash aggregate_signature witnesses))
                in
                let _ =
                  Certificate_streamers.push
                    dac_plugin
                    certificate_streamers
                    raw_root_hash
                    certificate
                in
                if
                  Certificate_repr.all_committee_members_have_signed
                    committee_members
                    certificate
                then
                  let _ =
                    Certificate_streamers.close
                      dac_plugin
                      certificate_streamers
                      raw_root_hash
                  in
                  ()
                else ())
              current_certificate_store_value
          in
          return (next, shutdown)
      | Error e -> fail e
  end

  module Observer = struct
    let handle_get_missing_page timeout cctxts page_store dac_plugin
        raw_root_hash =
      let open Lwt_result_syntax in
      let*? root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
      let remote_store =
        Page_store.Remote_with_flooding.(init {timeout; cctxts; page_store})
      in
      let* preimage =
        Page_store.Remote_with_flooding.load dac_plugin remote_store root_hash
      in
      let*! () = Event.emit_fetched_missing_page root_hash in
      return preimage
  end
end
