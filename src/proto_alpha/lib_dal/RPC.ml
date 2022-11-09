open Environment

type Error_monad.error += Cannot_serialize_page_payload

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"cannot_serialize_dac_page"
    ~title:"DAC page could not be serialized"
    ~description:"Error when serializing DAC page"
    ~pp:(fun ppf () -> Format.fprintf ppf "Error when serializing DAC page")
    Data_encoding.unit
    (function Cannot_serialize_page_payload -> Some () | _ -> None)
    (fun () -> Cannot_serialize_page_payload)

let map_proto_error (res : 'a tzresult Lwt.t) (error : Error_monad.error) :
    ('a, Error_monad.error Error_monad.trace) result Lwt.t =
  let open Lwt_result_syntax in
  let*! res = res in
  match res with
  | Ok res -> Error_monad.return res
  | Error _ -> Error_monad.fail error

module Registration = struct
  let register0_noctxt ~chunked s f dir =
    RPC_directory.register ~chunked dir s (fun _rpc_ctxt q i -> f q i)
end

module DAC = struct
  module Hashing_scheme = Dac_pages_encoding.Merkle_tree.V0
  module Hash_storage = Dac_preimage_data_manager.Reveal_hash

  module S = struct
    let dac_store_preimage =
      RPC_service.put_service
        ~description:"Split DAC reveal data"
        ~query:RPC_query.empty
        ~input:Data_encoding.bytes
        ~output:Hashing_scheme.hash_encoding
        RPC_path.(open_root / "dac" / "store_preimage")
  end

  let handle_serialize_dac_store_preimage data =
    let for_each_page _ = return () in
    let size = Protocol.Alpha_context.Constants.sc_rollup_message_size_limit in
    Hashing_scheme.serialize_payload ~max_page_size:size data ~for_each_page

  let register_serialize_dac_store_preimage =
    Registration.register0_noctxt
      ~chunked:false
      S.dac_store_preimage
      (fun () input ->
        map_proto_error
          (handle_serialize_dac_store_preimage input)
          Cannot_serialize_page_payload)

  let register () =
    (RPC_directory.empty : unit RPC_directory.t)
    |> register_serialize_dac_store_preimage
end

let rpc_services = DAC.register ()
