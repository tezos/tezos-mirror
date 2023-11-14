(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type error += Injector_server_unsupported_configuration of string

let () =
  register_error_kind
    ~id:"injector-server.unsupported_configuration"
    ~title:"Configuration not supported"
    ~description:"Configuration not supported"
    ~pp:(fun ppf reason ->
      Format.fprintf ppf "Configuration not supported: %s" reason)
    `Permanent
    Data_encoding.(obj1 (req "reason" string))
    (function
      | Injector_server_unsupported_configuration reason -> Some reason
      | _ -> None)
    (fun reason -> Injector_server_unsupported_configuration reason)

type error += Injector_server_cannot_deserialize_contract_hash of string

let () =
  register_error_kind
    ~id:"injector-server.cannot_deserialize_contract_hash"
    ~title:"Contract hash could not be deserialized"
    ~description:"Contract hash could not be deserialized"
    ~pp:(fun ppf hash ->
      Format.fprintf ppf "Contract hash %s could not be deserialized" hash)
    `Permanent
    Data_encoding.(obj1 (req "hash" string))
    (function
      | Injector_server_cannot_deserialize_contract_hash hash -> Some hash
      | _ -> None)
    (fun hash -> Injector_server_cannot_deserialize_contract_hash hash)

let make_signers_for_transactions signer block_delay =
  (* Multiple or no signers currently not supported. The encoding also
     sets a maximum length of 1 for the [signers] list. *)
  let open Result_syntax in
  let source = Signature.Public_key_hash.of_b58check_exn signer in
  return
    [
      ( [source],
        `Delay_block block_delay,
        [Injector_server.Configuration.Transaction] );
    ]

let register_dir () =
  let open Lwt_result_syntax in
  let open Injector_services in
  let dir =
    Tezos_rpc.Directory.register0
      Tezos_rpc.Directory.empty
      add_pending_transaction
      (fun () op ->
        let*! inj_operation_hash = Injector_server.add_pending_operation op in
        let*! () = Injector_server.inject () in
        Lwt.return inj_operation_hash)
  in
  let dir =
    Tezos_rpc.Directory.register0 dir operation_status (fun {op_hash} () ->
        let op_hash =
          Injector_server.Inj_operation.Hash.of_b58check_exn op_hash
        in
        let status = Injector_server.operation_status op_hash in
        return
        @@ Option.map
             (fun (status : Injector_server.status) ->
               match status with
               | Pending _ -> Pending
               | Injected info ->
                   Injected
                     {
                       injected_oph = info.oph;
                       injected_op_index = info.op_index;
                     }
               | Included info ->
                   Included
                     {
                       included_oph = info.oph;
                       included_op_index = info.op_index;
                       block = info.l1_block;
                       level = info.l1_level;
                     })
             status)
  in

  let dir =
    Tezos_rpc.Directory.register0 dir inject (fun () () ->
        let*! () = Injector_server.inject () in
        return_unit)
  in
  dir

let start ~rpc_address ~rpc_port () =
  let open Lwt_result_syntax in
  let rpc_address = P2p_addr.of_string_exn rpc_address in
  let mode = `TCP (`Port rpc_port) in
  let acl = RPC_server.Acl.allow_all in
  let dir = register_dir () in
  let server =
    RPC_server.init_server dir ~acl ~media_types:Media_type.all_media_types
  in
  Lwt.catch
    (fun () ->
      let*! () = Event.(emit listening) rpc_address in
      let*! () =
        RPC_server.launch
          ~host:(Ipaddr.V6.to_string rpc_address)
          server
          ~callback:(RPC_server.resto_callback server)
          mode
      in
      Lwt_utils.never_ending ())
    (function
      | Unix.Unix_error (Unix.EADDRINUSE, "bind", "") ->
          failwith "Port already in use."
      | exn -> fail_with_exn exn)

let run ~data_dir (cctxt : Client_context.full) =
  let open Lwt_result_syntax in
  let state : Injector_server.state =
    {
      cctxt;
      fee_parameters = Injector_server.Configuration.default_fee_parameters;
      minimal_block_delay = 15L;
      delay_increment_per_round = 8L;
    }
  in
  let* (Configuration.{rpc_address; rpc_port; data_dir = _; block_delay; signer}
       as config) =
    Configuration.load ~data_dir
  in
  let*? signers = make_signers_for_transactions signer block_delay in
  let* () = Injector_server.init cctxt ~data_dir state ~signers in
  let*! () = Event.(emit accepting_requests) ("HTTP", config.rpc_port) in
  start ~rpc_address ~rpc_port ()
