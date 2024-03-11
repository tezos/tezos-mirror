(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let genesis : Genesis.t =
  {
    time = Time.Protocol.of_notation_exn "2018-04-17T11:46:23Z";
    block =
      Block_hash.of_b58check_exn
        "BLockGenesisGenesisGenesisGenesisGenesisa52f8bUWPcg";
    protocol =
      Protocol_hash.of_b58check_exn
        "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im";
  }

let with_node f =
  let open Lwt_result_syntax in
  let run dir =
    let ( / ) = Filename.concat in
    let node_config : Node.config =
      {
        genesis;
        chain_name = Distributed_db_version.Name.of_string "TEZOS_DOCGEN";
        sandboxed_chain_name =
          Distributed_db_version.Name.of_string "SANDBOXED_TEZOS_DOCGEN";
        user_activated_upgrades = [];
        user_activated_protocol_overrides = [];
        operation_metadata_size_limit = Unlimited;
        internal_events = Tezos_base.Internal_event_config.stdout;
        patch_context = None;
        data_dir = dir;
        store_root = dir / "store";
        context_root = dir / "context";
        protocol_root = dir / "protocol";
        p2p = None;
        target = None;
        disable_mempool = true;
        enable_testchain = false;
        dal_config = Tezos_crypto_dal.Cryptobox.Config.default;
      }
    in
    let version =
      Tezos_version.Version.to_string
        Tezos_version_value.Current_git_info.version
    in
    let commit_info =
      ({
         commit_hash = Tezos_version_value.Current_git_info.commit_hash;
         commit_date = Tezos_version_value.Current_git_info.committer_date;
       }
        : Tezos_version.Octez_node_version.commit_info)
    in
    let* node =
      Node.create
        ~singleprocess:true
        ~version
        ~commit_info
        node_config
        Tezos_shell_services.Shell_limits.default_peer_validator_limits
        Tezos_shell_services.Shell_limits.default_block_validator_limits
        Tezos_shell_services.Shell_limits.default_prevalidator_limits
        Tezos_shell_services.Shell_limits.default_chain_validator_limits
        None
    in
    f node
  in
  let*! r = Lwt_utils_unix.with_tempdir "tezos_rpcdoc_" run in
  match r with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Format.eprintf "%a@." pp_print_trace err ;
      Stdlib.exit 1
