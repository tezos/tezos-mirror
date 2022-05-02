(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type mode = Observer | Accuser | Batcher | Maintenance | Operator | Custom

type signers = {
  operator : Signature.public_key_hash option;
  submit_batch : Signature.public_key_hash option;
  finalize_commitment : Signature.public_key_hash option;
  remove_commitment : Signature.public_key_hash option;
  rejection : Signature.public_key_hash option;
  dispatch_withdrawals : Signature.public_key_hash option;
}

type t = {
  data_dir : string;
  rollup_id : Protocol.Alpha_context.Tx_rollup.t;
  rollup_genesis : Block_hash.t option;
  rpc_addr : P2p_point.Id.t;
  reconnection_delay : float;
  mode : mode;
  signers : signers;
  allow_deposit : bool;
  l2_blocks_cache_size : int;
}

let default_data_dir rollup_id =
  let home = Sys.getenv "HOME" in
  let dir =
    ".tezos-tx-rollup-node-"
    ^ Protocol.Alpha_context.Tx_rollup.to_b58check rollup_id
  in
  Filename.concat home dir

let default_rpc_addr = (Ipaddr.V6.localhost, 9999)

let default_reconnection_delay = 2.0

let default_l2_blocks_cache_size = 64

let modes = [Observer; Accuser; Batcher; Maintenance; Operator; Custom]

let string_of_mode = function
  | Observer -> "observer"
  | Accuser -> "accuser"
  | Batcher -> "batcher"
  | Maintenance -> "maintenance"
  | Operator -> "operator"
  | Custom -> "custom"

let mode_of_string = function
  | "observer" -> Ok Observer
  | "accuser" -> Ok Accuser
  | "batcher" -> Ok Batcher
  | "maintenance" -> Ok Maintenance
  | "operator" -> Ok Operator
  | "custom" -> Ok Custom
  | _ -> Error [Exn (Failure "Invalid mode")]

let mode_encoding =
  Data_encoding.string_enum
    [
      ("observer", Observer);
      ("accuser", Accuser);
      ("batcher", Batcher);
      ("maintenance", Maintenance);
      ("operator", Operator);
      ("custom", Custom);
    ]

let signers_encoding =
  let open Data_encoding in
  conv
    (fun {
           operator;
           submit_batch;
           finalize_commitment;
           remove_commitment;
           rejection;
           dispatch_withdrawals;
         } ->
      ( operator,
        submit_batch,
        finalize_commitment,
        remove_commitment,
        rejection,
        dispatch_withdrawals ))
    (fun ( operator,
           submit_batch,
           finalize_commitment,
           remove_commitment,
           rejection,
           dispatch_withdrawals ) ->
      {
        operator;
        submit_batch;
        finalize_commitment;
        remove_commitment;
        rejection;
        dispatch_withdrawals;
      })
  @@ obj6
       (opt
          ~description:"The operator of the rollup (public key hash) if any"
          "operator"
          Signature.Public_key_hash.encoding)
       (opt
          "submit_batch"
          Signature.Public_key_hash.encoding
          ~description:"The public key hash of the signer for batch submission")
       (opt
          "finalize_commitment"
          Signature.Public_key_hash.encoding
          ~description:
            "The public key hash of the signer for finalization of commitments")
       (opt
          "remove_commitment"
          Signature.Public_key_hash.encoding
          ~description:
            "The public key hash of the signer for removals of commitments")
       (opt
          "rejection"
          Signature.Public_key_hash.encoding
          ~description:"The public key hash of the signer for rejections")
       (opt
          "dispatch_withdrawals"
          Signature.Public_key_hash.encoding
          ~description:
            "The public key hash of the signer for the dispatch of withdrawals")

let encoding =
  let open Data_encoding in
  conv
    (fun {
           data_dir;
           rollup_id;
           rollup_genesis;
           rpc_addr;
           reconnection_delay;
           mode;
           signers;
           allow_deposit;
           l2_blocks_cache_size;
         } ->
      ( Some data_dir,
        rollup_id,
        rollup_genesis,
        rpc_addr,
        reconnection_delay,
        mode,
        signers,
        allow_deposit,
        l2_blocks_cache_size ))
    (fun ( data_dir_opt,
           rollup_id,
           rollup_genesis,
           rpc_addr,
           reconnection_delay,
           mode,
           signers,
           allow_deposit,
           l2_blocks_cache_size ) ->
      let data_dir =
        match data_dir_opt with
        | Some dir -> dir
        | None -> default_data_dir rollup_id
      in
      {
        data_dir;
        rollup_id;
        rollup_genesis;
        rpc_addr;
        reconnection_delay;
        mode;
        signers;
        allow_deposit;
        l2_blocks_cache_size;
      })
  @@ obj9
       (opt
          ~description:
            "Location where the rollup node data (store, context, etc.) is \
             stored"
          "data_dir"
          string)
       (req
          ~description:"Rollup id of the rollup to target"
          "rollup_id"
          Protocol.Alpha_context.Tx_rollup.encoding)
       (opt
          ~description:"Hash of the block where the rollup was created"
          "origination_block"
          Block_hash.encoding)
       (dft
          ~description:"RPC address on which the rollup node listens"
          "rpc_addr"
          P2p_point.Id.encoding
          default_rpc_addr)
       (dft
          ~description:"The reconnection (to the tezos node) delay in seconds"
          "reconnection_delay"
          float
          default_reconnection_delay)
       (req ~description:"The mode for this rollup node" "mode" mode_encoding)
       (req
          ~description:"The signers for the various tx rollup operations"
          "signers"
          signers_encoding)
       (dft
          ~description:
            "Allow the operator to make a first deposit for commitments"
          "allow_deposit"
          bool
          false)
       (dft
          ~description:"The size of the L2 block cache in number of blocks"
          "l2_blocks_cache_size"
          int31
          default_l2_blocks_cache_size)

let get_configuration_filename data_dir =
  let filename = "config.json" in
  Filename.concat data_dir filename

let check_mode config =
  let config_signers =
    let add signer name acc =
      Option.fold ~none:acc ~some:(fun _ -> String.Set.add name acc) signer
    in
    String.Set.empty
    |> add config.signers.operator "operator"
    |> add config.signers.submit_batch "submit_batch"
    |> add config.signers.finalize_commitment "finalize_commitment"
    |> add config.signers.remove_commitment "remove_commitment"
    |> add config.signers.rejection "rejection"
    |> add config.signers.dispatch_withdrawals "dispatch_withdrawals"
  in
  let should_have signers =
    let signers = String.Set.of_list signers in
    if String.Set.equal config_signers signers then Ok ()
    else
      let missing_signers =
        String.Set.(diff signers config_signers |> elements)
      in
      let extra_signers =
        String.Set.(diff config_signers signers |> elements)
      in
      let mode = string_of_mode config.mode in
      Error
        [
          Error.Tx_rollup_mismatch_mode_signers
            {mode; missing_signers; extra_signers};
        ]
  in
  match config.mode with
  | Observer -> should_have []
  | Accuser -> should_have ["rejection"]
  | Batcher -> should_have ["submit_batch"]
  | Maintenance ->
      should_have
        [
          "operator";
          "finalize_commitment";
          "remove_commitment";
          "rejection";
          "dispatch_withdrawals";
        ]
  | Operator ->
      should_have
        [
          "operator";
          "submit_batch";
          "finalize_commitment";
          "remove_commitment";
          "rejection";
          "dispatch_withdrawals";
        ]
  | Custom -> Ok ()

let save configuration =
  let open Lwt_result_syntax in
  let*? () = check_mode configuration in
  let json = Data_encoding.Json.construct encoding configuration in
  let*! () = Lwt_utils_unix.create_dir configuration.data_dir in
  let file = get_configuration_filename configuration.data_dir in
  let*! v =
    Lwt_utils_unix.with_atomic_open_out file (fun chan ->
        let content = Data_encoding.Json.to_string json in
        Lwt_utils_unix.write_string chan content)
  in
  let* () =
    Lwt.return
      (Result.map_error
         (fun _ -> [Error.Tx_rollup_unable_to_write_configuration_file file])
         v)
  in
  return file

let load ~data_dir =
  let open Lwt_result_syntax in
  let file = get_configuration_filename data_dir in
  let*! exists = Lwt_unix.file_exists file in
  let* () =
    fail_unless exists (Error.Tx_rollup_configuration_file_does_not_exists file)
  in
  let* json = Lwt_utils_unix.Json.read_file file in
  let config = Data_encoding.Json.destruct encoding json in
  let*? () = check_mode config in
  return config
