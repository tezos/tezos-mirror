(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

let get_context index hash =
  Context.checkout index hash
  >>= function
  | None ->
      fail (Block_validator_errors.Failed_to_checkout_context hash)
  | Some ctx ->
      return ctx

(** The standard block validation method *)
module Seq_validator = struct
  include Internal_event.Legacy_logging.Make (struct
    let name = "validation_process.sequential"
  end)

  type validation_context = {context_index : Context.index}

  type t = validation_context

  let init context_index =
    lwt_log_notice "Initialized" >>= fun () -> Lwt.return {context_index}

  let close _ = lwt_log_notice "Shutting down..."

  let apply_block validator_process chain_state ~max_operations_ttl
      ~(predecessor_block_header : Block_header.t) ~block_header operations =
    let chain_id = State.Chain.id chain_state in
    get_context
      validator_process.context_index
      predecessor_block_header.shell.context
    >>=? fun predecessor_context ->
    Block_validation.apply
      chain_id
      ~max_operations_ttl
      ~predecessor_block_header
      ~predecessor_context
      ~block_header
      operations
end

(* Block validation using forked processes *)
module Fork_validator = struct
  include Internal_event.Legacy_logging.Make_semantic (struct
    let name = "shell.validation_process.fork"
  end)

  type validation_context = {
    context_index : Context.index;
    store_root : string;
    context_root : string;
    protocol_root : string;
    process_path : string;
  }

  type t = validation_context

  let init context_index store_root context_root protocol_root process_path =
    lwt_log_notice (fun f -> f "Initialized")
    >>= fun () ->
    Lwt.return
      {context_index; store_root; context_root; protocol_root; process_path}

  let close _vp =
    lwt_log_notice (fun f -> f "Shutting down ...") >>= fun () -> Lwt.return ()

  let check_process_status =
    let open Unix in
    let int_tag = Tag.def "int" Format.pp_print_int in
    function
    | WEXITED 0 ->
        lwt_log_notice (fun f -> f "The process terminated normally")
    | WEXITED i ->
        lwt_fatal_error
          Tag.DSL.(
            fun f ->
              f "The process terminated abnormally with value %a"
              -% a int_tag i)
    | WSIGNALED i ->
        lwt_fatal_error
          Tag.DSL.(
            fun f -> f "The process was killed by signal %a" -% a int_tag i)
    | WSTOPPED i ->
        lwt_fatal_error
          Tag.DSL.(
            fun f -> f "The process was stopped by signal %a" -% a int_tag i)

  let apply_block validator_process chain_state ~max_operations_ttl
      ~(predecessor_block_header : Block_header.t) ~block_header operations =
    let chain_id = State.Chain.id chain_state in
    let genesis = State.Chain.genesis chain_state in
    let genesis_encoded =
      Data_encoding.Json.construct State.Chain.genesis_encoding genesis
    in
    let process =
      Lwt_process.open_process_full
        ( validator_process.process_path,
          [|""; Data_encoding.Json.to_string genesis_encoded|] )
    in
    lwt_log_notice
      Tag.DSL.(
        fun f ->
          f "Block validation started on pid %a"
          -% a (Tag.def "int" Format.pp_print_int) process#pid)
    >>= fun () ->
    Lwt.catch
      (fun () ->
        let to_send =
          Data_encoding.Binary.to_bytes_exn
            Fork_validation.fork_parameters_encoding
            {
              store_root = validator_process.store_root;
              context_root = validator_process.context_root;
              protocol_root = validator_process.protocol_root;
              chain_id;
              block_header;
              predecessor_block_header;
              operations;
              max_operations_ttl;
            }
        in
        Fork_validation.send process#stdin (MBytes.to_string to_send)
        >>= fun () ->
        Fork_validation.recv process#stdout
        >>= fun data ->
        process#status
        >>= fun status ->
        check_process_status status
        >>= fun () ->
        Lwt.return
        @@ Data_encoding.Binary.of_bytes_exn
             (Error_monad.result_encoding Block_validation.result_encoding)
             (MBytes.of_string data))
      (function
        | errors ->
            process#status
            >>= fun status ->
            check_process_status status
            >>= fun () -> Lwt.return (error_exn errors))
end

type validator_kind =
  | Internal of Context.index
  | External of Context.index * string * string * string * string

type t = Sequential of Seq_validator.t | Fork of Fork_validator.t

let init = function
  | Internal index ->
      Seq_validator.init index >>= fun v -> Lwt.return (Sequential v)
  | External (index, store_root, context_root, protocol_root, process_path) ->
      Fork_validator.init
        index
        store_root
        context_root
        protocol_root
        process_path
      >>= fun v -> Lwt.return (Fork v)

let close = function
  | Sequential vp ->
      Seq_validator.close vp
  | Fork vp ->
      Fork_validator.close vp

let apply_block bvp ~predecessor block_header operations =
  let chain_state = State.Block.chain_state predecessor in
  let predecessor_block_header = State.Block.header predecessor in
  State.Block.max_operations_ttl predecessor
  >>=? fun max_operations_ttl ->
  let block_hash = Block_header.hash block_header in
  Chain.data chain_state
  >>= (fun chain_data ->
        if State.Block.equal chain_data.current_head predecessor then
          return (chain_data.live_blocks, chain_data.live_operations)
        else Chain_traversal.live_blocks predecessor max_operations_ttl)
  >>=? fun (live_blocks, live_operations) ->
  Block_validation.check_liveness
    ~live_operations
    ~live_blocks
    block_hash
    operations
  >>=? fun () ->
  match bvp with
  | Sequential vp ->
      Seq_validator.apply_block
        vp
        ~max_operations_ttl
        chain_state
        ~predecessor_block_header
        ~block_header
        operations
  | Fork vp ->
      Fork_validator.apply_block
        vp
        ~max_operations_ttl
        chain_state
        ~predecessor_block_header
        ~block_header
        operations
