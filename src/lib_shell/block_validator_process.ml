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
  include Block_validator_process_state.Seq_validator_events

  type validation_context = {
    context_index : Context.index;
    genesis : Genesis.t;
    user_activated_upgrades : User_activated.upgrades;
    user_activated_protocol_overrides : User_activated.protocol_overrides;
  }

  type t = validation_context

  let init ~genesis ~user_activated_upgrades ~user_activated_protocol_overrides
      context_index =
    lwt_emit Init
    >>= fun () ->
    Lwt.return
      {
        context_index;
        genesis;
        user_activated_upgrades;
        user_activated_protocol_overrides;
      }

  let close _ = lwt_emit Close

  let apply_block validator_process chain_state ~max_operations_ttl
      ~(predecessor_block_header : Block_header.t) ~block_header operations =
    let chain_id = State.Chain.id chain_state in
    get_context
      validator_process.context_index
      predecessor_block_header.shell.context
    >>=? fun predecessor_context ->
    lwt_timed_emit
      (Validation_request (Block_header.hash block_header, chain_id))
    >>= fun event_start ->
    Block_validation.apply
      chain_id
      ~user_activated_upgrades:validator_process.user_activated_upgrades
      ~user_activated_protocol_overrides:
        validator_process.user_activated_protocol_overrides
      ~max_operations_ttl
      ~predecessor_block_header
      ~predecessor_context
      ~block_header
      operations
    >>=? fun result ->
    lwt_emit (Validation_success (Block_header.hash block_header, event_start))
    >>= fun () -> return result
end

(** Block validation using an external process *)
module External_validator = struct
  include Block_validator_process_state.External_validator_events

  type validation_context = {
    context_root : string;
    protocol_root : string;
    genesis : Genesis.t;
    user_activated_upgrades : User_activated.upgrades;
    user_activated_protocol_overrides : User_activated.protocol_overrides;
    process_path : string;
    mutable validator_process : Lwt_process.process_full option;
    lock : Lwt_mutex.t;
    sandbox_parameters : Data_encoding.json option;
  }

  type t = validation_context

  let init ?sandbox_parameters ~context_root ~protocol_root ~genesis
      ~user_activated_upgrades ~user_activated_protocol_overrides ~process_path
      =
    lwt_emit Init
    >>= fun () ->
    Lwt.return
      {
        context_root;
        protocol_root;
        genesis;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        process_path;
        validator_process = None;
        lock = Lwt_mutex.create ();
        sandbox_parameters;
      }

  let close vp =
    lwt_emit Close
    >>= fun () ->
    match vp.validator_process with
    | Some process ->
        let request = External_validation.Terminate in
        lwt_emit (Request request)
        >>= fun () ->
        External_validation.send
          process#stdin
          External_validation.request_encoding
          request
        >>= fun () ->
        process#status
        >>= (function
              | Unix.WEXITED 0 ->
                  Lwt.return_unit
              | _ ->
                  process#terminate ; Lwt.return_unit)
        >>= fun () ->
        vp.validator_process <- None ;
        Lwt.return_unit
    | None ->
        Lwt.return_unit

  let send_request vp request result_encoding =
    let start_process () =
      let process =
        Lwt_process.open_process_full (vp.process_path, [|"tezos-validator"|])
      in
      lwt_emit (Validator_started process#pid)
      >>= fun () ->
      let parameters =
        {
          External_validation.context_root = vp.context_root;
          protocol_root = vp.protocol_root;
          sandbox_parameters = vp.sandbox_parameters;
          genesis = vp.genesis;
          user_activated_upgrades = vp.user_activated_upgrades;
          user_activated_protocol_overrides =
            vp.user_activated_protocol_overrides;
        }
      in
      vp.validator_process <- Some process ;
      External_validation.send
        process#stdin
        Data_encoding.Variable.bytes
        External_validation.magic
      >>= fun () ->
      External_validation.send
        process#stdin
        External_validation.parameters_encoding
        parameters
      >>= fun () -> Lwt.return process
    in
    ( match vp.validator_process with
    | Some process -> (
      match process#state with
      | Running ->
          Lwt.return process
      | Exited status ->
          vp.validator_process <- None ;
          lwt_emit (Process_status status) >>= fun () -> start_process () )
    | None ->
        start_process () )
    >>= fun process ->
    Lwt.catch
      (fun () ->
        (* Make sure that the promise is not canceled between a send and recv *)
        Lwt.protected
          (Lwt_mutex.with_lock vp.lock (fun () ->
               lwt_timed_emit (Request request)
               >>= fun event_start ->
               External_validation.send
                 process#stdin
                 External_validation.request_encoding
                 request
               >>= fun () ->
               External_validation.recv_result process#stdout result_encoding
               >>= fun res ->
               lwt_emit (Request_result (request, event_start))
               >>= fun () -> Lwt.return res))
        >>=? fun res ->
        match process#state with
        | Running ->
            return res
        | Exited status ->
            vp.validator_process <- None ;
            lwt_emit (Process_status status) >>= fun () -> return res)
      (function
        | errors ->
            process#status
            >>= fun status ->
            lwt_emit (Process_status status)
            >>= fun () ->
            vp.validator_process <- None ;
            Lwt.return (error_exn errors))
end

type validator_kind =
  | Internal of Context.index
  | External of {
      context_root : string;
      protocol_root : string;
      process_path : string;
      sandbox_parameters : Data_encoding.json option;
    }

type t = Sequential of Seq_validator.t | External of External_validator.t

let init ~genesis ~user_activated_upgrades ~user_activated_protocol_overrides
    kind =
  match kind with
  | Internal index ->
      Seq_validator.init
        ~genesis
        ~user_activated_upgrades
        ~user_activated_protocol_overrides
        index
      >>= fun v -> return (Sequential v)
  | External {context_root; protocol_root; process_path; sandbox_parameters} ->
      External_validator.init
        ?sandbox_parameters
        ~context_root
        ~protocol_root
        ~genesis
        ~user_activated_upgrades
        ~user_activated_protocol_overrides
        ~process_path
      >>= fun v ->
      External_validator.send_request
        v
        External_validation.Init
        Data_encoding.empty
      >>=? fun () -> return (External v)

let close = function
  | Sequential vp ->
      Seq_validator.close vp
  | External vp ->
      External_validator.close vp

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
  | External vp ->
      let chain_id = State.Chain.id chain_state in
      let request =
        External_validation.Validate
          {
            chain_id;
            block_header;
            predecessor_block_header;
            operations;
            max_operations_ttl;
          }
      in
      External_validator.send_request
        vp
        request
        Block_validation.result_encoding

let commit_genesis bvp ~chain_id =
  match bvp with
  | Sequential {context_index; genesis; _} ->
      Context.commit_genesis
        context_index
        ~chain_id
        ~time:genesis.time
        ~protocol:genesis.protocol
  | External vp ->
      let request = External_validation.Commit_genesis {chain_id} in
      External_validator.send_request vp request Context_hash.encoding

let init_test_chain bvp forking_block =
  let forked_header = State.Block.header forking_block in
  match bvp with
  | Sequential _ ->
      State.Block.context forking_block
      >>=? fun context ->
      Block_validation.init_test_chain context forked_header
  | External vp ->
      let context_hash = forked_header.shell.context in
      let request =
        External_validation.Fork_test_chain {context_hash; forked_header}
      in
      External_validator.send_request vp request Block_header.encoding

let restore_context_integrity bvp =
  match bvp with
  | Sequential {context_index; _} ->
      Lwt.return (Context.restore_integrity context_index)
  | External vp ->
      let request = External_validation.Restore_context_integrity in
      External_validator.send_request vp request Data_encoding.(option int31)
