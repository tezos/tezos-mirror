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

(* Block validation using an external processes *)
module External_validator = struct
  include Internal_event.Legacy_logging.Make_semantic (struct
    let name = "shell.validation_process.external"
  end)

  type validation_context = {
    context_root : string;
    protocol_root : string;
    process_path : string;
    mutable validator_process : Lwt_process.process_full option;
    lock : Lwt_mutex.t;
    sandbox_parameters : Data_encoding.json option;
  }

  type t = validation_context

  let init ?sandbox_parameters ~context_root ~protocol_root ~process_path =
    lwt_log_notice (fun f -> f "Initialized")
    >>= fun () ->
    Lwt.return
      {
        context_root;
        protocol_root;
        process_path;
        validator_process = None;
        lock = Lwt_mutex.create ();
        sandbox_parameters;
      }

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

  let close vp =
    lwt_log_notice (fun f -> f "Shutting down ...")
    >>= fun () ->
    match vp.validator_process with
    | Some process ->
        External_validation.send
          process#stdin
          External_validation.request_encoding
          External_validation.Terminate
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
      lwt_log_notice
        Tag.DSL.(
          fun f ->
            f "Block validation started on pid %a"
            -% a (Tag.def "int" Format.pp_print_int) process#pid)
      >>= fun () ->
      let parameters =
        {
          External_validation.context_root = vp.context_root;
          protocol_root = vp.protocol_root;
          sandbox_parameters = vp.sandbox_parameters;
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
          check_process_status status
          >>= fun () ->
          vp.validator_process <- None ;
          lwt_log_notice (fun f -> f "restarting validation process...")
          >>= fun () -> start_process () )
    | None ->
        start_process () )
    >>= fun process ->
    Lwt.catch
      (fun () ->
        (* Make sure that the promise is not canceled between a send and recv *)
        Lwt.protected
          (Lwt_mutex.with_lock vp.lock (fun () ->
               External_validation.send
                 process#stdin
                 External_validation.request_encoding
                 request
               >>= fun () ->
               External_validation.recv_result process#stdout result_encoding))
        >>=? fun res ->
        match process#state with
        | Running ->
            return res
        | Exited status ->
            vp.validator_process <- None ;
            check_process_status status >>= fun () -> return res)
      (function
        | errors ->
            process#status
            >>= fun status ->
            check_process_status status
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

let init = function
  | Internal index ->
      Seq_validator.init index >>= fun v -> return (Sequential v)
  | External {context_root; protocol_root; process_path; sandbox_parameters} ->
      External_validator.init
        ?sandbox_parameters
        ~context_root
        ~protocol_root
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

let commit_genesis bvp ~genesis_hash ~chain_id ~time ~protocol =
  match bvp with
  | Sequential {context_index} ->
      Context.commit_genesis context_index ~chain_id ~time ~protocol
      >>= fun res -> return res
  | External vp ->
      let request =
        External_validation.Commit_genesis
          {genesis_hash; chain_id; time; protocol}
      in
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
