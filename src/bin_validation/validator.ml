(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type status =
  | Initialized
  | Dynload_protocol of Protocol_hash.t
  | Validation_request of Block_header.t
  | Commit_genesis_request of Block_hash.t
  | Initialization_request
  | Fork_test_chain_request of Block_header.t
  | Termination_request
  | Terminated

let status_pp ppf = function
  | Initialized ->
      Format.fprintf ppf "Validator initialized and listening"
  | Dynload_protocol h ->
      Format.fprintf ppf "Dynamic loading of protocol %a" Protocol_hash.pp h
  | Validation_request bh ->
      Format.fprintf
        ppf
        "Validating block %a"
        Block_hash.pp
        (Block_header.hash bh)
  | Commit_genesis_request h ->
      Format.fprintf ppf "Committing genesis block %a" Block_hash.pp h
  | Initialization_request ->
      Format.fprintf ppf "Initializing validator's environment"
  | Fork_test_chain_request bh ->
      Format.fprintf
        ppf
        "Forking test chain at block %a"
        Block_hash.pp
        (Block_header.hash bh)
  | Termination_request ->
      Format.fprintf ppf "Terminating external validator"
  | Terminated ->
      Format.fprintf ppf "Validator terminated"

type s = status Time.System.stamped

module Validator_event_definition = struct
  let name = "external_validator"

  type t = s

  let encoding =
    let open Data_encoding in
    Time.System.stamped_encoding
    @@ union
         [ case
             (Tag 0)
             ~title:"Initialized"
             empty
             (function Initialized -> Some () | _ -> None)
             (fun () -> Initialized);
           case
             (Tag 1)
             ~title:"Dynload protocol"
             Protocol_hash.encoding
             (function Dynload_protocol h -> Some h | _ -> None)
             (fun h -> Dynload_protocol h);
           case
             (Tag 2)
             ~title:"Validation request"
             Block_header.encoding
             (function Validation_request h -> Some h | _ -> None)
             (fun h -> Validation_request h);
           case
             (Tag 3)
             ~title:"Commit genesis request"
             Block_hash.encoding
             (function Commit_genesis_request h -> Some h | _ -> None)
             (fun h -> Commit_genesis_request h);
           case
             (Tag 4)
             ~title:"Initialization request"
             empty
             (function Initialization_request -> Some () | _ -> None)
             (fun () -> Initialization_request);
           case
             (Tag 5)
             ~title:"Fork test chain request"
             Block_header.encoding
             (function Fork_test_chain_request h -> Some h | _ -> None)
             (fun h -> Fork_test_chain_request h);
           case
             (Tag 6)
             ~title:"Termination request"
             empty
             (function Termination_request -> Some () | _ -> None)
             (fun () -> Termination_request);
           case
             (Tag 7)
             ~title:"Terminated"
             empty
             (function Terminated -> Some () | _ -> None)
             (fun () -> Terminated) ]

  let pp ~short:_ ppf (status : t) =
    Format.fprintf ppf "%a" status_pp status.data

  let doc = "External validator status."

  let level (status : t) =
    match status.data with
    | Initialized | Terminated ->
        Internal_event.Info
    | Dynload_protocol _
    | Validation_request _
    | Commit_genesis_request _
    | Initialization_request
    | Fork_test_chain_request _
    | Termination_request ->
        Internal_event.Debug
end

module Validator_event = Internal_event.Make (Validator_event_definition)

let lwt_emit (status : status) =
  let time = Systime_os.now () in
  Validator_event.emit
    ~section:
      (Internal_event.Section.make_sanitized [Validator_event_definition.name])
    (fun () -> Time.System.stamp ~time status)
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error el ->
      Format.kasprintf
        Lwt.fail_with
        "External_validator_event.emit: %a"
        pp_print_error
        el

let ( // ) = Filename.concat

let load_protocol proto protocol_root =
  if Registered_protocol.mem proto then return_unit
  else
    let cmxs_file =
      protocol_root
      // Protocol_hash.to_short_b58check proto
      // Format.asprintf "protocol_%a.cmxs" Protocol_hash.pp proto
    in
    try
      lwt_emit (Dynload_protocol proto)
      >>= fun () ->
      Dynlink.loadfile_private cmxs_file ;
      return_unit
    with Dynlink.Error err ->
      Format.ksprintf
        (fun msg ->
          fail
            Block_validator_errors.(
              Validation_process_failed (Protocol_dynlink_failure msg)))
        "Cannot load file: %s. (Expected location: %s.)"
        (Dynlink.error_message err)
        cmxs_file

let inconsistent_handshake msg =
  Block_validator_errors.(
    Validation_process_failed (Inconsistent_handshake msg))

let handshake input output =
  External_validation.send
    output
    Data_encoding.Variable.bytes
    External_validation.magic
  >>= fun () ->
  External_validation.recv input Data_encoding.Variable.bytes
  >>= fun magic ->
  fail_when
    (not (Bytes.equal magic External_validation.magic))
    (inconsistent_handshake "bad magic")

let init input =
  lwt_emit Initialization_request
  >>= fun () ->
  External_validation.recv input External_validation.parameters_encoding
  >>= fun { context_root;
            protocol_root;
            sandbox_parameters;
            genesis;
            user_activated_upgrades;
            user_activated_protocol_overrides } ->
  let sandbox_param =
    Option.map (fun p -> ("sandbox_parameter", p)) sandbox_parameters
  in
  Context.init
    ~patch_context:(Patch_context.patch_context genesis sandbox_param)
    context_root
  >>= fun context_index ->
  Lwt.return
    ( context_index,
      protocol_root,
      genesis,
      user_activated_upgrades,
      user_activated_protocol_overrides )

let run input output =
  handshake input output
  >>=? fun () ->
  init input
  >>= fun ( context_index,
            protocol_root,
            genesis,
            user_activated_upgrades,
            user_activated_protocol_overrides ) ->
  let rec loop () =
    External_validation.recv input External_validation.request_encoding
    >>= (function
          | External_validation.Validate
              { chain_id;
                block_header;
                predecessor_block_header;
                operations;
                max_operations_ttl } ->
              lwt_emit (Validation_request block_header)
              >>= fun () ->
              Error_monad.protect (fun () ->
                  let pred_context_hash =
                    predecessor_block_header.shell.context
                  in
                  Context.checkout context_index pred_context_hash
                  >>= function
                  | Some context ->
                      return context
                  | None ->
                      fail
                        (Block_validator_errors.Failed_to_checkout_context
                           pred_context_hash))
              >>=? (fun predecessor_context ->
                     Context.get_protocol predecessor_context
                     >>= fun protocol_hash ->
                     load_protocol protocol_hash protocol_root
                     >>=? fun () ->
                     let env =
                       {
                         Block_validation.chain_id;
                         user_activated_upgrades;
                         user_activated_protocol_overrides;
                         max_operations_ttl;
                         predecessor_block_header;
                         predecessor_context;
                       }
                     in
                     Block_validation.apply env block_header operations
                     >>= function
                     | Error
                         [ Block_validator_errors.Unavailable_protocol
                             {protocol; _} ] as err -> (
                         (* If `next_protocol` is missing, try to load it *)
                         load_protocol protocol protocol_root
                         >>= function
                         | Error _ ->
                             Lwt.return err
                         | Ok () ->
                             Block_validation.apply env block_header operations
                         )
                     | result ->
                         Lwt.return result)
              >>= fun res ->
              External_validation.send
                output
                (Error_monad.result_encoding Block_validation.result_encoding)
                res
              >>= return
          | External_validation.Commit_genesis {chain_id} ->
              lwt_emit (Commit_genesis_request genesis.block)
              >>= fun () ->
              Error_monad.protect (fun () ->
                  Context.commit_genesis
                    context_index
                    ~chain_id
                    ~time:genesis.time
                    ~protocol:genesis.protocol
                  >>= fun commit -> return commit)
              >>=? fun commit ->
              External_validation.send
                output
                (Error_monad.result_encoding Context_hash.encoding)
                commit
              >>= return
          | External_validation.Init ->
              External_validation.send
                output
                (Error_monad.result_encoding Data_encoding.empty)
                (Ok ())
              >>= return
          | External_validation.Fork_test_chain {context_hash; forked_header}
            ->
              lwt_emit (Fork_test_chain_request forked_header)
              >>= (fun () ->
                    Context.checkout context_index context_hash
                    >>= function
                    | Some ctxt ->
                        Block_validation.init_test_chain ctxt forked_header
                        >>= (function
                              | Error
                                  [ Block_validator_errors.Missing_test_protocol
                                      protocol ] ->
                                  load_protocol protocol protocol_root
                                  >>=? fun () ->
                                  Block_validation.init_test_chain
                                    ctxt
                                    forked_header
                              | result ->
                                  Lwt.return result)
                        >>= fun result ->
                        External_validation.send
                          output
                          (Error_monad.result_encoding Block_header.encoding)
                          result
                    | None ->
                        External_validation.send
                          output
                          (Error_monad.result_encoding Data_encoding.empty)
                          (error
                             (Block_validator_errors.Failed_to_checkout_context
                                context_hash)))
              >>= return
          | External_validation.Terminate ->
              Lwt_io.flush_all ()
              >>= fun () -> lwt_emit Termination_request >>= fun () -> exit 0
          | External_validation.Restore_context_integrity ->
              let res = Context.restore_integrity context_index in
              External_validation.send
                output
                (Error_monad.result_encoding Data_encoding.(option int31))
                res
              >>= return)
    >>=? loop
  in
  loop ()

let main () =
  Internal_event_unix.init ()
  >>= fun () ->
  let pid = Unix.getpid () in
  let socket_path = External_validation.socket_path ~pid in
  let canceler = Lwt_canceler.create () in
  External_validation.create_socket_connect ~canceler ~socket_path
  >>= fun socket_process ->
  let socket_in = Lwt_io.of_fd ~mode:Input socket_process in
  let socket_out = Lwt_io.of_fd ~mode:Output socket_process in
  lwt_emit Initialized
  >>= fun () ->
  Lwt.catch
    (fun () ->
      run socket_in socket_out
      >>=? fun () -> Lwt_canceler.cancel canceler >>= fun () -> return 0)
    (fun e -> Lwt.return (error_exn e))
  >>= function
  | Ok v ->
      lwt_emit Terminated >>= fun () -> Lwt.return v
  | Error _ as errs ->
      External_validation.send
        socket_out
        (Error_monad.result_encoding Data_encoding.unit)
        errs
      >>= fun () -> Lwt.return 1
