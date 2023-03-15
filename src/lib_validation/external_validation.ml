(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type log_config = {
  lwt_log_sink_unix : Lwt_log_sink_unix.cfg;
  internal_events : Tezos_base.Internal_event_config.t;
}

type parameters = {
  context_root : string;
  protocol_root : string;
  genesis : Genesis.t;
  sandbox_parameters : Data_encoding.json option;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
  dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
  log_config : log_config;
}

type request =
  | Validate of {
      chain_id : Chain_id.t;
      block_header : Block_header.t;
      predecessor_block_header : Block_header.t;
      predecessor_block_metadata_hash : Block_metadata_hash.t option;
      predecessor_ops_metadata_hash :
        Operation_metadata_list_list_hash.t option;
      predecessor_resulting_context_hash : Context_hash.t;
      operations : Operation.t list list;
      max_operations_ttl : int;
      should_precheck : bool;
      simulate : bool;
    }
  | Preapply of {
      chain_id : Chain_id.t;
      timestamp : Time.Protocol.t;
      protocol_data : bytes;
      live_blocks : Block_hash.Set.t;
      live_operations : Operation_hash.Set.t;
      predecessor_shell_header : Block_header.shell_header;
      predecessor_hash : Block_hash.t;
      predecessor_max_operations_ttl : int;
      predecessor_block_metadata_hash : Block_metadata_hash.t option;
      predecessor_ops_metadata_hash :
        Operation_metadata_list_list_hash.t option;
      predecessor_resulting_context_hash : Context_hash.t;
      operations : Operation.t list list;
    }
  | Precheck of {
      chain_id : Chain_id.t;
      predecessor_block_header : Block_header.t;
      predecessor_block_hash : Block_hash.t;
      predecessor_resulting_context_hash : Context_hash.t;
      header : Block_header.t;
      operations : Operation.t list list;
      hash : Block_hash.t;
    }
  | Commit_genesis of {chain_id : Chain_id.t}
  | Fork_test_chain of {
      chain_id : Chain_id.t;
      context_hash : Context_hash.t;
      forked_header : Block_header.t;
    }
  | Context_garbage_collection of {
      context_hash : Context_hash.t;
      gc_lockfile_path : string;
    }
  | Context_split
  | Terminate
  | Reconfigure_event_logging of
      Tezos_base_unix.Internal_event_unix.Configuration.t

let request_pp ppf = function
  | Validate {block_header; chain_id; _} ->
      Format.fprintf
        ppf
        "block validation %a for chain %a"
        Block_hash.pp_short
        (Block_header.hash block_header)
        Chain_id.pp_short
        chain_id
  | Preapply {predecessor_hash; chain_id; _} ->
      Format.fprintf
        ppf
        "preapply block ontop of %a for chain %a"
        Block_hash.pp_short
        predecessor_hash
        Chain_id.pp_short
        chain_id
  | Precheck {hash; _} ->
      Format.fprintf ppf "precheck block %a" Block_hash.pp_short hash
  | Commit_genesis {chain_id} ->
      Format.fprintf
        ppf
        "commit genesis block for chain %a"
        Chain_id.pp_short
        chain_id
  | Fork_test_chain {forked_header; _} ->
      Format.fprintf
        ppf
        "test chain fork on block %a"
        Block_hash.pp_short
        (Block_header.hash forked_header)
  | Terminate -> Format.fprintf ppf "terminate validation process"
  | Context_garbage_collection {context_hash; gc_lockfile_path = _} ->
      Format.fprintf
        ppf
        "garbage collecting context below %a"
        Context_hash.pp
        context_hash
  | Context_split -> Format.fprintf ppf "splitting context"
  | Reconfigure_event_logging _ ->
      Format.fprintf ppf "reconfigure event logging"

let magic = Bytes.of_string "TEZOS_FORK_VALIDATOR_MAGIC_0"

let log_config_encoding =
  let open Data_encoding in
  conv
    (fun {internal_events; lwt_log_sink_unix} ->
      (internal_events, lwt_log_sink_unix))
    (fun (internal_events, lwt_log_sink_unix) ->
      {internal_events; lwt_log_sink_unix})
    (obj2
       (req "internal_events" Tezos_base.Internal_event_config.encoding)
       (req "lwt_log_sink_unix" Lwt_log_sink_unix.cfg_encoding))

let parameters_encoding =
  let open Data_encoding in
  conv
    (fun {
           context_root;
           protocol_root;
           genesis;
           user_activated_upgrades;
           user_activated_protocol_overrides;
           operation_metadata_size_limit;
           sandbox_parameters;
           dal_config;
           log_config;
         } ->
      ( context_root,
        protocol_root,
        genesis,
        user_activated_upgrades,
        user_activated_protocol_overrides,
        operation_metadata_size_limit,
        sandbox_parameters,
        dal_config,
        log_config ))
    (fun ( context_root,
           protocol_root,
           genesis,
           user_activated_upgrades,
           user_activated_protocol_overrides,
           operation_metadata_size_limit,
           sandbox_parameters,
           dal_config,
           log_config ) ->
      {
        context_root;
        protocol_root;
        genesis;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
        sandbox_parameters;
        dal_config;
        log_config;
      })
    (obj9
       (req "context_root" string)
       (req "protocol_root" string)
       (req "genesis" Genesis.encoding)
       (req "user_activated_upgrades" User_activated.upgrades_encoding)
       (req
          "user_activated_protocol_overrides"
          User_activated.protocol_overrides_encoding)
       (req
          "operation_metadata_size_limit"
          Shell_limits.operation_metadata_size_limit_encoding)
       (opt "sandbox_parameters" json)
       (req "dal_config" Tezos_crypto_dal.Cryptobox.Config.encoding)
       (req "log_config" log_config_encoding))

let case_validate tag =
  let open Data_encoding in
  case
    tag
    ~title:"validate"
    (obj10
       (req "chain_id" Chain_id.encoding)
       (req "block_header" (dynamic_size Block_header.encoding))
       (req "pred_header" (dynamic_size Block_header.encoding))
       (opt "pred_block_metadata_hash" Block_metadata_hash.encoding)
       (opt "pred_ops_metadata_hash" Operation_metadata_list_list_hash.encoding)
       (req "predecessor_resulting_context_hash" Context_hash.encoding)
       (req "max_operations_ttl" int31)
       (req "operations" (list (list (dynamic_size Operation.encoding))))
       (req "should_precheck" bool)
       (req "simulate" bool))
    (function
      | Validate
          {
            chain_id;
            block_header;
            predecessor_block_header;
            predecessor_block_metadata_hash;
            predecessor_ops_metadata_hash;
            predecessor_resulting_context_hash;
            max_operations_ttl;
            operations;
            should_precheck;
            simulate;
          } ->
          Some
            ( chain_id,
              block_header,
              predecessor_block_header,
              predecessor_block_metadata_hash,
              predecessor_ops_metadata_hash,
              predecessor_resulting_context_hash,
              max_operations_ttl,
              operations,
              should_precheck,
              simulate )
      | _ -> None)
    (fun ( chain_id,
           block_header,
           predecessor_block_header,
           predecessor_block_metadata_hash,
           predecessor_ops_metadata_hash,
           predecessor_resulting_context_hash,
           max_operations_ttl,
           operations,
           should_precheck,
           simulate ) ->
      Validate
        {
          chain_id;
          block_header;
          predecessor_block_header;
          predecessor_block_metadata_hash;
          predecessor_ops_metadata_hash;
          predecessor_resulting_context_hash;
          max_operations_ttl;
          operations;
          should_precheck;
          simulate;
        })

let case_preapply tag =
  let open Data_encoding in
  case
    tag
    ~title:"preapply"
    (merge_objs
       (obj10
          (req "chain_id" Chain_id.encoding)
          (req "timestamp" Time.Protocol.encoding)
          (req "protocol_data" bytes)
          (req "live_blocks" Block_hash.Set.encoding)
          (req "live_operations" Operation_hash.Set.encoding)
          (req "predecessor_shell_header" Block_header.shell_header_encoding)
          (req "predecessor_hash" Block_hash.encoding)
          (req "predecessor_max_operations_ttl" int31)
          (opt "predecessor_block_metadata_hash" Block_metadata_hash.encoding)
          (opt
             "predecessor_ops_metadata_hash"
             Operation_metadata_list_list_hash.encoding))
       (obj2
          (req "predecessor_resulting_context_hash" Context_hash.encoding)
          (req "operations" (list (list (dynamic_size Operation.encoding))))))
    (function
      | Preapply
          {
            chain_id;
            timestamp;
            protocol_data;
            live_blocks;
            live_operations;
            predecessor_shell_header;
            predecessor_hash;
            predecessor_max_operations_ttl;
            predecessor_block_metadata_hash;
            predecessor_ops_metadata_hash;
            predecessor_resulting_context_hash;
            operations;
          } ->
          Some
            ( ( chain_id,
                timestamp,
                protocol_data,
                live_blocks,
                live_operations,
                predecessor_shell_header,
                predecessor_hash,
                predecessor_max_operations_ttl,
                predecessor_block_metadata_hash,
                predecessor_ops_metadata_hash ),
              (predecessor_resulting_context_hash, operations) )
      | _ -> None)
    (fun ( ( chain_id,
             timestamp,
             protocol_data,
             live_blocks,
             live_operations,
             predecessor_shell_header,
             predecessor_hash,
             predecessor_max_operations_ttl,
             predecessor_block_metadata_hash,
             predecessor_ops_metadata_hash ),
           (predecessor_resulting_context_hash, operations) ) ->
      Preapply
        {
          chain_id;
          timestamp;
          protocol_data;
          live_blocks;
          live_operations;
          predecessor_shell_header;
          predecessor_hash;
          predecessor_max_operations_ttl;
          predecessor_block_metadata_hash;
          predecessor_ops_metadata_hash;
          predecessor_resulting_context_hash;
          operations;
        })

let case_precheck tag =
  let open Data_encoding in
  case
    tag
    ~title:"precheck"
    (obj7
       (req "chain_id" Chain_id.encoding)
       (req "predecessor_block_header" (dynamic_size Block_header.encoding))
       (req "predecessor_block_hash" Block_hash.encoding)
       (req "predecessor_resulting_context_hash" Context_hash.encoding)
       (req "header" (dynamic_size Block_header.encoding))
       (req "hash" Block_hash.encoding)
       (req "operations" (list (list (dynamic_size Operation.encoding)))))
    (function
      | Precheck
          {
            chain_id;
            predecessor_block_header;
            predecessor_block_hash;
            predecessor_resulting_context_hash;
            header;
            operations;
            hash;
          } ->
          Some
            ( chain_id,
              predecessor_block_header,
              predecessor_block_hash,
              predecessor_resulting_context_hash,
              header,
              hash,
              operations )
      | _ -> None)
    (fun ( chain_id,
           predecessor_block_header,
           predecessor_block_hash,
           predecessor_resulting_context_hash,
           header,
           hash,
           operations ) ->
      Precheck
        {
          chain_id;
          predecessor_block_header;
          predecessor_block_hash;
          predecessor_resulting_context_hash;
          header;
          operations;
          hash;
        })

let case_context_gc tag =
  let open Data_encoding in
  case
    tag
    ~title:"context_gc"
    (obj2
       (req "context_hash" Context_hash.encoding)
       (req "gc_lockfile_path" string))
    (function
      | Context_garbage_collection {context_hash; gc_lockfile_path} ->
          Some (context_hash, gc_lockfile_path)
      | _ -> None)
    (fun (context_hash, gc_lockfile_path) ->
      Context_garbage_collection {context_hash; gc_lockfile_path})

let case_context_split tag =
  let open Data_encoding in
  case
    tag
    ~title:"context_split"
    unit
    (function Context_split -> Some () | _ -> None)
    (fun () -> Context_split)

let request_encoding =
  let open Data_encoding in
  union
    [
      case_validate (Tag 0);
      case
        (Tag 1)
        ~title:"commit_genesis"
        (obj1 (req "chain_id" Chain_id.encoding))
        (function Commit_genesis {chain_id} -> Some chain_id | _ -> None)
        (fun chain_id -> Commit_genesis {chain_id});
      case
        (Tag 2)
        ~title:"fork_test_chain"
        (obj3
           (req "chain_id" Chain_id.encoding)
           (req "context_hash" Context_hash.encoding)
           (req "forked_header" Block_header.encoding))
        (function
          | Fork_test_chain {chain_id; context_hash; forked_header} ->
              Some (chain_id, context_hash, forked_header)
          | _ -> None)
        (fun (chain_id, context_hash, forked_header) ->
          Fork_test_chain {chain_id; context_hash; forked_header});
      case
        (Tag 3)
        ~title:"terminate"
        unit
        (function Terminate -> Some () | _ -> None)
        (fun () -> Terminate);
      case
        (Tag 4)
        ~title:"reconfigure_event_logging"
        Tezos_base_unix.Internal_event_unix.Configuration.encoding
        (function Reconfigure_event_logging c -> Some c | _ -> None)
        (fun c -> Reconfigure_event_logging c);
      case_preapply (Tag 5);
      case_precheck (Tag 6);
      case_context_gc (Tag 7);
      case_context_split (Tag 8);
    ]

let send pin encoding data =
  let open Lwt_syntax in
  let msg = Data_encoding.Binary.to_bytes_exn encoding data in
  let* () = Lwt_io.write_int pin (Bytes.length msg) in
  let* () = Lwt_io.write pin (Bytes.to_string msg) in
  Lwt_io.flush pin

let recv_result pout encoding =
  let open Lwt_syntax in
  let* count = Lwt_io.read_int pout in
  let buf = Bytes.create count in
  let* () = Lwt_io.read_into_exactly pout buf 0 count in
  return
    (Data_encoding.Binary.of_bytes_exn
       (Error_monad.result_encoding encoding)
       buf)

let recv pout encoding =
  let open Lwt_syntax in
  let* count = Lwt_io.read_int pout in
  let buf = Bytes.create count in
  let* () = Lwt_io.read_into_exactly pout buf 0 count in
  Lwt.return (Data_encoding.Binary.of_bytes_exn encoding buf)

let socket_path_prefix = "tezos-validation-socket-"

let socket_path ~socket_dir ~pid =
  let filename = Format.sprintf "%s%d" socket_path_prefix pid in
  Filename.concat socket_dir filename

(* To get optimized socket communication of processes on the same
   machine, we use Unix domain sockets: ADDR_UNIX. *)
let make_socket socket_path = Unix.ADDR_UNIX socket_path

let create_socket ~canceler =
  let open Lwt_syntax in
  let socket = Lwt_unix.socket PF_UNIX SOCK_STREAM 0o000 in
  Lwt_unix.set_close_on_exec socket ;
  Lwt_canceler.on_cancel canceler (fun () ->
      let* (_ : unit tzresult) = Lwt_utils_unix.safe_close socket in
      return_unit) ;
  Lwt_unix.setsockopt socket SO_REUSEADDR true ;
  Lwt.return socket

let create_socket_listen ~canceler ~max_requests ~socket_path =
  let open Lwt_result_syntax in
  let*! socket = create_socket ~canceler in
  let* () =
    Lwt.catch
      (fun () ->
        let*! () = Lwt_unix.bind socket (make_socket socket_path) in
        return_unit)
      (function
        | Unix.Unix_error (ENAMETOOLONG, _, _) ->
            (* Unix.ENAMETOOLONG (Filename too long (POSIX.1-2001)) can
               be thrown if the given directory has a too long path. *)
            tzfail
              Block_validator_errors.(
                Validation_process_failed (Socket_path_too_long socket_path))
        | Unix.Unix_error (EACCES, _, _) ->
            (* Unix.EACCES (Permission denied (POSIX.1-2001)) can be
               thrown when the given directory has wrong access rights.
               Unix.EPERM (Operation not permitted (POSIX.1-2001)) should
               not be thrown in this case. *)
            tzfail
              Block_validator_errors.(
                Validation_process_failed
                  (Socket_path_wrong_permission socket_path))
        | exn ->
            tzfail
              (Block_validator_errors.Validation_process_failed
                 (Cannot_run_external_validator (Printexc.to_string exn))))
  in
  Lwt_unix.listen socket max_requests ;
  return socket

let create_socket_connect ~canceler ~socket_path =
  let open Lwt_syntax in
  let* socket = create_socket ~canceler in
  let* () = Lwt_unix.connect socket (make_socket socket_path) in
  Lwt.return socket
