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

let name = "validator"

type parameters = {
  protocol_root : string;
  genesis : Genesis.t;
  readonly : bool;
  data_dir : string;
  sandbox_parameters : Data_encoding.json option;
  user_activated_upgrades : User_activated.upgrades;
  user_activated_protocol_overrides : User_activated.protocol_overrides;
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
  internal_events : Tezos_base.Internal_event_config.t;
}

type _ request =
  | Apply : {
      chain_id : Chain_id.t;
      block_header : Block_header.t;
      predecessor_block_header : Block_header.t;
      predecessor_block_metadata_hash : Block_metadata_hash.t option;
      predecessor_ops_metadata_hash :
        Operation_metadata_list_list_hash.t option;
      predecessor_resulting_context_hash : Context_hash.t;
      operations : Block_validation.operation list list;
      max_operations_ttl : int;
      should_validate : bool;
      simulate : bool;
    }
      -> Block_validation.result request
  | Preapply : {
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
      operations : Block_validation.operation list list;
    }
      -> (Block_header.shell_header * error Preapply_result.t list) request
  | Validate : {
      chain_id : Chain_id.t;
      predecessor_block_header : Block_header.t;
      predecessor_block_hash : Block_hash.t;
      predecessor_resulting_context_hash : Context_hash.t;
      header : Block_header.t;
      operations : Block_validation.operation list list;
      hash : Block_hash.t;
    }
      -> unit request
  | Commit_genesis : {chain_id : Chain_id.t} -> Context_hash.t request
  | Fork_test_chain : {
      chain_id : Chain_id.t;
      context_hash : Context_hash.t;
      forked_header : Block_header.t;
    }
      -> Block_header.t request
  | Context_garbage_collection : {
      context_hash : Context_hash.t;
      gc_lockfile_path : string;
    }
      -> unit request
  | Context_split : unit request
  | Terminate : unit request
  | Reconfigure_event_logging :
      Tezos_base_unix.Internal_event_unix.Configuration.t
      -> unit request

let request_pp : type a. Format.formatter -> a request -> unit =
 fun ppf -> function
  | Apply {block_header; chain_id; should_validate; _} ->
      Format.fprintf
        ppf
        "%s %a for chain %a"
        (if should_validate then "validate and apply block" else "apply block")
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
  | Validate {hash; _} ->
      Format.fprintf ppf "validate block %a" Block_hash.pp_short hash
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

let parameters_encoding =
  let open Data_encoding in
  conv
    (fun {
           protocol_root;
           genesis;
           readonly;
           data_dir;
           user_activated_upgrades;
           user_activated_protocol_overrides;
           operation_metadata_size_limit;
           sandbox_parameters;
           internal_events;
         }
       ->
      ( (protocol_root, genesis, readonly, data_dir),
        ( user_activated_upgrades,
          user_activated_protocol_overrides,
          operation_metadata_size_limit,
          sandbox_parameters,
          internal_events ) ))
    (fun ( (protocol_root, genesis, readonly, data_dir),
           ( user_activated_upgrades,
             user_activated_protocol_overrides,
             operation_metadata_size_limit,
             sandbox_parameters,
             internal_events ) )
       ->
      {
        protocol_root;
        genesis;
        readonly;
        data_dir;
        user_activated_upgrades;
        user_activated_protocol_overrides;
        operation_metadata_size_limit;
        sandbox_parameters;
        internal_events;
      })
  @@ merge_objs
       (obj4
          (req "protocol_root" string)
          (req "genesis" Genesis.encoding)
          (req "readonly" bool)
          (req "data_dir" string))
       (obj5
          (req "user_activated_upgrades" User_activated.upgrades_encoding)
          (req
             "user_activated_protocol_overrides"
             User_activated.protocol_overrides_encoding)
          (req
             "operation_metadata_size_limit"
             Shell_limits.operation_metadata_size_limit_encoding)
          (opt "sandbox_parameters" json)
          (req "internal_events" Tezos_base.Internal_event_config.encoding))

type packed_request = Erequest : _ request -> packed_request

let case_apply tag =
  let open Data_encoding in
  case
    tag
    ~title:"Apply"
    (obj10
       (req "chain_id" Chain_id.encoding)
       (req "block_header" (dynamic_size Block_header.encoding))
       (req "pred_header" (dynamic_size Block_header.encoding))
       (opt "pred_block_metadata_hash" Block_metadata_hash.encoding)
       (opt "pred_ops_metadata_hash" Operation_metadata_list_list_hash.encoding)
       (req "predecessor_resulting_context_hash" Context_hash.encoding)
       (req "max_operations_ttl" int31)
       (req
          "operations"
          (list (list (dynamic_size Block_validation.operation_encoding))))
       (req "should_validate" bool)
       (req "simulate" bool))
    (function
      | Erequest
          (Apply
             {
               chain_id;
               block_header;
               predecessor_block_header;
               predecessor_block_metadata_hash;
               predecessor_ops_metadata_hash;
               predecessor_resulting_context_hash;
               max_operations_ttl;
               operations;
               should_validate;
               simulate;
             }) ->
          Some
            ( chain_id,
              block_header,
              predecessor_block_header,
              predecessor_block_metadata_hash,
              predecessor_ops_metadata_hash,
              predecessor_resulting_context_hash,
              max_operations_ttl,
              operations,
              should_validate,
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
           should_validate,
           simulate )
       ->
      Erequest
        (Apply
           {
             chain_id;
             block_header;
             predecessor_block_header;
             predecessor_block_metadata_hash;
             predecessor_ops_metadata_hash;
             predecessor_resulting_context_hash;
             max_operations_ttl;
             operations;
             should_validate;
             simulate;
           }))

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
          (req
             "operations"
             (list (list (dynamic_size Block_validation.operation_encoding))))))
    (function
      | Erequest
          (Preapply
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
             }) ->
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
           (predecessor_resulting_context_hash, operations) )
       ->
      Erequest
        (Preapply
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
           }))

let case_validate tag =
  let open Data_encoding in
  case
    tag
    ~title:"validate"
    (obj7
       (req "chain_id" Chain_id.encoding)
       (req "predecessor_block_header" (dynamic_size Block_header.encoding))
       (req "predecessor_block_hash" Block_hash.encoding)
       (req "predecessor_resulting_context_hash" Context_hash.encoding)
       (req "header" (dynamic_size Block_header.encoding))
       (req "hash" Block_hash.encoding)
       (req
          "operations"
          (list (list (dynamic_size Block_validation.operation_encoding)))))
    (function
      | Erequest
          (Validate
             {
               chain_id;
               predecessor_block_header;
               predecessor_block_hash;
               predecessor_resulting_context_hash;
               header;
               operations;
               hash;
             }) ->
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
           operations )
       ->
      Erequest
        (Validate
           {
             chain_id;
             predecessor_block_header;
             predecessor_block_hash;
             predecessor_resulting_context_hash;
             header;
             operations;
             hash;
           }))

let case_context_gc tag =
  let open Data_encoding in
  case
    tag
    ~title:"context_gc"
    (obj2
       (req "context_hash" Context_hash.encoding)
       (req "gc_lockfile_path" string))
    (function
      | Erequest (Context_garbage_collection {context_hash; gc_lockfile_path})
        ->
          Some (context_hash, gc_lockfile_path)
      | _ -> None)
    (fun (context_hash, gc_lockfile_path) ->
      Erequest (Context_garbage_collection {context_hash; gc_lockfile_path}))

let case_context_split tag =
  let open Data_encoding in
  case
    tag
    ~title:"context_split"
    unit
    (function Erequest Context_split -> Some () | _ -> None)
    (fun () -> Erequest Context_split)

let request_encoding =
  let open Data_encoding in
  union
    [
      case_apply (Tag 0);
      case
        (Tag 1)
        ~title:"commit_genesis"
        (obj1 (req "chain_id" Chain_id.encoding))
        (function
          | Erequest (Commit_genesis {chain_id}) -> Some chain_id | _ -> None)
        (fun chain_id -> Erequest (Commit_genesis {chain_id}));
      case
        (Tag 2)
        ~title:"fork_test_chain"
        (obj3
           (req "chain_id" Chain_id.encoding)
           (req "context_hash" Context_hash.encoding)
           (req "forked_header" Block_header.encoding))
        (function
          | Erequest (Fork_test_chain {chain_id; context_hash; forked_header})
            ->
              Some (chain_id, context_hash, forked_header)
          | _ -> None)
        (fun (chain_id, context_hash, forked_header) ->
          Erequest (Fork_test_chain {chain_id; context_hash; forked_header}));
      case
        (Tag 3)
        ~title:"terminate"
        unit
        (function Erequest Terminate -> Some () | _ -> None)
        (fun () -> Erequest Terminate);
      case
        (Tag 4)
        ~title:"reconfigure_event_logging"
        Tezos_base_unix.Internal_event_unix.Configuration.encoding
        (function
          | Erequest (Reconfigure_event_logging c) -> Some c | _ -> None)
        (fun c -> Erequest (Reconfigure_event_logging c));
      case_preapply (Tag 5);
      case_validate (Tag 6);
      case_context_gc (Tag 7);
      case_context_split (Tag 8);
    ]

let result_encoding : type a. a request -> a Data_encoding.t = function
  | Apply _ -> Block_validation.result_encoding
  | Preapply _ -> Block_validation.preapply_result_encoding
  | Validate _ -> Data_encoding.unit
  | Commit_genesis _ -> Context_hash.encoding
  | Fork_test_chain _ -> Block_header.encoding
  | Context_garbage_collection _ -> Data_encoding.unit
  | Context_split -> Data_encoding.unit
  | Reconfigure_event_logging _ -> Data_encoding.unit
  | Terminate -> Data_encoding.unit

let socket_path_prefix = "tezos-validation-socket-"

let socket_path ~socket_dir ~pid =
  let filename = Format.sprintf "%s%d" socket_path_prefix pid in
  Filename.concat socket_dir filename

let internal_events {internal_events; _} = internal_events

let reconfigure_event_logging_request config = Reconfigure_event_logging config

let terminate_request = Erequest Terminate

let command_line_args ~socket_dir =
  ("octez-validator", ["--socket-dir"; socket_dir])

let hypervisor_name = "octez-validator-hypervisor"

let share_sink = false
