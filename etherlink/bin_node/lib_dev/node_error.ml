(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let exit_code_when_diverge = 100

let exit_code_when_out_of_sync = 101

let exit_code_when_flushed_blueprint = 102

let exit_code_when_error_blueprints_follower = 103

let exit_code_when_gcp_kms_auth_error = 104

let exit_code_when_background_task_fails = 105

type error_source = [`Node | `Kernel]

let error_source_encoding =
  let open Data_encoding in
  string_enum [("node", `Node); ("kernel", `Kernel)]

let pp_proxy_finalize_multichain_source fmt = function
  | `Node -> Format.fprintf fmt "Node"
  | `Kernel -> Format.fprintf fmt "Kernel"

type error +=
  | Diverged of {
      level : Z.t;
      expected_block_hash : Ethereum_types.block_hash;
      found_block_hash : Ethereum_types.block_hash option;
      must_exit : bool;
    }
  | Out_of_sync of {level_expected : int32; level_received : int32}
  | Cannot_handle_flushed_blueprint of Ethereum_types.quantity
  | Unexpected_multichain
  | Proxy_finalize_with_multichain of error_source
  | Singlechain_node_multichain_kernel
  | Mismatched_chain_family of {
      chain_id : L2_types.chain_id;
      node_family : L2_types.ex_chain_family;
      kernel_family : L2_types.ex_chain_family;
    }
  | Dream_rpc_tezlink
  | Set_next_block_info_while_executing of {
      new_level : Ethereum_types.quantity;
      current_level : Ethereum_types.quantity;
    }
  | Execute_single_transaction_no_block_info of {
      transaction_hash : Ethereum_types.hash;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.rollup_diverged"
    ~title:"Sequencer diverged from rollup node."
    ~description:"Sequencer diverged from rollup node."
    ~pp:(fun ppf (level, expected_hash, found_hash, must_exit) ->
      Format.fprintf
        ppf
        "Evm node sequencer diverged from rollup node at blueprint %a, \
         expected hash %a%a.%S"
        Z.pp_print
        level
        Ethereum_types.pp_block_hash
        expected_hash
        Format.(
          pp_print_option (fun fmt hash ->
              fprintf fmt " (found hash: %a)" Ethereum_types.pp_block_hash hash))
        found_hash
        (if must_exit then " The node must exit." else ""))
    Data_encoding.(
      obj4
        (req "blueprint_level" z)
        (req "expected_block_hash" Ethereum_types.block_hash_encoding)
        (opt "found_block_hash" Ethereum_types.block_hash_encoding)
        (req "must_exit" Data_encoding.bool))
    (function
      | Diverged {level; expected_block_hash; found_block_hash; must_exit} ->
          Some (level, expected_block_hash, found_block_hash, must_exit)
      | _ -> None)
    (fun (level, expected_block_hash, found_block_hash, must_exit) ->
      Diverged {level; expected_block_hash; found_block_hash; must_exit}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.rollup_out_of_sync"
    ~title:"Evm node out of sync with rollup node."
    ~description:"Sequencer and the rollup node are out of sync."
    ~pp:(fun ppf (expected, received) ->
      Format.fprintf
        ppf
        "Evm node received finalized level %ld but was expected %ld."
        received
        expected)
    Data_encoding.(obj2 (req "expected" int32) (req "received" int32))
    (function
      | Out_of_sync {level_expected; level_received} ->
          Some (level_expected, level_received)
      | _ -> None)
    (fun (level_expected, level_received) ->
      Out_of_sync {level_expected; level_received}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.evm_event_follower.flushed_blueprint"
    ~title:"Sequencer cannot handle flushed blueprint."
    ~description:"The sequencer cannot handle a flushed blueprint."
    ~pp:(fun ppf level ->
      Format.fprintf
        ppf
        "The sequencer cannot handle a flushed blueprint at level %a"
        Ethereum_types.pp_quantity
        level)
    Data_encoding.(obj1 (req "level" Ethereum_types.quantity_encoding))
    (function Cannot_handle_flushed_blueprint level -> Some level | _ -> None)
    (fun level -> Cannot_handle_flushed_blueprint level) ;
  register_error_kind
    `Permanent
    ~id:"unexpected_multichain"
    ~title:"Multiple chains configured to a single chain node"
    ~description:
      "Observer, proxy, and rpc nodes follow a single chain. However, the \
       configuration attempted to configure multiple chains."
    Data_encoding.empty
    (function Unexpected_multichain -> Some () | _ -> None)
    (fun () -> Unexpected_multichain) ;
  register_error_kind
    `Permanent
    ~id:"proxy_finalize_with_multichain"
    ~title:"Proxy node finalized_view with multichain"
    ~description:
      "The `finalized_view` and `l2_chains` features of the proxy node are not \
       compatible, please configure at most one of them."
    ~pp:(fun ppf source ->
      Format.fprintf
        ppf
        "The %a was configured in multichain mode, which is incompatible with \
         the proxy node's finalized_view option."
        pp_proxy_finalize_multichain_source
        source)
    Data_encoding.(obj1 (req "source" error_source_encoding))
    (function Proxy_finalize_with_multichain src -> Some src | _ -> None)
    (fun src -> Proxy_finalize_with_multichain src) ;
  register_error_kind
    `Permanent
    ~id:"singlechain_node_multichain_kernel"
    ~title:"Node is single chain while the Kernel is multichain"
    ~description:
      "The rollup has the mulichain feature enabled, please configure the \
       l2_chains experimental feature to specify which layer-2 chain this node \
       should follow."
    Data_encoding.empty
    (function Singlechain_node_multichain_kernel -> Some () | _ -> None)
    (fun () -> Singlechain_node_multichain_kernel) ;
  register_error_kind
    `Permanent
    ~id:"dream_rpc_tezlink"
    ~title:"Dream RPC node with Tezlink"
    ~description:"Tezlink is only compatible with Resto RPC nodes."
    Data_encoding.empty
    (function Dream_rpc_tezlink -> Some () | _ -> None)
    (fun () -> Dream_rpc_tezlink) ;
  register_error_kind
    `Permanent
    ~id:"mismatched_chain_family"
    ~title:"Mismatched chain family"
    ~description:
      "The node was configured with a chain family which does not match the \
       one found in the durable storage."
    ~pp:(fun
        ppf
        ( chain_id,
          L2_types.Ex_chain_family node_family,
          L2_types.Ex_chain_family kernel_family )
      ->
      Format.fprintf
        ppf
        "The node was configured with the %a chain family for chain %a but the \
         rollup expects the %a chain family for this chain."
        L2_types.Chain_id.pp
        chain_id
        L2_types.Chain_family.pp
        node_family
        L2_types.Chain_family.pp
        kernel_family)
    Data_encoding.(
      obj3
        (req "chain_id" L2_types.Chain_id.encoding)
        (req "node_family" L2_types.Chain_family.encoding)
        (req "kernel_family" L2_types.Chain_family.encoding))
    (function
      | Mismatched_chain_family {chain_id; node_family; kernel_family} ->
          Some (chain_id, node_family, kernel_family)
      | _ -> None)
    (fun (chain_id, node_family, kernel_family) ->
      Mismatched_chain_family {chain_id; node_family; kernel_family}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.set_next_block_info_while_executing"
    ~title:"set_next_block_info called while executing"
    ~description:
      "set_next_block_info was called while instant confirmation was in \
       Executing state. This should never happen — It should have transitioned \
       to Awaiting_next_block_info first."
    ~pp:(fun ppf (new_level, current_level) ->
      Format.fprintf
        ppf
        "set_next_block_info called for level %a while still executing \
         transaction for level %a"
        Ethereum_types.pp_quantity
        new_level
        Ethereum_types.pp_quantity
        current_level)
    Data_encoding.(
      obj2
        (req "new_level" Ethereum_types.quantity_encoding)
        (req "current_level" Ethereum_types.quantity_encoding))
    (function
      | Set_next_block_info_while_executing {new_level; current_level} ->
          Some (new_level, current_level)
      | _ -> None)
    (fun (new_level, current_level) ->
      Set_next_block_info_while_executing {new_level; current_level}) ;
  register_error_kind
    `Permanent
    ~id:"evm_node.dev.execute_single_transaction_no_block_info"
    ~title:"execute_single_transaction called without block info"
    ~description:
      "execute_single_transaction was called while instant confirmation was \
       awaiting block info. This should never happen — It should have \
       transitioned to Executing first."
    ~pp:(fun ppf transaction_hash ->
      Format.fprintf
        ppf
        "execute_single_transaction called for %a while awaiting block info"
        Ethereum_types.pp_hash
        transaction_hash)
    Data_encoding.(obj1 (req "transaction_hash" Ethereum_types.hash_encoding))
    (function
      | Execute_single_transaction_no_block_info {transaction_hash} ->
          Some transaction_hash
      | _ -> None)
    (fun transaction_hash ->
      Execute_single_transaction_no_block_info {transaction_hash})
