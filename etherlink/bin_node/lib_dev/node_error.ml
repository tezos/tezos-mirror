(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let exit_code_when_diverge = 100

let exit_code_when_out_of_sync = 101

let exit_code_when_flushed_blueprint = 102

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
  | Mismatched_multichain of error_source
  | Dream_rpc_tezlink

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
        expected
        received)
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
      "The proxy node with the finalized_view option is incompatible with the \
       multichain features (originating from either the Node or the Kernel)."
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
    ~id:"mismatched_multichain"
    ~title:"Node and Kernel multichain configuration mismatch"
    ~description:
      "Node and Kernel configurations are mismatched in their multichain \
       settings."
    ~pp:(fun ppf source ->
      match source with
      | `Node ->
          Format.fprintf
            ppf
            "The node is configured to work in a multichain environment, while \
             the kernel is configured as to work in the single chain \
             environment."
      | `Kernel ->
          Format.fprintf
            ppf
            "The kernel is configured to work in a multichain environment, \
             while the node is configured as to work in the single chain \
             environment.")
    Data_encoding.(obj1 (req "source" error_source_encoding))
    (function Mismatched_multichain source -> Some source | _ -> None)
    (fun source -> Mismatched_multichain source) ;
  register_error_kind
    `Permanent
    ~id:"dream_rpc_tezlink"
    ~title:"Dream RPC node with Tezlink"
    ~description:"Tezlink is only compatible with Resto RPC nodes."
    Data_encoding.empty
    (function Dream_rpc_tezlink -> Some () | _ -> None)
    (fun () -> Dream_rpc_tezlink)
