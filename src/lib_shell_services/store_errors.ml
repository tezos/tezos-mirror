(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type error +=
  | Block_not_found of {hash : Block_hash.t; distance : int}
  | Resulting_context_hash_not_found of {hash : Block_hash.t; level : int32}
  | Bad_level of {head_level : Int32.t; given_level : Int32.t}
  | Block_metadata_not_found of Block_hash.t
  | Protocol_not_found of {protocol_level : int}
  | Cannot_switch_history_mode of {
      previous_mode : History_mode.t;
      next_mode : History_mode.t;
    }
  | Invalid_head_switch of {
      checkpoint_level : int32;
      given_head : Block_hash.t * int32;
    }
  | Inconsistent_store_state of string
  | Inconsistent_operations_hash of {
      expected : Operation_list_list_hash.t;
      got : Operation_list_list_hash.t;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"store.block_not_found"
    ~title:"Block not found"
    ~description:"Block not found"
    ~pp:(fun ppf (block_hash, distance) ->
      Format.fprintf
        ppf
        "Cannot find block at distance %d from block %a."
        distance
        Block_hash.pp
        block_hash)
    Data_encoding.(
      obj1 (req "block_not_found" @@ tup2 Block_hash.encoding int31))
    (function
      | Block_not_found {hash; distance} -> Some (hash, distance) | _ -> None)
    (fun (hash, distance) -> Block_not_found {hash; distance}) ;
  register_error_kind
    `Permanent
    ~id:"store.resulting_context_hash_not_found"
    ~title:"Resulting context hash not found"
    ~description:"Resulting context hash not found"
    ~pp:(fun ppf (block_hash, level) ->
      Format.fprintf
        ppf
        "Cannot find the resulting context hash for the block %a (level: %ld)."
        Block_hash.pp
        block_hash
        level)
    Data_encoding.(
      obj1 (req "block_not_found" @@ tup2 Block_hash.encoding int32))
    (function
      | Resulting_context_hash_not_found {hash; level} -> Some (hash, level)
      | _ -> None)
    (fun (hash, level) -> Resulting_context_hash_not_found {hash; level}) ;
  register_error_kind
    `Permanent
    ~id:"store.bad_level"
    ~title:"Bad level"
    ~description:"Read a block at level past our current head."
    ~pp:(fun ppf (head_level, given_level) ->
      Format.fprintf
        ppf
        "The block at level %ld is beyond our current head's level : %ld."
        given_level
        head_level)
    Data_encoding.(obj2 (req "head_level" int32) (req "given_level" int32))
    (function
      | Bad_level {head_level; given_level} -> Some (head_level, given_level)
      | _ -> None)
    (fun (head_level, given_level) -> Bad_level {head_level; given_level}) ;
  register_error_kind
    `Permanent
    ~id:"store.metadata_not_found"
    ~title:"Block metadata not found"
    ~description:"Block metadata not found"
    ~pp:(fun ppf block_hash ->
      Format.fprintf
        ppf
        "Unable to find block %a's metadata."
        Block_hash.pp
        block_hash)
    Data_encoding.(obj1 (req "block_metadata_not_found" Block_hash.encoding))
    (function
      | Block_metadata_not_found block_hash -> Some block_hash | _ -> None)
    (fun block_hash -> Block_metadata_not_found block_hash) ;
  register_error_kind
    `Permanent
    ~id:"store.protocol_not_found"
    ~title:"Protocol not found"
    ~description:"Protocol not found"
    ~pp:(fun ppf protocol_level ->
      Format.fprintf ppf "Unable to find protocol %d." protocol_level)
    Data_encoding.(obj1 (req "protocol_level" int31))
    (function
      | Protocol_not_found {protocol_level} -> Some protocol_level | _ -> None)
    (fun protocol_level -> Protocol_not_found {protocol_level}) ;
  register_error_kind
    `Permanent
    ~id:"config_file.cannot_switch_history_mode"
    ~title:"Cannot switch history mode"
    ~description:"Cannot switch history mode."
    ~pp:(fun ppf (prev, next) ->
      Format.fprintf
        ppf
        "Cannot switch from history mode %a to %a. In order to change your \
         history mode please refer to the Tezos node documentation. If you \
         really want to change your history mode, run this command again with \
         the `--force-history-mode-switch` option."
        History_mode.pp
        prev
        History_mode.pp
        next)
    (Data_encoding.obj2
       (Data_encoding.req "previous_mode" History_mode.encoding)
       (Data_encoding.req "next_mode" History_mode.encoding))
    (function
      | Cannot_switch_history_mode x -> Some (x.previous_mode, x.next_mode)
      | _ -> None)
    (fun (previous_mode, next_mode) ->
      Cannot_switch_history_mode {previous_mode; next_mode}) ;
  register_error_kind
    `Permanent
    ~id:"store.invalid_head_switch"
    ~title:"Invalid head switch"
    ~description:
      "The given head is not consistent with the current store's savepoint"
    ~pp:(fun ppf (minimum_allowed_level, (head_hash, head_level)) ->
      Format.fprintf
        ppf
        "The given head %a (%ld) is below the minimum allowed level %ld."
        Block_hash.pp
        head_hash
        head_level
        minimum_allowed_level)
    Data_encoding.(
      obj2
        (req "minimum_allowed_level" int32)
        (req "given_head" @@ tup2 Block_hash.encoding int32))
    (function
      | Invalid_head_switch {checkpoint_level; given_head} ->
          Some (checkpoint_level, given_head)
      | _ -> None)
    (fun (checkpoint_level, given_head) ->
      Invalid_head_switch {checkpoint_level; given_head}) ;
  register_error_kind
    `Permanent
    ~id:"snapshots.inconsistent_operation_hashes"
    ~title:"Inconsistent operation hashes"
    ~description:"The operations given do not match their hashes."
    ~pp:(fun ppf (oph, oph') ->
      Format.fprintf
        ppf
        "Inconsistent operation hashes. Expected: %a, got %a."
        Operation_list_list_hash.pp
        oph
        Operation_list_list_hash.pp
        oph')
    Data_encoding.(
      obj2
        (req "expected_operation_hashes" Operation_list_list_hash.encoding)
        (req "received_operation_hashes" Operation_list_list_hash.encoding))
    (function
      | Inconsistent_operations_hash {expected; got} -> Some (expected, got)
      | _ -> None)
    (fun (expected, got) -> Inconsistent_operations_hash {expected; got})

type cemented_store_inconsistency =
  | Missing_cycle of {low_cycle : string; high_cycle : string}
  | Inconsistent_cycle_start of {
      cycle_start : Int32.t;
      expected_cycle_start : Int32.t;
    }
  | Bad_offset of {level : int; cycle : string}
  | Unexpected_level of {
      block_hash : Block_hash.t;
      expected : Int32.t;
      got : Int32.t;
    }
  | Corrupted_index of Block_hash.t
  | Inconsistent_highest_cemented_level of {
      highest_cemented_level : Int32.t;
      cementing_highwatermark : Int32.t;
    }

let cemented_store_inconsistency_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Missing cycle"
        (obj2 (req "low_cycle" string) (req "high_cycle" string))
        (function
          | Missing_cycle {low_cycle; high_cycle} -> Some (low_cycle, high_cycle)
          | _ -> None)
        (fun (low_cycle, high_cycle) -> Missing_cycle {low_cycle; high_cycle});
      case
        (Tag 1)
        ~title:"Inconsistent_cycle_start"
        (obj2 (req "cycle_start" int32) (req "expected_cycle_start" int32))
        (function
          | Inconsistent_cycle_start {expected_cycle_start; cycle_start} ->
              Some (expected_cycle_start, cycle_start)
          | _ -> None)
        (fun (expected_cycle_start, cycle_start) ->
          Inconsistent_cycle_start {expected_cycle_start; cycle_start});
      case
        (Tag 2)
        ~title:"Bad offset"
        (obj2 (req "level" int31) (req "cycle" string))
        (function
          | Bad_offset {level; cycle} -> Some (level, cycle) | _ -> None)
        (fun (level, cycle) -> Bad_offset {level; cycle});
      case
        (Tag 3)
        ~title:"Unexpected level"
        (obj3
           (req "block_hash" Block_hash.encoding)
           (req "expected" int32)
           (req "got" int32))
        (function
          | Unexpected_level {block_hash; expected; got} ->
              Some (block_hash, expected, got)
          | _ -> None)
        (fun (block_hash, expected, got) ->
          Unexpected_level {block_hash; expected; got});
      case
        (Tag 4)
        ~title:"Corrupted index"
        (obj1 (req "block_hash" Block_hash.encoding))
        (function Corrupted_index h -> Some h | _ -> None)
        (fun h -> Corrupted_index h);
      case
        (Tag 5)
        ~title:"Inconsistent highest cemented level"
        (obj2
           (req "highest_cemented_level" int32)
           (req "cementing_highwatermark" int32))
        (function
          | Inconsistent_highest_cemented_level
              {highest_cemented_level; cementing_highwatermark} ->
              Some (highest_cemented_level, cementing_highwatermark)
          | _ -> None)
        (fun (highest_cemented_level, cementing_highwatermark) ->
          Inconsistent_highest_cemented_level
            {highest_cemented_level; cementing_highwatermark});
    ]

type store_block_error =
  | Invalid_block
  | Invalid_operations_length of {validation_passes : int; operations : int}
  | Invalid_operations_data_length of {
      validation_passes : int;
      operations_data : int;
    }
  | Inconsistent_operations_lengths of {
      operations_lengths : string;
      operations_data_lengths : string;
    }
  | Invalid_last_allowed_fork_level of {
      last_allowed_fork_level : int32;
      genesis_level : int32;
    }

let store_block_error_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Invalid operations length"
        (obj2 (req "validation_passes" int31) (req "operations" int31))
        (function
          | Invalid_operations_length {validation_passes; operations} ->
              Some (validation_passes, operations)
          | _ -> None)
        (fun (validation_passes, operations) ->
          Invalid_operations_length {validation_passes; operations});
      case
        (Tag 1)
        ~title:"Invalid operations data length"
        (obj2 (req "validation_passes" int31) (req "operations_data" int31))
        (function
          | Invalid_operations_data_length {validation_passes; operations_data}
            ->
              Some (validation_passes, operations_data)
          | _ -> None)
        (fun (validation_passes, operations_data) ->
          Invalid_operations_data_length {validation_passes; operations_data});
      case
        (Tag 2)
        ~title:"Inconsistent operations length"
        (obj2
           (req "operations_lengths" string)
           (req "operations_data_lengths" string))
        (function
          | Inconsistent_operations_lengths
              {operations_lengths; operations_data_lengths} ->
              Some (operations_lengths, operations_data_lengths)
          | _ -> None)
        (fun (operations_lengths, operations_data_lengths) ->
          Inconsistent_operations_lengths
            {operations_lengths; operations_data_lengths});
    ]

type error +=
  | Cannot_write_in_readonly
  | Wrong_predecessor of Block_hash.t * int
  | Invalid_blocks_to_cement
  | Wrong_floating_kind_swap
  | Cannot_update_floating_store
  | Cannot_instanciate_temporary_floating_store
  | Merge_already_running
  | Merge_error
  | Cannot_load_degraded_store
  | Cannot_merge_store of {status : string}
  | Failed_to_init_cemented_block_store of string
  | Cannot_cement_blocks_metadata of [`Empty | `Not_cemented]
  | Cannot_cement_blocks of [`Empty | `Higher_cemented]
  | Temporary_cemented_file_exists of string
  | Inconsistent_cemented_file of string * string
  | Inconsistent_cemented_store of cemented_store_inconsistency
  | Missing_last_allowed_fork_level_block
  | Inconsistent_block_hash of {
      level : Int32.t;
      expected_hash : Block_hash.t;
      computed_hash : Block_hash.t;
    }
  | Inconsistent_block_predecessor of {
      block_hash : Block_hash.t;
      level : Int32.t;
      expected_hash : Block_hash.t;
      computed_hash : Block_hash.t;
    }
  | Cannot_encode_block of Block_hash.t
  | Cannot_store_block of Block_hash.t * store_block_error
  | Cannot_checkout_context of Block_hash.t * Context_hash.t
  | Cannot_find_protocol of int
  | Invalid_genesis_marking
  | Cannot_retrieve_savepoint of Int32.t
  | Cannot_set_target of (Block_hash.t * Int32.t)
  | Missing_commit_info of string
  | Inconsistent_chain_store
  | Fork_testchain_not_allowed
  | Cannot_fork_testchain of Chain_id.t
  | Cannot_load_testchain of string
  | Missing_activation_block of Block_hash.t * Protocol_hash.t * History_mode.t
  | Inconsistent_protocol_commit_info of Block_hash.t * Protocol_hash.t
  | Failed_to_get_live_blocks of Block_hash.t
  | Target_mismatch
  | Bad_head_invariant

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.cannot_write_in_readonly"
    ~title:"Cannot write in readonly"
    ~description:"Cannot write data in store when in readonly"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Cannot write data in store when in readonly.")
    Data_encoding.empty
    (function Cannot_write_in_readonly -> Some () | _ -> None)
    (fun () -> Cannot_write_in_readonly) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.wrong_predecessor"
    ~title:"Wrong predecessor"
    ~description:"Failed to get block's predecessor"
    ~pp:(fun ppf (hash, offset) ->
      Format.fprintf
        ppf
        "Failed to get the nth predecessor of %a. The offset is invalid: %d."
        Block_hash.pp
        hash
        offset)
    Data_encoding.(obj2 (req "hash" Block_hash.encoding) (req "offset" int31))
    (function
      | Wrong_predecessor (hash, offset) -> Some (hash, offset) | _ -> None)
    (fun (hash, offset) -> Wrong_predecessor (hash, offset)) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.invalid_blocks_to_cement"
    ~title:"Invalid blocks to cement"
    ~description:"Invalid block list to cement"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Invalid block list to cement: the block list must be correctly \
         chained and their levels growing strictly by one between each block.")
    Data_encoding.empty
    (function Invalid_blocks_to_cement -> Some () | _ -> None)
    (fun () -> Invalid_blocks_to_cement) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.wrong_floating_kind_swap"
    ~title:"Wrong floating kind swap"
    ~description:"Try to swap wrong floating store kind"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to swap floating stores: tried to swap floating store of the \
         same kind.")
    Data_encoding.empty
    (function Wrong_floating_kind_swap -> Some () | _ -> None)
    (fun () -> Wrong_floating_kind_swap) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_update_floating_store"
    ~title:"Cannot update floating store"
    ~description:"Cannot update floating store"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Cannot update the floating store: failed to retrieve enough blocks to \
         cement.")
    Data_encoding.empty
    (function Cannot_update_floating_store -> Some () | _ -> None)
    (fun () -> Cannot_update_floating_store) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_instanciate_temporary_floating_store"
    ~title:"Cannot instanciate temporary floating store"
    ~description:"Cannot instanciate temporary floating store"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Cannot instanciate temporary floating store.")
    Data_encoding.empty
    (function
      | Cannot_instanciate_temporary_floating_store -> Some () | _ -> None)
    (fun () -> Cannot_instanciate_temporary_floating_store) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.merge_error"
    ~title:"Merge error"
    ~description:"Error while merging the store"
    ~pp:(fun ppf () -> Format.fprintf ppf "Error while merging the store.")
    Data_encoding.empty
    (function Merge_error -> Some () | _ -> None)
    (fun () -> Merge_error) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.merge_already_running"
    ~title:"Merge already running"
    ~description:"The store's merge is already running"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The store's merge is already running.")
    Data_encoding.empty
    (function Merge_already_running -> Some () | _ -> None)
    (fun () -> Merge_already_running) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.cannot_load_degraded_store"
    ~title:"Cannot load degraded store"
    ~description:"Cannot load a degraded block store."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Cannot load a degraded store. Its consistency must first be restored.")
    Data_encoding.empty
    (function Cannot_load_degraded_store -> Some () | _ -> None)
    (fun () -> Cannot_load_degraded_store) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.cannot_merge_store"
    ~title:"Cannot merge store"
    ~description:"Cannot merge the store."
    ~pp:(fun ppf status ->
      Format.fprintf ppf "Cannot merge the store, unexpected status: %s." status)
    Data_encoding.(obj1 (req "status" string))
    (function Cannot_merge_store {status} -> Some status | _ -> None)
    (fun status -> Cannot_merge_store {status}) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.failed_to_init_cemented_block_store"
    ~title:"Failed to init cemented block store"
    ~description:"Failed to initialize the cemented block store"
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "Failed to initialize the cemented block store: file %s is not a \
         directory."
        path)
    Data_encoding.(obj1 (req "path" string))
    (function
      | Failed_to_init_cemented_block_store path -> Some path | _ -> None)
    (fun path -> Failed_to_init_cemented_block_store path) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_cement_blocks_metadata"
    ~title:"Cannot cement blocks metadata"
    ~description:"Cannot cement blocks metadata"
    ~pp:(fun ppf reason ->
      Format.fprintf
        ppf
        "Failed to cement the blocks metadata: %s."
        (match reason with
        | `Empty -> "the given list of blocks is empty"
        | `Not_cemented -> "the given blocks ar not cemented"))
    Data_encoding.(
      obj1
        (req
           "reason"
           (string_enum [("empty", `Empty); ("not_cemented", `Not_cemented)])))
    (function Cannot_cement_blocks_metadata r -> Some r | _ -> None)
    (fun r -> Cannot_cement_blocks_metadata r) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_cement_blocks"
    ~title:"Cannot cement blocks"
    ~description:"Cannot cement blocks"
    ~pp:(fun ppf reason ->
      Format.fprintf
        ppf
        "Failed to merge the store: %s."
        (match reason with
        | `Empty -> "no valid cycles were found"
        | `Higher_cemented ->
            "the highest cemented block has a higher level than the given \
             blocks"))
    Data_encoding.(
      obj1
        (req
           "reason"
           (string_enum
              [("empty", `Empty); ("higher_cemented", `Higher_cemented)])))
    (function Cannot_cement_blocks r -> Some r | _ -> None)
    (fun r -> Cannot_cement_blocks r) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.temporary_cemented_file_exists"
    ~title:"Temporary cemented file exists"
    ~description:"The temporary cemented file already exists"
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "Error while merging the store: the temporary cemented file %s already \
         exists."
        path)
    Data_encoding.(obj1 (req "path" string))
    (function Temporary_cemented_file_exists path -> Some path | _ -> None)
    (fun path -> Temporary_cemented_file_exists path) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.inconsistent_cemented_file"
    ~title:"Inconsistent cemented file"
    ~description:"Failed to read a cemented file"
    ~pp:(fun ppf (path, trace) ->
      Format.fprintf
        ppf
        "Failed to read the cemented file %s. Unexpected failure: %s."
        path
        trace)
    Data_encoding.(obj2 (req "path" string) (req "trace" string))
    (function
      | Inconsistent_cemented_file (path, trace) -> Some (path, trace)
      | _ -> None)
    (fun (path, trace) -> Inconsistent_cemented_file (path, trace)) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.inconsistent_cemented_store"
    ~title:"Inconsistent cemented store"
    ~description:"Failed to check indexes consistency"
    ~pp:(fun ppf csi ->
      Format.fprintf
        ppf
        "The store is in an unexpected and inconsistent state: %s."
        (match csi with
        | Missing_cycle {low_cycle; high_cycle} ->
            Format.sprintf
              "missing cycle between %s and %s"
              low_cycle
              high_cycle
        | Inconsistent_cycle_start {cycle_start; expected_cycle_start} ->
            Format.asprintf
              "inconsistent cycle starting at %ld but expected at %ld"
              cycle_start
              expected_cycle_start
        | Bad_offset {level; cycle} ->
            Format.asprintf
              "bad offset found for block %d in cycle %s"
              level
              cycle
        | Unexpected_level {block_hash; expected; got} ->
            Format.asprintf
              "bad level found for block %a - expected %ld got %ld"
              Block_hash.pp
              block_hash
              expected
              got
        | Corrupted_index h ->
            Format.asprintf
              "%a was not found in the imported store"
              Block_hash.pp
              h
        | Inconsistent_highest_cemented_level
            {highest_cemented_level; cementing_highwatermark} ->
            Format.sprintf
              "the most recent cemented block (%ld) is not the previous \
               cemented highwatermark (%ld)"
              highest_cemented_level
              cementing_highwatermark))
    Data_encoding.(
      obj1
        (req "inconsistent_cemented_file" cemented_store_inconsistency_encoding))
    (function Inconsistent_cemented_store csi -> Some csi | _ -> None)
    (fun csi -> Inconsistent_cemented_store csi) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.missing_last_allowed_fork_level_block"
    ~title:"Missing last allowed fork level block"
    ~description:
      "Current head's last allowed fork level block (or its associated \
       metadata) cannot be found in the store."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Current head's last allowed fork level block or (its associated \
         metadata) cannot be found in the store.")
    Data_encoding.empty
    (function Missing_last_allowed_fork_level_block -> Some () | _ -> None)
    (fun () -> Missing_last_allowed_fork_level_block) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.inconsistent_block_hash"
    ~title:"Inconsistent block hash"
    ~description:"Inconsistent block hash found"
    ~pp:(fun ppf (level, expected_hash, computed_hash) ->
      Format.fprintf
        ppf
        "Inconsistent block: inconsistent hash found for block %ld. Expected \
         %a, got %a."
        level
        Block_hash.pp
        expected_hash
        Block_hash.pp
        computed_hash)
    Data_encoding.(
      obj3
        (req "level" int32)
        (req "expected_hash" Block_hash.encoding)
        (req "computed_hash" Block_hash.encoding))
    (function
      | Inconsistent_block_hash {level; expected_hash; computed_hash} ->
          Some (level, expected_hash, computed_hash)
      | _ -> None)
    (fun (level, expected_hash, computed_hash) ->
      Inconsistent_block_hash {level; expected_hash; computed_hash}) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.inconsistent_block_predecessor"
    ~title:"Inconsistent block predecessor"
    ~description:"Inconsistent block predecessor"
    ~pp:(fun ppf (block_hash, level, expected_hash, computed_hash) ->
      Format.fprintf
        ppf
        "Inconsistent block: inconsistent predecessor found for block %a (%ld) \
         - expected: %a vs got: %a."
        Block_hash.pp
        block_hash
        level
        Block_hash.pp
        expected_hash
        Block_hash.pp
        computed_hash)
    Data_encoding.(
      obj4
        (req "block_hash" Block_hash.encoding)
        (req "level" int32)
        (req "expected_hash" Block_hash.encoding)
        (req "computed_hash" Block_hash.encoding))
    (function
      | Inconsistent_block_predecessor
          {block_hash; level; expected_hash; computed_hash} ->
          Some (block_hash, level, expected_hash, computed_hash)
      | _ -> None)
    (fun (block_hash, level, expected_hash, computed_hash) ->
      Inconsistent_block_predecessor
        {block_hash; level; expected_hash; computed_hash}) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_encode_block"
    ~title:"Cannot encode block"
    ~description:"Failed to encode block"
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Failed to write block in floating store: cannot encode block %a."
        Block_hash.pp
        hash)
    Data_encoding.(obj1 (req "hash" Block_hash.encoding))
    (function Cannot_encode_block hash -> Some hash | _ -> None)
    (fun hash -> Cannot_encode_block hash) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_store_block"
    ~title:"Cannot store block"
    ~description:"Failed to store block"
    ~pp:(fun ppf (hash, err) ->
      Format.fprintf
        ppf
        "Failed to store block %a: %s."
        Block_hash.pp
        hash
        (match err with
        | Invalid_block -> "the block is marked as invalid"
        | Invalid_operations_length {validation_passes; operations} ->
            Format.sprintf
              "invalid operations length %d (%d was expected)"
              operations
              validation_passes
        | Invalid_operations_data_length {validation_passes; operations_data} ->
            Format.sprintf
              "invalid operation_data length %d (%d was expected)"
              validation_passes
              operations_data
        | Inconsistent_operations_lengths
            {operations_lengths; operations_data_lengths} ->
            Format.sprintf
              "inconsistent operations (%s) and operations_data (%s) lengths"
              operations_lengths
              operations_data_lengths
        | Invalid_last_allowed_fork_level
            {last_allowed_fork_level; genesis_level} ->
            Format.sprintf
              "block's last allowed fork level (%ld) is below the genesis \
               level (%ld)"
              last_allowed_fork_level
              genesis_level))
    Data_encoding.(
      obj2
        (req "hash" Block_hash.encoding)
        (req "err" store_block_error_encoding))
    (function Cannot_store_block (hash, err) -> Some (hash, err) | _ -> None)
    (fun (hash, err) -> Cannot_store_block (hash, err)) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_checkout_context"
    ~title:"Cannot checkout context"
    ~description:"Failed to checkout context"
    ~pp:(fun ppf (bh, ch) ->
      Format.fprintf
        ppf
        "Failed to checkout the context (%a) for block %a."
        Context_hash.pp
        ch
        Block_hash.pp
        bh)
    Data_encoding.(
      obj2
        (req "block_hash" Block_hash.encoding)
        (req "context_hash" Context_hash.encoding))
    (function Cannot_checkout_context (bh, ch) -> Some (bh, ch) | _ -> None)
    (fun (bh, ch) -> Cannot_checkout_context (bh, ch)) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_find_protocol"
    ~title:"Cannot find protocol"
    ~description:"Cannot find protocol"
    ~pp:(fun ppf proto_level ->
      Format.fprintf ppf "Cannot find protocol with level %d." proto_level)
    Data_encoding.(obj1 (req "protocol_level" int31))
    (function
      | Cannot_find_protocol proto_level -> Some proto_level | _ -> None)
    (fun proto_level -> Cannot_find_protocol proto_level) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.invalid_genesis_marking"
    ~title:"Invalid genesis marking"
    ~description:"Cannot mark genesis as invalid"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Cannot mark the genesis block is invalid.")
    Data_encoding.empty
    (function Invalid_genesis_marking -> Some () | _ -> None)
    (fun () -> Invalid_genesis_marking) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_retrieve_savepoint"
    ~title:"Cannot retrieve savepoint"
    ~description:"Failed to retrieve savepoint"
    ~pp:(fun ppf level ->
      Format.fprintf
        ppf
        "Failed to retrieve the new savepoint hash (expected at level %ld)."
        level)
    Data_encoding.(obj1 (req "level" int32))
    (function Cannot_retrieve_savepoint level -> Some level | _ -> None)
    (fun level -> Cannot_retrieve_savepoint level) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_set_target"
    ~title:"Cannot set target"
    ~description:"The given block to be set as target is invalid."
    ~pp:(fun ppf (given_target_hash, given_target_level) ->
      Format.fprintf
        ppf
        "Failed to set the given target %a (%ld): it is either invalid, or not \
         a predecessor of the checkpoint."
        Block_hash.pp
        given_target_hash
        given_target_level)
    Data_encoding.(obj1 (req "given_target" (tup2 Block_hash.encoding int32)))
    (function Cannot_set_target given_target -> Some given_target | _ -> None)
    (fun given_target -> Cannot_set_target given_target) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.missing_commit_info"
    ~title:"Missing commit info"
    ~description:"Failed to retreive commit info"
    ~pp:(fun ppf trace ->
      Format.fprintf
        ppf
        "Failed to retreive commit info: %s@.Is the context initialized?"
        trace)
    Data_encoding.(obj1 (req "trace" string))
    (function Missing_commit_info trace -> Some trace | _ -> None)
    (fun trace -> Missing_commit_info trace) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.inconsistent_chain_store"
    ~title:"Inconsistent chain store"
    ~description:"Failed to load chain store"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to load the chain store: could not retrieve head metadata.")
    Data_encoding.empty
    (function Inconsistent_chain_store -> Some () | _ -> None)
    (fun () -> Inconsistent_chain_store) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.fork_testchain_not_allowed"
    ~title:"Fork testchain not allowed"
    ~description:"Forking the test chain is not allowed"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to fork the test chain: it is not allowed by the store's \
         configuration.")
    Data_encoding.empty
    (function Fork_testchain_not_allowed -> Some () | _ -> None)
    (fun () -> Fork_testchain_not_allowed) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_fork_testchain"
    ~title:"Cannot fork testchain"
    ~description:"Failed to fork testchain"
    ~pp:(fun ppf chain_id ->
      Format.fprintf
        ppf
        "Failed to fork the testchain: the testchain %a already exists."
        Chain_id.pp
        chain_id)
    Data_encoding.(obj1 (req "chain_id" Chain_id.encoding))
    (function Cannot_fork_testchain chain_id -> Some chain_id | _ -> None)
    (fun chain_id -> Cannot_fork_testchain chain_id) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.cannot_load_testchain"
    ~title:"Cannot load testchain"
    ~description:"Failed to load the testchain"
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "Failed to load the testchain as it was not found in %s."
        path)
    Data_encoding.(obj1 (req "path" string))
    (function Cannot_load_testchain path -> Some path | _ -> None)
    (fun path -> Cannot_load_testchain path) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.missing_activation_block"
    ~title:"Missing activation block"
    ~description:"Missing activation block while restoring snapshot"
    ~pp:(fun ppf (bh, ph, hm) ->
      Format.fprintf
        ppf
        "Failed to restore snapshot: the expected activation block %a \
         originating the protocol %a was not found for %a."
        Block_hash.pp
        bh
        Protocol_hash.pp
        ph
        History_mode.pp
        hm)
    Data_encoding.(
      obj3
        (req "block_hash" Block_hash.encoding)
        (req "protocol_hash" Protocol_hash.encoding)
        (req "history_mode" History_mode.encoding))
    (function
      | Missing_activation_block (bh, ph, hm) -> Some (bh, ph, hm) | _ -> None)
    (fun (bh, ph, hm) -> Missing_activation_block (bh, ph, hm)) ;
  Error_monad.register_error_kind
    `Temporary
    ~id:"store.inconsistent_protocol_commit_info"
    ~title:"Inconsistent protocol commit info"
    ~description:"Inconsistent protocol commit info while restoring snapshot"
    ~pp:(fun ppf (bh, ph) ->
      Format.fprintf
        ppf
        "Failed to restore snapshot: inconsistent commit info found for \
         transition block %a activating protocol %a."
        Block_hash.pp
        bh
        Protocol_hash.pp
        ph)
    Data_encoding.(
      obj2
        (req "block_hash" Block_hash.encoding)
        (req "protocol_hash" Protocol_hash.encoding))
    (function
      | Inconsistent_protocol_commit_info (bh, ph) -> Some (bh, ph) | _ -> None)
    (fun (bh, ph) -> Inconsistent_protocol_commit_info (bh, ph)) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.failed_to_get_live_blocks"
    ~title:"Fail to get live blocks"
    ~description:"Unable to compute live blocks from a given block."
    ~pp:(fun ppf (hash : Block_hash.t) ->
      Format.fprintf
        ppf
        "Failed to get live blocks from block %a"
        Block_hash.pp
        hash)
    Data_encoding.(obj1 (req "hash" Block_hash.encoding))
    (function Failed_to_get_live_blocks h -> Some h | _ -> None)
    (fun h -> Failed_to_get_live_blocks h) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.target_mismatch"
    ~title:"target mismatch"
    ~description:"Target is reached but it is not a head's ancestor."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Target is reached but it is not a head's ancestor.")
    Data_encoding.empty
    (function Target_mismatch -> Some () | _ -> None)
    (fun () -> Target_mismatch) ;
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.bad_head_invariant"
    ~title:"Bad head invariant"
    ~description:"Bad invariant during Store.set_head"
    ~pp:(fun ppf () -> Format.fprintf ppf "Bad invariant during Store.set_head")
    Data_encoding.empty
    (function Bad_head_invariant -> Some () | _ -> None)
    (fun () -> Bad_head_invariant)

(* Consistency errors: *)
type error +=
  | Unexpected_missing_block of {block_name : string}
  | Unexpected_missing_block_metadata of {block_name : string}
  | Unexpected_missing_activation_block of {
      block : Block_hash.t;
      protocol : Protocol_hash.t;
    }
  | Unexpected_missing_protocol of {protocol_level : int}
  | Inconsistent_genesis of {expected : Block_hash.t; got : Block_hash.t}
  | Inconsistent_cementing_highwatermark of {
      highest_cemented_level : Int32.t;
      cementing_highwatermark : Int32.t;
    }
  | Inconsistent_history_mode of History_mode.t
  | Bad_ordering_invariant of {
      genesis : Int32.t;
      caboose : Int32.t;
      savepoint : Int32.t;
      cementing_highwatermark : Int32.t option;
      checkpoint : Int32.t;
      head : Int32.t;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"store.unexpected_missing_block"
    ~title:"Unexpected missing block"
    ~description:"A block is unexpectedly missing from the store."
    ~pp:(fun ppf block_name ->
      Format.fprintf
        ppf
        "The block '%s' is unexpectedly missing from the store."
        block_name)
    Data_encoding.(obj1 (req "missing_block" string))
    (function
      | Unexpected_missing_block {block_name} -> Some block_name | _ -> None)
    (fun block_name -> Unexpected_missing_block {block_name}) ;
  register_error_kind
    `Permanent
    ~id:"store.unexpected_missing_block_metadata"
    ~title:"Unexpected missing block metadata"
    ~description:"A block's metadata is unexpectedly missing from the store."
    ~pp:(fun ppf block_name ->
      Format.fprintf
        ppf
        "The block '%s' metadata is unexpectedly missing from the store."
        block_name)
    Data_encoding.(obj1 (req "missing_block_metadata" string))
    (function
      | Unexpected_missing_block_metadata {block_name} -> Some block_name
      | _ -> None)
    (fun block_name -> Unexpected_missing_block_metadata {block_name}) ;
  register_error_kind
    `Permanent
    ~id:"store.unexpected_missing_activation_block"
    ~title:"Unexpected missing activaiton block"
    ~description:"An activation block is unexpectedly missing from the store."
    ~pp:(fun ppf (block, proto) ->
      Format.fprintf
        ppf
        "The block %a activating protocol %a is unexpectedly missing from the \
         store."
        Block_hash.pp
        block
        Protocol_hash.pp
        proto)
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (req "protocol" Protocol_hash.encoding))
    (function
      | Unexpected_missing_activation_block {block; protocol} ->
          Some (block, protocol)
      | _ -> None)
    (fun (block, protocol) ->
      Unexpected_missing_activation_block {block; protocol}) ;
  register_error_kind
    `Permanent
    ~id:"store.unexpected_missing_protocol"
    ~title:"Unexpected missing protocol"
    ~description:"A protocol is unexpectedly missing from the store."
    ~pp:(fun ppf protocol_level ->
      Format.fprintf
        ppf
        "The protocol %d is unexpectedly missing from the store."
        protocol_level)
    Data_encoding.(obj1 (req "protocol_level" int31))
    (function
      | Unexpected_missing_protocol {protocol_level} -> Some protocol_level
      | _ -> None)
    (fun protocol_level -> Unexpected_missing_protocol {protocol_level}) ;
  register_error_kind
    `Permanent
    ~id:"store.inconsistent_genesis"
    ~title:"Inconsistent genesis"
    ~description:"The given genesis block is inconsistent with the store."
    ~pp:(fun ppf (expected, got) ->
      Format.fprintf
        ppf
        "The genesis (%a) found in the store is not the one expected (%a)."
        Block_hash.pp
        got
        Block_hash.pp
        expected)
    Data_encoding.(
      obj2 (req "expected" Block_hash.encoding) (req "got" Block_hash.encoding))
    (function
      | Inconsistent_genesis {expected; got} -> Some (expected, got) | _ -> None)
    (fun (expected, got) -> Inconsistent_genesis {expected; got}) ;
  register_error_kind
    `Permanent
    ~id:"store.inconsistent_cementing_highwatermark"
    ~title:"Inconsistent cementing highwatermark"
    ~description:
      "The stored cementing highwatermark is inconsistent with the store."
    ~pp:(fun ppf (highest_cemented_level, cementing_highwatermark) ->
      Format.fprintf
        ppf
        "The stored cemented highwatermark (level: %ld) differs from the \
         highest cemented block (level: %ld)"
        cementing_highwatermark
        highest_cemented_level)
    Data_encoding.(
      obj2
        (req "highest_cemented_level" int32)
        (req "cementing_highwatermark" int32))
    (function
      | Inconsistent_cementing_highwatermark
          {highest_cemented_level; cementing_highwatermark} ->
          Some (highest_cemented_level, cementing_highwatermark)
      | _ -> None)
    (fun (highest_cemented_level, cementing_highwatermark) ->
      Inconsistent_cementing_highwatermark
        {highest_cemented_level; cementing_highwatermark}) ;
  register_error_kind
    `Permanent
    ~id:"store.inconsistent_history_mode"
    ~title:"Inconsistent history mode"
    ~description:"The history mode does not correspond to the store."
    ~pp:(fun ppf history_mode ->
      Format.fprintf
        ppf
        "the history mode (%a) is not compatible with the store"
        History_mode.pp
        history_mode)
    Data_encoding.(obj1 (req "history_mode" History_mode.encoding))
    (function Inconsistent_history_mode hm -> Some hm | _ -> None)
    (fun hm -> Inconsistent_history_mode hm) ;
  register_error_kind
    `Permanent
    ~id:"store.bad_ordering_invariant"
    ~title:"Bad ordering invariant"
    ~description:"The ordering invariant does not hold"
    ~pp:
      (fun ppf
           ( genesis,
             caboose,
             savepoint,
             cementing_highwatermark,
             checkpoint,
             head ) ->
      Format.fprintf
        ppf
        "Invariant '%ld (genesis) ≤ %ld (caboose) ≤ %ld (savepoint) ≤ %a \
         [cementing_highwatermark] ≤\n\
        \ %ld (checkpoint) ≤ all(alternate_heads ∪ (%ld) current_head)' does \
         not hold"
        genesis
        caboose
        savepoint
        (Format.pp_print_option
           ~none:(fun ppf () -> Format.fprintf ppf "(n/a)")
           (fun ppf -> Format.fprintf ppf "%ld"))
        cementing_highwatermark
        checkpoint
        head)
    Data_encoding.(
      obj6
        (req "genesis" int32)
        (req "caboose" int32)
        (req "savepoint" int32)
        (req "cementing_highwatermark" (option int32))
        (req "checkpoint" int32)
        (req "head" int32))
    (function
      | Bad_ordering_invariant
          {
            genesis;
            caboose;
            savepoint;
            cementing_highwatermark;
            checkpoint;
            head;
          } ->
          Some
            ( genesis,
              caboose,
              savepoint,
              cementing_highwatermark,
              checkpoint,
              head )
      | _ -> None)
    (fun (genesis, caboose, savepoint, cementing_highwatermark, checkpoint, head)
         ->
      Bad_ordering_invariant
        {genesis; caboose; savepoint; cementing_highwatermark; checkpoint; head})

type corruption_kind =
  | Inferred_head of Block_hash.t * Int32.t
  | Cannot_find_floating_savepoint
  | Cannot_find_savepoint_candidate
  | Cannot_find_floating_caboose
  | Cannot_find_caboose_candidate
  | Cannot_find_block_with_metadata
  | Cannot_find_activation_block of int
  | Missing_genesis

let corruption_kind_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Inferred_head"
        (obj2 (req "hash" Block_hash.encoding) (req "level" int32))
        (function
          | Inferred_head (hash, level) -> Some (hash, level) | _ -> None)
        (fun (hash, level) -> Inferred_head (hash, level));
      case
        (Tag 1)
        ~title:"Cannot_find_floating_savepoint"
        empty
        (function Cannot_find_floating_savepoint -> Some () | _ -> None)
        (fun () -> Cannot_find_floating_savepoint);
      case
        (Tag 2)
        ~title:"Cannot_find_savepoint_candidate"
        empty
        (function Cannot_find_savepoint_candidate -> Some () | _ -> None)
        (fun () -> Cannot_find_savepoint_candidate);
      case
        (Tag 3)
        ~title:"Cannot_find_floating_caboose"
        empty
        (function Cannot_find_floating_caboose -> Some () | _ -> None)
        (fun () -> Cannot_find_floating_caboose);
      case
        (Tag 4)
        ~title:"Cannot_find_caboose_candidate"
        empty
        (function Cannot_find_caboose_candidate -> Some () | _ -> None)
        (fun () -> Cannot_find_caboose_candidate);
      case
        (Tag 5)
        ~title:"Cannot_find_block_with_metadata"
        empty
        (function Cannot_find_block_with_metadata -> Some () | _ -> None)
        (fun () -> Cannot_find_block_with_metadata);
      case
        (Tag 6)
        ~title:"Cannot_find_activation_block"
        (obj1 (req "proto_level" int31))
        (function
          | Cannot_find_activation_block proto_level -> Some proto_level
          | _ -> None)
        (fun proto_level -> Cannot_find_activation_block proto_level);
      case
        (Tag 7)
        ~title:"Missing_genesis"
        empty
        (function Missing_genesis -> Some () | _ -> None)
        (fun () -> Missing_genesis);
    ]

let pp_corruption_kind ppf = function
  | Inferred_head (hash, level) ->
      Format.fprintf
        ppf
        "inferred head (%a, %ld) must have metadata"
        Block_hash.pp
        hash
        level
  | Cannot_find_floating_savepoint ->
      Format.fprintf
        ppf
        "failed to find a valid savepoint in the floating store. No block with \
         metadata were found"
  | Cannot_find_savepoint_candidate ->
      Format.fprintf ppf "failed to find the savepoint candidate in the store"
  | Cannot_find_floating_caboose ->
      Format.fprintf ppf "failed to find a valid caboose in the floating store"
  | Cannot_find_caboose_candidate ->
      Format.fprintf ppf "failed to find the caboose candidate in the store"
  | Cannot_find_block_with_metadata ->
      Format.fprintf
        ppf
        "cannot find block with metadata in the store. At least the head must \
         have metadata"
  | Cannot_find_activation_block proto_level ->
      Format.fprintf
        ppf
        "failed to find a valid activation block for protocol %d"
        proto_level
  | Missing_genesis ->
      Format.fprintf ppf "the genesis block is not available in the store"

type error += Corrupted_store of corruption_kind

let () =
  Error_monad.register_error_kind
    `Permanent
    ~id:"store.corrupted_store"
    ~title:"Corrupted store"
    ~description:"The store is corrupted"
    ~pp:(fun ppf kind ->
      Format.fprintf
        ppf
        "The store is corrupted irremediably: %a."
        pp_corruption_kind
        kind)
    Data_encoding.(obj1 (req "kind" corruption_kind_encoding))
    (function Corrupted_store k -> Some k | _ -> None)
    (fun k -> Corrupted_store k)

(* Storage upgrade errors *)
type error +=
  | V_3_0_upgrade_missing_floating_block of {
      block_hash : Block_hash.t;
      block_level : Int32.t;
      floating_kind : string;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"block_store.v_3_0_upgrade_missing_floating_block"
    ~title:"V.3.0 upgrade missing floating block"
    ~description:"Failed to upgrade the floating store"
    ~pp:(fun ppf (block_hash, block_level, floating_kind) ->
      Format.fprintf
        ppf
        "Failed to upgrade block %a (level %ld) for %s floating store: block \
         not found in the index."
        Block_hash.pp
        block_hash
        block_level
        floating_kind)
    Data_encoding.(
      obj3
        (req "block_hash" Block_hash.encoding)
        (req "block_level" int32)
        (req "floating_kind" string))
    (function
      | V_3_0_upgrade_missing_floating_block
          {block_hash; block_level; floating_kind} ->
          Some (block_hash, block_level, floating_kind)
      | _ -> None)
    (fun (block_hash, block_level, floating_kind) ->
      V_3_0_upgrade_missing_floating_block
        {block_hash; block_level; floating_kind})
