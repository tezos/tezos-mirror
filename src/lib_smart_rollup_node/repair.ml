(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let group =
  {
    Tezos_clic.name = "smart_rollup.node.repair";
    title = "Commands to repair the smart rollup node.";
  }

module Cli = struct
  include Cli

  include Binary_dependent_args (struct
    let binary_name = "smart rollup node"
  end)
end

type fix_action =
  | Nothing
  | Remove of Sc_rollup_block.t
  | Add of Commitment.t * Sc_rollup_block.t
  | Patch of Sc_rollup_block.t

(** Recompute the commitment for [level] and store it if necessary. Commitments
    that were incorrectly computed before are removed. Pre-condition:
    commitments for inbox levels before [level] must be correct.  *)
let fix_commitment (node_ctxt : Node_context.rw) level =
  let open Lwt_result_syntax in
  let* l2_block = Node_context.get_l2_block_by_level node_ctxt level in
  let stored_commitment_hash = l2_block.header.commitment_hash in
  let* plugin = Protocol_plugins.proto_plugin_for_level node_ctxt level in
  let (module Plugin) = plugin in
  let* constants =
    let constants_level = Int32.max level node_ctxt.genesis_info.level in
    Protocol_plugins.get_constants_of_protocol
      node_ctxt
      ~level:constants_level
      Plugin.protocol
  in
  let protocol =
    {
      Node_context.hash = Plugin.protocol;
      proto_level = (Reference.get node_ctxt.current_protocol).proto_level;
      (* leave as is because unimportant for the fix *)
      constants;
    }
  in
  let* previous_commitment_hash =
    if level = node_ctxt.genesis_info.level then
      (* Previous commitment for rollup genesis is itself. *)
      return node_ctxt.genesis_info.commitment_hash
    else
      let+ pred =
        Node_context.get_l2_block node_ctxt l2_block.header.predecessor
      in
      Sc_rollup_block.most_recent_commitment pred.header
  in
  Reference.set node_ctxt.current_protocol protocol ;
  let* ctxt =
    Node_context.checkout_context node_ctxt l2_block.header.block_hash
  in
  let* new_commitment =
    Publisher.create_commitment_if_necessary
      plugin
      node_ctxt
      ~predecessor:l2_block.header.predecessor
      level
      ctxt
  in
  let l2_block, action =
    if
      Commitment.Hash.(
        previous_commitment_hash = l2_block.header.previous_commitment_hash)
    then (l2_block, Nothing)
    else
      let l2_block =
        {l2_block with header = {l2_block.header with previous_commitment_hash}}
      in
      (l2_block, Patch l2_block)
  in
  let action =
    match (new_commitment, stored_commitment_hash) with
    | None, None -> action
    | Some new_commitment, _ -> (
        let new_commitment_hash = Commitment.hash new_commitment in
        match stored_commitment_hash with
        | Some stored_commitment_hash
          when Commitment.Hash.(new_commitment_hash = stored_commitment_hash) ->
            action
        | _ ->
            let l2_block =
              {
                l2_block with
                header =
                  {
                    l2_block.header with
                    commitment_hash = Some new_commitment_hash;
                  };
              }
            in
            Add (new_commitment, l2_block))
    | None, Some _stored_commitment_hash ->
        let l2_block =
          {l2_block with header = {l2_block.header with commitment_hash = None}}
        in
        Remove l2_block
  in
  let* () =
    match action with
    | Nothing -> return_unit
    | Patch l2_block -> Node_context.save_l2_block node_ctxt l2_block
    | Remove l2_block ->
        Format.printf "Removing incorrect commitment for level %ld@." level ;
        Node_context.save_l2_block node_ctxt l2_block
    | Add (new_commitment, l2_block) ->
        let* new_commitment_hash =
          Node_context.save_commitment node_ctxt new_commitment
        in
        Format.printf
          "Registering new commitment %a for level %ld@."
          Commitment.Hash.pp
          new_commitment_hash
          new_commitment.inbox_level ;
        Node_context.save_l2_block node_ctxt l2_block
  in
  return action

(** Fixes commitments for inbox levels between [first_level] and the current
    head. *)
let fix_commitments node_ctxt first_level =
  let open Lwt_result_syntax in
  Format.printf "Fixing commitments starting at level %ld@." first_level ;
  let* l2_head = Node_context.last_processed_head_opt node_ctxt in
  let l2_head = WithExceptions.Option.get ~loc:__LOC__ l2_head in
  let*? () =
    if first_level > l2_head.header.level then
      error_with
        "First level to fix commitments is %ld but head is at %ld"
        first_level
        l2_head.header.level
    else Ok ()
  in
  let levels =
    Stdlib.List.init
      (Int32.to_int (Int32.sub l2_head.header.level first_level) + 1)
      (fun x -> Int32.add (Int32.of_int x) first_level)
  in
  let* (removed, added, patched), last =
    List.fold_left_es
      (fun ((removed, added, patched), _last) level ->
        let+ action = fix_commitment node_ctxt level in
        let counters =
          match action with
          | Nothing -> (removed, added, patched)
          | Remove _ -> (removed + 1, added, patched)
          | Add _ -> (removed, added + 1, patched)
          | Patch _ -> (removed, added, patched + 1)
        in
        (counters, action))
      ((0, 0, 0), Nothing)
      levels
  in
  let* () =
    match last with
    | (Add (_, b) | Remove b | Patch b)
      when b.header.level = l2_head.header.level ->
        Node_context.set_l2_head node_ctxt b
    | _ -> return_unit
  in
  Format.printf
    "Completed!\n\
     Removed commitments: %3d\n\
     Added commitments: %5d\n\
     Patched blocks: %8d@."
    removed
    added
    patched ;
  return_unit

(** Fixes commitments for [protocol] (and the following ones) up to the current
    head. *)
let fix_commitments_for_protocol node_ctxt protocol =
  let open Lwt_result_syntax in
  Format.printf "Fixing commitments for protocol %a@." Protocol_hash.pp protocol ;
  let* act_level = Node_context.protocol_activation_level node_ctxt protocol in
  let first_level =
    match act_level with
    | First_known l -> l
    | Activation_level l -> Int32.succ l
  in
  fix_commitments node_ctxt first_level

let command_fix_commitments_for_protocol cctxt ~data_dir protocol =
  Snapshots.with_modify_data_dir cctxt ~data_dir ~apply_unsafe_patches:false
  @@ fun node_ctxt ~head:_ -> fix_commitments_for_protocol node_ctxt protocol

let repair_commitments_command =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Repair commitments of L2 chain for a given protocol."
    (args1 Cli.data_dir_arg)
    (prefixes ["repair"; "commitments"; "for"]
    @@ Cli.protocol_hash_param @@ stop)
    (fun data_dir protocol cctxt ->
      command_fix_commitments_for_protocol cctxt ~data_dir protocol)

let recompute_outbox_messages node_ctxt first_level =
  let open Lwt_result_syntax in
  let* l2_head = Node_context.last_processed_head_opt node_ctxt in
  let l2_head = WithExceptions.Option.get ~loc:__LOC__ l2_head in
  let* () =
    when_ (first_level > l2_head.header.level) @@ fun () ->
    failwith
      "First level to recompute outbox messages is %ld but head is at %ld"
      first_level
      l2_head.header.level
  in
  let levels =
    Stdlib.List.init
      (Int32.to_int (Int32.sub l2_head.header.level first_level) + 1)
      (fun x -> Int32.add (Int32.of_int x) first_level)
  in
  let lcc = Reference.get node_ctxt.lcc in
  List.iter_es
    (fun level ->
      let* plugin = Protocol_plugins.proto_plugin_for_level node_ctxt level in
      let (module Plugin) = plugin in
      let ctxt_level = if level <= lcc.level then lcc.level else level in
      let* ctxt_block =
        Node_context.get_l2_block_by_level node_ctxt ctxt_level
      in
      let* ctxt =
        Node_context.checkout_context node_ctxt ctxt_block.header.block_hash
      in
      let*! outbox_messages =
        let*! pvm_state = Context.PVMState.find ctxt in
        match pvm_state with
        | None -> Lwt.return_nil
        | Some pvm_state ->
            Plugin.Pvm.get_outbox_messages
              node_ctxt
              pvm_state
              ~outbox_level:level
      in
      match outbox_messages with
      | [] -> return_unit
      | _ ->
          Format.eprintf
            "%ld : %d outbox messages@."
            level
            (List.length outbox_messages) ;
          let indexes = List.map fst outbox_messages in
          Node_context.register_outbox_messages
            node_ctxt
            ~outbox_level:level
            ~indexes)
    levels

let command_recompute_outbox_messages cctxt ~data_dir level =
  Snapshots.with_modify_data_dir cctxt ~data_dir ~apply_unsafe_patches:false
  @@ fun node_ctxt ~head:_ -> recompute_outbox_messages node_ctxt level

let recompute_outbox_messages_command =
  let open Tezos_clic in
  command
    ~group
    ~desc:
      "Recompute outbox messages from a given level up to the current head. It \
       does not overwrite already stored outbox messages."
    (args1 Cli.data_dir_arg)
    (prefixes ["recompute"; "outbox"; "messages"; "from"]
    @@ Cli.level_param @@ stop)
    (fun data_dir level cctxt ->
      command_recompute_outbox_messages cctxt ~data_dir level)

(** Commands exported by the [Repair] module. *)
let commands = [repair_commitments_command; recompute_outbox_messages_command]
