(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

let get_boot_sector (module Plugin : Protocol_plugin_sig.PARTIAL)
    genesis_block_hash (node_ctxt : _ Node_context.t) =
  let open Lwt_result_syntax in
  match node_ctxt.config.boot_sector_file with
  | None -> Plugin.Layer1_helpers.get_boot_sector genesis_block_hash node_ctxt
  | Some boot_sector_file ->
      let*! boot_sector = Lwt_utils_unix.read_file boot_sector_file in
      let*? boot_sector =
        Option.value_e
          ~error:
            [
              Rollup_node_errors.Unparsable_boot_sector {path = boot_sector_file};
            ]
          (Plugin.Pvm.parse_boot_sector node_ctxt.kind boot_sector)
      in
      return boot_sector

(** Apply potential unsafe patches to the PVM state. *)
let apply_unsafe_patches (module Plugin : Protocol_plugin_sig.PARTIAL)
    ~genesis_block_hash (node_ctxt : _ Node_context.t) state =
  let open Lwt_result_syntax in
  match
    (node_ctxt.unsafe_patches
      :> (Pvm_patches.unsafe_patch * Pvm_patches.kind) list)
  with
  | [] -> return_unit
  | patches ->
      let has_user_provided_patches =
        List.exists
          (function
            | _, Pvm_patches.User_provided -> true | _, Hardcoded -> false)
          patches
      in
      let*? () =
        error_when
          (has_user_provided_patches
          && not node_ctxt.config.apply_unsafe_patches)
          (Rollup_node_errors.Needs_apply_unsafe_flag (List.map fst patches))
      in
      let* whitelist =
        Plugin.Layer1_helpers.find_whitelist
          node_ctxt.cctxt
          ~block:genesis_block_hash
          node_ctxt.config.sc_rollup_address
      in
      let private_rollup = whitelist <> None in
      let*? () =
        error_unless
          private_rollup
          Rollup_node_errors.Cannot_patch_pvm_of_public_rollup
      in
      List.iter_es
        (fun (patch, _kind) ->
          let*! () = Interpreter_event.patching_genesis_state patch in
          Plugin.Pvm.Unsafe.apply_patch node_ctxt.kind state patch)
        patches

type 'a patched = Patched constraint 'a = [< `Read | `Write > `Read]

type 'a unpatched = Unpatched constraint 'a = [< `Read | `Write > `Read]

type ('a, 'b) both =
  | Both
  constraint 'a = [< `Read | `Write > `Read]
  constraint 'b = [< `Read | `Write > `Read]

type _ genesis_state =
  | Patched : 'perm Context.pvmstate -> 'perm patched genesis_state
  | Unpatched : 'perm Context.pvmstate -> 'perm unpatched genesis_state
  | Both : {
      patched : 'perm_patched Context.pvmstate;
      original : 'perm_orig Context.pvmstate;
    }
      -> ('perm_orig, 'perm_patched) both genesis_state

type _ genesis_state_mode =
  | Patched : 'a Access_mode.t -> 'a patched genesis_state_mode
  | Unpatched : 'a Access_mode.t -> 'a unpatched genesis_state_mode
  | Both :
      'a Access_mode.t * 'b Access_mode.t
      -> ('a, 'b) both genesis_state_mode

let genesis_state (type m) (mode : m genesis_state_mode)
    (module Plugin : Protocol_plugin_sig.PARTIAL) ?genesis_block ?empty
    node_ctxt : m genesis_state tzresult Lwt.t =
  let open Lwt_result_syntax in
  let* genesis_block_hash =
    match genesis_block with
    | Some b -> return b
    | None -> Node_context.hash_of_level node_ctxt node_ctxt.genesis_info.level
  in
  let* boot_sector =
    get_boot_sector (module Plugin) genesis_block_hash node_ctxt
  in
  (* Genesis state *)
  let state =
    match empty with
    | Some s -> s
    | None -> Context.PVMState.empty node_ctxt.context
  in
  let*! () = Plugin.Pvm.set_initial_state node_ctxt.kind ~empty:state in
  (* Unpatched genesis state *)
  let*! () = Plugin.Pvm.install_boot_sector node_ctxt.kind state boot_sector in
  match mode with
  | Unpatched access_mode ->
      let state = Context.PVMState.maybe_readonly access_mode state in
      return (Unpatched state : m genesis_state)
  | Patched access_mode ->
      (* Patch genesis state *)
      let* () =
        apply_unsafe_patches (module Plugin) node_ctxt ~genesis_block_hash state
      in
      let state = Context.PVMState.maybe_readonly access_mode state in
      return (Patched state : m genesis_state)
  | Both (access_mode_orig, access_mode_patched) ->
      (* Copy and patch genesis state *)
      let original =
        Context.PVMState.copy state
        |> Context.PVMState.maybe_readonly access_mode_orig
      in
      let* () =
        apply_unsafe_patches (module Plugin) node_ctxt ~genesis_block_hash state
      in
      let state = Context.PVMState.maybe_readonly access_mode_patched state in
      return (Both {patched = state; original} : m genesis_state)

let state_of_head plugin node_ctxt ctxt Layer1.{hash; level} =
  let open Lwt_result_syntax in
  let*! state = Context.PVMState.find ctxt in
  match state with
  | None ->
      let genesis_level = node_ctxt.Node_context.genesis_info.level in
      if level = genesis_level then
        let+ (Patched state) =
          genesis_state
            (Patched (Context.access_mode_state ctxt))
            plugin
            ~genesis_block:hash
            node_ctxt
        in
        state
      else tzfail (Rollup_node_errors.Missing_PVM_state (hash, level))
  | Some state -> return state

(** [transition_pvm plugin node_ctxt ctxt predecessor head] runs a PVM at the
    previous state from block [predecessor] by consuming as many messages as
    possible from block [head]. *)
let transition_pvm (module Plugin : Protocol_plugin_sig.PARTIAL) node_ctxt ctxt
    predecessor Layer1.{hash = _; _} inbox_messages =
  let open Lwt_result_syntax in
  (* Retrieve the previous PVM state from store. *)
  let* state = state_of_head (module Plugin) node_ctxt ctxt predecessor in
  let*! initial_tick = Plugin.Pvm.get_tick node_ctxt.kind state in
  let* {
         state_info = {state_hash; inbox_level; tick; _};
         num_messages;
         num_ticks;
       } =
    Octez_telemetry.Trace.with_tzresult ~service_name:"Pvm" "eval_block"
    @@ fun scope ->
    Node_context.reset_kernel_tracing scope ;
    Plugin.Pvm.Fueled.Free.eval_block_inbox
      ~fuel:(Fuel.Free.of_ticks 0L)
      node_ctxt
      inbox_messages
      state
  in
  let*! () = Context.PVMState.set ctxt state in
  (* Produce events. *)
  let*! () =
    Interpreter_event.transitioned_pvm inbox_level state_hash tick num_messages
  in
  return (num_messages, Z.to_int64 num_ticks, initial_tick)

(** [process_head plugin node_ctxt ctxt ~predecessor head inbox_and_messages] runs the PVM for the given
    head. *)
let process_head plugin (node_ctxt : _ Node_context.t) ctxt
    ~(predecessor : Layer1.head) (head : Layer1.head) inbox_and_messages =
  let open Lwt_result_syntax in
  let first_inbox_level = node_ctxt.genesis_info.level |> Int32.succ in
  if head.level >= first_inbox_level then
    transition_pvm plugin node_ctxt ctxt predecessor head inbox_and_messages
  else if head.level = node_ctxt.genesis_info.level then
    let*! empty_state = Context.PVMState.find ctxt in
    let* (Patched state) =
      genesis_state
        (Patched (Context.access_mode_state ctxt))
        plugin
        ~genesis_block:head.hash
        ?empty:empty_state
        node_ctxt
    in
    let*! () = Context.PVMState.set ctxt state in
    return (0, 0L, Z.zero)
  else return (0, 0L, Z.zero)

(** Returns the starting evaluation before the evaluation of the block. It
    contains the PVM state at the end of the execution of the previous block and
    the messages the block ([remaining_messages]). *)
let start_state_of_block plugin node_ctxt (block : Sc_rollup_block.t) =
  let open Lwt_result_syntax in
  let pred_level = Int32.pred block.header.level in
  let* ctxt =
    Node_context.checkout_context node_ctxt block.header.predecessor
  in
  let* state =
    state_of_head
      plugin
      node_ctxt
      ctxt
      Layer1.{hash = block.header.predecessor; level = pred_level}
  in
  let* inbox = Node_context.get_inbox node_ctxt block.header.inbox_hash in
  let inbox_level = Octez_smart_rollup.Inbox.inbox_level inbox in
  let module Plugin = (val plugin) in
  let*! tick = Plugin.Pvm.get_tick node_ctxt.kind state in
  let*! state_hash = Plugin.Pvm.state_hash node_ctxt.kind state in
  let* messages =
    Node_context.get_messages node_ctxt block.header.inbox_witness
  in
  let info =
    Pvm_plugin_sig.
      {
        state_hash;
        inbox_level;
        tick;
        message_counter_offset = 0;
        remaining_fuel = Fuel.Accounted.of_ticks 0L;
        remaining_messages = messages;
      }
  in
  return Pvm_plugin_sig.{state; info}

(** [run_for_ticks plugin node_ctxt start_state tick_distance] starts the
    evaluation of messages in the [start_state] for at most [tick_distance]. *)
let run_to_tick (module Plugin : Protocol_plugin_sig.PARTIAL) node_ctxt
    start_state tick =
  let open Lwt_result_syntax in
  let tick_distance =
    Z.sub tick start_state.Pvm_plugin_sig.info.tick |> Z.to_int64
  in
  let+ eval_result =
    Octez_telemetry.Trace.with_tzresult ~service_name:"Pvm" "eval_messages"
    @@ fun _ ->
    (* Set remaining fuel to needed tick for evaluation *)
    let info =
      {
        start_state.info with
        remaining_fuel = Fuel.Accounted.of_ticks tick_distance;
      }
    in
    Plugin.Pvm.Fueled.Accounted.eval_messages node_ctxt {start_state with info}
  in
  eval_result.state_info

let state_of_tick_aux plugin node_ctxt ~start_state (event : Sc_rollup_block.t)
    tick =
  let open Lwt_result_syntax in
  let* start_state =
    match start_state with
    | Some start_state
      when start_state.Pvm_plugin_sig.info.inbox_level = event.header.level ->
        return start_state
    | _ ->
        (* Recompute start state on level change or if we don't have a
           starting state on hand. *)
        start_state_of_block plugin node_ctxt event
  in
  let state = start_state.state in
  (* TODO: #3384
     We should test that we always have enough blocks to find the tick
     because [state_of_tick] is a critical function. *)
  let+ run_info = run_to_tick plugin node_ctxt start_state tick in
  Pvm_plugin_sig.{state; info = run_info}

(** This function memoizes [f tick], but puts an immutable state copy in the
    cache, and returns a mutable version. Either the one obtained from [f]
    directly or a copy of the one in the cache. *)
let memo_tick_state cache tick f =
  match Pvm_plugin_sig.Tick_state_cache.bind cache tick Lwt.return with
  | Some p_imm -> Lwt_result.map Pvm_plugin_sig.to_mut_eval_state p_imm
  | None ->
      let p_mut = f tick in
      let p_imm = Lwt_result.map Pvm_plugin_sig.to_imm_eval_state p_mut in
      Pvm_plugin_sig.Tick_state_cache.put cache tick p_imm ;
      p_mut

(* Global cache to share states between different parallel refutation games. *)
let global_tick_state_cache =
  Pvm_plugin_sig.make_state_cache 64 (* size of 2 dissections *)

(* Memoized version of [state_of_tick_aux] using global cache. *)
let global_memo_state_of_tick_aux plugin node_ctxt ~start_state
    (event : Sc_rollup_block.t) tick =
  memo_tick_state global_tick_state_cache tick
  @@ state_of_tick_aux plugin node_ctxt ~start_state event

(* Memoized version of [state_of_tick_aux] using both global and local caches. *)
let memo_state_of_tick_aux plugin node_ctxt state_cache ~start_state
    (event : Sc_rollup_block.t) tick =
  memo_tick_state state_cache tick
  @@ global_memo_state_of_tick_aux plugin node_ctxt ~start_state event

(** [state_of_tick plugin node_ctxt ?start_state ~tick level] returns [Some
    end_state] for [tick] if [tick] happened before
    [level]. Otherwise, returns [None].*)
let state_of_tick plugin node_ctxt state_cache ?start_state ~tick level =
  let open Lwt_result_syntax in
  let min_level =
    match start_state with
    | None -> None
    | Some s -> Some s.Pvm_plugin_sig.info.inbox_level
  in
  let* event =
    Node_context.block_with_tick node_ctxt ?min_level ~max_level:level tick
  in
  match event with
  | None -> return_none
  | Some event ->
      assert (event.header.level <= level) ;
      let* result_state =
        if Node_context.is_loser node_ctxt then
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/5253
             The failures/loser mode does not work properly when restarting
             from intermediate states. *)
          state_of_tick_aux plugin node_ctxt ~start_state:None event tick
        else
          memo_state_of_tick_aux
            plugin
            node_ctxt
            state_cache
            ~start_state
            event
            tick
      in
      return_some result_state
