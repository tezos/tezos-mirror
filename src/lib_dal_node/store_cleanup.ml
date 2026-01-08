(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This function removes old data starting from [last_processed_level -
   storage_period] to [target_level - storage_period], where [storage_period] is
   the period for which the DAL node stores data related to attested slots and
   [target_level] is the level at which we connect the P2P and switch to
   processing blocks in sync with the L1. [target_level] is set to [head_level -
   3]. It also inserts skip list cells if needed in the period [head_level -
   storage_level].

   FIXME: https://gitlab.com/tezos/tezos/-/issues/7429
   We don't call [may_add_plugin], so there is a chance the plugin changes
   and we don't detect it if this code starts running just before the migration
   level, and the head changes meanwhile to be above the migration level.

   TODO: https://gitlab.com/tezos/tezos/-/issues/7779
   Improve the runtime of this function. It may be better to do the clean-up and
   the "catch-up" (that is, updating of the skip list store) separately. *)
let clean_up_store_and_catch_up_for_refutation_support ctxt cctxt
    ~last_processed_level ~first_seen_level head_level proto_parameters =
  let open Lwt_result_syntax in
  let store_skip_list_cells ~level =
    let*? (module Plugin) =
      Node_context.get_plugin_for_level ctxt ~level:(Int32.pred level)
    in
    let*? dal_constants =
      Node_context.get_proto_parameters ctxt ~level:(`Level level)
    in
    Block_handler.fetch_and_store_skip_list_cells
      ctxt
      cctxt
      dal_constants
      ~attested_level:level
      (module Plugin : Dal_plugin.T)
  in
  let store = Node_context.get_store ctxt in
  let last_processed_level_store = Store.last_processed_level store in
  (* [target_level] identifies the level wrt to head level at which we want to
     start the P2P and process blocks as usual. It's set to [head_level - 3]
     because the first level the DAL node should process should be a final
     one. *)
  let target_level head_level = Int32.(sub head_level 3l) in
  let first_level_for_skip_list_storage period level =
    (* Note that behind this first level we do not have the plugin. *)
    Int32.(sub level (of_int period))
  in
  let should_store_skip_list_cells ~head_level =
    let profile_ctxt = Node_context.get_profile_ctxt ctxt in
    let period =
      Profile_manager.get_storage_period
        profile_ctxt
        proto_parameters
        ~head_level
        ~first_seen_level
      + History_check.skip_list_offset proto_parameters
    in
    let first_level = first_level_for_skip_list_storage period head_level in
    fun ~level -> level >= first_level
  in
  let rec do_clean_up last_processed_level head_level =
    let last_level = target_level head_level in
    let should_store_skip_list_cells =
      should_store_skip_list_cells ~head_level
    in
    let rec clean_up_at_level level =
      if level > last_level then return_unit
      else
        let* () =
          Block_handler.remove_old_level_stored_data proto_parameters ctxt level
        in
        let* () =
          if should_store_skip_list_cells ~level then
            store_skip_list_cells ~level
          else return_unit
        in
        let* () =
          Store.Last_processed_level.save last_processed_level_store level
        in
        let*! () =
          if Int32.to_int level mod 1000 = 0 then
            Event.emit_catching_up ~current_level:level
          else Lwt.return_unit
        in
        clean_up_at_level (Int32.succ level)
    in
    (* Clean up from [last_processed_level] to [last_level]. *)
    let* () = clean_up_at_level (Int32.succ last_processed_level) in
    (* As this iteration may be slow, the head level might have advanced in the
       meanwhile. *)
    let* header =
      Shell_services.Blocks.Header.shell_header cctxt ~block:(`Head 0) ()
    in
    let new_head_level = header.Block_header.level in

    L1_crawler_status.catching_up_or_synced_status
      ~head_level:new_head_level
      ~last_processed_level:last_level
    |> Node_context.set_l1_crawler_status ctxt ;

    if new_head_level > head_level then do_clean_up last_level new_head_level
    else
      let*! () = Event.emit_end_catchup () in
      return_unit
  in
  let start_level = Int32.succ last_processed_level in
  let end_level = target_level head_level in
  let levels_to_clean_up = Int32.(succ @@ sub end_level start_level) in
  if levels_to_clean_up > 0l then
    let*! () =
      Event.emit_start_catchup ~start_level ~end_level ~levels_to_clean_up
    in
    do_clean_up last_processed_level head_level
  else return_unit

let clean_up_store_and_catch_up_for_no_refutation_support ctxt
    ~last_processed_level head_level proto_parameters =
  let open Lwt_result_syntax in
  let profile_ctxt = Node_context.get_profile_ctxt ctxt in
  let storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
    |> Int32.of_int
  in
  (* We clean-up *for* (not at) levels between [last_processed_level + 1] and
     [finalized_level - 1], because [last_processed_level] was the last level
     for which there was already a clean-up, and [finalized_level] will be the
     first level to be processed after this restart. However, there is no need
     to clean-up for levels higher than [last_processed_level + storage_period]
     because there is no data corresponding to such levels.

     ("Level *for* cleaning" refers to the level passed to
     [Handler.remove_old_level_stored_data], not to the level at which there is
     data to be wiped.)

     Examples: Say [last_processed_level = 1000] and [storage_period =
     100]. Thus we have data stored for levels 901 to 1000.

     Example 1: Say [finalized_level = 1060]. We clean-up for levels 1001 up to
     1060, that is, we wipe data from level 901 up to level 960.

     Example 2: Say [finalized_level = 3000]. We clean-up for levels 1001 up to
     1100 (so at levels 901 up to 1000). *)
  let finalized_level = Int32.sub head_level 2l in
  let new_last_processed_level = Int32.(max 1l (pred finalized_level)) in
  let last_level_for_cleaning =
    let highest_level_with_data_for_cleaning =
      Int32.add last_processed_level storage_period
    in
    Int32.(min new_last_processed_level highest_level_with_data_for_cleaning)
  in
  let rec cleanup level =
    if level > last_level_for_cleaning then
      let store = Node_context.get_store ctxt in
      let last_processed_level_store = Store.last_processed_level store in
      let* () =
        Store.Last_processed_level.save
          last_processed_level_store
          new_last_processed_level
      in
      let*! () = Event.emit_end_catchup () in
      return_unit
    else
      let* () =
        Block_handler.remove_old_level_stored_data proto_parameters ctxt level
      in
      L1_crawler_status.catching_up_or_synced_status
        ~head_level
        ~last_processed_level:level
      |> Node_context.set_l1_crawler_status ctxt ;
      cleanup @@ Int32.succ level
  in
  let start_level = Int32.succ last_processed_level in
  let end_level = last_level_for_cleaning in
  let levels_to_clean_up = Int32.(succ @@ sub end_level start_level) in
  if levels_to_clean_up > 0l then
    let*! () =
      Event.emit_start_catchup ~start_level ~end_level ~levels_to_clean_up
    in
    cleanup start_level
  else return_unit

let clean_up_store_and_catch_up ctxt cctxt ~last_processed_level
    ~first_seen_level ~head_level proto_parameters =
  if Node_context.supports_refutations ctxt then
    clean_up_store_and_catch_up_for_refutation_support
      ctxt
      cctxt
      ~last_processed_level
      ~first_seen_level
      head_level
      proto_parameters
  else
    clean_up_store_and_catch_up_for_no_refutation_support
      ctxt
      ~last_processed_level
      head_level
      proto_parameters
