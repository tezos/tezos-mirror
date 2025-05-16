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

(* This function checks that in case the history mode is Rolling with a custom
   number of blocks, these number of blocks are sufficient. *)
let check_history_mode config profile_ctxt proto_parameters =
  let open Lwt_result_syntax in
  let supports_refutations =
    Profile_manager.supports_refutations profile_ctxt
  in
  let storage_period =
    Profile_manager.get_attested_data_default_store_period
      profile_ctxt
      proto_parameters
  in
  match config.Configuration_file.history_mode with
  | Rolling {blocks = `Some b} ->
      let minimal_levels =
        if supports_refutations then storage_period
        else proto_parameters.attestation_lag
      in
      if b < minimal_levels then
        tzfail @@ Errors.Not_enough_history {stored_levels = b; minimal_levels}
      else return_unit
  | Rolling {blocks = `Auto} | Full -> return_unit

(* We need more levels because [store_skip_list_cells level] needs the plugin
   for [attestation_lag + 1] levels in the past wrt to the target [level]. Note
   {!get_storage_period} refers to published levels (not attested levels). The
   plus one comes from the technical details of {!store_skip_list_cells}. *)
let skip_list_offset proto_parameters =
  proto_parameters.Types.attestation_lag + 1

(* This function checks the L1 node stores enough block data for the DAL node to
   function correctly. *)
let check_l1_history_mode profile_ctxt cctxt proto_parameters ~head_level
    ~first_seen_level =
  let open Lwt_result_syntax in
  let* l1_history_mode =
    let* l1_mode, blocks_preservation_cycles_opt =
      Config_services.history_mode cctxt
    in
    (* Note: For the DAL node it does not matter if the L1 node is in Full or
       Rolling mode, because the DAL node is not interested in blocks outside of
       a certain time window. *)
    return
    @@
    match (l1_mode, blocks_preservation_cycles_opt) with
    | Archive, _ -> `L1_archive
    | Full None, None
    | Rolling None, None
    | Full (Some _), None
    | Rolling (Some _), None
    | Full None, Some _
    | Rolling None, Some _ ->
        (* unreachable cases, at least for now *) assert false
    | Full (Some additional_cycles), Some blocks_preservation_cycles
    | Rolling (Some additional_cycles), Some blocks_preservation_cycles ->
        `L1_rolling (additional_cycles.offset + blocks_preservation_cycles)
  in
  let check ~dal_blocks ~l1_cycles =
    let blocks_per_cycle =
      Int32.to_int proto_parameters.Types.blocks_per_cycle
    in
    let dal_cycles = dal_blocks / blocks_per_cycle in
    let minimal_cycles =
      if dal_blocks mod blocks_per_cycle = 0 then dal_cycles else 1 + dal_cycles
    in
    if l1_cycles < minimal_cycles then
      tzfail
      @@ Errors.Not_enough_l1_history
           {stored_cycles = l1_cycles; minimal_cycles}
    else return_unit
  in
  match l1_history_mode with
  | `L1_archive -> return_unit
  | `L1_rolling l1_cycles ->
      (* For the non-"refutation supporting" profiles, we don't currently need
         that many levels in the past, because we don't for instance retrieve the
         protocol parameters for such past levels; though we should. *)
      let dal_blocks =
        Profile_manager.get_storage_period
          profile_ctxt
          proto_parameters
          ~head_level
          ~first_seen_level
        +
        if Profile_manager.supports_refutations profile_ctxt then
          skip_list_offset proto_parameters
        else 0
      in
      check ~dal_blocks ~l1_cycles
