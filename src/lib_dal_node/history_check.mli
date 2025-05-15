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

(** [check_history_mode config profile_ctxt proto_parameters] verifies that the
    configured history mode for the DAL node is sufficient to store the required
    number of past levels. This depends on the active profile and whether
    refutation support is enabled. It raises [Not_enough_history] if the
    configuration is insufficient. *)
val check_history_mode :
  Configuration_file.t ->
  Profile_manager.t ->
  Types.proto_parameters ->
  unit tzresult Lwt.t

(** [check_l1_history_mode profile_ctxt cctxt proto_parameters ~head_level
    ~first_seen_level] verifies that the L1 node retains enough past levels for
    the DAL node to function correctly, based on the storage period determined
    from the profile. It computes the minimal number of L1 cycles needed and
    compares it to what the L1 node provides. Raises [Not_enough_l1_history] if
    insufficient. *)
val check_l1_history_mode :
  Profile_manager.t ->
  Rpc_context.t ->
  Types.proto_parameters ->
  head_level:int32 ->
  first_seen_level:int32 option ->
  unit tzresult Lwt.t

(** [skip_list_offset proto_parameters] returns the number of extra levels
    required when storing skip list cells, defined as [attestation_lag + 1]. *)
val skip_list_offset : Types.proto_parameters -> int
