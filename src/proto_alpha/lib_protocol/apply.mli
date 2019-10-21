(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Alpha_context
open Apply_results

type error += Wrong_endorsement_predecessor of Block_hash.t * Block_hash.t

type error += Duplicate_endorsement of Signature.Public_key_hash.t

type error += Invalid_endorsement_level

type error += Unwrapped_endorsement

type error += Invalid_commitment of {expected : bool}

type error += Internal_operation_replay of packed_internal_operation

type error += Invalid_double_endorsement_evidence

type error +=
  | Inconsistent_double_endorsement_evidence of {
      delegate1 : Signature.Public_key_hash.t;
      delegate2 : Signature.Public_key_hash.t;
    }

type error +=
  | Too_early_double_endorsement_evidence of {
      level : Raw_level.t;
      current : Raw_level.t;
    }

type error +=
  | Outdated_double_endorsement_evidence of {
      level : Raw_level.t;
      last : Raw_level.t;
    }

type error +=
  | Invalid_double_baking_evidence of {
      hash1 : Block_hash.t;
      level1 : Int32.t;
      hash2 : Block_hash.t;
      level2 : Int32.t;
    }

type error +=
  | Inconsistent_double_baking_evidence of {
      delegate1 : Signature.Public_key_hash.t;
      delegate2 : Signature.Public_key_hash.t;
    }

type error +=
  | Too_early_double_baking_evidence of {
      level : Raw_level.t;
      current : Raw_level.t;
    }

type error +=
  | Outdated_double_baking_evidence of {
      level : Raw_level.t;
      last : Raw_level.t;
    }

type error += Invalid_activation of {pkh : Ed25519.Public_key_hash.t}

type error += Gas_quota_exceeded_init_deserialize

type error += Inconsistent_sources

type error +=
  | Not_enough_endorsements_for_priority of {
      required : int;
      priority : int;
      endorsements : int;
      timestamp : Time.t;
    }

type error += (* `Permanent *) Failing_noop_error

val begin_partial_construction : t -> (t, error trace) result Lwt.t

val begin_full_construction :
  t ->
  Time.t ->
  Block_header.contents ->
  (t * Block_header.contents * public_key * Period.t, error trace) result Lwt.t

val begin_application :
  t ->
  Chain_id.t ->
  Block_header.t ->
  Time.t ->
  (t * public_key * Period.t, error trace) result Lwt.t

val apply_operation :
  t ->
  Chain_id.t ->
  Script_ir_translator.unparsing_mode ->
  Block_hash.t ->
  public_key_hash ->
  Operation_list_hash.elt ->
  'a operation ->
  (t * 'a operation_metadata, error trace) result Lwt.t

val finalize_application :
  t ->
  Block_header.contents ->
  public_key_hash ->
  block_delay:Period.t ->
  Receipt.balance_updates ->
  (t * block_metadata, error trace) result Lwt.t

val apply_manager_contents_list :
  t ->
  Script_ir_translator.unparsing_mode ->
  public_key_hash ->
  Chain_id.t ->
  'a Kind.manager contents_list ->
  (t * 'a Kind.manager contents_result_list) Lwt.t

val apply_contents_list :
  t ->
  Chain_id.t ->
  Script_ir_translator.unparsing_mode ->
  Block_hash.t ->
  public_key_hash ->
  'kind operation ->
  'kind contents_list ->
  (t * 'kind contents_result_list) tzresult Lwt.t

val check_minimum_endorsements :
  t -> Block_header.contents -> Period.t -> int -> (unit, error trace) result
