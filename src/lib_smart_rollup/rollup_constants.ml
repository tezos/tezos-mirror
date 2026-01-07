(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** Protocol agnostic representation for protocol constants. *)

type dal_constants = {
  feature_enable : bool;
  attestation_lag : int;
  attestation_lags : int list;
  dynamic_lag_enable : bool;
  number_of_slots : int;
  cryptobox_parameters : Tezos_crypto_dal.Cryptobox.parameters;
}

type reveal_activation_level = {
  blake2B : int32;
  metadata : int32;
  dal_page : int32;
  dal_parameters : int32;
  dal_attested_slots_validity_lag : int32;
}

type sc_rollup_constants = {
  challenge_window_in_blocks : int;
  commitment_period_in_blocks : int;
  reveal_activation_level : reveal_activation_level option;
  max_number_of_stored_cemented_commitments : int;
  max_active_outbox_levels : int;
}

type protocol_constants = {
  minimal_block_delay : int64;
  delay_increment_per_round : int64;
  sc_rollup : sc_rollup_constants;
  dal : dal_constants;
}

let reveal_activation_level_encoding =
  let open Data_encoding in
  conv
    (fun {
           blake2B;
           metadata;
           dal_page;
           dal_parameters;
           dal_attested_slots_validity_lag;
         }
       ->
      ( blake2B,
        metadata,
        dal_page,
        dal_parameters,
        dal_attested_slots_validity_lag ))
    (fun ( blake2B,
           metadata,
           dal_page,
           dal_parameters,
           dal_attested_slots_validity_lag )
       ->
      {
        blake2B;
        metadata;
        dal_page;
        dal_parameters;
        dal_attested_slots_validity_lag;
      })
  @@ obj5
       (req "blake2B" int32)
       (req "metadata" int32)
       (req "dal_page" int32)
       (req "dal_parameters" int32)
       (req "dal_attested_slots_validity_lag" int32)

let encoding =
  let open Data_encoding in
  conv
    (fun {
           minimal_block_delay;
           delay_increment_per_round;
           sc_rollup =
             {
               challenge_window_in_blocks;
               commitment_period_in_blocks;
               reveal_activation_level;
               max_number_of_stored_cemented_commitments;
               max_active_outbox_levels;
             };
           dal =
             {
               feature_enable;
               attestation_lag;
               attestation_lags;
               dynamic_lag_enable;
               number_of_slots;
               cryptobox_parameters;
             };
         }
       ->
      ( minimal_block_delay,
        delay_increment_per_round,
        ( challenge_window_in_blocks,
          commitment_period_in_blocks,
          reveal_activation_level,
          max_number_of_stored_cemented_commitments,
          max_active_outbox_levels ),
        ( feature_enable,
          attestation_lag,
          attestation_lags,
          dynamic_lag_enable,
          number_of_slots,
          cryptobox_parameters ) ))
    (fun ( minimal_block_delay,
           delay_increment_per_round,
           ( challenge_window_in_blocks,
             commitment_period_in_blocks,
             reveal_activation_level,
             max_number_of_stored_cemented_commitments,
             max_active_outbox_levels ),
           ( feature_enable,
             attestation_lag,
             attestation_lags,
             dynamic_lag_enable,
             number_of_slots,
             cryptobox_parameters ) )
       ->
      {
        minimal_block_delay;
        delay_increment_per_round;
        sc_rollup =
          {
            challenge_window_in_blocks;
            commitment_period_in_blocks;
            reveal_activation_level;
            max_number_of_stored_cemented_commitments;
            max_active_outbox_levels;
          };
        dal =
          {
            feature_enable;
            attestation_lag;
            attestation_lags;
            dynamic_lag_enable;
            number_of_slots;
            cryptobox_parameters;
          };
      })
  @@ obj4
       (req "minimal_block_delay" int64)
       (req "delay_increment_per_round" int64)
       (req
          "sc_rollup"
          (obj5
             (req "challenge_window_in_blocks" int31)
             (req "commitment_period_in_blocks" int31)
             (opt "reveal_activation_level" reveal_activation_level_encoding)
             (req "max_number_of_stored_cemented_commitments" int31)
             (req "max_active_outbox_levels" int31)))
       (req
          "dal"
          (obj6
             (req "feature_enable" bool)
             (req "attestation_lag" int31)
             (dft "attestation_lags" (list int31) [])
             (dft "dynamic_lag_enable" bool false)
             (req "number_of_slots" int31)
             (req
                "cryptobox_parameters"
                Tezos_crypto_dal.Cryptobox.parameters_encoding)))
