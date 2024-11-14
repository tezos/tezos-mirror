(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021-2023 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2022-2023 Trili Tech <contact@trili.tech>         *)
(* SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>              *)
(*                                                                           *)
(*****************************************************************************)

(** Although we consider data-encoding to be its own product, we do not (yet)
   release it on its own. Instead, we treat it as a library of octez in the same
   vein as error-monad or crypto. *)

(* List of paths in which the source is stored *)
val product_source : string list

val data_encoding : Manifest.target

val json_data_encoding : Manifest.target

(* TODO: https://gitlab.com/tezos/tezos/-/merge_requests/13631
   Remove Bson support form Resto *)
val json_data_encoding_bson : Manifest.target
