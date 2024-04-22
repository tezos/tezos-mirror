(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2021-2023 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-FileCopyrightText: 2022-2023 Trili Tech <contact@trili.tech>         *)
(* SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>              *)
(*                                                                           *)
(*****************************************************************************)

open Manifest
open Externals

let json_data_encoding =
  external_lib ~js_compatible:true "json-data-encoding" (V.exactly "1.0.1")

let data_encoding =
  external_lib
    ~js_compatible:true
    ~main_module:"Data_encoding"
    "data-encoding"
    (V.exactly "1.0.1")
