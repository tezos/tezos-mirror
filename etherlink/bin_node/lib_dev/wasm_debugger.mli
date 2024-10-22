(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val read_kernel_from_file :
  Lwt_io.file_name -> (string * bool, tztrace) result Lwt.t

val check_kernel :
  binary:bool ->
  name:string ->
  Tezos_scoru_wasm.Wasm_pvm_state.version ->
  string ->
  unit tzresult Lwt.t
