(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type runtime = Tezos

let runtime_of_string_opt, string_of_runtime, known_runtimes =
  let known_runtimes = [("tezos", Tezos)] in
  let known_runtime_names = List.map (fun (x, y) -> (y, x)) known_runtimes in
  ( (fun n -> List.assoc_opt ~equal:String.equal n known_runtimes),
    (fun n -> Stdlib.List.assoc n known_runtime_names),
    List.map snd known_runtimes )

let pp_runtime fmt runtime =
  Format.pp_print_string fmt (string_of_runtime runtime)

let feature_flag = function Tezos -> "/evm/feature_flags/enable_tezos_runtime"

let runtime_encoding : runtime Data_encoding.t =
  let open Data_encoding in
  (* FIXME: use string_enum instead once we have more than one runtime *)
  conv (fun _ -> ()) (fun () -> Tezos) (constant "tezos")
