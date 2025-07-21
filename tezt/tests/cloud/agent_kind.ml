(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t =
  | Bootstrap
  | Baker of int
  | Producer of int
  | Observer of [`Index of int | `Pkh of string]
  | Reverse_proxy
  | Etherlink_operator
  | Etherlink_dal_operator
  | Etherlink_dal_observer of {slot_index : int}
  | Etherlink_producer of int
  | Echo_rollup_operator
  | Echo_rollup_dal_observer of {slot_index : int}

let name_of = function
  | Bootstrap -> "bootstrap"
  | Baker i -> Format.asprintf "attester-%d" i
  | Producer i -> Format.asprintf "dal-producer-%d" i
  | Observer (`Index i) -> Format.asprintf "dal-observer-%d" i
  | Observer (`Pkh pkh) ->
      (* Shorting the pkh enables to get better logs. *)
      Format.asprintf "dal-observer-%s" (String.sub pkh 0 8)
  | Reverse_proxy -> "dal-reverse-proxy"
  | Etherlink_operator -> "etherlink-operator"
  | Etherlink_dal_operator -> "etherlink-dal-operator"
  | Etherlink_dal_observer {slot_index} ->
      Format.asprintf "etherlink-dal-operator-%d" slot_index
  | Etherlink_producer i -> Format.asprintf "etherlink-producer-%d" i
  | Echo_rollup_operator -> "echo-rollup-operator"
  | Echo_rollup_dal_observer {slot_index} ->
      Format.sprintf "echo-rollup-dal-node-%d" slot_index
