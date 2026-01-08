(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

include Tezos_base.TzPervasives.Result_syntax

let wrap = Environment.wrap_tzresult

let ( let@ ) m f =
  let x = wrap m in
  f x

let ( let*@ ) m f =
  let* x = wrap m in
  f x

let ( let+@ ) m f =
  let+ x = wrap m in
  f x
