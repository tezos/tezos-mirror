(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

include Stdlib.Format

let err_formatter = Stdlib.Format.formatter_of_out_channel Stdlib.stderr

let eprintf (fmt : ('a, formatter, unit) format) : 'a =
  Stdlib.Format.fprintf err_formatter fmt
