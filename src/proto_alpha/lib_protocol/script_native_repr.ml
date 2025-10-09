(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = Accumulator

type with_storage = {kind : t; storage : Script_repr.lazy_expr}

let encoding : t Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Accumulator"
        unit
        (function Accumulator -> Some ())
        (fun () -> Accumulator);
    ]

let rpc_arg =
  RPC_arg.make
    ~descr:"A native contract kind"
    ~name:"kind"
    ~construct:(function Accumulator -> "Accumulator")
    ~destruct:(fun _ -> Ok Accumulator)
    ()

let equal k1 k2 = match (k1, k2) with Accumulator, Accumulator -> true
