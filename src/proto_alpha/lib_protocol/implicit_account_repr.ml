(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Protocol-side representation of an implicit account identifier.

    A thin wrapper that re-exports {!Account_hash} and adds the storage
    {!Index}. Kept abstract so the protocol manipulates account ids without
    depending on their underlying encoding. *)

include Account_hash

(* WARNING: The path MUST be compatible before and after Stateful Activation for tz1-5 *)
module Index = struct
  type nonrec t = t

  include Account_hash.Path

  let rpc_arg = rpc_arg

  let encoding = encoding

  let compare = compare
end
