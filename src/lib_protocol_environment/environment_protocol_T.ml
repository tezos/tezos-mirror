(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Environment_context

(* This module contains the real module signature that the Shell sees
   from an economic protocol. There is actually only one signature to
   avoid [if]-[then]-[else] expressions inside the shell.

   When we change the module signature output of the environment, we
   need to implement a forward-compatible interface. This is done by
   upgrading the old interface to the new one.

   The first change in this signature was introduced by the [V3] environment.
   This is why we implement a functor from the initial environment [V0] to [V3]
   directly because neither [V1] nor [V2] change the module signature output of
   the environment.

   All the equalities constraints are here for typing only. We use a destructive
   substitution ([:=]) for types that are defined by the shell, or that are
   common to all the economic protocol environments, and an equality-constraint
   ([=]) for the types that are abstracted from the economic protocol.

   [module type T] is the same signature as the last [Vx] environment
   ([module type Vx_T]). *)
module type T = sig
  (* Documentation for this interface may be found in
     module type [PROTOCOL] of [sigs/v3/updater.mli]. *)
  include Environment_protocol_T_V3.T

  val environment_version : Protocol.env_version
end

module V0toV3
    (E : Environment_protocol_T_V0.T
           with type context := Context.t
            and type quota := quota
            and type validation_result := validation_result
            and type rpc_context := rpc_context
            and type 'a tzresult := 'a Error_monad.tzresult) :
  Environment_protocol_T_V3.T
    with type context := Context.t
     and type quota := quota
     and type validation_result := validation_result
     and type rpc_context := rpc_context
     and type 'a tzresult := 'a Error_monad.tzresult
     and type block_header_data = E.block_header_data
     and type block_header = E.block_header
     and type block_header_metadata = E.block_header_metadata
     and type operation_data = E.operation_data
     and type operation = E.operation
     and type operation_receipt = E.operation_receipt
     and type validation_state = E.validation_state = struct
  include E

  (* Add backwards compatibility shadowing here *)
  let relative_position_within_block = compare_operations
end

(* [module type PROTOCOL] is protocol signature that the shell can use*)
module type PROTOCOL =
  T
    with type context := Context.t
     and type quota := quota
     and type validation_result := validation_result
     and type rpc_context := rpc_context
     and type 'a tzresult := 'a Error_monad.tzresult
