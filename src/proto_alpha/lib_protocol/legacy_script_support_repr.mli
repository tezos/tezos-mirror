(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2019 Cryptium Labs <contact@cryptium-labs.com>              *)
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

(** This code mimics the now defunct scriptless KT1s.

    The manager contract is from:
    https://gitlab.com/nomadic-labs/mi-cho-coq/blob/7b42f2e970e1541af54f8a9b6820b4f18e847575/src/contracts/manager.tz
    The formal proof is at:
    https://gitlab.com/nomadic-labs/mi-cho-coq/blob/a7603e12021166e15890f6d504feebec2f945502/src/contracts_coq/manager.v *)
val manager_script_code: Script_repr.lazy_expr

(** This code mimics the now defunct "spendable" flags of KT1s by
    adding a [do] entrypoint, preserving the original script's at
    'default' entrypoint.

    The pseudo-code for the applied transformations is from:
    https://gitlab.com/nomadic-labs/mi-cho-coq/blob/7b42f2e970e1541af54f8a9b6820b4f18e847575/src/contracts/transform/add_do.tz *)
val add_do:
  manager_pkh: Signature.Public_key_hash.t ->
  script_code: Script_repr.lazy_expr ->
  script_storage: Script_repr.lazy_expr ->
  (Script_repr.lazy_expr * Script_repr.lazy_expr) tzresult Lwt.t

(** This code mimics the now defunct "spendable" flags of KT1s by
    adding a [do] entrypoint, preserving the original script's at
    'default' entrypoint.

    The pseudo-code for the applied transformations is from:
    https://gitlab.com/nomadic-labs/mi-cho-coq/blob/7b42f2e970e1541af54f8a9b6820b4f18e847575/src/contracts/transform/add_set_delegate.tz *)
val add_set_delegate:
  manager_pkh: Signature.Public_key_hash.t ->
  script_code: Script_repr.lazy_expr ->
  script_storage: Script_repr.lazy_expr ->
  (Script_repr.lazy_expr * Script_repr.lazy_expr) tzresult Lwt.t

(** Checks if a contract was declaring a default entrypoint somewhere
   else than at the root, in which case its type changes when
   entrypoints are activated. *)
val has_default_entrypoint:
  Script_repr.lazy_expr -> bool

(** Adds a [%root] annotation on the toplevel parameter construct. *)
val add_root_entrypoint:
  script_code: Script_repr.lazy_expr ->
  Script_repr.lazy_expr tzresult Lwt.t
