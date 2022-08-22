(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Tezos_rpc.RPC_directory

type conflict =
  | CService of Tezos_rpc.RPC_service.meth
  | CDir
  | CBuilder
  | CTail
  | CTypes of Tezos_rpc.RPC_arg.descr * Tezos_rpc.RPC_arg.descr
  | CType of Tezos_rpc.RPC_arg.descr * string list

exception Conflict of step list * conflict

let merge d1 d2 =
  try merge ~strategy:`Raise d1 d2 with
  | Tezos_rpc.RPC_directory.Conflict (sl, Tezos_rpc.RPC_directory.CService m) ->
      raise (Conflict (sl, CService m))
  | Tezos_rpc.RPC_directory.Conflict (sl, Tezos_rpc.RPC_directory.CDir) ->
      raise (Conflict (sl, CDir))
  | Tezos_rpc.RPC_directory.Conflict (sl, Tezos_rpc.RPC_directory.CBuilder) ->
      raise (Conflict (sl, CBuilder))
  (* Here we reproduce the old behavior before
     https://gitlab.com/tezos/tezos/-/merge_requests/6085#note_1075865206.
     This conflit is raised when merging dynamic directories.
     As dynamic directories were not mergeable, [CBuilder] was raised.*)
  | Tezos_rpc.RPC_directory.Conflict
      (sl, Tezos_rpc.RPC_directory.CDynDescr (_, _)) ->
      raise (Conflict (sl, CBuilder))
  | Tezos_rpc.RPC_directory.Conflict (sl, Tezos_rpc.RPC_directory.CTail) ->
      raise (Conflict (sl, CTail))
  | Tezos_rpc.RPC_directory.Conflict
      (sl, Tezos_rpc.RPC_directory.CTypes (arg1, arg2)) ->
      raise (Conflict (sl, CTypes (arg1, arg2)))
  | Tezos_rpc.RPC_directory.Conflict (sl, Tezos_rpc.RPC_directory.CType (d, l))
    ->
      raise (Conflict (sl, CType (d, l)))
