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

include Tezos_rpc.Directory

type conflict =
  | CService of Tezos_rpc.Service.meth
  | CDir
  | CBuilder
  | CTail
  | CTypes of Tezos_rpc.Arg.descr * Tezos_rpc.Arg.descr
  | CType of Tezos_rpc.Arg.descr * string list

exception Conflict of step list * conflict

let merge d1 d2 =
  try merge ~strategy:`Raise d1 d2 with
  | Tezos_rpc.Directory.Conflict (sl, Tezos_rpc.Directory.CService m) ->
      raise (Conflict (sl, CService m))
  | Tezos_rpc.Directory.Conflict (sl, Tezos_rpc.Directory.CDir) ->
      raise (Conflict (sl, CDir))
  | Tezos_rpc.Directory.Conflict (sl, Tezos_rpc.Directory.CBuilder) ->
      raise (Conflict (sl, CBuilder))
  (* Here we reproduce the old behavior before
     https://gitlab.com/tezos/tezos/-/merge_requests/6085#note_1075865206.
     This conflit is raised when merging dynamic directories.
     As dynamic directories were not mergeable, [CBuilder] was raised.*)
  | Tezos_rpc.Directory.Conflict (sl, Tezos_rpc.Directory.CDynDescr (_, _)) ->
      raise (Conflict (sl, CBuilder))
  | Tezos_rpc.Directory.Conflict (sl, Tezos_rpc.Directory.CTail) ->
      raise (Conflict (sl, CTail))
  | Tezos_rpc.Directory.Conflict (sl, Tezos_rpc.Directory.CTypes (arg1, arg2))
    ->
      raise (Conflict (sl, CTypes (arg1, arg2)))
  | Tezos_rpc.Directory.Conflict (sl, Tezos_rpc.Directory.CType (d, l)) ->
      raise (Conflict (sl, CType (d, l)))
