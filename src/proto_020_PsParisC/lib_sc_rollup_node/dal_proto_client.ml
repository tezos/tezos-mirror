(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol.Alpha_context
open Tezos_rpc

type cctxt = Dal_node_client.cctxt

type 'rpc service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) Service.service
  constraint
    'rpc =
    < meth : 'meth
    ; prefix : 'prefix
    ; params : 'params
    ; query : 'query
    ; input : 'input
    ; output : 'output >

let cell_hash_arg : Dal.Slots_history.Pointer_hash.t Arg.t =
  Arg.make
    ~descr:"The hash of a DAL skip list cell"
    ~name:"skip_list_cell_hash"
    ~construct:Dal.Slots_history.Pointer_hash.to_b58check
    ~destruct:(fun h ->
      match Dal.Slots_history.Pointer_hash.of_b58check_opt h with
      | Some b -> Ok b
      | None -> Error "Cannot parse skip list cell hash")
    ()

let hash_content :
    < meth : [`GET]
    ; input : unit
    ; output : Dal.Slots_history.t
    ; prefix : unit
    ; params : unit * Dal.Slots_history.Pointer_hash.t
    ; query : unit >
    service =
  Service.get_service
    ~description:"Returns the DAL skip list cell of the given hash"
    ~query:Query.empty
    ~output:Dal.Slots_history.encoding
    Path.(
      open_root
      / Protocol_hash.to_b58check Protocol.hash
      / "commitments_history" / "hash" /: cell_hash_arg)

let get_commitments_history_hash_content (cctxt : cctxt) hash =
  Dal_node_client.call
    cctxt
    (Tezos_rpc.Service.prefix Tezos_rpc.Path.(root / "plugin") hash_content)
    ((), hash)
    ()
    ()
