(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context
open Tezos_rpc

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

module Commitments_history = struct
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
end
