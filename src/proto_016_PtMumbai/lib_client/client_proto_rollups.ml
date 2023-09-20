(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol
open Alpha_context

type rollup_entity = {rollup : Tx_rollup.t; origination_level : int32 option}

module TxRollupEntity = struct
  type t = rollup_entity

  include Compare.Make (struct
    type t = rollup_entity

    let compare r1 r2 = Tx_rollup.compare r1.rollup r2.rollup
  end)

  let encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"rollup without origination level"
          (Tag 0)
          Tx_rollup.encoding
          (function
            | {rollup; origination_level = None} -> Some rollup | _ -> None)
          (fun rollup -> {rollup; origination_level = None});
        case
          ~title:"rollup with origination level"
          (Tag 1)
          (obj2
             (req "rollup" Tx_rollup.encoding)
             (req "origination_level" int32))
          (function
            | {rollup; origination_level = Some level} -> Some (rollup, level)
            | _ -> None)
          (fun (rollup, level) -> {rollup; origination_level = Some level});
      ]

  let of_source s =
    let open Lwt_result_syntax in
    let*? rollup =
      Tx_rollup.of_b58check s |> Environment.wrap_tzresult
      |> record_trace_eval (fun () ->
             error_of_fmt "bad transaction rollup notation")
    in
    return {rollup; origination_level = None}

  let to_source {rollup; _} = return (Tx_rollup.to_b58check rollup)

  let name = "tx_rollup"
end

module TxRollupAlias = Client_aliases.Alias (TxRollupEntity)

module EpoxyEntity = struct
  include Zk_rollup.Address

  let of_source s =
    let open Lwt_result_syntax in
    let* rollup =
      match Zk_rollup.Address.of_b58check_opt s with
      | None -> tzfail @@ error_of_fmt "bad epoxy notation"
      | Some rollup -> return rollup
    in
    return rollup

  let to_source rollup = return (Zk_rollup.Address.to_b58check rollup)

  let name = "epoxy"
end

module Epoxy_alias = Client_aliases.Alias (EpoxyEntity)

module Soru_entity = struct
  include Sc_rollup.Address

  let of_source s =
    let open Lwt_result_syntax in
    match of_b58check_opt s with
    | Some address -> return address
    | None -> tzfail @@ error_of_fmt "bad smart rollup notation"

  let to_source s = return (to_b58check s)

  let name = Smart_rollup_alias.Address.Entity.name
end

module Soru_alias = Client_aliases.Alias (Soru_entity)
