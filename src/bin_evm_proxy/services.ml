(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Tezos_rpc
open Rpc_encodings

let version_service =
  Service.get_service
    ~description:"version"
    ~query:Query.empty
    ~output:Data_encoding.string
    Path.(root / "version")

let version dir =
  Directory.register0 dir version_service (fun () () ->
      Format.printf "Version\n%!" ;
      Lwt.return_ok Tezos_version.Bin_version.version_string)

let dispatch_service =
  Service.post_service
    ~query:Query.empty
    ~input:Input.encoding
    ~output:Output.encoding
    Path.(root)

(** Mocked values used until we can retrieve the specific values from an EVM kernel. *)
module Mock = struct
  open Ethereum_types

  let qty_f = quantity_of_z

  (* Default chain_id for ethereum custom networks with Ganache. *)
  let chain_id = qty_f (Z.of_int 1337)

  let block_height = block_height_of_z Z.zero

  let balance = qty_f @@ Z.of_int64 Int64.max_int
end

let dispatch dir =
  Directory.register0 dir dispatch_service (fun () input ->
      match input with
      | Accounts.Input _ -> Lwt.return_ok (Accounts.Output (Ok []))
      | Network_id.Input _ ->
          Lwt.return_ok (Network_id.Output (Ok Mock.chain_id))
      | Chain_id.Input _ -> Lwt.return_ok (Chain_id.Output (Ok Mock.chain_id))
      | Get_balance.Input _ ->
          Lwt.return_ok (Get_balance.Output (Ok Mock.balance))
      | Block_number.Input _ ->
          Format.printf "Accessing blockNumber\n%!" ;
          Lwt.return_ok (Block_number.Output (Ok Mock.block_height))
      | _ -> Error_monad.failwith "Unsupported method\n%!")

let directory = Directory.empty |> version |> dispatch
