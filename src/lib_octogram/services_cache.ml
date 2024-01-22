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

type service_kind = Metrics | Rpc | Http | P2p

type node_kind = Octez_node | Rollup_node | Dac_node | Http_server | Dal_node

let int_of_service_kind = function
  | Octez_node -> 0
  | Rollup_node -> 1
  | Dac_node -> 2
  | Http_server -> 3
  | Dal_node -> 4

module Cache = Map.Make (struct
  type t = node_kind * string

  let compare (k, n) (k', n') =
    let x = Int.compare (int_of_service_kind k) (int_of_service_kind k') in
    if x = 0 then String.compare n n' else x
end)

type t = (service_kind * int) list Cache.t

let empty = Cache.empty

let add cache name node_kind services =
  Cache.update
    (node_kind, name)
    (function
      | Some existing_services -> Some (services @ existing_services)
      | None -> Some services)
    cache

let remove cache name node_kind = Cache.remove (node_kind, name) cache

let get cache name node_kind service_kind =
  Cache.find (node_kind, name) cache |> List.assoc service_kind
