(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs, <contact@nomadic-labs.com>                    *)
(* Copyright (c) Functori, <contact@functori.com>                            *)
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

open Injector_sigs

exception Protocol_not_found

module Make (Parameters : PARAMETERS) = struct
  type proto_client =
    (module PROTOCOL_CLIENT
       with type state = Parameters.state
        and type operation = Parameters.Operation.t)

  let proto_clients : proto_client Protocol_hash.Table.t =
    Protocol_hash.Table.create 7

  let register protocol proto_client =
    Protocol_hash.Table.replace proto_clients protocol proto_client

  let proto_client_for_protocol protocol =
    WithExceptions.Option.to_exn ~none:Protocol_not_found
    @@ Protocol_hash.Table.find proto_clients protocol

  let registered_proto_clients () =
    Protocol_hash.Table.to_seq proto_clients |> List.of_seq

  let check_registered_proto_clients state =
    Protocol_hash.Table.iter_e
      (fun _proto_hash (proto_client : proto_client) ->
        let module Proto_client = (val proto_client) in
        Proto_client.checks state)
      proto_clients
end
