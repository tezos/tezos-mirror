(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let msg : string =
  "In proxy mode, all RPCs of protocol genesis should be forwarded to the \
   node. Hence this code should not be reached, because the RPC directory is \
   empty."

let () =
  let open Tezos_proxy.Registration in
  let module M : Proxy_sig = struct
    module Protocol = Protocol_client_context.Lifted_protocol

    let protocol_hash = Protocol.hash

    let directory = RPC_directory.empty

    let hash _ ?chain ?block _ =
      ignore chain ;
      ignore block ;
      failwith "%s" msg

    let init_env_rpc_context _ = failwith "%s" msg

    let merkle_tree _ _ _ = failwith "%s" msg

    let time_between_blocks _ _ _ = failwith "%s" msg
  end in
  register_proxy_context (module M)
