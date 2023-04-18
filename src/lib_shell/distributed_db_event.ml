(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

module Distributed_db_event = struct
  include Internal_event.Simple

  let section = ["node"; "distributed_db"]

  let multiple_p2p_reader =
    declare_1
      ~section
      ~name:"multiple_p2p_reader"
      ~msg:"multiple p2p_reader for {peer}, a connection may be stuck"
      ~level:Warning
      ("peer", P2p_peer.Id.encoding)

  let p2p_reader_shutdown_failed =
    declare_2
      ~section
      ~name:"p2p_reader_shutdow_failed"
      ~msg:"p2p_reader of {peer} failed to shut down with {exn}"
      ~level:Warning
      ("peer", P2p_peer.Id.encoding)
      ("exn", Data_encoding.string)
end

module Requester_event = struct
  include Internal_event.Simple

  let section = ["node"; "distributed_db"; "requester"]

  let shutting_down_requester =
    declare_0
      ~section
      ~name:"shutting_down_requester"
      ~msg:"shutting down requester"
      ~level:Notice
      ()
end

module P2p_reader_event = struct
  include Internal_event.Simple

  let section = ["node"; "distributed_db"; "p2p_reader"]

  let read_message =
    declare_2
      ~section
      ~name:"read_message"
      ~msg:"read message from peer {peer_id}: {message}"
      ~level:Debug
      ("peer_id", P2p_peer.Id.encoding)
      ("message", Distributed_db_message.(P2p_message.encoding encoding))

  let received_future_block =
    declare_2
      ~section
      ~name:"received_future_block"
      ~msg:"received future block {block_hash} from peer {peer_id}"
      ~level:Notice
      ("block_hash", Block_hash.encoding)
      ("peer_id", P2p_peer.Id.encoding)
end
