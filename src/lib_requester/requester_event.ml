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

let section = ["node"; "requester"; "scheduler"]

module Make (Hash : sig
  type t

  val name : string

  val encoding : t Data_encoding.t
end) =
struct
  include Internal_event.Simple

  let mk_name s = s ^ "__" ^ Hash.name

  let notify_push =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_push")
      ~msg:"push received {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let notify_push_cancellation =
    declare_1
      ~section
      ~name:(mk_name "requester_notify_push_cancellation")
      ~msg:"push cancellation {name}"
      ~level:Debug
      ("name", Hash.encoding)

  let notify_push_invalid =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_push_invalid")
      ~msg:"push received invalid {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let notify_push_duplicate =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_push_duplicate")
      ~msg:"push received duplicate {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let notify_push_unrequested =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_push_unrequested")
      ~msg:"push received unrequested {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let registering_request =
    declare_2
      ~section
      ~name:(mk_name "requester_registering_request")
      ~msg:"registering request {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", Data_encoding.option P2p_peer.Id.encoding)

  let registering_request_replaced =
    declare_2
      ~section
      ~name:(mk_name "requester_registering_request_replaced")
      ~msg:"registering request {name} from {peer} -> replaced"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", Data_encoding.option P2p_peer.Id.encoding)

  let registering_request_added =
    declare_2
      ~section
      ~name:(mk_name "requester_registering_request_added")
      ~msg:"registering request {name} from {peer} -> added"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", Data_encoding.option P2p_peer.Id.encoding)

  let notify_received =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_received")
      ~msg:"received {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let notify_cancelled =
    declare_1
      ~section
      ~name:(mk_name "requester_notify_cancelled")
      ~msg:"cancelled {name}"
      ~level:Debug
      ("name", Hash.encoding)

  let notify_invalid =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_invalid")
      ~msg:"received invalid {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let notify_unrequested =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_unrequested")
      ~msg:"received unrequested {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let notify_duplicate =
    declare_2
      ~section
      ~name:(mk_name "requester_notify_duplicate")
      ~msg:"received unrequested {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let requested =
    declare_2
      ~section
      ~name:(mk_name "requester_requested")
      ~msg:"requested {name} from {peer}"
      ~level:Debug
      ("name", Hash.encoding)
      ("peer", P2p_peer.Id.encoding)

  let terminated =
    declare_0
      ~section
      ~name:(mk_name "requester_worker_terminated")
      ~msg:"requester worker terminating"
      ~level:Debug
      ()

  let timeout =
    declare_0
      ~section
      ~name:(mk_name "requester_worker_timeout")
      ~msg:"requester worker timout"
      ~level:Debug
      ()

  let no_active_peers =
    declare_0
      ~section
      ~name:(mk_name "no_active_peers")
      ~msg:"no active peers, cannot fetch data, waiting for peers"
      ~level:Notice
      ()
end
