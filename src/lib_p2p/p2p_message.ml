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

type 'msg t =
  | Bootstrap
  | Advertise of P2p_point.Id.t list
  | Swap_request of P2p_point.Id.t * P2p_peer.Id.t
  | Swap_ack of P2p_point.Id.t * P2p_peer.Id.t
  | Message of 'msg
  | Disconnect

let encoding msg_encoding =
  let open Data_encoding in
  check_size (100 * 1024 * 1024)
  (*Very high, arbitrary upper bound for message encodings  *)
  @@ dynamic_size
  (* MAX SIZE:
     4(size of size info)
     + MAX SIZE of encoding *)
  @@ union (* MAX SIZE: max MAX SIZE of cases *)
       ~tag_size:`Uint16
       ([
          (* MAX SIZE: 2(tag)
             Note that tags can be 1 or 2 bytes depending on the size of the
             union. This union is of unknown size because it depends on
             [msg_encoding]. As a result, we assume a maximum tag size of 2
             bytes. *)
          case
            (Tag 0x01)
            ~title:"Disconnect"
            (obj1 (req "kind" (constant "Disconnect")))
            (function Disconnect -> Some () | _ -> None)
            (fun () -> Disconnect);
          (* MAX SIZE: 2(tag) *)
          case
            (Tag 0x02)
            ~title:"Bootstrap"
            (obj1 (req "kind" (constant "Bootstrap")))
            (function Bootstrap -> Some () | _ -> None)
            (fun () -> Bootstrap);
          (* MAX SIZE:
             2(tag)
             + (100(list length)
               * ((8(number of IPv6 chunks) * 4(size of IPv6 chunks))
                  + 7(IPv6 chunk separators)
                  + 1(port separator)
                  + 5(size of port number))
             = 4502
          *)
          case
            (Tag 0x03)
            ~title:"Advertise"
            (obj2
               (req "id" (Variable.list ~max_length:100 P2p_point.Id.encoding))
               (req "kind" (constant "Advertise")))
            (function Advertise points -> Some (points, ()) | _ -> None)
            (fun (points, ()) -> Advertise points);
          (* MAX SIZE:
             2(tag)
             + (8 * 4) + 7 + 1 + 5 (point)
             + 16 (peer)
             = 63
          *)
          case
            (Tag 0x04)
            ~title:"Swap_request"
            (obj3
               (req "point" P2p_point.Id.encoding)
               (req "peer_id" P2p_peer.Id.encoding)
               (req "kind" (constant "Swap_request")))
            (function
              | Swap_request (point, peer_id) -> Some (point, peer_id, ())
              | _ -> None)
            (fun (point, peer_id, ()) -> Swap_request (point, peer_id));
          (* MAX SIZE:
             2(tag)
             + (8 * 4) + 7 + 1 + 5 (point)
             + 16 (peer)
             = 63
          *)
          case
            (Tag 0x05)
            ~title:"Swap_ack"
            (obj3
               (req "point" P2p_point.Id.encoding)
               (req "peer_id" P2p_peer.Id.encoding)
               (req "kind" (constant "Swap_ack")))
            (function
              | Swap_ack (point, peer_id) -> Some (point, peer_id, ())
              | _ -> None)
            (fun (point, peer_id, ()) -> Swap_ack (point, peer_id));
        ]
       @ ListLabels.map msg_encoding ~f:(function
             | P2p_params.Encoding
                 {tag; title; encoding; wrap; unwrap; max_length = _ (* ?? *)}
             ->
             Data_encoding.case
               (Tag tag)
               ~title
               encoding
               (function Message msg -> unwrap msg | _ -> None)
               (fun msg -> Message (wrap msg))))
