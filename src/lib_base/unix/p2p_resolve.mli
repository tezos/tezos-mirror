(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type error += Failed_to_parse_address of (string * string)

(** [resolve_addr_with_peer_id ~default_addr ~default_port ?passive
    addr] parses [addr] to return the corresponding points.

    The format of [addr] can be either an IP address (v4 or v6) or a
    domain name. If a domain name is provided, a DNS lookup is made
    (see {!val:Unix.getaddrinfo}.

    Moreover, a peer id can provided by postfixing the addr with
    [#<peer_id>]. The peer id should be provided using the b58 format.

    An event is emitted if the DNS lookup returned 0 points.

    The error [Failed_to_parse_address] is returned if the parsing failed.  *)
val resolve_addr_with_peer_id :
  default_addr:string ->
  default_port:int ->
  ?passive:bool ->
  string ->
  (P2p_point.Id.t * P2p_peer.Id.t option) list tzresult Lwt.t

(** [resolve_addr] is the same as [resolve_addr_with_peer_id] but no
    peer id is expected.

    A warning event is emitted if a peer_id was provided.
*)
val resolve_addr :
  default_addr:string ->
  default_port:int ->
  ?passive:bool ->
  string ->
  P2p_point.Id.t list tzresult Lwt.t
