(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <contact@tezos.com> *)
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

open Lib_test.Qcheck_helpers
open QCheck

let ipv4 =
  map ~rev:Ipaddr.V4.to_int32 Ipaddr.V4.of_int32 int32
  |> set_print Ipaddr.V4.to_string

let ipv6 =
  map ~rev:Ipaddr.V6.to_int64 Ipaddr.V6.of_int64 (pair int64 int64)
  |> set_print Ipaddr.V6.to_string

let ipv4_as_v6 =
  let open QCheck in
  map Ipaddr.v6_of_v4 ipv4 |> set_print Ipaddr.V6.to_string

let addr_port_id =
  let gen =
    let open Gen in
    let open P2p_point.Id in
    let* addr = map Ipaddr.V4.to_string @@ gen ipv4
    and* port = opt @@ gen Lib_test.Qcheck_helpers.uint16 in
    pure {addr; port; peer_id = None}
  in
  make gen ~print:P2p_point.Id.addr_port_id_to_string

let port = uint16

let port_opt = QCheck.option port

(* could not craft a [p2p_identity QCheck.gen], we use instead a
   constant [unit -> p2p_identity] which will be applied at each
   testing points. *)

let peer_id =
  QCheck.option QCheck.(map P2p_identity.generate_with_pow_target_0 unit)

let ip = QCheck.choose [ipv4_as_v6; ipv6]

let ipv4_as_v6_or_v6 = QCheck.choose [ipv4_as_v6; ipv6]

let ipv4t = QCheck.triple ipv4 port_opt peer_id

let ipv6t = QCheck.triple ipv6 port_opt peer_id

let p2p_point_id_t = QCheck.pair ip port
