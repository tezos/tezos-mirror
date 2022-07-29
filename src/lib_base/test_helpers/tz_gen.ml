(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Lib_test.Qcheck2_helpers
open QCheck2.Gen

let ipv4 = map Ipaddr.V4.of_int32 int32

let ipv6 = map Ipaddr.V6.of_int64 (pair int64 int64)

let ipv4_as_v6 = map Ipaddr.v6_of_v4 ipv4

let addr_port_id =
  let open P2p_point.Id in
  let* addr = map Ipaddr.V4.to_string ipv4
  and* port = opt Lib_test.Qcheck2_helpers.uint16 in
  pure {addr; port; peer_id = None}

let port = uint16

let port_opt = opt port

(* could not craft a [p2p_identity QCheck.gen], we use instead a
   constant [unit -> p2p_identity] which will be applied at each
   testing points. *)

let peer_id = opt (map P2p_identity.generate_with_pow_target_0 unit)

let ip = oneof [ipv4_as_v6; ipv6]

let ipv4_as_v6_or_v6 = oneof [ipv4_as_v6; ipv6]

let ipv4t = triple ipv4 port_opt peer_id

let ipv6t = triple ipv6 port_opt peer_id

let p2p_point_id_t = pair ip port
