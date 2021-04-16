(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Base, P2p
   Invocation:   dune build @src/lib_base/test/runtest_p2p_addr
   Subject:      Check the parsing of addresses with domain names
*)

open Lib_test.Qcheck_helpers

let ipv4 =
  let open QCheck in
  map ~rev:Ipaddr.V4.to_int32 Ipaddr.V4.of_int32 int32
  |> set_print Ipaddr.V4.to_string

let ipv4_as_v6 =
  let open QCheck in
  map Ipaddr.v6_of_v4 ipv4 |> set_print Ipaddr.V6.to_string

(* Do not test the all standard ipv6 *)
let ipv6 =
  let open QCheck in
  map
    ~rev:Ipaddr.V6.to_int32
    (fun (a, b, c, d) -> Ipaddr.V6.of_int32 (a, b, c, d))
    (quad int32 int32 int32 int32)
  |> set_print Ipaddr.V6.to_string

let port = uint16

let port_opt = QCheck.option port

(* could not craft a [p2p_identity QCheck.gen], we use instead a
   constant [unit -> p2p_identity] which will be applied at each
   testing points.  *)

let peer_id =
  QCheck.option QCheck.(map P2p_identity.generate_with_pow_target_0 unit)

let ip = QCheck.choose [ipv4_as_v6; ipv6]

let ipv4_as_v6_or_v6 = QCheck.choose [ipv4_as_v6; ipv6]

let ipv4t = QCheck.triple ipv4 port_opt peer_id

let ipv6t = QCheck.triple ipv6 port_opt peer_id

let p2p_point_id_t = QCheck.pair ip port

(* To check the round trip property we change the printer for ipv4 and
   ipv6. Otherwise the printer returns an ipv4 printed as an ipv6. *)

let pp_addr_port_id fmt {P2p_point.Id.addr; port; peer_id} =
  let open Format in
  let port_s = Option.fold ~none:"" ~some:(asprintf ":%d") port in
  let peer_id_s =
    Option.fold ~none:"" ~some:(asprintf "#%a" P2p_peer_id.pp) peer_id
  in
  fprintf fmt "%s%s%s" addr port_s peer_id_s

let addr_port_id_v4 =
  QCheck.map
    (fun (ip, port, peer_id) ->
      let peer_id = Option.map (fun gen -> gen.P2p_identity.peer_id) peer_id in
      P2p_point.Id.{addr = Ipaddr.V4.to_string ip; port; peer_id})
    ipv4t
  |> QCheck.set_print (Format.asprintf "%a" pp_addr_port_id)

let addr_port_id_v6 =
  QCheck.map
    (fun (ip, port, peer_id) ->
      let peer_id = Option.map (fun gen -> gen.P2p_identity.peer_id) peer_id in
      P2p_point.Id.{addr = P2p_addr.to_string ip; port; peer_id})
    ipv6t
  |> QCheck.set_print (Format.asprintf "%a" pp_addr_port_id)

let remove_brackets addr =
  let len = String.length addr in
  if len > 1 then
    if addr.[0] = '[' && addr.[len - 1] = ']' then String.sub addr 1 (len - 2)
    else addr
  else addr

let eq l r =
  let open P2p_point.Id in
  let eq_addr addrl addrr =
    addrl = addrr
    || remove_brackets addrl = addrr
    || addrl = remove_brackets addrr
  in
  let eq_peer_id idl idr =
    match (idl, idr) with
    | (None, None) ->
        true
    | (Some idl, Some idr) ->
        P2p_peer_id.(idl = idr)
    | _ ->
        false
  in
  eq_addr l.addr r.addr && l.port = r.port && eq_peer_id l.peer_id r.peer_id

let process_points filename f =
  let file = open_in filename in
  try
    while true do
      f (input_line file)
    done
  with End_of_file -> close_in file

let ok_points = process_points "points.ok"

let ko_points = process_points "points.ko"

let ip_to_string_from_string =
  let open QCheck in
  Test.make ~name:"Base.P2p_addr.ip.to-string-from-string" ip (fun t ->
      let open P2p_addr in
      let s = to_string t in
      match of_string_opt s with
      | None ->
          Test.fail_report "cannot parse printed address"
      | Some t' ->
          qcheck_eq' ~pp ~expected:t ~actual:t' ())

let ipv6_to_string_from_string =
  let open QCheck in
  Test.make
    ~name:"Base.P2p_point.addr_port_id.ipv6.to-string-from-string-ok"
    addr_port_id_v6
    (fun t ->
      let open P2p_point.Id in
      let s = addr_port_id_to_string t in
      match parse_addr_port_id s with
      | Error err ->
          Test.fail_report
            (Format.asprintf
               "cannot parse address '%s': %s"
               s
               (P2p_point.Id.string_of_parsing_error err))
      | Ok res ->
          qcheck_eq' ~pp:pp_addr_port_id ~eq ~expected:t ~actual:res ())

let ipv4_to_string_from_string =
  let open QCheck in
  Test.make
    ~name:"Base.P2p_point.addr_port_id.ipv4.to-string-from-string"
    addr_port_id_v4
    (fun t ->
      let open P2p_point.Id in
      let s = addr_port_id_to_string t in
      match parse_addr_port_id s with
      | Error err ->
          Test.fail_report
            (Format.asprintf
               "cannot parse address '%s': %s"
               s
               (P2p_point.Id.string_of_parsing_error err))
      | Ok res ->
          qcheck_eq' ~pp:pp_addr_port_id ~eq ~expected:t ~actual:res ())

(* Data are not generated but retrieved from [point.ok] file *)
let domain_to_string_from_string_ok () =
  let f addr =
    match P2p_point.Id.parse_addr_port_id addr with
    | Error err ->
        Alcotest.fail
          (Format.asprintf
             "cannot parse address '%s': %s"
             addr
             (P2p_point.Id.string_of_parsing_error err))
    | Ok res ->
        Alcotest.(check string)
          "same addresses"
          addr
          (P2p_point.Id.addr_port_id_to_string res)
  in
  ok_points f

(* Data are not generated but retrieved from [point.ko] file *)
let domain_to_string_from_string_ko () =
  let f addr =
    match P2p_point.Id.parse_addr_port_id addr with
    | Error _ ->
        ()
    | Ok _ ->
        Alcotest.fail
          (Format.asprintf
             "Address '%s' was parsed successfully but it is not valid."
             addr)
  in
  ko_points f

let encode_decode =
  let open QCheck in
  Test.make
    ~name:"Base.P2p_point.id.encode-decode roundtrip"
    p2p_point_id_t
    (fun t ->
      let open P2p_point.Id in
      let b = Data_encoding.Binary.to_bytes_exn encoding t in
      let actual = Data_encoding.Binary.of_bytes_exn encoding b in
      qcheck_eq' ~pp ~expected:t ~actual ())

let p2p_addr = [ip_to_string_from_string]

let p2p_point =
  [ipv6_to_string_from_string; ipv4_to_string_from_string; encode_decode]

let tests =
  [ Alcotest.test_case
      "domain_to_string_from_string_ok"
      `Quick
      domain_to_string_from_string_ok;
    Alcotest.test_case
      "domain_to_string_from_string_ko"
      `Quick
      domain_to_string_from_string_ko ]

let () =
  Alcotest.run
    "Base.P2p"
    [ ("P2p_addr", qcheck_wrap p2p_addr);
      ("P2p_point", qcheck_wrap p2p_point);
      ("P2p_point.domain", tests) ]
