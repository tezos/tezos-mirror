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
   Invocation:   dune exec src/lib_base/test/main.exe \
                  -- --file test_p2p_addr.ml
   Subject:      Check the parsing of addresses with domain names
*)

open Qcheck2_helpers

(* Note, this is a duplicate of some functions from
   Tezos_base_test_helpers.Tz_gen. Unfortunately it's impossible to
   reuse them, because that would introduce a cyclic dependency between
   tezos-base-test-helpers and tezos-base packages, which opam would not
   allow. *)
module Generator = struct
  open QCheck2

  let port = uint16

  let port_opt = Gen.opt port

  (* could not craft a [p2p_identity QCheck2.gen], we use instead a
     constant [unit -> p2p_identity] which will be applied at each
     testing points. *)

  let peer_id = Gen.(opt @@ map P2p_identity.generate_with_pow_target_0 unit)

  let ipv4 = Gen.(map Ipaddr.V4.of_int32 int32)

  let ipv6 = Gen.(map Ipaddr.V6.of_int64 (pair int64 int64))

  let ipstring =
    let open Gen in
    oneof [map Ipaddr.V4.to_string ipv4; map Ipaddr.V6.to_string ipv6]

  let ipt = Gen.triple ipstring port_opt peer_id

  let addr_port_id =
    Gen.map
      (fun (ip, port, peer_id) ->
        let peer_id =
          Option.map (fun gen -> gen.P2p_identity.peer_id) peer_id
        in
        P2p_point.Id.{addr = ip; port; peer_id})
      ipt

  let ipv4_as_v6 = Gen.map Ipaddr.v6_of_v4 ipv4

  let ip = Gen.oneof [ipv4_as_v6; ipv6]

  let print_ip = Ipaddr.V6.to_string

  let p2p_point_id_t = Gen.pair ip port

  let print_p2p_point_id_t (ip, port) =
    Format.asprintf "(%s, %d)" (print_ip ip) port
end

open QCheck2

(* To check the round trip property we change the printer for ipv4 and
   ipv6. Otherwise the printer returns an ipv4 printed as an ipv6. *)

let pp_addr_port_id fmt {P2p_point.Id.addr; port; peer_id} =
  let open Format in
  fprintf fmt "%s" addr ;
  Option.iter (fprintf fmt ":%d") port ;
  Option.iter (fprintf fmt "#%a" P2p_peer_id.pp) peer_id

let print_addr_port_id = Format.asprintf "%a" pp_addr_port_id

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
    | None, None -> true
    | Some idl, Some idr -> P2p_peer_id.(idl = idr)
    | _ -> false
  in
  eq_addr l.addr r.addr && l.port = r.port && eq_peer_id l.peer_id r.peer_id

let process_points filename f =
  let file = open_in (project_root // "src/lib_base/test" // filename) in
  try
    while true do
      f (input_line file)
    done
  with End_of_file -> close_in file

let ok_points = process_points "points.ok"

let ko_points = process_points "points.ko"

let ip_to_string_from_string =
  Test.make
    ~name:"Base.P2p_addr.ip.to-string-from-string"
    ~print:Generator.print_ip
    Generator.ip
    (fun t ->
      let open P2p_addr in
      let s = to_string t in
      match of_string_opt s with
      | None -> Test.fail_report "cannot parse printed address"
      | Some t' -> qcheck_eq' ~pp ~expected:t ~actual:t' ())

let point_to_string_from_string =
  Test.make
    ~name:"Base.P2p_point.addr_port_id.to-string-from-string"
    ~print:print_addr_port_id
    Generator.addr_port_id
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
      | Ok res -> qcheck_eq' ~pp:pp_addr_port_id ~eq ~expected:t ~actual:res ())

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
    | Error _ -> ()
    | Ok _ ->
        Alcotest.fail
          (Format.asprintf
             "Address '%s' was parsed successfully but it is not valid."
             addr)
  in
  ko_points f

let encode_decode =
  Test.make
    ~name:"Base.P2p_point.id.encode-decode roundtrip"
    ~print:Generator.print_p2p_point_id_t
    Generator.p2p_point_id_t
    (fun t ->
      let open P2p_point.Id in
      let b = Data_encoding.Binary.to_bytes_exn encoding t in
      let actual = Data_encoding.Binary.of_bytes_exn encoding b in
      qcheck_eq' ~pp ~expected:t ~actual ())

(** Test that reading a point in streaming fails fast in case of invalid size, i.e.
    without even reading the data.

    The max size of a binary encoded {!P2p_point.Id.t} is the same logic as in {!P2p_point.Id.encoding}
    (51) minus the 4 bytes used in the header to store the size, at the beginning (hence 47). This
    corresponds to an IPv6 with port and the optional enclosing brackets. *)
let p2p_point_encoding_eager_fail =
  let max_binary_size_point = 47 in
  let max_possible_size_on_4_bytes = Int32.(to_int max_int) in
  Test.make
    ~name:"Base.P2p_point.Id.encoding eagerly fails on too big input"
    ~print:string_of_int
    Gen.(max_binary_size_point + 1 -- max_possible_size_on_4_bytes)
    (fun excessive_size ->
      (* The size header is stored on 4 bytes (the 4 bytes we remove from the
         total size of [max_binary_size_point] compared to the max size coded
         in the encoding). *)
      let size_header = Bytes.create 4 in
      Bytes.set_int32_be size_header 0 (Int32.of_int excessive_size) ;
      let continue =
        match Data_encoding.Binary.read_stream P2p_point.Id.encoding with
        | Await continue -> continue
        | _ ->
            Test.fail_report
              "Opening an empty reading stream should await for further input, \
               but status is already Success or Error"
      in
      match continue size_header with
      | Error Data_encoding.Binary.Size_limit_exceeded -> true
      | _ ->
          Test.fail_report
            "Binary stream reading of a point should fail fast when the size \
             is too big")

let p2p_addr = [ip_to_string_from_string]

let p2p_point =
  [point_to_string_from_string; encode_decode; p2p_point_encoding_eager_fail]

let tests =
  [
    Alcotest.test_case
      "domain_to_string_from_string_ok"
      `Quick
      domain_to_string_from_string_ok;
    Alcotest.test_case
      "domain_to_string_from_string_ko"
      `Quick
      domain_to_string_from_string_ko;
  ]

let () =
  Alcotest.run
    ~__FILE__
    "Base.P2p"
    [
      ("P2p_addr", qcheck_wrap p2p_addr);
      ("P2p_point", qcheck_wrap p2p_point);
      ("P2p_point.domain", tests);
    ]
