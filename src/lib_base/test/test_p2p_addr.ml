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

let ipv4 =
  let open Crowbar in
  map [Crowbar.int32] Ipaddr.V4.of_int32

let ipv4_as_v6 =
  let open Crowbar in
  map [int32; int32; int32; int32] (fun a b c d ->
      Ipaddr.V6.of_int32 (a, b, c, d))

(* Do not test the all standard ipv6 *)
let ipv6 =
  let open Crowbar in
  map
    [int16; int16; int16; int16; int16; int16; int16; int16]
    (fun a b c d e f g h -> Ipaddr.V6.make a b c d e f g h)

let port = Crowbar.option Crowbar.uint16

(* could not craft a [p2p_identity Crowbar.gen], we use instead a
   constant [unit -> p2p_identity] which will be applied at each
   testing points.  *)

let peer_id =
  Crowbar.option (Crowbar.const P2p_identity.generate_with_pow_target_0)

let none_int : int option Crowbar.gen = Crowbar.const None

let none_peer_id : (unit -> P2p_identity.t) option Crowbar.gen =
  Crowbar.const None

let ip = Crowbar.choose [ipv4_as_v6; ipv6]

let ip' ip = Crowbar.pair (Crowbar.pair ip none_int) none_peer_id

let ip_port ip = Crowbar.pair (Crowbar.pair ip port) none_peer_id

let ip_peer_id ip = Crowbar.pair (Crowbar.pair ip none_int) peer_id

let ip_port_peer_id ip = Crowbar.pair (Crowbar.pair ip port) peer_id

let ipv4t =
  Crowbar.choose
    ((List.map (fun gen -> gen ipv4))
       [ip'; ip_port; ip_peer_id; ip_port_peer_id])

let ipv6t =
  Crowbar.choose
    ((List.map (fun gen -> gen ipv6))
       [ip'; ip_port; ip_peer_id; ip_port_peer_id])

(* To check the round trip property we change the printer for ipv4 and
   ipv6. Otherwise the printer returns an ipv4 printed as an ipv6. *)

let to_addr_port_id_v4 ((ip, port), peer_id) =
  let peer_id =
    Option.map (fun gen -> (gen ()).P2p_identity.peer_id) peer_id
  in
  P2p_point.Id.{addr = Ipaddr.V4.to_string ip; port; peer_id}

let to_addr_port_id_v6 ((ip, port), peer_id) =
  let peer_id =
    Option.map (fun gen -> (gen ()).P2p_identity.peer_id) peer_id
  in
  P2p_point.Id.{addr = P2p_addr.to_string ip; port; peer_id}

let pp_addr_port_id fmt {P2p_point.Id.addr; port; peer_id} =
  match (port, peer_id) with
  | (None, None) ->
      Format.fprintf fmt "%s" addr
  | (None, Some peer_id) ->
      Format.fprintf fmt "%s#%a" addr P2p_peer_id.pp peer_id
  | (Some port, None) ->
      Format.fprintf fmt "%s:%d" addr port
  | (Some port, Some peer_id) ->
      Format.fprintf fmt "%s:%d#%a" addr port P2p_peer_id.pp peer_id

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

let unit_gen = Crowbar.const ()

let () =
  (* Property:
     forall [a]: [t],
       [to_string]/[of_string_opt] roundtrip modulo option. *)
  Crowbar.add_test
    ~name:"Base.P2p_addr.ip.to-string-from-string"
    [ip]
    (fun t ->
      let open P2p_addr in
      let s = to_string t in
      match of_string_opt s with
      | None ->
          Crowbar.fail "cannot parse printed address"
      | Some t' ->
          Crowbar.check_eq ~pp t t') ;
  Crowbar.add_test
    ~name:"Base.P2p_point.addr_port_id.ipv6.to-string-from-string-ok"
    [ipv6t]
    (fun t ->
      let open P2p_point.Id in
      let t' = to_addr_port_id_v6 t in
      let s = addr_port_id_to_string t' in
      match parse_addr_port_id s with
      | Error err ->
          Crowbar.fail
            (Format.asprintf
               "cannot parse address '%s': %s"
               s
               (P2p_point.Id.string_of_parsing_error err))
      | Ok res ->
          Crowbar.check_eq ~pp:pp_addr_port_id ~eq t' res) ;
  Crowbar.add_test
    ~name:"Base.P2p_point.addr_port_id.ipv4.to-string-from-string"
    [ipv4t]
    (fun t ->
      let open P2p_point.Id in
      let t' = to_addr_port_id_v4 t in
      let s = addr_port_id_to_string t' in
      match parse_addr_port_id s with
      | Error err ->
          Crowbar.fail
            (Format.asprintf
               "cannot parse address '%s': %s"
               s
               (P2p_point.Id.string_of_parsing_error err))
      | Ok res ->
          Crowbar.check_eq ~pp:pp_addr_port_id t' res) ;
  (* We use crowbar for uniformity but it is not necessary here, data
     are not generated but retrieved from [point.ok] file   *)
  Crowbar.add_test
    ~name:"Base.P2p_point.addr_port_id.domain.ok.from-string-to-string"
    [unit_gen]
    (fun _ ->
      let f addr =
        match P2p_point.Id.parse_addr_port_id addr with
        | Error err ->
            Crowbar.fail
              (Format.asprintf
                 "cannot parse address '%s': %s"
                 addr
                 (P2p_point.Id.string_of_parsing_error err))
        | Ok res ->
            Crowbar.check_eq addr (P2p_point.Id.addr_port_id_to_string res)
      in
      ok_points f) ;
  (* We use crowbar for uniformity but it is not necessary here, data
     are not generated but retrieved from [point.ko] file *)
  Crowbar.add_test
    ~name:"Base.P2p_point.addr_port_id.domain.ko.from-string-to-string"
    [unit_gen]
    (fun _ ->
      let f addr =
        match P2p_point.Id.parse_addr_port_id addr with
        | Error _ ->
            Crowbar.check true
        | Ok _ ->
            Crowbar.fail
              (Format.asprintf
                 "Address '%s' was parsed successfully but it is not valid."
                 addr)
      in
      ko_points f) ;
  Crowbar.add_test
    ~name:"Base.P2p_point.id.encode-decode"
    [Crowbar.pair ip Crowbar.uint16]
    (fun t ->
      let open P2p_point.Id in
      let len = Data_encoding.Binary.length encoding t in
      let buf = Bytes.create len in
      let state =
        Stdlib.Option.get
        @@ Data_encoding.Binary.make_writer_state
             buf
             ~offset:0
             ~allowed_bytes:len
      in
      match Data_encoding.Binary.write encoding t state with
      | Ok len -> (
          let buf = Bytes.unsafe_to_string buf in
          match Data_encoding.Binary.read_opt encoding buf 0 len with
          | None ->
              Crowbar.fail "cannot parse encoded  address"
          | Some (_, t') ->
              Crowbar.check_eq ~pp t t' )
      | Error _ ->
          Crowbar.fail "cannot encode address")
