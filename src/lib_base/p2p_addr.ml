(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = Ipaddr.V6.t

let encoding =
  let open Data_encoding in
  def "p2p_address" ~description:"An address for locating peers."
  @@ splitted
       ~json:(conv Ipaddr.V6.to_string Ipaddr.V6.of_string_exn string)
       ~binary:(conv Ipaddr.V6.to_octets Ipaddr.V6.of_octets_exn string)

type port = int

let pp ppf addr =
  match Ipaddr.v4_of_v6 addr with
  | Some addr -> Format.fprintf ppf "%a" Ipaddr.V4.pp addr
  | None -> Format.fprintf ppf "[%a]" Ipaddr.V6.pp addr

let of_string_opt str =
  match Ipaddr.of_string str with
  | Ok (Ipaddr.V4 addr) -> Some (Ipaddr.v6_of_v4 addr)
  | Ok (V6 addr) -> Some addr
  | Error (`Msg _) -> None

let of_string_exn str =
  match of_string_opt str with
  | None -> Stdlib.failwith "P2p_addr.of_string"
  | Some t -> t

let of_string str =
  try Ok (of_string_exn str) with
  | Invalid_argument s -> Error s
  | Failure s -> Error s
  | _ -> Error "P2p_point.of_string"

let to_string saddr = Format.asprintf "%a" pp saddr

let rpc_arg =
  RPC_arg.make
    ~name:"addr"
    ~descr:"A network address (ipv4 or [ipv6])."
    ~destruct:of_string
    ~construct:to_string
    ()

let () = Data_encoding.Registration.register ~pp encoding
