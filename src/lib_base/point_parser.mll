(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This file parses a string encoding a point with an expected peer
   id.  Because the address part of the point may be given with a
   domain we keep this field as a string. The string will be
   eventually given to `getaddrinfo` and therefore we remove brackets
   around ipv6 address. The expected peer id is given using the b58
   format. We also ensure that the port is between the range
   [0-65535]. *)

{

  type addr_port_id = {
    addr : string;
    port : int option;
    peer_id : P2p_peer_id.t option;
  }

  type parsing_error =
    | Port_not_in_range of int
    | Bad_id_format of string
    | Bad_format

  let string_of_parsing_error = function
    | Port_not_in_range p ->
      Format.asprintf "port '%d' not in range [0-65535]" p
    | Bad_id_format s ->
      Format.asprintf "peer id '%s' is not valid (b58 format expected)" s
    | Bad_format ->
      Format.asprintf "expected <addr>[:<port>][#<peer_id>]"

  let parse_port p k =
      match p with
      | None -> k None
      | Some p ->
            let p = int_of_string p in
      	    if 0 <= p && p <= 65535 then
	          k (Some p)
	   else
                  Error (Port_not_in_range p)


  let parse_peer_id peer_id k =
      match peer_id with
      | None -> k None
      | Some peer_id ->
            match P2p_peer_id.of_b58check_opt peer_id with
      	    | None -> Error (Bad_id_format peer_id)
	    | Some id -> k (Some id)

  (* Remove square brackets for ipv6 addresses *)
  let parse_addr addr k =
      let len = String.length addr in
      if len > 0 && String.get addr 0 = '[' && String.get addr (len - 1) = ']' then
           k (String.sub addr 1 (len - 2))
      else
           k addr

  let make_full_addr addr port peer_id =
    parse_port port (fun port ->
    parse_peer_id peer_id (fun peer_id ->
    parse_addr addr (fun addr ->
    Ok {addr;port;peer_id})))

}

let peer_id = ['a'-'z' 'A'-'Z' '0'-'9']+

let empty = ['_']?

let domain = ['a'-'z' 'A'-'Z' '0' - '9' '.' '-']+

let ipv4seg = ['0' - '9'] ['0' - '9']? ['0' - '9']?

let ipv4 = ipv4seg '.' ipv4seg '.' ipv4seg '.' ipv4seg

let ipv6_char = ['a'-'f' 'A'-'F' '0'-'9' ':' '.']

let ipv6 = '[' ipv6_char+ ']'

let port = ['0'-'9']+

let addr = ipv4 | ipv6 | domain | empty

rule parse_full_addr = parse
    | (addr as addr) (':' (port as port))? ('#' (peer_id as peer_id))? eof
    {make_full_addr addr port peer_id}
    | _ + eof
    {Error Bad_format}
