(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

type fork_parameter = {
  store_root : string;
  context_root : string;
  protocol_root : string;
  chain_id : Chain_id.t;
  block_header : Block_header.t;
  predecessor_block_header : Block_header.t;
  operations : Operation.t list list;
  max_operations_ttl : int;
}

let fork_parameters_encoding =
  let open Data_encoding in
  conv
    (fun { store_root;
           context_root;
           protocol_root;
           chain_id;
           block_header;
           predecessor_block_header;
           operations;
           max_operations_ttl } ->
      ( store_root,
        context_root,
        protocol_root,
        chain_id,
        block_header,
        predecessor_block_header,
        operations,
        max_operations_ttl ))
    (fun ( store_root,
           context_root,
           protocol_root,
           chain_id,
           block_header,
           predecessor_block_header,
           operations,
           max_operations_ttl ) ->
      {
        store_root;
        context_root;
        protocol_root;
        chain_id;
        block_header;
        predecessor_block_header;
        operations;
        max_operations_ttl;
      })
    (obj8
       (req "store_root" (dynamic_size string))
       (req "context_root" (dynamic_size string))
       (req "protocol_root" (dynamic_size string))
       (req "chain_id" (dynamic_size Chain_id.encoding))
       (req "block_header" (dynamic_size Block_header.encoding))
       (req "pred_header" (dynamic_size Block_header.encoding))
       (req "operations" (list @@ list @@ dynamic_size Operation.encoding))
       (req "max_operations_ttl" (dynamic_size int31)))

let data_size msg = String.length msg

let send pin msg =
  Lwt_io.write_int pin (data_size msg) >>= fun () -> Lwt_io.write pin msg

let recv pout =
  Lwt_io.read_int pout
  >>= fun count ->
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly pout buf 0 count
  >>= fun () -> Lwt.return @@ Bytes.to_string buf
