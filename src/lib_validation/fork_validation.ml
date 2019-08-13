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

type parameters = {context_root : string; protocol_root : string}

type request = {
  chain_id : Chain_id.t;
  block_header : Block_header.t;
  predecessor_block_header : Block_header.t;
  operations : Operation.t list list;
  max_operations_ttl : int;
}

let magic = MBytes.of_string "TEZOS_FORK_VALIDATOR_MAGIC_0"

let parameters_encoding =
  let open Data_encoding in
  conv
    (fun {context_root; protocol_root} -> (context_root, protocol_root))
    (fun (context_root, protocol_root) -> {context_root; protocol_root})
    (obj2 (req "context_root" string) (req "protocol_root" string))

let request_encoding =
  let open Data_encoding in
  conv
    (fun { chain_id;
           block_header;
           predecessor_block_header;
           max_operations_ttl;
           operations } ->
      ( chain_id,
        block_header,
        predecessor_block_header,
        max_operations_ttl,
        operations ))
    (fun ( chain_id,
           block_header,
           predecessor_block_header,
           max_operations_ttl,
           operations ) ->
      {
        chain_id;
        block_header;
        predecessor_block_header;
        max_operations_ttl;
        operations;
      })
    (obj5
       (req "chain_id" Chain_id.encoding)
       (req "block_header" (dynamic_size Block_header.encoding))
       (req "pred_header" (dynamic_size Block_header.encoding))
       (req "max_operations_ttl" int31)
       (req "operations" (list (list (dynamic_size Operation.encoding)))))

let data_size msg = String.length msg

let send pin encoding data =
  let msg = Data_encoding.Binary.to_bytes_exn encoding data in
  let msg = MBytes.to_string msg in
  Lwt_io.write_int pin (data_size msg) >>= fun () -> Lwt_io.write pin msg

let recv_result pout encoding =
  Lwt_io.read_int pout
  >>= fun count ->
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly pout buf 0 count
  >>= fun () ->
  Lwt.return
    (Data_encoding.Binary.of_bytes_exn
       (Error_monad.result_encoding encoding)
       (MBytes.of_string (Bytes.to_string buf)))

let recv pout encoding =
  Lwt_io.read_int pout
  >>= fun count ->
  let buf = Bytes.create count in
  Lwt_io.read_into_exactly pout buf 0 count
  >>= fun () ->
  Lwt.return
    (Data_encoding.Binary.of_bytes_exn
       encoding
       (MBytes.of_string (Bytes.to_string buf)))
